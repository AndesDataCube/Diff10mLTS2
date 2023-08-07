library(sf)
library(rgee)
library(spatstat)
library(rnaturalearth)

sf_use_s2(FALSE)

# 1. Read the data
s2_tiles <- read_sf("/home/csaybar/Downloads/TilesWorld_new.gpkg", layer = "Sentinel2")
landsat_tiles <- read_sf("/home/csaybar/Downloads/TilesWorld.gpkg", layer = "L8L9")

# get a world map
world <- ne_countries(scale = "small", returnclass = "sf")
world <- world[world$name != "Antarctica",]
world <- st_union(st_cast(st_make_valid(world$geom), "POLYGON"))

points <- st_read("Data/s2landsatpairs.geojson")

mapview::mapview(points)
# 2. We need a planar projection to use spatstat (i'm using mercator, probably no the best idea)
# I'm doing the searching point in a simple geometry (st_bbox) because it will be faster
roi <- world %>%
    st_transform(crs = 3857) %>% 
    st_bbox() %>% 
    st_as_sfc()
    
# 3. Generate a point pattern using the hard-core model
set.seed(100)
params <- list(beta=100, hc=5*(1000))
model <- rmhmodel(cif="hardcore",  par = params, w = as.owin(roi))
results <- rmh(model, start = list(n.start = 50000))


# 4. Convert the results to sf in EPSG:4326
points <- st_as_sf(results)[2:results$n,] %>%
  st_set_crs(3857) %>%
  st_transform(4326)
points_intersected <- st_intersection(points$geom, world)


# 5. Check if the point is in a two EPSG region
container <- list()
counter <- 0
for (index in 6966:length(points_intersected)) {
    print(index)
    point <- points_intersected[index]

    ## Intersect one point with the S2 tiles
    s2_intersectss <- st_intersects(point, s2_tiles$geom, sparse = FALSE)[1,]

    ## If the point is not in a S2 tile, we can skip it
    if (sum(s2_intersectss) == 0) {
      next
    }

    ## Intersect one point with the L8L9 tiles
    l89_intersectss <- st_intersects(point, landsat_tiles$geom, sparse = FALSE)[1,]

    ## If the point is not in a L8L9 tile, we can skip it
    if (sum(l89_intersectss) == 0) {
      next
    }

    ## From WGS84 to robinson (equal area)
    s2_tile <- s2_tiles[s2_intersectss,] %>% 
      st_transform("+proj=robin")
    s2_tile <- s2_tile[1,]

    landsat_tile <- landsat_tiles[l89_intersectss,] %>% 
      st_transform("+proj=robin")
    
    ## Create a 11520 x 11520 meters buffer 
    point_utm <- st_transform(point, "+proj=robin") %>% 
      st_buffer(6000, endCapStyle = "SQUARE")

    ## Is the point_utm within the s2_tile?
    check01 <- st_contains(s2_tile, point_utm, sparse = FALSE)[1]
    check02 <- sapply(1:nrow(landsat_tile), function(x) st_contains(landsat_tile[x,], point_utm, sparse = FALSE)[1])
    
    ## If the ROI is not within the tile, we can skip it
    if (!check01) {
      next
    }

    if (sum(check02) == 0) {
      next
    }

    ## Create the metadata
    l8data <- landsat_tile[which(check02)[1],]
    s2data <- s2_tile
    
    df <- data.frame(
      s2tile = s2data$Name,
      l89_path = l8data$PATH,
      l89_row = l8data$ROW,
      x = st_coordinates(point)[,"X"],
      y = st_coordinates(point)[,"Y"]
    )
    counter <- counter + 1
    container[[counter]] <- df
}

do.call(rbind, container) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
  st_write("/home/csaybar/s2landsatpairs.geojson", layer = "points")
