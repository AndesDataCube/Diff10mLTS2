library(rgee)
library(sf)

ee_Initialize()



point_coordinates <- function(point, proj) {
  Coords <- ee$Feature(point)$
    transform(proj$crs())$
    geometry()$
    coordinates()
}



init_coord_image <- function(axis, proj) {
  name <- NULL
  if (axis == "x") {
    name <- "elt_0_2"
  } else if (axis == "y") {
    name <- "elt_1_2"
  } else {
    stop("Axis must be 'x' or 'y'")
  }
  ee$Number$parse(
    ee$String(
      proj$transform()$
        match(sprintf('"%s", (-?\\d+\\.?\\d*)', name))$
        get(1)
    )
  )
}



coord_transfer <- function(coord, coord_ini, scale) {
  coord_ini$
    add(coord$
          add(coord_ini$multiply(-1))$
          multiply(1 / scale)$
          round()$
          multiply(scale))$
    add(scale / 2)
}



get_trans_coords <- function(point) {
  ee_point <- sf_as_ee(point$geometry)
  ee_ic_ref <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>% 
    ee$ImageCollection$filterBounds(ee_point) %>% 
    ee$ImageCollection$filter(ee$Filter$eq('MGRS_TILE', point$s2tile)) %>% 
    ee$ImageCollection$first()
  proj <- ee_ic_ref$select("B2")$projection()
  Coords <- point_coordinates(ee_point, proj)
  x <- ee$Number(Coords$get(0))
  y <- ee$Number(Coords$get(1))
  x_ini <- init_coord_image("x", proj)
  y_ini <- init_coord_image("y", proj)
  new_x <- coord_transfer(x, x_ini, 10)
  new_y <- coord_transfer(y, y_ini, 10)
  ee_ic_ref$set(list("new_x" = new_x))$
    set(list("new_y" = new_y))$
    set(list("new_crs" = ee$Number$parse(proj$crs()$match("EPSG:(\\d+)")$get(1))))
}


points <- read_sf("Data/s2landsatpairs.geojson")
container <- list()
for(i in 1:nrow(points)) {
  print(i)
  container[[i]] <-  get_trans_coords(points[i, ])
}
imgs_container <- ee$ImageCollection$fromImages(container)
df <- data.frame(
  ee$Dictionary$
    fromLists(
      c("new_x", 
        "new_y",
        "new_crs"), 
      c(imgs_container$aggregate_array("new_x"), 
        imgs_container$aggregate_array("new_y"),
        imgs_container$aggregate_array("new_crs")))$getInfo()
)



a1 <- get_crs(points[1, ])
a2 <- get_crs(points[2, ])

ee$ImageCollection$fromImages(container)$aggregate_array("new_x")$getInfo()
ee$ImageCollection$fromImages(container)$aggregate_array("new_y")$getInfo()


ee_point <- points[8, ]$geometry %>% 
  sf_as_ee()
ee_ic_ref <- ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>% 
  ee$ImageCollection$filterBounds(ee_point) %>% 
  ee$ImageCollection$filter(ee$Filter$eq('MGRS_TILE', "12SYH")) %>% 
  ee$ImageCollection$first()
proj <- ee_ic_ref$select("B2")$projection()
ee_point_utm <- point_coordinates(ee_point, proj)


point_coordinates <- function(point, proj) {
  Coords <- point$
    transform(proj$crs())$
    geometry()$
    coordinates()
}




Coords <- point_coordinates(ee_point_utm, proj)
x <- ee$Number(Coords$get(0))
y <- ee$Number(Coords$get(1))
x_ini <- init_coord_image("x", proj)
y_ini <- init_coord_image("y", proj)
new_x <- coord_transfer(x, x_ini, 10)
new_y <- coord_transfer(y, y_ini, 10)
ee_ic_ref$
  set(list("new_x" = new_x))$
  set(list("new_y" = new_y))






# obtain a sf or sfc object
coord <- st_point(c(54.0984170753486, 63.64421296953)) %>% st_sfc(crs=4326)

# Obtain the planar local CRS for that specific point
crs_point <- get_crs(coord)

st_transform(coord, crs_point)




s2_tiles <- read_sf("Data/TilesWorld.gpkg", layer = "Sentinel2") %>% 
  st_transform(crs = 3857)
lt_tiles <- read_sf("Data/TilesWorld.gpkg", layer = "L8L9") %>% 
  st_transform(crs = 3857)
points <- read_sf("Data/s2landsatpairs.geojson") %>% 
  st_transform(crs = 3857)
buffers <- st_buffer(x = points, dist = sqrt(2) * (10240 / 2))
nrow(points)
index <- c()
for (i in 1:nrow(points)) {
  s2 <- s2_tiles[unlist(st_within(buffers[i, ], s2_tiles)), ]
  lt <- lt_tiles[unlist(st_within(buffers[i, ], lt_tiles)), ]
  if(nrow(s2) > 0 & nrow(lt) > 0) {
    index <- c(index, i)
  }
  print(i)
}

points_selected <- points[index, ]
st_write(points_selected, "Data/points_f1.geojson")

index <- c()
for (i in 1:nrow(points)) {
  s2_i <- s2_tiles[unlist(st_within(points[i, ], s2_tiles)), ]
  lt_i <- lt_tiles[unlist(st_within(points[i, ], lt_tiles)), ]
  s2 <- s2_tiles[unlist(st_within(buffers[i, ], s2_tiles)), ]
  lt <- lt_tiles[unlist(st_within(buffers[i, ], lt_tiles)), ]
  if((nrow(s2) > 0 & nrow(s2_i) == nrow(s2)) & (nrow(lt) > 0 & nrow(lt_i) == nrow(lt))) {
    index <- c(index, i)
  }
  print(i)
}

