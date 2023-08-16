# Load the 'sf' library for working with spatial data
library(sf)

sf_use_s2(TRUE)

# Read spatial points data from a geojson file
points <- read_sf("data/points.csv")
points <- st_as_sf(points, coords = c("X", "Y"), crs = 4326)

# Read Landsat tiles data from a geopackage file
landsat_tiles <- read_sf("data/TilesWorld.gpkg", layer = "L8L9")

# Loop through each point in 'points'
for (i in 1:nrow(points)) {
  
  # Find intersecting Landsat tiles with the current point
  l89_intersectss <- st_intersects(points[i, ]$geometry, 
                                   landsat_tiles$geom, 
                                   sparse = FALSE)[1, ]
  
  # Select the intersecting Landsat tile and transform its projection
  landsat_tile <- landsat_tiles[l89_intersectss, ] %>%
    st_transform("+proj=robin")
  
  # Create a buffer around the current point and transform its projection
  buffer <- st_transform(points[i, ], "+proj=robin") %>%
    st_buffer(11520 / 2, endCapStyle = "SQUARE")
  
  # Check if the Landsat tile contains each buffer
  check <- sapply(1:nrow(landsat_tile), 
                  function(x) st_contains(landsat_tile[x, ], buffer, sparse = FALSE)[1, ])
  
  # Filter Landsat tiles based on buffer containment
  landsat_tile <- landsat_tile[check, ]
  
  # Update the 'PR' column in the 'points' dataframe
  if (nrow(landsat_tile) == 0) {
    points[i, "PR"] <- NA
    next
  } else {
    points[i, "PR"] <- paste(paste0(landsat_tile$PATH, landsat_tile$ROW), collapse = "-")
  }
  print(i)
}

# Convert the 'points' dataframe to an 'sf' object and filter out rows with NA in 'PR'
sf_points <- st_as_sf(points, coords = c("X", "Y"), crs = 4326)
sf_points <- sf_points[!is.na(sf_points$PR), ]

# Write the 'sf_points' to a geojson file
# st_write(sf_points, "data/points.geojson")

