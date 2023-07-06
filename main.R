# /*
# MIT License
#
# Copyright (c) [2023] [AndesDataCube team]
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.



# Load required libraries
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling

# Load the utility functions from 'utils.R' file
source("utils.R")

# Initialize EE
ee_Initialize()

# Load initial dataset
metadata <- read_sf("Data/s2landsatpairs.geojson")

# Create metadata table for Landsat 8 (L8), Landsat 9 (L9) OLI, and Sentinel-2 MSI images with a time difference of 10 minutes
container <- list()

for (index in 1:5) { # nrow(metada)
  # Print the index value
  print(index)

  # Get the coordinate data for the current row
  coordinate <- metadata[index, ]

  # Get metadata for satellite images
  container[[index]] <- get_metadata(
    sensorMSI = "COPERNICUS/S2_HARMONIZED",
    sensorOLI8T1 = "LANDSAT/LC08/C02/T1_TOA",
    sensorOLI8T2 = "LANDSAT/LC08/C02/T2_TOA",
    sensorOLI9T1 = "LANDSAT/LC09/C02/T1_TOA",
    sensorOLI9T2 = "LANDSAT/LC09/C02/T2_TOA",
    timediff = 10, # Set the time difference to 10 minutes
    point = coordinate
  )

  # # Download satellite images
  # if (sum(is.na(container[[index]])) == 0) {
  #   for (x in 1:nrow(container[[index]])) {
  #     download(
  #       img1 = container[[index]][x, ]$msi_id,
  #       img2 = container[[index]][x, ]$oli_id,
  #       point = coordinate,
  #       output = "Results"
  #     )
  #   }
  # } else {
  #   print(sprintf("Point %d - %s has no matches", index, coordinate$s2tile))
  # }
}
# Combine the metadata from all the containers into a single table
id_metadata <- do.call(rbind, container)
id_metadata <- id_metadata[!is.na(id_metadata$msi_id),]
write.csv(id_metadata, "exports/metadata.csv", row.names = F)