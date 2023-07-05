# Load required libraries
library(lubridate) # For date and time manipulation
library(rgeeExtra) # Extended functionality for Google Earth Engine
library(dplyr) # Data manipulation and transformation
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling


#' Check if a DataFrame is empty
#'
#' This function checks if a DataFrame is empty by evaluating the number of rows.
#'
#' @param df The DataFrame to be checked.
#'
#' @return TRUE if the DataFrame is empty, FALSE otherwise.
check_01 <- function(df) {
  if (nrow(df) == 0) {
    TRUE
  } else {
    FALSE
  }
}

#' Get Metadata
#'
#' This function retrieves metadata from different image collections based on specified parameters.
#'
#' @param sensorMSI The sensor ID for the MSI image collection.
#' @param sensorOLI8T1 The sensor ID for the OLI8 T1 image collection.
#' @param sensorOLI8T2 The sensor ID for the OLI8 T2 image collection.
#' @param sensorOLI9T1 The sensor ID for the OLI9 T1 image collection.
#' @param sensorOLI9T2 The sensor ID for the OLI9 T2 image collection.
#' @param timediff The time difference in minutes used for image filtering.
#' @param point The spatial point used as the filter bounds.
#' @param output The output directory for the resulting metadata.
#'
#' @return A data frame containing the retrieved metadata, including MSI ID, OLI ID, ROI ID, and time difference.
get_metadata <- function(sensorMSI, sensorOLI8T1, sensorOLI8T2, sensorOLI9T1, sensorOLI9T2, timediff, point, output) {
  # From local to EE server
  ee_point <- sf_as_ee(point$geometry)

  # Get the dates of the MSI image
  msi_ic <- ee$ImageCollection(sensorMSI) %>%
    ee$ImageCollection$filterBounds(ee_point) %>%
    ee_get_date_ic()

  # Check if there is any image in the collection
  if (check_01(msi_ic)) {
    return(NULL)
  }

  # Get the dates of the OLI8 images
  oli8_ic1 <- ee$ImageCollection(sensorOLI8T1) %>%
    ee$ImageCollection$filterBounds(ee_point) %>%
    ee_get_date_ic()
  oli8_ic2 <- ee$ImageCollection(sensorOLI8T2) %>%
    ee$ImageCollection$filterBounds(ee_point) %>%
    ee_get_date_ic()

  # Get the dates of the OLI9 images
  oli9_ic1 <- ee$ImageCollection(sensorOLI9T1) %>%
    ee$ImageCollection$filterBounds(ee_point) %>%
    ee_get_date_ic()
  oli9_ic2 <- ee$ImageCollection(sensorOLI9T2) %>%
    ee$ImageCollection$filterBounds(ee_point) %>%
    ee_get_date_ic()

  # Merge OLI
  oli_ic <- rbind(oli8_ic1, oli8_ic2, oli9_ic1, oli9_ic2)

  # Check if there is any image in the collection
  if (check_01(oli_c)) {
    return(NULL)
  }

  # Get the images on the same time interval (timediff)
  r_collocation <- sapply(
    X = oli_ic$time_start,
    FUN = function(x) {
      vresults <- abs(as.numeric(msi_ic$time_start - x, units = "mins"))
      c(min(vresults), which.min(vresults))
    }
  )
  r_difftime <- r_collocation[1, ]
  idx_mss <- r_collocation[2, ]

  # Check if the images are on the time interval (timediff)
  if (sum(r_difftime < timediff) == 0) {
    return(NULL)
  }

  # Create the final dataset
  final_oli_ic <- oli_ic[r_difftime < timediff, ]
  final_time <- r_difftime[r_difftime < timediff]
  final_msi_ic <- msi_ic[idx_mss[r_difftime < timediff], ]

  # Return the metadata
  data.frame(
    msi_id = final_msi_ic$id,
    oli_id = final_oli_ic$id,
    roi_id = point$s2tile,
    dif_time = round(final_time, 1)
  )
}

#' Download Earth Engine Images
#'
#' This function downloads Earth Engine images based on the specified parameters.
#' It also applies an algorithm to adjust the geotransform of Sentinel-2 images to
#' ensure proper alignment and prevent errors in the downloaded images.
#'
#' @param img1 The Earth Engine image ID for the Sentinel-2 image.
#' @param img2 The Earth Engine image ID for the Landsat image.
#' @param point The spatial point used as the reference for the download.
#' @param output The output directory for saving the downloaded images.
#'
#' @return A list containing the downloaded satellite data for both Sentinel 2 MSI and Landsat 8 9 OLI
download <- function(img1, img2, point, output) {
  # Read Earth Engine images Select the bands: Red, Green, Blue, NIR
  S2 <- ee$Image(img1)$select(c("B2", "B3", "B4", "B8"))$unmask(-99, sameFootprint = FALSE)
  LT <- ee$Image(img2)$select(paste0("SR_", c("B2", "B3", "B4", "B5")))$unmask(-99, sameFootprint = FALSE)

  # Obtain proj metadata
  proj_metadata <- S2$select("B2")$projection()$getInfo()
  proj_transform <- proj_metadata$transform
  proj_crs <- proj_metadata$crs

  # Make that both images have the same CRS
  proj_transform[1] <- 30
  proj_transform[5] <- -30
  LT_crs <- LT$reproject(proj_crs, proj_transform)

  # Move the pixel to align to the geotransform
  geom <- point$geometry
  geom_utm <- st_transform(geom, proj_crs)
  x <- geom_utm[[1]][1]
  y <- geom_utm[[1]][2]
  new_x <- proj_transform[3] + round((x - proj_transform[3]) / 10) * 10 + 10 / 2
  new_y <- proj_transform[6] + round((y - proj_transform[6]) / 10) * 10 + 10 / 2
  new_geom_utm <- st_sfc(st_point(c(new_x, new_y)), crs = proj_crs)

  # Create ROI
  roi <- new_geom_utm %>% st_buffer(556 * 10, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)

  # Download the data
  output_file1 <- paste0(output, "/", "S2_", gsub(".*/.*/(.*)", "\\1", img1), ".tif")
  if (!file.exists(output_file1)) {
    lr_image <- ee_as_rast(
      image = S2,
      region = ee_roi,
      scale = 10,
      dsn = output_file1
    )
  }

  output_file2 <- paste0(output, "/", gsub(".*/.*/(.*)", "\\1", img2), "_", point$s2tile, ".tif")
  if (!file.exists(output_file2)) {
    hr_image <- ee_as_rast(
      image = LT_crs,
      region = ee_roi,
      scale = 30,
      dsn = output_file2
    )
  }
}
