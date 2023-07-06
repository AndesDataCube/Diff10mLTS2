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
get_metadata <- function(point, timediff = 10) {
    
    # Sensors L8/9 y Sentinel-2
    sensorMSI <- "COPERNICUS/S2_HARMONIZED"
    sensorOLI8T1 <- "LANDSAT/LC08/C02/T1_TOA"
    sensorOLI8T2 <- "LANDSAT/LC08/C02/T2_TOA"
    sensorOLI9T1 <- "LANDSAT/LC09/C02/T1_TOA"
    sensorOLI9T2 <- "LANDSAT/LC09/C02/T2_TOA"
    
    # From local to EE server
    ee_point <- sf_as_ee(point$geometry)

    # Get the dates of the MSI image
    msi_ic <- ee$ImageCollection(sensorMSI) %>%
        ee$ImageCollection$filterBounds(ee_point)

    # Get the dates of the OLI8 images
    oli8_ic1 <- ee$ImageCollection(sensorOLI8T1) %>%
        ee$ImageCollection$filterBounds(ee_point)
    
    oli8_ic2 <- ee$ImageCollection(sensorOLI8T2) %>%
        ee$ImageCollection$filterBounds(ee_point)
    
    # Get the dates of the OLI9 images
    oli9_ic1 <- ee$ImageCollection(sensorOLI9T1) %>%
        ee$ImageCollection$filterBounds(ee_point)
    
    oli9_ic2 <- ee$ImageCollection(sensorOLI9T2) %>%
        ee$ImageCollection$filterBounds(ee_point)
    
    # merge all the ic together
    all_together <- msi_ic$merge(oli8_ic1)$merge(oli8_ic2)$merge(oli9_ic1)$merge(oli9_ic2)
    all_together_db <- all_together %>% ee_get_date_ic()
    
    S2_db <- all_together_db[grepl("COPERNICUS", all_together_db$id),]
    oli_ic_db <- all_together_db[grepl("LANDSAT", all_together_db$id),]
  
    # Get the images on the same time interval (timediff)
    r_collocation <- sapply(
        X = oli_ic_db$time_start,
        FUN = function(x) {
          vresults <- abs(as.numeric(S2_db$time_start - x, units = "mins"))
          c(min(vresults), which.min(vresults))
        }
    )
  
    r_difftime <- r_collocation[1, ]
    idx_mss <- r_collocation[2, ]

    # Check if the images are on the time interval (timediff)
    if (sum(r_difftime < timediff) == 0) {
        return(NA)
    }

    # Create the final dataset
    final_oli_ic <- oli_ic_db[r_difftime < timediff, ]
    final_time <- r_difftime[r_difftime < timediff]
    final_msi_ic <- S2_db[idx_mss[r_difftime < timediff], ]

    # Sensor LT
    Slt <- substr(final_oli_ic$id, 9, 12)
    Tiers <- substr(final_oli_ic$id, 18, 19)
    first <- substr(Slt, 1, 1)
    last <- substr(Slt, nchar(Slt), nchar(Slt))
    mission <- paste0(first, last)

    # Return the metadata
    df1 <- data.frame(
        msi_id = final_msi_ic$id,
        lt_mission = mission,
        Tier = Tiers,
        oli_id = final_oli_ic$id,
        dif_time = round(final_time, 10),
        roi_id = point$s2tile,
        roi_x = st_coordinates(point)[1],
        roi_y = st_coordinates(point)[2]
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
  S2 <- ee$Image(img1)$
           select(c("B1", "B2", "B3", "B4","B8A", "B11", "B12"))$
           unmask(-99, sameFootprint = FALSE)
  LT <- ee$Image(img2)$
           select(c("B1", "B2", "B3", "B4", "B5", "B6", "B7"))$
           unmask(-99, sameFootprint = FALSE)

  # Obtain proj metadata
  proj_metadata <- S2$select("B2")$
                      projection()$
                      getInfo()
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
  roi <- new_geom_utm %>% st_buffer(11520/2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)

  # Download the data
  dir.create(sprintf("%s/S2", output), showWarnings = FALSE, recursive = TRUE)
  dir.create(sprintf("%s/Landsat", output), showWarnings = FALSE, recursive = TRUE)
  
  output_file1 <- sprintf("%s/S2/ROI_%05d__%s.tif", output, index, basename(img1))
  if (!file.exists(output_file1)) {
    lr_image <- ee_as_rast(
      image = S2,
      region = ee_roi,
      scale = 10,
      dsn = output_file1
    )
  }
  
  output_file2 <- sprintf("%s/Landsat/ROI_%05d__%s.tif", output, index, basename(img2))
  if (!file.exists(output_file2)) {
    hr_image <- ee_as_rast(
      image = LT_crs,
      region = ee_roi,
      scale = 10,
      dsn = output_file2
    )
  }
}


display_map <- function(row, max = 0.4) {
    eeimg1 <- ee$Image(row$msi_id)$divide(10000)
    eeimg2 <- ee$Image(row$oli_id)
    
    eel1 <- list(min=0, max=max, bands=c("B4", "B3", "B2"))
    eel2 <- list(min=0, max=max, bands=c("B4", "B3", "B2"))
    
    Map$centerObject(eeimg1)
    Map$addLayer(eeimg1, eel1) | Map$addLayer(eeimg2, eel2)
}


get_metadata_try <- function(point, timediff = 10) {
    results <- try(
      get_metadata(point=point, timediff=timediff)
    )
    counter <- 1
    if (inherits(class(results),  "try-error")) {
        get_metadata_try(point=point, timediff=timediff)
        counter <- counter + 1
        if (counter == 4) {
            stop("Probably internet connection lost")
        }
    }
    results
}

