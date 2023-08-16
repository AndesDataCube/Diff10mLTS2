# Load required libraries
library(lubridate) # For date and time manipulation
library(rgeeExtra) # Extended functionality for Google Earth Engine
library(dplyr) # Data manipulation and transformation
library(rgee) # Interacting with Google Earth Engine
library(sf) # Spatial data handling



#' Check if a dataframe is empty.
#'
#' This function checks whether the provided dataframe is empty, i.e., it does not contain any rows.
#'
#' @param df A dataframe. It should be an object of class 'data.frame'.
#'
#' @return A logical value. TRUE if the dataframe is empty (contains no rows), and FALSE otherwise.
#'
#' @examples
#' df_empty <- data.frame()
#' df_non_empty <- data.frame(a = 1:3, b = c('A', 'B', 'C'))
#' check_01(df_empty)
#' # [1] TRUE
#' check_01(df_non_empty)
#' # [1] FALSE
#'
#' @seealso \code{\link{nrow}}
#'
#' @keywords dataframe manipulation check empty
#' @export
check_01 <- function(df) {
  if (nrow(df) == 0) {
    TRUE
  } else {
    FALSE
  }
}



#' Get the projection of a specified band from an image.
#'
#' This function takes an image object and retrieves the projection information for a specified band.
#'
#' @param image An image object representing raster data.
#'   This should be a valid image object that supports the `select()` and `projection()` methods.
#'
#' @return The projection information for the specified band as returned by the `projection()` function.
#'
#' @examples
#' # Assuming 'image' is a valid image object
#' proj_info <- image_proj(image)
#' print(proj_info)
#'
#' @seealso \code{\link{select}}, \code{\link{projection}}
#'
#' @keywords image processing projection band
#' @export
image_proj <- function(image) {
  image$select("B2")$projection()
}



#' Get the coordinates of a point feature in a specified projection.
#'
#' This function takes a point feature object and converts its coordinates to the specified projection.
#'
#' @param point An Earth Engine point feature object.
#'   This should be a valid point feature object representing a geographic point.
#'   It must be compatible with the Earth Engine API and support the `transform()`, `geometry()`, and `coordinates()` methods.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `crs()` method.
#'
#' @return The coordinates of the point feature in the specified projection.
#'
#' @examples
#' # Assuming 'point' and 'proj' are valid objects
#' coords <- point_coordinates(point, proj)
#' print(coords)
#'
#' @seealso \code{\link{ee$Feature}}, \code{\link{transform}}, \code{\link{geometry}}, \code{\link{coordinates}}
#'
#' @keywords point coordinates projection Earth Engine
#' @export
point_coordinates <- function(point, proj) {
  Coords <- ee$Feature(point)$
    transform(proj$crs())$
    geometry()$
    coordinates()
}



#' Initialize coordinate value for image axis using regular expressions.
#'
#' This function initializes a coordinate value for the specified image axis based on the provided projection.
#' The function uses regular expressions to extract the coordinate value from the projection.
#'
#' @param axis A character indicating the axis to initialize. Should be either "x" or "y".
#' 
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `transform()` and `get()` methods.
#'
#' @return A numeric value representing the coordinate value for the specified axis.
#'
#' @examples
#' # Assuming 'axis' is either "x" or "y", and 'proj' is a valid projection object
#' coord_value <- init_coord_image(axis, proj)
#' print(coord_value)
#'
#' @export
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



#' Create a region of interest (ROI) around a point.
#'
#' This function creates a region of interest (ROI) around the specified point with a given side length.
#'
#' @param point An Earth Engine point geometry representing the center of the ROI.
#'
#' @param side The side length of the ROI.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'
#' @return An Earth Engine geometry object representing the region of interest.
#'
#' @examples
#' # Assuming 'point' is a valid Earth Engine point geometry, 'side' is a valid numeric side length, and 'proj' is a valid projection object
#' roi_geometry <- create_roi(point, side, proj)
#' print(roi_geometry)
#'
#' @export
create_roi <- function(point, side, proj) {
  point$buffer(
    distance = side / 2,
    proj = proj$crs())$
    bounds(proj = proj$crs())
}



#' Generate a study region (ROI) around a point for tile-based analysis.
#'
#' This function generates a study region (ROI) around the specified point for tile-based analysis.
#'
#' @param point An Earth Engine point geometry representing the center of the study region.
#'
#' @param proj An Earth Engine projection object.
#'   This should be a valid projection object that defines the target coordinate reference system (CRS).
#'   It must support the `transform()`, `split()`, and `get()` methods.
#'
#' @param scale A numeric value representing the scale at which to analyze the region.
#'   It is used in the `coord_transfer` function to adjust the coordinates based on the scale.
#'
#' @param side The side length of the study region.
#'
#' @return An Earth Engine geometry object representing the study region (ROI).
#'
#' @seealso \code{\link{point_coordinates}}, \code{\link{init_coord_image}}, \code{\link{coord_transfer}}, \code{\link{create_point}}, \code{\link{create_roi}}
#'
#' @export
tile_study <- function(point, proj, side) {
  ee_new_point <- point$transform(proj$crs())
  create_roi(ee_new_point, side, proj)
}



#' Generate a quality mask for cloud-free pixels.
#'
#' This function generates a quality mask for cloud-free pixels based on the 'QA_PIXEL' band in the image.
#'
#' @param image An Earth Engine image object.
#'   This should be a valid image object with the 'QA_PIXEL' band to generate the quality mask.
#'
#' @return An Earth Engine image object representing the quality mask for cloud-free pixels.
#'
#' @export
quality_mask <- function(image) { 
  image$select('QA_PIXEL')$
    bitwiseAnd(strtoi('11000', base = 2))$
    eq(0)
}



#' Count the number of cloud-free pixels in an image within a region of interest (ROI).
#'
#' This function counts the number of cloud-free pixels in an image within a specified region of interest (ROI).
#'
#' @param image An Earth Engine image object.
#'   This should be a valid image object containing the pixels to count within the ROI.
#'
#' @param roi An Earth Engine geometry object representing the region of interest (ROI).
#'
#' @param scale A numeric value representing the scale at which to perform the reduction operation.
#'
#' @return A numeric value representing the count of cloud-free pixels within the ROI.
#'
#' @export
cloud_free_count <- function(image, roi, scale) {
  image$reduceRegion(
    reducer = ee$Reducer$sum(),
    geometry = roi,
    scale = scale * 3
  )
}



#' Calculate cloud percentage based on the number of cloud-free pixels.
#'
#' This function calculates the cloud percentage based on the number of cloud-free pixels
#' within a given number of pixels and the scale of analysis.
#'
#' @param pixels A numeric value representing the count of cloud-free pixels.
#'
#' @param scale A numeric value representing the scale of analysis.
#'
#' @param side The side length of the study region.
#'
#' @return A numeric value representing the cloud percentage.
#'
#' @export
cloud_per_cal <- function(pixels, scale, side) {
  ee$Number(1)$
    add(pixels$
          getNumber("QA_PIXEL")$
          multiply(1 / ((side / (scale * 3))** 2))$
          multiply(-1))$
    multiply(100)
}



#' Calculate cloud percentage for a tile around a given point.
#'
#' This function calculates the cloud percentage for a tile around the specified point based on the given scale and side length.
#'
#' @param point An Earth Engine point geometry representing the center of the tile.
#'
#' @param scale A numeric value representing the scale of analysis.
#'
#' @param side The side length of the study region.
#'
#' @return A function that calculates the cloud percentage for the tile when applied to an Earth Engine image.
#'
#' @seealso \code{\link{image_proj}}, \code{\link{tile_study}}, \code{\link{quality_mask}}, \code{\link{cloud_free_count}}, \code{\link{cloud_per_cal}}
#'
#' @export
cloud_tile <- function(point, scale, side){
  img_cloud_tile <- function(image) {
    projection <- image_proj(image) #
    ee_roi <- tile_study(point, projection, side)
    cloud_mask <- quality_mask(image) #
    count <- cloud_free_count(cloud_mask, ee_roi, scale)
    prop_cloud <- cloud_per_cal(count, scale, side)
    image$set(list("cloud" = prop_cloud))
  }
  img_cloud_tile
}



#' Filter image collection by geographic bounds around a point.
#'
#' This function filters an Earth Engine image collection to include only images that intersect the geographic bounds around a specified point.
#'
#' @param snippet An Earth Engine image collection snippet.
#'
#' @param point An Earth Engine point geometry representing the center of the geographic bounds.
#'
#' @return An Earth Engine image collection filtered by geographic bounds around the point.
#'
#' @export
images_bounds <- function(snippet, point) {
  ee$ImageCollection(snippet) %>%
    ee$ImageCollection$filterBounds(point)
}



#' Merge image collections and get the date intervals.
#'
#' This function merges a list of Earth Engine image collections and retrieves the date intervals for the resulting merged image collection.
#'
#' @param listing A list of Earth Engine image collections to be merged.
#'
#' @return An Earth Engine image collection representing the merged images with date intervals.
#'
#' @seealso \code{\link{ee_get_date_ic}}
#'
#' @export
id_dates_ic <- function(listing) {
  IC <- NA
  for (i in 1:length(listing)) {
    if(i == 1) {
      IC <- listing[[i]]
    } else {
      IC <- IC$merge(listing[[i]])
    }
  }
  IC %>% ee_get_date_ic()
}



#' Filter image dates with a specific pattern in their IDs.
#'
#' This function filters the input image dates to include only those with a specific pattern in their IDs.
#'
#' @param idates An Earth Engine image collection containing image dates with IDs.
#'
#' @param pattern A character pattern to match in the IDs of the image dates.
#'
#' @return An Earth Engine image collection filtered based on the specified pattern in the IDs.
#'
#' @export
sensor_pattern <- function(idates, pattern) {
  IDateS <- idates[grepl(pattern, idates$id),]
  return(IDateS)
}



#' Calculate time difference and index of collocation between two sensors.
#'
#' This function calculates the time difference and the index of collocation between two sensors based on their start times.
#'
#' @param sensor1 An Earth Engine image collection with 'time_start' property representing the start times of the first sensor.
#'
#' @param sensor2 An Earth Engine image collection with 'time_start' property representing the start times of the second sensor.
#'
#' @param units The time units to use for the time difference calculation (e.g., "hours", "days").
#'
#' @return A data frame with two columns: 'time' representing the time difference and 'index' representing the index of collocation.
#'
#' @export
time_between <- function(sensor1, sensor2, units) {
  r_collocation <- sapply(
    X = sensor1$time_start,
    FUN = function(x) {
      vresults <- abs(as.numeric(sensor2$time_start - x, units = units))
      c(min(vresults), which.min(vresults))
    }
  )
  return(data.frame(time = r_collocation[1, ], 
                    index = r_collocation[2, ]))
}



#' Retrieve Metadata for Selected Points and Image Collections
#'
#' This function retrieves metadata for selected points and image collections based on various criteria,
#' including sensor patterns, time intervals, cloud cover, and more.
#'
#' @param point A spatial point object representing the selected point.
#' @param snip1 A list of spatial polygons representing the first set of image snippets.
#' @param snip2 A list of spatial polygons representing the second set of image snippets.
#' @param sensors A character vector containing the names of the two sensors.
#' @param units A character string indicating the time units for comparison (e.g., "days").
#' @param scale A numeric value representing the scale parameter for image processing.
#' @param side A numeric value representing the side parameter for image processing.
#' @param timediff The maximum time difference between images from different sensors.
#' @param max_ob The maximum number of observations to be retained after filtering.
#'
#' @return A data frame containing metadata information for selected points and images.
#'
#' @examples
#' get_metadata(
#'   point = selected_point,
#'   snip1 = list_of_snippets1,
#'   snip2 = list_of_snippets2,
#'   sensors = c("OLI", "MSI"),
#'   units = "days",
#'   scale = 10,
#'   side = 5,
#'   timediff = 30,
#'   max_ob = 100
#' )
#'
#' @export
get_metadata <- function(point, snip1, snip2, sensors, units, scale, side, timediff, max_ob) {
  
  # Convert point geometries to Earth Engine format
  ee_point <- sf_as_ee(point$geometry)
  
  # Apply the 'images_bounds' function to each element in 'snip1' and 'snip2'
  listing1 <- lapply(snip1, images_bounds, ee_point)
  listing2 <- lapply(snip2, images_bounds, ee_point)
  
  # Convert lists of geometries to image collections
  imgsmsi <- ee$ImageCollection(ee$FeatureCollection(listing1)$flatten())
  imgsoli <- ee$ImageCollection(ee$FeatureCollection(listing2)$flatten())
  
  # Apply the 'cloud_tile' function to each image in 'imgsmsi' and 'CloudImgs'
  CloudImgs <- imgsoli$map(cloud_tile(ee_point, scale, side))
  
  # Filter images in 'CloudImgs' based on cloud metadata
  CloudImgsFilter <- CloudImgs$filterMetadata("cloud", "less_than", 15)
  
  # Combine the 'CloudImgsFilter' and 'imgsmsi' image collections
  all_together_db <- id_dates_ic(list(CloudImgsFilter, imgsmsi))
  
  # Filter First Sensor ID
  sensor1 <- sensor_pattern(all_together_db, sensors[1])
  
  # Check if there is any image in the collection
  if (check_01(sensor1)) {
    return(NA)
  }
  
  # Filter Second Sensor ID
  sensor2 <- sensor_pattern(all_together_db, sensors[2])
  
  # Check if there is any image in the collection
  if (check_01(sensor2)) {
    return(NA)
  }
  
  # Get time interval between images from different sensors
  Tindex <- time_between(sensor1, sensor2, units)
  difft <-  Tindex$time < timediff
  
  # Check if the images are on the time interval 
  if (sum(difft) == 0) {
    return(NA)
  }
  
  # Filter IDs of each sensor in set time condition
  final_msi_ic <- sensor1[difft, ] 
  final_time <- Tindex$time[difft]
  final_oli_ic <- sensor2[Tindex$index[difft], ]
  
  # Sensor LT
  Slt <- substr(final_oli_ic$id, 9, 12)
  Tiers <- substr(final_oli_ic$id, 18, 19)
  first <- substr(Slt, 1, 1)
  last <- substr(Slt, nchar(Slt), nchar(Slt))
  mission <- paste0(first, last)
  
  # Return the metadata
  df <- data.frame(
      msi_id = final_msi_ic$id,
      lt_mission = mission,
      Tier = Tiers,
      oli_id = final_oli_ic$id,
      dif_time = round(final_time, 10),
      roi_id = point$s2tile,
      roi_x = st_coordinates(point)[1],
      roi_y = st_coordinates(point)[2],
      roi_x_utm = point$E,
      roi_y_utm = point$N,
      epsg = point$CRS
  )
  
  
  # Filter by maximum number of IDs established per point
  if (max_ob) {
    n <- min(max_ob, nrow(df)) 
    random_indices <- sample(1:nrow(df), n, replace = FALSE)
    df <- df[random_indices, ]
  }
  
  # Sort 'df' based on the 'dif_time' column
  df <- df[order(df$dif_time), ]

  # Create empty vectors to store Earth Engine images
  Imgo <- c()
  Imgm <- c()
  
  # Loop through each index in the 'df' DataFrame to collect OLI and MSI image IDs
  for (i in 1:length(df$oli_id)) {
    Imgo <- c(Imgo, ee$Image(df$oli_id[i]))
  }
  
  for (i in 1:length(df$msi_id)) {
    Imgm <- c(Imgm, ee$Image(df$msi_id[i]))
  }
  
  # Create Earth Engine ImageCollections from the collected OLI and MSI images
  Imgso <- ee$ImageCollection(Imgo)
  Imgsm <- ee$ImageCollection(Imgm)
  
  # Compute cloud information for each image in 'Imgso' and assign to 'df$cloud'
  df$cloud <- unlist(
    Imgso$map(cloud_tile(ee_point, scale, side))$  
      aggregate_array("cloud")$  
      getInfo()
  )
  
  # Create a data frame 'df_meta' with additional metadata information
  df_meta <- data.frame(
    ee$Dictionary$
      fromLists(
        c("sun_azimuth", 
          "sun_elevation",
          "mean_sol_aza",
          "mean_sol_zea"), 
        c(Imgso$aggregate_array("SUN_AZIMUTH"),
          Imgso$aggregate_array("SUN_ELEVATION"),
          Imgsm$aggregate_array("MEAN_SOLAR_AZIMUTH_ANGLE"),
          Imgsm$aggregate_array("MEAN_SOLAR_ZENITH_ANGLE")))$getInfo()
  )
  # Combine 'df' and 'df_meta' data frames to create the final result
  return(cbind(df, df_meta))
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
download <- function(row, sensors, side, output) {
  # Approach of previous data
  options <- list(LT = c("tm_id", 30), LM = c("mss_id", 60), MSI = c("msi_id", 10), OLI = c("oli_id", 30))
  dir.create(sprintf("%s/%s", output, sensors[1]), 
             showWarnings = FALSE, 
             recursive = TRUE)
  dir.create(sprintf("%s/%s", output, sensors[2]), 
             showWarnings = FALSE, 
             recursive = TRUE)
  scale <- as.numeric(options[[sensors[1]]][2])
  
  # Entering the point and ID of the images
  point <- st_as_sf(x = row, coords = c("roi_x", "roi_y"), crs = 4326)
  img1 <- as.character(row[options[[sensors[1]]][1]])
  img2 <- as.character(row[options[[sensors[2]]][1]])
  
  # Read Earth Engine images Select the bands: Red, Green, Blue, NIR
  ImageC1 <- ee$Image(img1)$
    select(c("B1", "B2", "B3", "B4","B8A", "B11", "B12"))$
    int16()$
    unmask(-99, sameFootprint = FALSE)
  ImageC2 <- ee$Image(img2)$
    select(c("B1", "B2", "B3", "B4", "B5", "B6", "B7"))$
    multiply(10000)$
    int16()$
    unmask(-99, sameFootprint = FALSE)
  
  # Obtain proj metadata
  proj_metadata <- ImageC1$select("B2")$
                      projection()$
                      getInfo()
  proj_crs <- proj_metadata$crs
  
  # Move the pixel to align to the geotransform
  geom <- point$geometry
  geom_utm <- st_transform(geom, proj_crs)
  ImageC2_crs <- ImageC2$reproject(proj_crs) #, proj_transform

  # Create ROI
  roi <- geom_utm %>%
    st_buffer(side / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)

  # Download
  output_file1 <- sprintf("%s/%s/%s__%s.tif", output, sensors[1], row$roi_id, basename(img1))
  if (!file.exists(output_file1)) {
    lr_image <- ee_as_rast(
      image = ImageC1,
      region = ee_roi,
      scale = scale,
      dsn = output_file1
    )
  }
  
  output_file2 <- sprintf("%s/%s/%s__%s.tif", output, sensors[2], row$roi_id, basename(img2))
  if (!file.exists(output_file2)) {
    hr_image <- ee_as_rast(
      image = ImageC2_crs,
      region = ee_roi,
      scale = scale,
      dsn = output_file2
    )
  }
}



#' Display Landsat image transformation and comparison on the map.
#'
#' This function displays Landsat image transformation and comparison on the map based on the specified mode.
#'
#' @param row A data frame representing the information about the location.
#'   It should contain the following columns: "msi_id", "roi_x", "roi_y", "oli_id", "dif_time", and other required columns.
#'
#' @param mode The mode in which the images will be displayed on the map.
#'   It can be "points" or "comparison".
#'
#' @param max The maximum value to set for the image layers. Default is 0.3.
#'
#' @importFrom sf st_as_sf st_transform st_point st_buffer
#' @importFrom raster sf_as_ee
#' @export
DisplayTransform <- function(row, mode, distance, max = 0.3) {
  img_msi <-  ee$Image(row$msi_id)
  proj_metadata <- img_msi$select("B2")$projection()$getInfo()
  proj_transform <- proj_metadata$transform
  proj_crs <- proj_metadata$crs
  point <- st_as_sf(x = row, coords = c("roi_x", "roi_y"), crs = 4326)
  geom_utm <- st_transform(point$geometry, proj_crs)
  ee_point <- sf_as_ee(geom_utm)
  roi <- geom_utm %>% st_buffer(distance / 2, endCapStyle = "SQUARE")
  ee_roi <- sf_as_ee(roi, proj = proj_crs)
  eeimg1 <- img_msi$
    clip(ee_roi)$
    divide(10000)
  eeimg2 <- ee$Image(row$oli_id)$
    reproject(proj_crs)$ # reproject(proj_crs, proj_transform)$
    clip(ee_roi)
  eel <- list(min = 0, max = max, bands = c("B4", "B3", "B2"))
  print(sprintf("Diferencia de %s minutos entre OLI: %s y MSI: %s", row$dif_time, row$oli_id, row$msi_id))
  Map$centerObject(eeimg1)
  if (mode == "points") {
    Map$addLayer(eeimg1, eel, basename(row$msi_id)) +
      Map$addLayer(eeimg2, eel, basename(row$oli_id)) +
      Map$addLayer(ee_point, list(color = "#E4FF00"), "Centered point (yellow)")
  } else if (mode == "comparison") {
    Map$addLayer(eeimg1, eel, basename(row$msi_id)) | Map$addLayer(eeimg2, eel, basename(row$oli_id))
  } else {
    stop("Modo no válido. Debe ser 'points' o 'comparison'.")
  }
}



#' Get metadata for a given point and sensor, with retry mechanism on failure.
#'
#' This function retrieves metadata for a specific point and sensor using the `get_metadata` function. If an error occurs
#' during the metadata retrieval, the function will retry up to four additional times (by default) before raising a
#' custom error message indicating possible internet connection issues.
#'
#' @param point A spatial point object representing the location of interest.
#' @param sensor A character string specifying the sensor for which metadata is requested.
#' @param timediff The time difference to consider when fetching the metadata.
#' @param counter An optional integer specifying the retry counter. Defaults to 1.
#' @return The metadata retrieved using the `get_metadata` function, or an error message if retries fail.
get_metadata_try <- function(point, snip1, snip2, sensors, units, scale, side, timediff = 30, max_ob = F, counter = 1) {
  results <- try(
    get_metadata(point = point, snip1 = snip1, snip2 = snip2, sensors = sensors, units = units, scale = scale, side = side, timediff = timediff, max_ob = max_ob)
  )
  if (inherits(class(results), "try-error")) {
    counter <- counter + 1
    if (counter == 5) stop("Probably internet connection lost")
    get_metadata_try(point, snip1, snip2, sensors, units, scale, side, timediff = 30, max_ob = F, counter = 1)
  }
  
  results
}
