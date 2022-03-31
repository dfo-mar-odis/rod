# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




#' Downloads an open data record
#'
#' This is a helper function to download and unzip an open data resource into a
#' temporary directory. It returns the path of the directory that the files
#' were downloaded into. The directory and downloaded data should be deleted
#' using unlink().
#'
#' @param resId String of the resource id associated with the open data resource
#' @return File path of the directory
download_res <- function(resId) {
  ckanr::ckanr_setup(url="https://open.canada.ca/data")
  tryCatch({res <- ckanr::resource_show(resId)}, error = function(e) {
    warning(paste("Resource corresponding to that resId not found:", e))})

  dataDir <- file.path(tempdir(), "dataDir")
  dataDirSucess <- dir.create(dataDir, showWarnings = FALSE)
  dataZip <- file.path(dataDir, "temp.zip")

  download.file(res$url, dataZip)
  utils::unzip(dataZip, exdir = dataDir)

  return(dataDir)
}

#' Performs cleaning of an sf object
#'
#' This is a helper function to perform common cleaning operations on sf
#' objects. These include setting the geometry type to multipolygon if there are
#' multiple geometry types, running sf::st_make_valid, cropping to an sf if provided
#' and setting the crs.
#'
#' @param resId String of the resource id associated with the open data resource
#' @return File path of the directory
sf_clean <- function(in_sf, crop_sf, crs) {
  if  (inherits(sf::st_geometry(in_sf), "sfc_GEOMETRY")) {
    in_sf <- sf::st_cast(in_sf, "MULTIPOLYGON")
  }
  out_sf <- sf::st_make_valid(in_sf)

  if (!is.null(crop_sf)) {
    newCrop_sf <- sf::st_transform(crop_sf, crs = sf::st_crs(out_sf))
    out_sf <- sf::st_crop(out_sf, newCrop_sf)
  }

  out_sf <- sf::st_transform(out_sf, crs = crs)
  return(out_sf)
}


#' Read an open data record
#'
#' These functions download and import open data spatial data based on a
#' resource id. The default data format for the output is an sf object, with
#' the option of cropping results to an input sf object.
#'
#' @section After function section:
#' Maybe details here?
#' @describeIn getRes
#'
#' @param resId String of the resource id associated with the open data resource
#' @param cropSf Sf object to crop the spatial data too
#' @param crs Coordinate reference system used for output sf
#' @return An sf object
#' @export
get_shp_res <- function(resId, cropSf=NULL, crs=4326) {
  dataDir <- download_res(resId)

  shpFile <- list.files(dataDir, recursive=TRUE, pattern="\\.shp$",
                        full.names = TRUE)
  if (length(shpFile) > 0) {
    tryCatch({out_sf <- sf::st_read(shpFile, stringsAsFactors = FALSE)},
             error = function(e) {
               stop(paste("Error reading shapefile:"))
               })
  } else {
    stop(paste("No Shapefile downloaded. Downloaded files are at: ", dataDir))
  }
  # cleanup
  out_sf <- sf_clean(out_sf)
  unlink(dataDir, recursive = TRUE)
  return(out_sf)
}


# --------------download_extract_validate_sf-----------------
# Function that downloads and processes a file into an sf object
#
# Inputs:
# 1. zipUrl: URL string of zipped file containing the spatial data
# 2. region_sf: Sf object to crop the output sf too
# 3. gdbLayer: if the file is a geodatabase, name of the layer to be read
# 4. tifFile: if the unzipped files are Tifs, name of the file to be used
# 5. returnRaster: if False, rasters will be converted into vector files
#
# Outputs:
# 1 .out_sf: output sf object containing spatial data
download_extract_validate_sf <- function(zipUrl, region_sf = NULL, gdbLayer = NULL, tifFile = NULL, returnRaster=FALSE) {
  tempDir <- here::here("reports/temp")
  temp <- file.path(tempDir, "temp.zip")

  download.file(zipUrl, temp)
  utils::unzip(temp, exdir = tempDir)
  # if there's a shape file read that:
  shpFile <- list.files(tempDir, recursive=TRUE, pattern="\\.shp$", full.names = TRUE)
  gdbDir <- list.files(tempDir, recursive=TRUE, pattern="\\.gdb$",
                       include.dirs	= TRUE, full.names = TRUE)
  tifRasterFile <- list.files(tempDir, recursive=TRUE, pattern = paste("\\",tifFile, "$", sep = ""),
                              include.dirs	= TRUE, full.names = TRUE)
  if (length(shpFile) > 0) {
    out_sf <- st_read(shpFile, stringsAsFactors = FALSE)
  } else if (length(gdbDir) > 0) {
    out_sf <- st_read(gdbDir, stringsAsFactors = FALSE, layer = gdbLayer)
    out_sf$geometry <- st_geometry(out_sf)
  } else if (length(tifRasterFile) > 0) {
    tifRaster <- raster(tifRasterFile)
    if (returnRaster){
      if (!is.null(region_sf)) {
        tifRaster <- projectRaster(tifRaster, crs=crs(region_sf))
        if (length(st_intersection(st_as_sfc(st_bbox(tifRaster)), region_sf)) > 0) {
          tifRaster <- crop(tifRaster, region_sf)
        } else {
          tifRaster <- NULL
        }
      }
      return(tifRaster)
    }
    out_sf <- stars::st_as_stars(tifRaster) %>% st_as_sf()
  }

  if  (inherits(st_geometry(out_sf), "sfc_GEOMETRY")) {
    out_sf <- st_cast(out_sf, "MULTIPOLYGON")
  }
  out_sf <- st_make_valid(out_sf)

  if (!is.null(region_sf)) {
    newRegion_sf <- sf::st_transform(region_sf, crs = sf::st_crs(out_sf))
    out_sf <- sf::st_crop(out_sf, newRegion_sf)
  }

  out_sf <- sf::st_transform(out_sf, crs = 4326)

  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE)

  return(out_sf)
}

