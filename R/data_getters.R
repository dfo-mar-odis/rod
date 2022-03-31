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

#' Read a geodatabase layer
#'
#' This is a helper function to read in a geodatabase layer to an sf object.
#'
#' @param gdbLayer String name of the layer to be read
#' @param gdbPath file path of the geodatabase
#' @return sf object
get_gdb_layer <- function(gdbLayer, gdbPath) {
  out_sf <- sf::st_read(gdbPath, stringsAsFactors = FALSE, layer = gdbLayer)
  out_sf$geometry <- sf::st_geometry(out_sf)
  return(out_sf)
}

#' Read an esri layer
#'
#' This is a helper function to read in a geodatabase layer to an sf object.
#'
#' @param esriUrl string url of the mapserver
#' @param esriLayer String name of the layer to be read
#' @param where where clause to filter feature rows
#' @param geometry geometry_sf to filter feature rows
#' @return sf object
get_esri_layer <- function(esriLayer, esriUrl, where, geometry) {
  mapserverUrl <- httr::parse_url(esriUrl)
  mapserverUrl$path <- paste(mapserverUrl$path, esriLayer, "query", sep = "/")
  if (is.null(geometry)) {
    mapserverUrl$query <- list(where = where,
                      outFields = "*",
                      returnGeometry = "true",
                      f = "geojson")
  } else {
    mapserverUrl$query <- list(geometry = geometry,
                      where = where,
                      outFields = "*",
                      returnGeometry = "true",
                      f = "geojson")
  }

  request <- httr::build_url(mapserverUrl)
  out_sf <- sf::st_read(request)
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
get_shp_res <- function(resId, crop_sf=NULL, crs=4326) {
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
  out_sf <- sf_clean(out_sf, crop_sf, crs)
  unlink(dataDir, recursive = TRUE)
  return(out_sf)
}


#' @describeIn getRes
#'
#' @param resId String of the resource id associated with the open data resource
#' @param cropSf Sf object to crop the spatial data too
#' @param crs Coordinate reference system used for output sf
#' @param gdbLayer string name of layer to extract, if null all layers will be
#' extracted
#' @return A named list of sfs object, named according to layer
#' @export
get_gdb_res <- function(resId, crop_sf=NULL, crs=4326, gdbLayer=NULL) {
  dataDir <- download_res(resId)

  gdbDir <- list.files(tempDir, recursive=TRUE, pattern="\\.gdb$",
                       include.dirs	= TRUE, full.names = TRUE)
  if (length(gdbDir) > 0) {
    tryCatch({
      if (!is.null(gdbLayer)){
        # download specific layer
        out_sf <- sf::st_read(gdbDir, stringsAsFactors = FALSE, layer = gdbLayer)
        out_sf$geometry <- sf::st_geometry(out_sf)
        sfList <- list(gdbLayer=out_sf)
      } else {
        # download all layers into list
        layerList <- sf::st_layers(gdbDir)[1]$name
        sfList <- sapply(layerList, get_gdb_layer, gdbPath=gdbDir,
                         simplify = FALSE,USE.NAMES = TRUE)
      }
    }, error = function(e) {stop(paste("Error reading geodatabase:"))})
  } else {
    stop(paste("No geodatabase downloaded. Downloaded files are at: ", dataDir))
  }
  # cleanup
  sfList <- sapply(sfList, sf_clean, crop_sf=crop_sf, crs=crs, simplify=FALSE,
                   USE.NAMES=TRUE)
  return(sfList)
}


#' @describeIn getRes
#'
#' @param resId String of the resource id associated with the open data resource
#' @param cropSf Sf object to crop the spatial data too
#' @param crs Coordinate reference system used for output sf
#' @param esriLayer String layer number of the layer to be extracted
#' @param esriWhere where clause to filter esri results
#' @return A  named list of sf objects, named according to their layer
#' @export
get_esri_res <- function(resId, crop_sf=NULL, crs=4326, esriLayer=NULL, esriWhere="1=1") {
  ckanr::ckanr_setup(url="https://open.canada.ca/data")
  tryCatch({res <- ckanr::resource_show(resId)}, error = function(e) {
    warning(paste("Resource corresponding to that resId not found:", e))})
  if (!is.null(layer)) {
    sfList = list(esriLayer=get_esri_layer(esriLayer = esriLayer,
                                           esriUrl = res$url,
                                           where = esriWhere,
                                           geometry = crop_sf))
  } else {
    # get layers, then extract:
    mapServerUrl <- httr::parse_url(res$url)
    mapServerUrl$path <- paste(mapServerUrl$path, "layers?f=json", sep = "/")
    request <- httr::build_url(mapServerUrl)
    layerIds <- jsonlite::fromJSON(request)$layers$id
    layerIds <- sapply(layerIds, toString)

    layerNames <- jsonlite::fromJSON(request)$layers$name

    sfList <- lapply(layerIds, get_esri_layer, esriUrl=res$url,
                     where=esriWhere, geometry=crop_sf)
    names(sfList) <- layerNames
  }
  # cleanup
  sfList <- sapply(sfList, sf_clean, crop_sf=crop_sf, crs=crs, simplify=FALSE,
                   USE.NAMES=TRUE)
  return(sfList)
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
download_extract_validate_sf <- function(zipUrl, region_sf = NULL,
                                         gdbLayer = NULL, tifFile = NULL,
                                         returnRaster=FALSE) {
  if (FALSE) {
  tifRasterFile <- list.files(tempDir, recursive=TRUE, pattern = paste("\\",tifFile, "$", sep = ""),
                              include.dirs	= TRUE, full.names = TRUE)
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
}


