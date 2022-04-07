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
#' @param resUrl String of url to download the resource
#' @return File path of the directory
#'
#' @examples
#' dataDir <- rod:::download_url(paste0("https://github.com/stoyelq/rod/raw/main/tests/",
#' "testthat/testdata/testShp.zip"))
#' list.files(dataDir)
download_url <- function(resUrl) {
  dataDir <- file.path(tempdir(), "dataDir")
  if (dir.exists(dataDir)){
    unlink(dataDir, recursive=TRUE)
  }
  dataDirSucess <- dir.create(dataDir, showWarnings = FALSE)
  dataZip <- file.path(dataDir, "temp.zip")

  utils::download.file(resUrl, dataZip)
  if (is_zip(dataZip)){
    utils::unzip(dataZip, exdir = dataDir)
  }
  return(dataDir)
}

#' Performs cleaning of an sf object
#'
#' This is a helper function to perform common cleaning operations on sf
#' objects. These include setting the geometry type to multipolygon if there are
#' multiple geometry types, running sf::st_make_valid, cropping to an sf if provided
#' and setting the crs.
#'
#' @param in_sf Sf object to be cleaned.
#' @param crop_sf Sf object to crop the spatial data to
#' @param crs Coordinate reference system used for output sf
#' @return File path of the directory
#'
#' @examples
#' # load sample data from sf package:
#' nc_sf <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' # create a bounding box around the first few entries:
#' nc_bbox <- sf::st_as_sfc(sf::st_bbox(head(nc_sf)))
#' ncClean_sf <- rod:::sf_clean(nc_sf, nc_bbox, crs=4326)
sf_clean <- function(in_sf, crop_sf=NULL, crs=NULL) {
  if  (inherits(sf::st_geometry(in_sf), "sfc_GEOMETRY")) {
    in_sf <- sf::st_cast(in_sf, "MULTIPOLYGON")
  }
  out_sf <- sf::st_make_valid(in_sf)

  if (!is.null(crop_sf)) {
    newCrop_sf <- sf::st_transform(crop_sf, crs = sf::st_crs(out_sf))
    suppressWarnings(out_sf <- sf::st_crop(out_sf, newCrop_sf))
  }

  if (!is.null(crs)){
    out_sf <- sf::st_transform(out_sf, crs = crs)
  }

  return(out_sf)
}

#' Read a geodatabase layer
#'
#' This is a helper function to read in a geodatabase layer to an sf object.
#'
#' @param gdbLayer String name of the layer to be read
#' @param gdbPath file path of the geodatabase
#' @return sf object
#'
get_gdb_layer <- function(gdbLayer, gdbPath) {
  out_sf <- sf::st_read(gdbPath, stringsAsFactors = FALSE, layer = gdbLayer)
  out_sf$geometry <- sf::st_geometry(out_sf)
  return(out_sf)
}

#' Read a Tif file
#'
#' This is a helper function to read in a tif file to stars object.
#'
#' @param tifPath File path of the tif file
#' @param crop_sf Sf object to crop the spatial data to
#' @param crs Coordinate reference system used for output sf
#' @return stars object
get_star_tif <- function(tifPath, crs, crop_sf) {
  out_star <- stars::read_stars(tifPath) %>%
    sf::st_transform(crs=crs)
  if (!is.null(crop_sf)) {
    if (length(sf::st_intersection(sf::st_bbox(out_star), crop_sf)) > 0) {
      out_star <- sf::st_crop(out_star, crop_sf)
    } else {
      out_star <- NULL
    }
  }
  return(out_star)
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
  warning(request)
  out_sf <- sf::st_read(system.file("shape/nc.shp", package="sf"), stringsAsFactors = FALSE)
  return(out_sf)
}

#' Check if a file is a zip file
#'
#' Helper function that sees if a file can be unziped or not.
#'
#' @param filepath filepath of possible zip file
#' @return boolean TRUE if file is zipped, else FALSE
is_zip <- function(filepath){
  result <- tryCatch({
    utils::unzip(filepath, list = TRUE)
    return(TRUE)
  }, error = function(e){
    return(FALSE)
  })
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
#' @param resUrl Url of the resource to download
#' @param crop_sf Sf object to crop the spatial data to
#' @param crs Coordinate reference system used for output sf
#' @return An sf object
#' @export
#'
#' @examples
#' ckanr::ckanr_setup(url="https://open.canada.ca/data")
#' #res <- ckanr::resource_show("some-resource-id-1234")
#' #res_sf <- get_shp_res(res$url)
#'
get_shp_res <- function(resUrl, crop_sf=NULL, crs=4326) {
  dataDir <- download_url(resUrl)

  shpFile <- list.files(dataDir, recursive=TRUE, pattern="\\.shp$",
                        full.names = TRUE)
  if (length(shpFile) > 0) {
    tryCatch({out_sf <- sf::st_read(shpFile, stringsAsFactors = FALSE)},
             error = function(e) {
               stop(paste("Error reading shapefile:", e,
                          "Downloaded files are at: ", dataDir), sep="\n")
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
#' @param resUrl Url of the resource to download
#' @param crop_sf Sf object to crop the spatial data too
#' @param crs Coordinate reference system used for output sf
#' @param gdbLayer string name of layer to extract, if null all layers will be
#' extracted
#' @return A named list of sfs object, named according to layer
#' @export
get_gdb_res <- function(resUrl, crop_sf=NULL, crs=4326, gdbLayer=NULL) {
  dataDir <- download_url(resUrl)

  gdbDir <- list.files(dataDir, recursive=TRUE, pattern="\\.gdb$",
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
  unlink(dataDir, recursive = TRUE)
  return(sfList)
}


#' @describeIn getRes
#'
#' @param resUrl Url of the resource to download
#' @param crop_sf Sf object to crop the spatial data too
#' @param crs Coordinate reference system used for output sf
#' @param tifFile string name of tif file to be extracted
#' @param getSf Boolean. If TRUE, function will return list of sf, may
#' SIGNIFICANTLY increase processing time and resources. Default is FALSE
#' @return A named list of stars/sf objects depending on getSf flag, named
#' according to filename.
#' @export
get_tif_res <- function(resUrl, crop_sf=NULL, crs=4326, tifFile=NULL,
                        getSf=FALSE) {
  dataDir <- download_url(resUrl)

  if (!is.null(tifFile)) {
    # specific file to fetch
    tifRasterFiles <- list.files(dataDir, recursive = TRUE, pattern =
                                   paste("\\",tifFile, "$", sep = ""),
                                 full.names = TRUE)
    tifRasterNames <- list.files(dataDir, recursive = TRUE, pattern =
                                   paste("\\",tifFile, "$", sep = ""),
                                 full.names = FALSE)
  } else {
    # fetch all downloaded tifs
    tifRasterFiles <- list.files(dataDir, recursive = TRUE, pattern = "\\.tif$",
                                 include.dirs	= TRUE, full.names = TRUE)
    tifRasterNames <- list.files(dataDir, recursive = TRUE, pattern = "\\.tif$",
                                 full.names = FALSE)
  }

  if (length(tifRasterFiles) > 0) {
    tifRasterNames <- gsub(".tif", "", tifRasterNames)
    outList <- lapply(tifRasterFiles, get_star_tif, crs=crs, crop_sf=crop_sf)
    names(outList) <- tifRasterNames

    if (getSf){
      outList <- sapply(outList, sf::st_as_sf, simplify=FALSE,
                       USE.NAMES=TRUE)
    }
  } else {
    stop(paste("No matching Tif files downloaded. Downloaded files are at: ",
               dataDir))
  }
  # cleanup
  unlink(dataDir, recursive = TRUE)
  return(outList)
}

#' @describeIn getRes
#'
#' @param resUrl Url of the resource to download
#' @param fileName String name of file to save
#' @param savePath Full Path file should be saved to.
#' @return Directory containing the downloaded files.
#' @export
get_data_res <- function(resUrl, fileName=NULL, savePath=NULL) {
  dataDir <- download_url(resUrl)

  if (!is.null(fileName)) {
    filePaths <- list.files(dataDir, recursive = TRUE, pattern =
                                   paste("\\", fileName, "$", sep = ""),
                                 full.names = TRUE)
  } else {
    filePaths <- list.files(dataDir, recursive = TRUE,
                                 include.dirs	= FALSE, full.names = TRUE)
  }

  if (!is.null(savePath)) {
    if (!dir.exists(savePath)) {
      dir.create(savePath, showWarnings = FALSE)
    }
    file.copy(filePaths, savePath, recursive=TRUE)
    unlink(dataDir, recursive = TRUE)
    return(savePath)
  } else {
    return(dataDir)
  }
}


#' @describeIn getRes
#'
#' @param serverUrl Url of the map server to query
#' @param crop_sf Sf object to crop the spatial data too
#' @param crs Coordinate reference system used for output sf
#' @param esriLayer String layer number of the layer to be extracted
#' @param esriWhere where clause to filter esri results
#' @return A  named list of sf objects, named according to their layer
#' @export
get_esri_res <- function(serverUrl, crop_sf=NULL, crs=4326, esriLayer=NULL, esriWhere="1=1") {
  if (!is.null(esriLayer)) {
    sfList = list(esriLayer=get_esri_layer(esriLayer = esriLayer,
                                           esriUrl = serverUrl,
                                           where = esriWhere,
                                           geometry = crop_sf))
  } else {
    # get all layers, then extract:
    mapServerUrl <- httr::parse_url(serverUrl)
    mapServerUrl$path <- paste(mapServerUrl$path, "layers?f=json", sep = "/")
    request <- httr::build_url(mapServerUrl)
    layerIds <- jsonlite::fromJSON(request)$layers$id
    layerIds <- sapply(layerIds, toString)

    layerNames <- jsonlite::fromJSON(request)$layers$name

    sfList <- lapply(layerIds, get_esri_layer, esriUrl=serverUrl,
                     where=esriWhere, geometry=crop_sf)
    names(sfList) <- layerNames
  }
  # cleanup
  sfList <- sapply(sfList, sf_clean, crop_sf=crop_sf, crs=crs, simplify=FALSE,
                   USE.NAMES=TRUE)
  return(sfList)
}
