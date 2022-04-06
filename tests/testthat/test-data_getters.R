test_that("sf_clean crops and transforms", {
  # load sample data from sf package:
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package="sf"))
  ncClean_sf <- rod::sf_clean(nc_sf, crs=4326)
  expect_equal(sf::st_crs(ncClean_sf)$input, "EPSG:4326")

  # create a bounding box around the first few entries:
  nc_bbox <- sf::st_as_sfc(sf::st_bbox(head(nc_sf)))
  ncCropped_sf <- rod:::sf_clean(nc_sf, nc_bbox)
  expect_equal(sf::st_bbox(ncCropped_sf), sf::st_bbox(sf::st_bbox(head(nc_sf))))
})

test_that("download url", {
  zipFile <- "https://github.com/stoyelq/rod/raw/test-framwork/tests/testthat/testdata/testShp.zip"
  dataDir <- rod:::download_url(zipFile)
  testthat::expect_gt(length(list.files(dataDir)), 0)
})

test_that("trigger error on res not found", {
  shpFile <- "https://github.com/stoyelq/rod/raw/test-framwork/tests/testthat/testdata/testShp.zip"
  expect_error(rod:::get_gdb_res(shpFile, crs=4326))
})

test_that("shp getter", {
  shpFile <- "https://github.com/stoyelq/rod/raw/test-framwork/tests/testthat/testdata/testShp.zip"
  out_sf <- rod:::get_shp_res(shpFile, crs=4326)
  expect_equal(sf::st_crs(out_sf)$input, "EPSG:4326")
})

test_that("gdb getter", {
  gdbFile <- "https://github.com/stoyelq/rod/raw/test-framwork/tests/testthat/testdata/testGdb.zip"
  outSf_list<- rod:::get_gdb_res(gdbFile, crs=4326)
  expect_equal(sf::st_crs(outSf_list[[1]])$input, "EPSG:4326")
})

test_that("csv getter", {
  csvFile <- "https://github.com/stoyelq/rod/raw/test-framwork/tests/testthat/testdata/test.csv"
  outDir <- rod:::get_data_res(csvFile)
  expect_gt(length(list.files(outDir)), 0)
})

test_that("tif getter", {
  tifFile <- "https://github.com/stoyelq/rod/raw/test-framwork/tests/testthat/testdata/testTif.zip"
  outTif_list <- rod:::get_tif_res(tifFile)
  expect_equal(sf::st_crs(outTif_list[[1]])$input, "EPSG:4326")
})

test_that("esri getter", {
  serverUrl <- "https://geoappext.nrcan.gc.ca/arcgis/rest/services/Energy/clean_energy_generating_stations_fgp/MapServer/"
  outSf_list <- rod::get_esri_res(serverUrl)
  expect_equal(sf::st_crs(outSf_list[[1]])$input, "EPSG:4326")
})
