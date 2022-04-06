

test_that("sf_clean crops and transforms", {
  # load sample data from sf package:
  nc_sf <- sf::st_read(system.file("shape/nc.shp", package="sf"))
  ncClean_sf <- rod::sf_clean(nc_sf, crs=4326)
  expect_equal(sf::st_crs(ncClean_sf)$input, "EPSG:4326")

  # create a bounding box around the first few entries:
  nc_bbox <- sf::st_as_sfc(sf::st_bbox(head(nc_sf)))
  ncCropped_sf <- rod::sf_clean(nc_sf, nc_bbox)
  expect_equal(sf::st_bbox(ncCropped_sf), sf::st_bbox(sf::st_bbox(head(nc_sf))))
})


