
test_that("longlake data loader works as expected", {

  test_env <- new.env(parent = emptyenv())
  load_longlake_data(env = test_env, vector_format = "sf", raster_format = "raster")
  expect_length(test_env, 8)
  expect_s3_class(test_env$longlake_depthdf, "sf")
  expect_s4_class(test_env$longlake_osm, "RasterBrick")

  test_env2 <- new.env(parent = emptyenv())
  load_longlake_data(
    env = test_env2,
    vector_format = "sp", raster_format = "stars",
    which = c("longlake_depthdf", "longlake_osm")
  )
  expect_setequal(names(test_env2), c("longlake_depthdf", "longlake_osm"))
  expect_s4_class(test_env2$longlake_depthdf, "SpatialPointsDataFrame")
  expect_s3_class(test_env2$longlake_osm, "stars")

  test_env3 <- new.env(parent = emptyenv())
  load_longlake_data(env = test_env3, raster_format = "stars_proxy", which = c("longlake_depth_raster", "longlake_osm"))
  expect_s3_class(test_env3$longlake_osm, "stars_proxy")
  expect_s3_class(test_env3$longlake_depth_raster, "stars_proxy")

  test_env4 <- new.env(parent = emptyenv())
  load_longlake_data(
    env = test_env4, raster_format = "terra",
    which = c("longlake_depth_raster", "longlake_osm")
  )

  expect_length(test_env4, 2)
  expect_s4_class(test_env4$longlake_depth_raster, "SpatRaster")
  expect_s4_class(test_env4$longlake_osm, "SpatRaster")

})
