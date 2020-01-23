context("test-df-spatial")

test_that("duplicate column name fixing works", {
  tbl <- tibble::as_tibble(list(x = 1:5, y = 1:5, x = letters[1:5]), validate = FALSE)
  expect_identical(
    expect_message(fix_duplicate_cols(tbl), "Renamed columns"),
    tibble::tibble(x = 1:5, y = 1:5, x..3 = letters[1:5])
  )

  tbl2 <- tibble::as_tibble(list(x = 1:5, y = 1:5, z = letters[1:5]))
  expect_identical(
    expect_silent(fix_duplicate_cols(tbl2)),
    tbl2
  )
})

test_that("Raster* objects are converted properly by df_spatial", {
  skip_if_not_installed("raster")

  load_longlake_data(which = c("longlake_depth_raster", "longlake_osm"), raster_format = "raster")
  expect_df_spatial(longlake_depth_raster)
  expect_df_spatial(longlake_osm)
  expect_equal(
    nrow(df_spatial(longlake_depth_raster)),
    longlake_depth_raster@nrows * longlake_depth_raster@ncols
  )
  expect_equal(
    nrow(df_spatial(longlake_osm)),
    longlake_osm@nrows * longlake_osm@ncols
  )

  # manual check
  # ggplot(df_spatial(longlake_depth_raster)) + geom_raster(aes(x, y, fill = band1))
})

test_that("stars objects are converted properly by df_spatial", {
  stars_rast <- stars::read_stars(system.file("longlake/longlake_depth.tif", package = "ggspatial"))
  stars_rgb <- stars::read_stars(system.file("longlake/longlake.tif", package = "ggspatial"))

  expect_equal(colnames(df_spatial(stars_rast)), c("x", "y", "value_name", "band1"))
  expect_equal(colnames(df_spatial(stars_rgb)), c("x", "y", "value_name", "band1", "band2", "band3"))
})
