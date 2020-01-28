
context("test-df-spatial-raster")

test_that("Raster* objects are converted properly by df_spatial", {
  skip_if_not_installed("raster")

  load_longlake_data(which = c("longlake_depth_raster", "longlake_osm"), raster_format = "raster")
  expect_df_spatial(longlake_depth_raster, "band1")
  expect_df_spatial(longlake_osm, c("band1", "band2", "band3"))
  expect_equal(
    nrow(df_spatial(longlake_depth_raster)),
    longlake_depth_raster@nrows * longlake_depth_raster@ncols
  )
  expect_equal(
    nrow(df_spatial(longlake_osm)),
    longlake_osm@nrows * longlake_osm@ncols
  )

  vdiffr::expect_doppelganger(
    "df_spatial(), raster",
    ggplot(df_spatial(longlake_depth_raster)) + ggplot2::geom_raster(aes(x, y, fill = band1))
  )

  vdiffr::expect_doppelganger(
    "df_spatial(), nband raster",
    ggplot(df_spatial(longlake_osm)) + ggplot2::geom_raster(aes(x, y, fill = band1))
  )
})
