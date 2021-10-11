
context("test-df-spatial-terra")

test_that("SpatRaster objects are converted properly by df_spatial", {
  skip_if_not_installed("terra")

  load_longlake_data(
    which = c(
      "longlake_depth_terra",
      "longlake_osm_terra"
    ),
    raster_format = "terra"
  )

  expect_s4_class(longlake_depth_terra, "SpatRaster")
  expect_s4_class(longlake_osm_terra, "SpatRaster")

  expect_df_spatial(longlake_depth_terra, "band1")
  expect_df_spatial(longlake_osm_terra, c("band1", "band2", "band3"))
  expect_equal(
    nrow(df_spatial(longlake_depth_terra)),
    terra::nrow(longlake_depth_terra) * terra::ncol(longlake_depth_terra)
  )

  # Get the same result than I would get with raster
  load_longlake_data(raster_format = "raster")

  df_rast <- df_spatial(longlake_osm)
  df_terra <- df_spatial(longlake_osm_terra)

  expect_equal(df_terra, df_rast)

  expect_equal(
    nrow(df_spatial(longlake_osm_terra)),
    terra::nrow(longlake_osm_terra) * terra::ncol(longlake_osm_terra)
  )

  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger(
    "df_spatial(), terra",
    ggplot(df_spatial(longlake_depth_terra)) +
      ggplot2::geom_raster(aes(x, y, fill = band1))
  )

  vdiffr::expect_doppelganger(
    "df_spatial(), nband terra",
    ggplot(df_spatial(longlake_osm_terra)) +
      ggplot2::geom_raster(aes(x, y, fill = band1))
  )
})



test_that("na.rm works on df_spatial.SpatRaster()", {
  load_longlake_data(which = "longlake_osm_terra", raster_format = "terra")
  df <- df_spatial(longlake_osm_terra)
  expect_true(any(is.na(df$band1)))
  expect_true(any(is.na(df$band2)))
  expect_true(any(is.na(df$band3)))

  df_finite <- df_spatial(longlake_osm_terra, na.rm = TRUE)
  expect_false(any(is.na(df_finite$band1)))
  expect_false(any(is.na(df_finite$band2)))
  expect_false(any(is.na(df_finite$band3)))
})
