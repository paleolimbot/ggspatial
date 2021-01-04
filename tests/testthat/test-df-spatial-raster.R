
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

  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger(
    "df_spatial(), raster",
    ggplot(df_spatial(longlake_depth_raster)) + ggplot2::geom_raster(aes(x, y, fill = band1))
  )

  vdiffr::expect_doppelganger(
    "df_spatial(), nband raster",
    ggplot(df_spatial(longlake_osm)) + ggplot2::geom_raster(aes(x, y, fill = band1))
  )
})

test_that("x and y coordinates are exactly right for raster df", {
  rst <- raster::raster(
    matrix(
      rep_len(c(0, 1), 9),
      ncol = 3
    ),
    xmn = 0, xmx = 3,
    ymn = 0, ymx = 6
  )

  df <- df_spatial(rst)

  expect_setequal(df$x, c(0.5, 1.5, 2.5))
  expect_setequal(df$y, c(1, 3, 5))

  df2 <- df_spatial(rst, hjust = 0, vjust = 0)
  expect_setequal(df2$x, c(0, 1, 2))
  expect_setequal(df2$y, c(0, 2, 4))

  df3 <- df_spatial(rst, hjust = 1, vjust = 1)
  expect_setequal(df3$x, c(1, 2, 3))
  expect_setequal(df3$y, c(2, 4, 6))
})

test_that("na.rm works on df_spatial.Raster()", {
  load_longlake_data(which = "longlake_osm", raster_format = "raster")
  df <- df_spatial(longlake_osm)
  expect_true(any(is.na(df$band1)))
  expect_true(any(is.na(df$band2)))
  expect_true(any(is.na(df$band3)))

  df_finite <- df_spatial(longlake_osm, na.rm = TRUE)
  expect_false(any(is.na(df_finite$band1)))
  expect_false(any(is.na(df_finite$band2)))
  expect_false(any(is.na(df_finite$band3)))
})
