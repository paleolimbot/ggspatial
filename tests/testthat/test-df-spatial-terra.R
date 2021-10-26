
test_that("SpatRaster objects are converted properly by df_spatial", {
  skip_if_not_installed("terra")

  load_longlake_data(
    which = c(
      "longlake_depth_raster",
      "longlake_osm"
    ),
    raster_format = "terra"
  )

  expect_s4_class(longlake_depth_raster, "SpatRaster")
  expect_s4_class(longlake_osm, "SpatRaster")

  expect_df_spatial(longlake_depth_raster, "band1")
  expect_df_spatial(longlake_osm, c("band1", "band2", "band3"))
  expect_equal(
    nrow(df_spatial(longlake_depth_raster)),
    terra::nrow(longlake_depth_raster) * terra::ncol(longlake_depth_raster)
  )

  expect_s4_class(longlake_osm, "SpatRaster")
  df_terra <- df_spatial(longlake_osm)

  skip_if_not_installed("raster")
  # Get the same result than I would get with raster
  test_env <- new.env(parent = emptyenv())
  load_longlake_data(
    env = test_env,
    raster_format = "raster"
  )

  expect_s4_class(test_env$longlake_osm, "Raster")
  df_rast <- df_spatial(test_env$longlake_osm)


  expect_equal(df_terra, df_rast)


  skip_if_not_installed("vdiffr")

  # Recheck
  expect_s4_class(longlake_depth_raster, "SpatRaster")
  expect_s4_class(longlake_osm, "SpatRaster")

  expect_doppelganger_extra(
    "df_spatial(), terra",
    ggplot(df_spatial(longlake_depth_raster)) +
      ggplot2::geom_raster(aes(x, y, fill = band1))
  )

  expect_doppelganger_extra(
    "df_spatial(), nband terra",
    ggplot(df_spatial(longlake_osm)) +
      ggplot2::geom_raster(aes(x, y, fill = band1))
  )
})

test_that("x and y coordinates are exactly right for terra df", {

  skip_if_not_installed("terra")

  rst <- terra::rast(
    matrix(
      rep_len(c(0, 1), 9),
      ncol = 3
    )
  )

  terra::ext(rst) <- c(0, 3, 0, 6)

  df <- df_spatial(rst)

  expect_setequal(df$x, c(0.5, 1.5, 2.5))
  expect_setequal(df$y, c(1, 3, 5))
})

test_that("na.rm works on df_spatial.SpatRaster()", {

  skip_if_not_installed("terra")

  load_longlake_data(which = "longlake_osm", raster_format = "terra")
  expect_s4_class(longlake_osm, "SpatRaster")
  df <- df_spatial(longlake_osm)
  expect_true(any(is.na(df$band1)))
  expect_true(any(is.na(df$band2)))
  expect_true(any(is.na(df$band3)))

  df_finite <- df_spatial(longlake_osm, na.rm = TRUE)
  expect_false(any(is.na(df_finite$band1)))
  expect_false(any(is.na(df_finite$band2)))
  expect_false(any(is.na(df_finite$band3)))
})

test_that("Factor SpatRast objects are properly converted", {

  skip_if_not_installed("terra")

  # Test factor SpatRast
  r_num <- terra::rast(
    nrows = 3, ncols = 3,
    crs = sf::st_crs(4326)$proj4string,
    xmin = 0, xmax = 3,
    ymin = 0, ymax = 3,
    vals = c(1, 2, 3, 3, 1, 2, 2, 3, 1)
  )

  r_fac <- terra::merge(r_num, r_num)

  levels(r_fac) <-
    data.frame(ID = 1:3, landcover = c("grassland", "savannah", "forest"))

  expect_true(inherits(df_spatial(r_num)[["band1"]], "numeric"))
  expect_true(inherits(df_spatial(r_fac)[["band1"]], "factor"))
  expect_identical(
    levels(df_spatial(r_fac)[["band1"]]),
    c("grassland", "savannah", "forest")
  )
})

test_that("Handle multi-layer objects", {
  skip_if_not_installed("terra")

  r_six_layers <- terra::rast(
    nrows = 3, ncols = 3,
    nlyrs = 6,
    crs = sf::st_crs(4326)$wkt,
    vals = c(1, 2, 3, 3, 1, 2, 2, 3, 1)
  )


  expect_equal(terra::nlyr(r_six_layers), 6)
  expect_df_spatial(
    r_six_layers,
    paste0(
      "band",
      seq_len(terra::nlyr(r_six_layers))
    )
  )
})
