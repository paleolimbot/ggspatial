
test_that("stars objects are converted properly by df_spatial", {
  skip_if_not_installed("stars")

  load_longlake_data(
    which = c(
      "longlake_depth_raster",
      "longlake_osm"
    ),
    raster_format = "stars"
  )

  expect_s3_class(longlake_depth_raster, "stars")
  expect_s3_class(longlake_osm, "stars")

  expect_df_spatial(longlake_depth_raster, "band1")
  expect_df_spatial(longlake_osm, c("band1", "band2", "band3"))
  expect_equal(
    nrow(df_spatial(longlake_depth_raster)),
    prod(dim(longlake_depth_raster))
  )

  df_stars <- df_spatial(longlake_osm)

  skip_if_not_installed("terra")
  # Get the same result than I would get with terra
  test_env <- new.env(parent = emptyenv())
  load_longlake_data(
    env = test_env,
    raster_format = "terra"
  )

  expect_s4_class(test_env$longlake_osm, "SpatRaster")
  df_terra <- df_spatial(test_env$longlake_osm)


  expect_equal(df_stars, df_terra)


  skip_if_not_installed("vdiffr")

  # Recheck
  expect_s3_class(longlake_depth_raster, "stars")
  expect_s3_class(longlake_osm, "stars")

  expect_doppelganger_extra(
    "df_spatial(), stars",
    ggplot(df_spatial(longlake_depth_raster)) +
      ggplot2::geom_raster(aes(x, y, fill = band1))
  )

  expect_doppelganger_extra(
    "df_spatial(), nband stars",
    ggplot(df_spatial(longlake_osm)) +
      ggplot2::geom_raster(aes(x, y, fill = band1))
  )
})

test_that("x and y coordinates are exactly right for stars df", {
  skip_if_not_installed("stars")

  # Create stars object from df
  df <- data.frame(
    x = rep(seq(0.5, 2.5, 1), 3),
    y = rep(seq(5, 1, -2), 3)
  )

  rst <- stars::st_as_stars(df)
  rst$band <- matrix(
    rep_len(c(0, 1), 9),
    ncol = 3
  )

  df <- df_spatial(rst)

  expect_setequal(df$x, c(0.5, 1.5, 2.5))
  expect_setequal(df$y, c(1, 3, 5))
})

test_that("na.rm works on df_spatial.stars()", {
  skip_if_not_installed("stars")

  load_longlake_data(which = "longlake_osm", raster_format = "stars")
  expect_s3_class(longlake_osm, "stars")
  df <- df_spatial(longlake_osm)
  expect_true(any(is.na(df$band1)))
  expect_true(any(is.na(df$band2)))
  expect_true(any(is.na(df$band3)))

  df_finite <- df_spatial(longlake_osm, na.rm = TRUE)
  expect_false(any(is.na(df_finite$band1)))
  expect_false(any(is.na(df_finite$band2)))
  expect_false(any(is.na(df_finite$band3)))
})


test_that("Factor stars objects are properly converted", {
  skip_if_not_installed("stars")
  # Test factor

  # Create stars object from df
  df <- data.frame(
    x = rep(seq(0.5, 2.5, 1), 3),
    y = rep(seq(5, 1, -2), 3)
  )

  r_num <- stars::st_as_stars(df)
  # Test here also a change of name: band_with_another_name
  r_num$band_with_another_name <- matrix(
    rep_len(c(0, 1), 9),
    ncol = 3
  )

  # Assess factors

  r_fac <- r_num
  factrs <- c("grassland", "savannah", "forest")
  landcover <- factor(factrs, levels = factrs)
  r_fac$band_with_another_name <- landcover


  expect_true(inherits(df_spatial(r_num)[["band1"]], "numeric"))
  expect_true(inherits(df_spatial(r_fac)[["band1"]], "factor"))
  expect_identical(
    levels(df_spatial(r_fac)[["band1"]]),
    c("grassland", "savannah", "forest")
  )
})

test_that("Handle multi-layer objects stars", {
  skip_if_not_installed("stars")


  df <- data.frame(
    x = rep(seq(0.5, 2.5, 1), 6),
    y = rep(seq(5, 1, -2), 6),
    band = rep(seq(1,6,1), 6),
    value = c(0,1)
  )

  r_six_layers <- stars::st_as_stars(df, dims = c("x", "y", "band"))
  
  expect_equal(as.integer(dim(r_six_layers))[3], 6)
  expect_df_spatial(
    r_six_layers,
    paste0(
      "band",
      seq_len(dim(r_six_layers)[3])
    )
  )
})
