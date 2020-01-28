
context("test-df-spatial-stars")

test_that("stars objects are converted properly by df_spatial", {
  skip_if_not_installed("raster")
  skip_if_not_installed("stars")

  load_longlake_data(which = c("longlake_osm", "longlake_depth_raster"))
  stars_rast <- stars::read_stars(system.file("longlake/longlake_depth.tif", package = "ggspatial"))
  stars_rgb <- stars::read_stars(system.file("longlake/longlake.tif", package = "ggspatial"))

  stars_rast_df <- df_spatial(stars_rast)
  stars_rgb_df <- df_spatial(stars_rgb)

  expect_equal(colnames(stars_rast_df), c("x", "y", "value_name", "band1"))
  expect_equal(colnames(stars_rgb_df), c("x", "y", "value_name", "band1", "band2", "band3"))

  rast_rast_df <- df_spatial(longlake_depth_raster)
  rast_rgb_df <- df_spatial(longlake_osm)

  expect_equal(stars_rast_df$band1, rast_rast_df$band1)
  expect_equal(stars_rast_df$x, rast_rast_df$x)
  expect_equal(stars_rast_df$y, rast_rast_df$y)

  # coords are in a different order for rgb, probalby because of
  # gather/spread
  expect_setequal(stars_rgb_df$band1, rast_rgb_df$band1)
  expect_setequal(stars_rgb_df$band2, rast_rgb_df$band2)
  expect_setequal(stars_rgb_df$band3, rast_rgb_df$band3)

  # near numerical equality for coords
  expect_true(
    all(
      abs(head(sort(unique(stars_rgb_df$x))) - head(sort(unique(rast_rgb_df$x)))) < 1e-9
    )
  )

  expect_true(
    all(
      abs(head(sort(unique(stars_rgb_df$y))) - head(sort(unique(rast_rgb_df$y)))) < 1e-9
    )
  )
})
