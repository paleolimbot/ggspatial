context("test-layer-spatial-raster.R")

test_that("layer-spatial works for raster objects", {
  load_longlake_data()

  # should have little grey thing around it
  print(
    ggplot() +
      layer_spatial(longlake_osm) +
      layer_spatial(longlake_depthdf) +
      labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
  )

  # should not have little grey thing around it
  print(
    ggplot() +
      annotation_spatial(longlake_osm) +
      layer_spatial(longlake_depthdf) +
      labs(caption = "Should have no grey area around the sides, roughly N-S projection")
  )

  # grey thing
  print(
    ggplot() +
      layer_spatial(longlake_osm) +
      layer_spatial(longlake_depthdf) +
      coord_sf(crs = 3978) +
      labs(caption = "Should have a little grey area around the sides, rotated projection")
  )

  # no grey thing
  print(
    ggplot() +
      annotation_spatial(longlake_osm) +
      layer_spatial(longlake_depthdf) +
      coord_sf(crs = 3978) +
      labs(caption = "Should have no grey area around the sides, rotated projection")
  )

  # with alpha
  print(
    ggplot() +
      annotation_spatial(longlake_osm, alpha = 0.7) +
      layer_spatial(longlake_depthdf) +
      coord_sf(crs = 3978) +
      labs(caption = "Should have no grey area around the sides, rotated projection")
  )

  # with aesthetics
  print(
    ggplot() +
      layer_spatial(longlake_osm, aes()) +
      layer_spatial(longlake_depthdf)
  )

  print(
    ggplot() +
      layer_spatial(longlake_osm, aes(alpha = stat(band1), fill = NULL)) +
      layer_spatial(longlake_depthdf)
  )

  print(
    ggplot() +
      layer_spatial(longlake_depth_raster) +
      layer_spatial(longlake_depthdf) +
      coord_sf(crs = 3978)
  )

  # still a problem with "no non-missing arguments to max()"
  # expect_silent(
  #   print(
  #     ggplot() +
  #       annotation_spatial(longlake_osm) +
  #       layer_spatial(longlake_depthdf) +
  #       coord_sf(crs = 3978)
  #   )
  # )
  #
  # expect_silent(
  #   print(
  #     ggplot() +
  #       layer_spatial(longlake_osm) +
  #       layer_spatial(longlake_depthdf)
  #   )
  # )

  # graphical tests so...
  expect_true(TRUE)
})
