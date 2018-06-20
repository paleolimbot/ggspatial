context("test-layer-spatial-raster.R")

test_that("layer-spatial works for raster objects", {
  load_longlake_data()

  # should have little grey thing around it
  print(
    ggplot() +
      layer_spatial(longlake_osm, is_annotation = FALSE) +
      layer_spatial(longlake_depthdf)
  )

  # should not have little grey thing around it
  print(
    ggplot() +
      layer_spatial(longlake_osm, is_annotation = TRUE) +
      layer_spatial(longlake_depthdf)
  )

  # grey thing
  print(
    ggplot() +
      layer_spatial(longlake_osm, is_annotation = FALSE) +
      layer_spatial(longlake_depthdf) +
      coord_sf(crs = 3857)
  )

  # no grey thing
  print(
    ggplot() +
      layer_spatial(longlake_osm, is_annotation = TRUE) +
      layer_spatial(longlake_depthdf) +
      coord_sf(crs = 3857)
  )

  # still a problem with CRS systems and "no non-missing arguments to max()"
  expect_silent(
    print(
      ggplot() +
        layer_spatial(longlake_osm, is_annotation = TRUE) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3857)
    )
  )

  expect_silent(
    print(
      ggplot() +
        layer_spatial(longlake_osm, is_annotation = FALSE) +
        layer_spatial(longlake_depthdf)
    )
  )

  # graphical tests so...
  expect_true(TRUE)
})
