context("test-layer-spatial.R")

test_that("layer_spatial() works as intended", {
  load_longlake_data()

  print(
    ggplot() +
      layer_spatial(longlake_roadsdf, size = 1, col = "black") +
      layer_spatial(longlake_roadsdf, size = 0.8, col = "white") +
      layer_spatial(longlake_waterdf, fill = "lightblue", col = NA) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH_M)) +
      labs(caption = "Should show long lake, round lake, etc.")
  )

  print(
    ggplot() +
      annotation_spatial(longlake_roadsdf, size = 1, col = "black") +
      annotation_spatial(longlake_roadsdf, size = 0.8, col = "white") +
      annotation_spatial(longlake_waterdf, fill = "lightblue", col = NA) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH_M)) +
      labs(caption = "Should show only long lake")
  )

  # sp objects converted to sf
  ggplot() + layer_spatial(as(longlake_depthdf, "Spatial"), aes(col = DEPTH_M))
  ggplot() + layer_spatial(longlake_depthdf, aes(col = DEPTH_M))

  # visual test
  expect_true(TRUE)
})

# this is depedent on the fix of an sf bug, which has been closed but not released to CRAN
# https://github.com/r-spatial/sf/issues/905
#
# test_that("3D sp data can be used with layer_spatial()", {
#   spoints <- sp::SpatialPoints(
#     matrix(c(0, 0, 1, 1, 1, 0), nrow = 2, byrow = TRUE),
#     proj4string = sp::CRS("+init=epsg:4326")
#   )
#   expect_silent(ggplot() + layer_spatial(spoints))
# })
