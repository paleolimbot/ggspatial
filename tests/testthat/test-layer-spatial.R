context("test-layer-spatial.R")

test_that("layer_spatial() works as intended", {
  load_longlake_data()

  print(
    ggplot() +
      layer_spatial(longlake_roadsdf, size = 1, col = "black") +
      layer_spatial(longlake_roadsdf, size = 0.8, col = "white") +
      layer_spatial(longlake_waterdf, fill = "lightblue", col = NA) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH.M)) +
      labs(caption = "Should show long lake, round lake, etc.")
  )

  print(
    ggplot() +
      annotation_spatial(longlake_roadsdf, size = 1, col = "black") +
      annotation_spatial(longlake_roadsdf, size = 0.8, col = "white") +
      annotation_spatial(longlake_waterdf, fill = "lightblue", col = NA) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH.M)) +
      labs(caption = "Should show only long lake")
  )

  # sp objects converted to sf
  ggplot() + layer_spatial(as(longlake_depthdf, "Spatial"), aes(col = DEPTH.M))
  ggplot() + layer_spatial(longlake_depthdf, aes(col = DEPTH.M))

  # visual test
  expect_true(TRUE)
})
