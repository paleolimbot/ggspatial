context("test-layer-spatial.R")

test_that("layer_spatial() works as intended", {
  skip_if_not_installed("vdiffr")

  load_longlake_data(which = c("longlake_roadsdf", "longlake_waterdf", "longlake_depthdf"))

  vdiffr::expect_doppelganger(
    "layer_spatial()",
    ggplot() +
      layer_spatial(longlake_roadsdf, size = 1, col = "black") +
      layer_spatial(longlake_roadsdf, size = 0.8, col = "white") +
      layer_spatial(longlake_waterdf, fill = "lightblue", col = NA) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH_M))
  )

  vdiffr::expect_doppelganger(
    "shadow_spatial()",
    ggplot() +
      shadow_spatial(longlake_roadsdf) +
      shadow_spatial(longlake_waterdf) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH_M))
  )

  vdiffr::expect_doppelganger(
    "annotation_spatial()",
    ggplot() +
      annotation_spatial(longlake_roadsdf, size = 1, col = "black") +
      annotation_spatial(longlake_roadsdf, size = 0.8, col = "white") +
      annotation_spatial(longlake_waterdf, fill = "lightblue", col = NA) +
      layer_spatial(longlake_depthdf, aes(col = DEPTH_M))
  )

  # sp objects converted to sf
  expect_is(layer_spatial(suppressWarnings(as(longlake_depthdf, "Spatial")), aes(col = DEPTH_M)), "list")
  expect_is(layer_spatial(longlake_depthdf, aes(col = DEPTH_M)), "list")
})

test_that("3D sp data can be used with layer_spatial()", {
  spoints <- sp::SpatialPoints(
    matrix(c(0, 0, 1, 1, 1, 0), nrow = 2, byrow = TRUE),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84 +no_defs +type=crs")
  )
  expect_silent(ggplot() + layer_spatial(spoints))
})
