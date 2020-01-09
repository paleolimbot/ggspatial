
context("layer-spatial-bbox")

test_that("bbox functions work", {
  load_longlake_data(which = "longlake_waterdf")
  box <- sf::st_bbox(longlake_waterdf)
  expect_is(sf_bbox_to_sf(box), "sf")
  expect_is(sf_bbox_to_sf(box)$geometry, "sfc_POLYGON")
  expect_identical(sf::st_crs(sf_bbox_to_sf(box)), sf::st_crs(longlake_waterdf))
})

test_that("bbox plotting works", {
  load_longlake_data(which = c("longlake_waterdf", "longlake_depthdf"))

  vdiffr::expect_doppelganger(
    "layer_spatial.bbox()",
    ggplot() +
      layer_spatial(sf::st_bbox(longlake_waterdf)) +
      layer_spatial(longlake_depthdf)
  )

  vdiffr::expect_doppelganger(
    "annotation_spatial.bbox()",
    ggplot() +
      annotation_spatial(sf::st_bbox(longlake_waterdf)) +
      layer_spatial(longlake_depthdf)
  )
})
