
test_that("bbox functions work", {
  load_longlake_data(which = "longlake_waterdf")
  box <- sf::st_bbox(longlake_waterdf)
  expect_s3_class(sf_bbox_to_sf(box), "sf")
  expect_s3_class(sf_bbox_to_sf(box)$geometry, "sfc_POLYGON")
  expect_identical(sf::st_crs(sf_bbox_to_sf(box)), sf::st_crs(longlake_waterdf))
  expect_identical(sf::st_bbox(sf_bbox_to_sf(box)), box)
})

test_that("bbox functions work with detail arg", {
  load_longlake_data(which = "longlake_waterdf")
  box <- sf::st_bbox(longlake_waterdf)
  expect_s3_class(sf_bbox_to_sf(box, detail = 30), "sf")
  expect_s3_class(sf_bbox_to_sf(box, detail = 30)$geometry, "sfc_POLYGON")
  expect_identical(
    sf::st_crs(sf_bbox_to_sf(box, detail = 30)),
    sf::st_crs(longlake_waterdf)
  )
  expect_identical(sf::st_bbox(sf_bbox_to_sf(box, detail= 30)), box)

  poly_detail <- sf_bbox_to_sf(box, detail = 2)
  poly_no_detail <- sf_bbox_to_sf(box, detail = NULL)
  expect_equal(nrow(poly_no_detail$geometry[[1]][[1]]), 5)
  expect_true(nrow(poly_detail$geometry[[1]][[1]]) > 5)
})

test_that("bbox plotting works", {
  skip_if_not_installed("vdiffr")

  load_longlake_data(which = c("longlake_waterdf", "longlake_depthdf"))

  expect_doppelganger(
    "layer_spatial.bbox()",
    ggplot() +
      layer_spatial(sf::st_bbox(longlake_waterdf)) +
      layer_spatial(longlake_depthdf)
  )

  expect_doppelganger(
    "annotation_spatial.bbox()",
    ggplot() +
      annotation_spatial(sf::st_bbox(longlake_waterdf)) +
      layer_spatial(longlake_depthdf)
  )
})
