
test_that("geom_spatial_(h|v)line work", {
  cities <- data.frame(
    x = c(-63.58595, 116.41214, 0),
    y = c(44.64862, 40.19063, 89.9),
    city = c("Halifax", "Beijing", "North Pole")
  )

  p <- ggplot(cities, aes(x, y, label = city)) +
    geom_spatial_point(crs = 4326) +
    # view of the north pole
    coord_sf(crs = 3995)
})


test_that("annotation_spatial_xline() works with a warning in cartesian coords", {
  cities <- data.frame(
    x = c(-63.58595, 116.41214, 0),
    y = c(44.64862, 40.19063, 89.9),
    city = c("Halifax", "Beijing", "North Pole")
  )

  p <- ggplot(cities, aes(x, y, label = city)) +
    geom_point()

  expect_warning(
    vdiffr::expect_doppelganger(
      "annotation_spatial_hline()",
      p + annotation_spatial_hline(intercept = 60)
    ),
    "Ignoring transformation"
  )

  expect_warning(
    vdiffr::expect_doppelganger(
      "annotation_spatial_vline()",
      p + annotation_spatial_vline(intercept = 15)
    ),
    "Ignoring transformation"
  )
})
