context("test-geom-spatial-segment")

test_that("geom_spatial_segment() works", {
  skip_if_not_installed("vdiffr")
  skip_if_not_installed("lwgeom")

  cities <- data.frame(
    x = c(-63.58595, 116.41214, 13.50, -149.75),
    y = c(44.64862, 40.19063, 52.51, 61.20),
    city = c("Halifax", "Beijing", "Berlin", "Anchorage")
  )

  cities$xend <- cities$x[c(2, 4, 1, 3)]
  cities$yend <- cities$y[c(2, 4, 1, 3)]

  p <- ggplot(cities, aes(x, y, xend = xend, yend = yend)) +
    geom_spatial_point(crs = 4326) +
    # view of the north pole
    coord_sf(crs = 3995)

  expect_message(
    ggplot2::ggplot_build(p + geom_spatial_segment()),
    "Assuming `crs = 4326`"
  )

  expect_silent(
    ggplot2::ggplot_build(p + geom_spatial_segment(crs = 4326))
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_segment(), great circle wrap",
    p + geom_spatial_segment(
      crs = 4326,
      great_circle = TRUE,
      wrap_dateline = TRUE,
      arrow = grid::arrow()
    )
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_segment(), great circle no wrap",
    p + geom_spatial_segment(
      crs = 4326,
      great_circle = TRUE,
      wrap_dateline = FALSE,
      arrow = grid::arrow()
    )
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_segment(), no great circle",
    p + geom_spatial_segment(crs = 4326, great_circle = FALSE)
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_segment(), no great circle + detail",
    p + geom_spatial_segment(detail = 100, great_circle = FALSE, crs = 4326)
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_segment(), great circle merc",
    # don't use halifax -> beijing for this one
    ggplot(
      cities[cities$city != "Halifax", ],
      aes(x, y, xend = xend, yend = yend)
    ) +
      geom_spatial_point(crs = 4326) +
      coord_sf(crs = 3857) +
      geom_spatial_segment(crs = 4326, great_circle = TRUE)
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_segment(), no great circle merc",
    ggplot(cities, aes(x, y, xend = xend, yend = yend)) +
      geom_spatial_point(crs = 4326) +
      coord_sf(crs = 3857) +
      geom_spatial_segment(crs = 4326, great_circle = FALSE)
  )
})
