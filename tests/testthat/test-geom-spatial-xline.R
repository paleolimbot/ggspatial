
test_that("annotation_spatial_(h|v)line work with coord_sf()", {
  skip_if_not_installed("vdiffr")

  cities <- data.frame(
    x = c(-63.58595, 116.41214, 0),
    y = c(44.64862, 40.19063, 89.9),
    city = c("Halifax", "Beijing", "North Pole")
  )

  p <- ggplot(cities, aes(x, y, label = city)) +
    geom_spatial_point(crs = 4326) +
    # view of the north pole
    coord_sf(crs = 3995)

  expect_doppelganger(
    "spatial_xline/mapped aes",
    p +
      annotation_spatial_hline(
        aes(intercept = y, col = city),
        crs = 4326
      ) +
      annotation_spatial_vline(
        aes(intercept = x, lty = city),
        crs = 4326
      )
  )

  expect_message(
    expect_doppelganger(
      "spatial_hline() NULL crs",
      p +
        annotation_spatial_hline(intercept = seq(0, 80, by = 10), limits = c(-180, 180))
    ),
    "Assuming `crs = 4326`"
  )

  expect_message(
    expect_doppelganger(
      "spatial_vline() NULL crs",
      p +
        annotation_spatial_vline(intercept = seq(-180, 180, by = 10), limits = c(0, 90))
    ),
    "Assuming `crs = 4326`"
  )

  expect_silent(
    ggplot2::ggplot_gtable(
      ggplot2::ggplot_build(
        p +
          annotation_spatial_vline(
            intercept = seq(-180, 180, by = 10), limits = c(0, 90),
            crs = 4326
          )
      )
    )
  )

  expect_silent(
    ggplot2::ggplot_gtable(
      ggplot2::ggplot_build(
        p +
          annotation_spatial_hline(
            intercept = seq(0, 80, by = 10),
            limits = c(-180, 180),
            crs = 4326
          )
      )
    )
  )

  expect_doppelganger(
    "hline/vline + guessed limits",
    p +
      annotation_spatial_hline(
        intercept = seq(0, 80, by = 10),
        crs = 4326
      ) +
      annotation_spatial_vline(
        intercept = seq(-180, 180, by = 10),
        crs = 4326
      )
  )
})


test_that("annotation_spatial_xline() works with a warning in cartesian coords", {
  skip_if_not_installed("vdiffr")

  cities <- data.frame(
    x = c(-63.58595, 116.41214, 0),
    y = c(44.64862, 40.19063, 89.9),
    city = c("Halifax", "Beijing", "North Pole")
  )

  p <- ggplot(cities, aes(x, y, label = city)) +
    ggplot2::geom_point()

  expect_warning(
    expect_doppelganger(
      "annotation_spatial_hline()",
      p + annotation_spatial_hline(intercept = 60, crs = 4326)
    ),
    "Ignoring transformation"
  )

  expect_warning(
    expect_doppelganger(
      "annotation_spatial_vline()",
      p + annotation_spatial_vline(intercept = 15, crs = 4326)
    ),
    "Ignoring transformation"
  )

  p2 <- ggplot(cities, aes(x, y)) +
    geom_spatial_point(crs = 4326) +
    coord_sf(crs = 4326)

  expect_doppelganger(
    "spatial_hline, coord_sf/4326",
    p2 + annotation_spatial_hline(intercept = 60)
  )

  expect_doppelganger(
    "spatial_vline, coord_sf/4326",
    p2 + annotation_spatial_vline(intercept = 15, crs = 4326)
  )

})
