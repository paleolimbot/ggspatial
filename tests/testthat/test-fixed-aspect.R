context("test-fixed-aspect")

test_that("aspect adjuster works", {

  equal <- adjust_aspect(c(0, 10), c(0, 10), desired_aspect = 1)
  expect_equal(equal$xlim, c(0, 10))
  expect_equal(equal$ylim, c(0, 10))

  wider <- adjust_aspect(c(0, 10), c(0, 10), desired_aspect = 2)
  expect_equal(wider$xlim, c(-5, 15))
  expect_equal(wider$ylim, c(0, 10))

  taller <- adjust_aspect(c(0, 10), c(0, 10), desired_aspect = 0.5)
  expect_equal(taller$xlim, c(0, 10))
  expect_equal(taller$ylim, c(-5, 15))

  non_finite <- adjust_aspect(c(NA, 10), c(0, 10), desired_aspect = 1)
  expect_null(non_finite$xlim)
  expect_null(non_finite$ylim)
})

test_that("fixed aspect works", {
  df <- tibble::tibble(x =  0:5, y =  seq(0, 10, length.out = 6))
  p <- ggplot(df, aes(x, y)) +
    ggplot2::geom_point() +
    fixed_plot_aspect(ratio = 1) +
    ggplot2::coord_fixed()

  expect_is(p, "gg_fixed_plot_aspect")

  built <- ggplot2::ggplot_build(p)
  xlim <- built$layout$panel_scales_x[[1]]$get_limits()
  ylim <- built$layout$panel_scales_y[[1]]$get_limits()

  expect_equal(xlim, c(-2.5, 7.5))
  expect_equal(ylim, c(0, 10))

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("stat_aspect() square", p)
})

test_that("fixed aspect does not fail with  zero layers", {
  p <- ggplot() + fixed_plot_aspect()
  expect_is(p, "gg_fixed_plot_aspect")
  expect_is(ggplot2::ggplot_build(p), "ggplot_built")
})

test_that("fixed aspect works with spatial data", {
  cities <- data.frame(
    x = c(-63.58595, 116.41214),
    y = c(44.64862, 40.19063),
    city = c("Halifax", "Beijing")
  )

  cities_sf <- sf::st_as_sf(cities, coords = c("x", "y"), crs = 4326)

  p <- ggplot() +
    layer_spatial(cities_sf) +
    coord_sf(crs = 3857) +
    fixed_plot_aspect(0.5)

  expect_is(p, "gg_fixed_plot_aspect")
  built <- ggplot2::ggplot_build(p)
  xlim <- built$layout$panel_scales_x[[1]]$get_limits()
  ylim <- built$layout$panel_scales_y[[1]]$get_limits()
  expect_equal(diff(xlim) / diff(ylim), 0.5)
})

