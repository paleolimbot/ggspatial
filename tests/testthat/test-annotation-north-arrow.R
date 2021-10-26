
test_that("north arrow drawing works", {
  skip_if_not_installed("vdiffr")

  load_longlake_data(which = "longlake_waterdf")

  expect_doppelganger(
    "north arrow default (cartesian)",
    ggplot() +
      ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_north_arrow()
  )

  expect_doppelganger(
    "north arrow default (sf)",
    ggplot() +
      geom_sf(data = longlake_waterdf) +
      annotation_north_arrow()
  )
})

test_that("north arrow math is correct", {
  # -63 longitude is the centre of the UTM 20 timezone
  crs_points <- sf::st_sfc(
    sf::st_point(c(-63, 45)),
    sf::st_point(c(-63, 60)),
    sf::st_point(c(-63, 80)),

    sf::st_point(c(-66, 45)),
    sf::st_point(c(-66, 60)),
    sf::st_point(c(-66, 80)),

    sf::st_point(c(-60, 45)),
    sf::st_point(c(-60, 60)),
    sf::st_point(c(-60, 80)),

    crs = 4326
  ) %>%
    sf::st_transform(32620) %>%
    sf::st_coordinates() %>%
    as.data.frame()

  crs_points$north_angle <- mapply(true_north, crs_points$X, crs_points$Y, crs = 32620)

  expect_true(all(abs(crs_points$north_angle[c(1, 2, 3)]) < 1e-1))
  expect_true(all(crs_points$north_angle[c(4, 5, 6)] > 0))
  expect_true(
    abs(sum(crs_points$north_angle[c(4, 5, 6)], crs_points$north_angle[c(7, 8, 9)])) < 1e-1)
})

test_that("true north arrow points in the right direction", {
  skip_if_not_installed("vdiffr")

  load_longlake_data(which = "longlake_waterdf")

  expect_doppelganger(
    "north arrows (grid north)",
    ggplot() +
      geom_sf(data = longlake_waterdf) +
      annotation_north_arrow(location = "tl", which_north = "grid") +
      annotation_north_arrow(location = "tr", which_north = "grid") +
      annotation_north_arrow(location = "bl", which_north = "grid") +
      annotation_north_arrow(location = "br", which_north = "grid") +
      coord_sf(crs = 26922) # utm zone 22...has some angle to it
  )

  expect_doppelganger(
    "north arrows (true north)",
    ggplot() +
      geom_sf(data = longlake_waterdf) +
      annotation_north_arrow(location = "tl", which_north = "true") +
      annotation_north_arrow(location = "tr", which_north = "true") +
      annotation_north_arrow(location = "bl", which_north = "true") +
      annotation_north_arrow(location = "br", which_north = "true") +
      coord_sf(crs = 26922) # utm zone 22...has some angle to it
  )

  expect_doppelganger(
    "north arrows (true north, global)",
    ggplot() +
      geom_spatial_point(
        mapping = aes(x, y),
        data = data.frame(x = c(-63.58595, 116.41214), y = c(44.64862, 40.19063), city = c("Halifax", "Beijing")),
        crs = 4326
      ) +
      annotation_north_arrow(location = "tl", which_north = "true") +
      annotation_north_arrow(location = "tr", which_north = "true") +
      annotation_north_arrow(location = "bl", which_north = "true") +
      annotation_north_arrow(location = "br", which_north = "true") +
      coord_sf(crs = 3995)
  )
})

test_that("all built-in styles of north arrow rotate properly", {
  skip_if_not_installed("vdiffr")

  p <- ggplot() +
    geom_spatial_point(
      mapping = aes(x, y),
      data = data.frame(x = c(-63.58595, 116.41214), y = c(44.64862, 40.19063), city = c("Halifax", "Beijing")),
      crs = 4326
    ) +
    coord_sf(crs = 3995)

  expect_doppelganger(
    "north arrows (styles as functions)",
    p +
      annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering) +
      annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
      annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_minimal) +
      annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_nautical)
  )

  expect_doppelganger(
    "north arrows (styles as calls)",
    p +
      annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering()) +
      annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
      annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_minimal()) +
      annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_nautical())
  )

  expect_doppelganger(
    "north arrows (styles, grid north)",
    p +
      annotation_north_arrow(location = "tl", which_north = "grid", style = north_arrow_orienteering) +
      annotation_north_arrow(location = "tr", which_north = "grid", style = north_arrow_fancy_orienteering) +
      annotation_north_arrow(location = "bl", which_north = "grid", style = north_arrow_minimal) +
      annotation_north_arrow(location = "br", which_north = "grid", style = north_arrow_nautical)
  )
})

test_that("colour on north arrows is propogated through for all north arrow styles", {
  skip_if_not_installed("vdiffr")

  p <- ggplot() +
    geom_spatial_point(
      mapping = aes(x, y),
      data = data.frame(x = c(-63.58595, 116.41214), y = c(44.64862, 40.19063), city = c("Halifax", "Beijing")),
      crs = 4326
    ) +
    coord_sf(crs = 3995)

  expect_doppelganger(
    "north arrows (text colours)",
    p +
      annotation_north_arrow(location = "tl", style = north_arrow_orienteering(text_col = "purple")) +
      annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering(text_col = "blue")) +
      annotation_north_arrow(location = "bl", style = north_arrow_minimal(text_col = "green")) +
      annotation_north_arrow(location = "br", style = north_arrow_nautical(text_col = "red"))
  )
})

test_that("certain parameters can be passed as aesthetics to show up on different panels", {
  skip_if_not_installed("vdiffr")

  load_longlake_data(which = "longlake_waterdf")

  # note that passing NA in the label column makes this not work
  arrow_params <- tibble::tibble(
    label = c("Round Lake", "Long Lake"),
    which_north = c("true", "grid"),
    location = c("bl", "tr")
  )

  expect_doppelganger(
    "north arrows (use of aesthetics)",
    ggplot() +
      layer_spatial(longlake_waterdf) +
      annotation_north_arrow(
        aes(which_north = which_north, location = location),
        data = arrow_params
      ) +
      ggplot2::facet_wrap(~label)
  )
})
