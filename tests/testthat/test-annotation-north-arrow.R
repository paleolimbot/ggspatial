context("test-annotation-north-arrow.R")

test_that("north arrow drawing works", {
  load_longlake_data()

  print(
    ggplot() +
      geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_north_arrow() +
      labs(caption = "default behaviour of north arrow in cartesian coordinates")
  )

  print(
    ggplot() +
      geom_sf(data = longlake_waterdf) +
      annotation_north_arrow() +
      labs(caption = "default behaviour of north arrow in sf coordinates")
  )

  expect_true(TRUE)
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
    sf::st_transform(26920) %>%
    sf::st_coordinates() %>%
    as.data.frame()

  crs_points$north_angle <- mapply(true_north, crs_points$X, crs_points$Y, crs = 26920)

  expect_true(all(crs_points$north_angle[c(1, 2, 3)] == 0))
  expect_true(all(crs_points$north_angle[c(4, 5, 6)] > 0))
  expect_equal(sum(crs_points$north_angle[c(4, 5, 6)], crs_points$north_angle[c(7, 8, 9)]), 0)
})

test_that("true north arrow points in the right direction", {
  load_longlake_data()

  print(
    ggplot() +
      geom_sf(data = longlake_waterdf) +
      annotation_north_arrow(location = "tl", which_north = "grid") +
      annotation_north_arrow(location = "tr", which_north = "grid") +
      annotation_north_arrow(location = "bl", which_north = "grid") +
      annotation_north_arrow(location = "br", which_north = "grid") +
      coord_sf(crs = 26922) + # utm zone 22...has some angle to it
      labs(caption = "North arrow pointing to 'grid' north")
  )

  print(
    ggplot() +
      geom_sf(data = longlake_waterdf) +
      annotation_north_arrow(location = "tl", which_north = "true") +
      annotation_north_arrow(location = "tr", which_north = "true") +
      annotation_north_arrow(location = "bl", which_north = "true") +
      annotation_north_arrow(location = "br", which_north = "true") +
      coord_sf(crs = 26922) + # utm zone 22...has some angle to it
      labs(caption = "North arrow pointing to 'true' north, 'N' is straight up and down")
  )

  print(
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
      coord_sf(crs = 3995) +
      labs(caption = "All four arrows should point to the north pole")
  )

  expect_true(TRUE)
})

test_that("all built-in styles of north arrow rotate properly", {

  p <- ggplot() +
    geom_spatial_point(
      mapping = aes(x, y),
      data = data.frame(x = c(-63.58595, 116.41214), y = c(44.64862, 40.19063), city = c("Halifax", "Beijing")),
      crs = 4326
    ) +
    coord_sf(crs = 3995)

  print(
    p +
      annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering) +
      annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering) +
      annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_minimal) +
      annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_nautical) +
      labs(caption = "All four arrows should point to the north pole and have different styles")
  )

  print(
    p +
      annotation_north_arrow(location = "tl", which_north = "true", style = north_arrow_orienteering()) +
      annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_fancy_orienteering()) +
      annotation_north_arrow(location = "bl", which_north = "true", style = north_arrow_minimal()) +
      annotation_north_arrow(location = "br", which_north = "true", style = north_arrow_nautical()) +
      labs(caption = "All four arrows should point to the north pole and have different styles with rotated text")
  )

  print(
    p +
      annotation_north_arrow(location = "tl", which_north = "grid", style = north_arrow_orienteering) +
      annotation_north_arrow(location = "tr", which_north = "grid", style = north_arrow_fancy_orienteering) +
      annotation_north_arrow(location = "bl", which_north = "grid", style = north_arrow_minimal) +
      annotation_north_arrow(location = "br", which_north = "grid", style = north_arrow_nautical) +
      labs(caption = "All four arrows should point straight up and have different styles")
  )

  expect_true(TRUE)
})

test_that("certain parameters can be passed as aesthetics to show up on different panels", {
  load_longlake_data()

  # note that passing NA in the label column makes this not work
  arrow_params <- tibble::tibble(
    label = c("Round Lake", "Long Lake"),
    which_north = c("true", "grid"),
    location = c("bl", "tr")
  )

  print(
    ggplot() +
      layer_spatial(longlake_waterdf) +
      annotation_north_arrow(
        aes(which_north = which_north, location = location),
        data = arrow_params
      ) +
      facet_wrap(~label) +
      labs(caption = "two north arrows in different panels with different parameters")
  )

  # visual test
  expect_true(TRUE)
})

