
test_that("scale bar parameters are generated correctly", {
  load_longlake_data(which = "longlake_depthdf")
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  expect_equal(
    scalebar_params(
      sf::st_bbox(longlake_depthdf),
      plotunit = "m",
      widthhint = 0.25
    )$labeltext,
    "400 m"
  )

  expect_message(
    scalebar_params(
      sf::st_bbox(longlake_depthdf),
      widthhint = 0.25
    ),
    "Using plotunit"
  )

  expect_equal(
    scalebar_params(
      sf::st_bbox(longlake_depthdf),
      sf_crs = 26920,
      widthhint = 0.25
    )$labeltext,
    "400 m"
  )

  expect_equal(
    scalebar_params(
      sf::st_bbox(nc),
      sf_crs = sf::st_crs(nc),
      widthhint = 0.25
    )$labeltext,
    "200 km"
  )
})

test_that("annotation scale works as intended", {
  skip_if_not_installed("vdiffr")

  load_longlake_data(which = "longlake_depthdf")

  # defaults are ok
  expect_doppelganger(
    "scale bar (defaults, coord_sf)",
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale()
  )

  expect_message(
    expect_doppelganger(
      "scale bar (defaults, cartesian)",
      ggplot() +
        ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
        annotation_scale(pad_x = unit(0, "cm")) +
        ggplot2::coord_fixed(expand = FALSE)
    ),
    "Using plotunit"
  )


  expect_doppelganger(
    "scale bar (metric plotunit, cartesian)",
    ggplot() +
      ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:4/10, y = -(0:4)/10)) +
      annotation_scale(pad_x = unit(0, "cm"), plot_unit = "m") +
      ggplot2::coord_fixed(expand = FALSE)
  )

  expect_doppelganger(
    "scale bar (imperial plotunit)",
    ggplot() +
      ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_scale(unit_category = "imperial", plot_unit = "ft", width_hint = 0.6, pad_x = unit(0, "cm")) +
      ggplot2::coord_fixed(expand = FALSE)
  )

  expect_doppelganger(
    "scale bar (imperial unit)",
    ggplot() +
      ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:15, y = -(0:15))) +
      annotation_scale(unit_category = "imperial", plot_unit = "ft", width_hint = 0.7, pad_x = unit(0, "cm")) +
      ggplot2::coord_fixed(expand = FALSE)
  )

  # position
  expect_doppelganger(
    "scale bar (defaults, bottom left)",
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "bl")
  )

  expect_doppelganger(
    "scale bar (defaults, bottom right)",
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "br")
  )

  expect_doppelganger(
    "scale bar (defaults, top left)",
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "tl")
  )

  expect_doppelganger(
    "scale bar (defaults, top right)",
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "tr")
  )

  # styles
  expect_doppelganger(
    "scale bar (ticks)",
    ggplot() +
      ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_scale(plot_unit = "m", style = "ticks") +
      ggplot2::coord_fixed()
  )
})

test_that("font items are passed on to annotation_scale()", {
  skip_if_not_installed("vdiffr")

  expect_doppelganger(
    "scale bar (fonts)",
    ggplot() +
      ggplot2::geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_scale(plot_unit = "m", text_face = "bold") +
      annotation_scale(plot_unit = "m", pad_y = unit(1, "cm")) +
      annotation_scale(plot_unit = "m", pad_y = unit(2, "cm"), text_family = "serif") +
      ggplot2::coord_fixed()
  )
})


test_that("certain parameters can be passed as aesthetics to show up on different panels", {
  skip_if_not_installed("vdiffr")

  df <- tibble::tibble(
    facet_var = c("one", "two", "three", "four"),
    data = list(data.frame(x = 0:4, y = -(0:4)))
  ) %>%
    tidyr::unnest(data)

  scale_params <- tibble::tibble(
    facet_var = c("one", "two", "three"),
    width_hint = c(0.25, 0.5, 0.75),
    style = c("bar", "ticks", "bar"),
    location = c("bl", "tr", "tl"),
    unit_category = c("metric", "imperial", "metric"),
    text_col = c("black", "red", "blue"),
    line_col = c("black", "red", "blue")
  )

  expect_doppelganger(
    "scale bar (parameters as aesthetics)",
    ggplot(df) +
      ggplot2::geom_point(aes(x, y)) +
      annotation_scale(
        aes(width_hint = width_hint, style = style, location = location, unit_category = unit_category,
            text_col = text_col, line_col = line_col),
        data = scale_params,
        plot_unit = "m"
      ) +
      ggplot2::facet_wrap(~facet_var) +
      ggplot2::coord_fixed()
  )
})
