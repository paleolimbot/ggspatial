context("test-annotation-scale.R")

test_that("scale bar parameters are generated correctly", {
  load_longlake_data()
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
  load_longlake_data()
  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

  # defaults are ok
  print(
    ggplot() +
      geom_sf(data = nc) +
      annotation_scale() +
      labs(caption = "defaults for annotation_scale() with coord_sf()")
  )

  expect_message(
    print(
      ggplot() +
        geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
        annotation_scale(pad_x = unit(0, "cm")) +
        labs(caption = "defaults for annotation_scale() with non-coord_sf()") +
        coord_fixed(expand = FALSE)
    ),
    "Using plotunit"
  )


  print(
    ggplot() +
      geom_point(aes(x, y), data = data.frame(x = 0:4/10, y = -(0:4)/10)) +
      annotation_scale(pad_x = unit(0, "cm"), plot_unit = "m") +
      labs(caption = "mixing metric units") +
      coord_fixed(expand = FALSE)
  )

  print(
    ggplot() +
      geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_scale(unit_category = "imperial", plot_unit = "ft", width_hint = 0.6, pad_x = unit(0, "cm")) +
      labs(caption = "mixing imperial units") +
      coord_fixed(expand = FALSE)
  )

  print(
    ggplot() +
      geom_point(aes(x, y), data = data.frame(x = 0:15, y = -(0:15))) +
      annotation_scale(unit_category = "imperial", plot_unit = "ft", width_hint = 0.7, pad_x = unit(0, "cm")) +
      labs(caption = "imperial units") +
      coord_fixed(expand = FALSE)
  )

  # position
  print(
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "bl") +
      labs(caption = "defaults for annotation_scale(), legend bottom left")
  )

  print(
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "br") +
      labs(caption = "defaults for annotation_scale(), legend bottom right")
  )

  print(
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "tl") +
      labs(caption = "defaults for annotation_scale(), legend top left")
  )

  print(
    ggplot() +
      geom_sf(data = longlake_depthdf) +
      annotation_scale(location = "tr") +
      labs(caption = "defaults for annotation_scale(), legend top right")
  )

  # styles
  print(
    ggplot() +
      geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_scale(plot_unit = "m", style = "ticks") +
      labs(caption = "ticks style scale") +
      coord_fixed()
  )

})

test_that("font items are passed on to annotation_scale()", {
  print(
    ggplot() +
      geom_point(aes(x, y), data = data.frame(x = 0:4, y = -(0:4))) +
      annotation_scale(plot_unit = "m", text_face = "bold") +
      annotation_scale(plot_unit = "m", pad_y = unit(1, "cm")) +
      annotation_scale(plot_unit = "m", pad_y = unit(2, "cm"), text_family = "serif") +
      labs(caption = "serif label, default label, bold label") +
      coord_fixed()
  )

  expect_true(TRUE)
})
