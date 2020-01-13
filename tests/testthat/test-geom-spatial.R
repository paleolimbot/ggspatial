context("test-geom_spatial.R")

test_that("xy_transform works as intended", {
  # regular case
  expect_is(xy_transform(c(1, 2, 3), c(1, 2, 3), to = 3857), "data.frame")
  expect_equal(nrow(xy_transform(c(1, 2, 3), c(1, 2, 3), to = 3857)), 3)
  expect_equal(colnames(xy_transform(c(1, 2, 3), c(1, 2, 3), to = 3857)), c("x", "y"))

  # NA cases, observations have to stay in order
  expect_identical(xy_transform(c(NA, 2, 3), c(1, 2, 3), to = 3857, na.rm = TRUE)$y[1], NA_real_)
  expect_identical(xy_transform(c(1, NA, 3), c(1, 2, 3), to = 3857, na.rm = TRUE)$y[2], NA_real_)
  expect_identical(xy_transform(c(1, 2, NA), c(1, 2, 3), to = 3857, na.rm = TRUE)$y[3], NA_real_)

  # messaging
  expect_warning(
    xy_transform(c(NA, 2, 3), c(1, 2, 3), to = 3857, na.rm = FALSE),
    "non-finite points removed"
  )
  expect_silent(xy_transform(c(NA, 2, 3), c(1, 2, 3), to = 3857, na.rm = TRUE))

  # zero-length cases
  expect_equal(nrow(xy_transform(numeric(0), numeric(0), to = 3857)), 0)
  expect_equal(colnames(xy_transform(numeric(0), numeric(0), to = 3857)), c("x", "y"))
})

test_that("geom_spatial_* geoms work properly", {
  load_longlake_data(which = c("longlake_depthdf", "longlake_waterdf"))

  point_df <- df_spatial(longlake_depthdf)

  point <- ggplot(point_df, aes(x, y)) +
    geom_spatial_point(aes(col = DEPTH_M), crs = 26920) +
    coord_sf(crs = 3857)

  vdiffr::expect_doppelganger(
    "geom_spatial_point()",
    point
  )

  path <- ggplot(point_df[order(point_df$WAYPOINT_I),], aes(x, y)) +
    geom_spatial_path(aes(col = DEPTH_M), crs = 26920) +
    coord_sf(crs = 3857)

  vdiffr::expect_doppelganger(
    "geom_spatial_path()",
    path
  )

  poly_df <- df_spatial(longlake_waterdf[2,])
  poly <- ggplot(poly_df, aes(x, y)) +
    geom_spatial_polygon(crs = 26920) +
    coord_sf(crs = 3857)

  vdiffr::expect_doppelganger(
    "geom_spatial_polygon()",
    poly
  )

})

test_that("spatial labellers work properly", {
  cities <- data.frame(
    x = c(-63.58595, 116.41214, 0),
    y = c(44.64862, 40.19063, 89.9),
    city = c("Halifax", "Beijing", "North Pole")
  )

  p <-  ggplot(cities, aes(x, y, label = city)) +
    geom_spatial_point(crs = 4326) +
    coord_sf(crs = 3857)

  vdiffr::expect_doppelganger(
    "geom_spatial_text()",
    p +
      geom_spatial_text(crs = 4326)
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_label()",
    p +
      geom_spatial_label(crs = 4326)
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_text_repel()",
    p +
      geom_spatial_text_repel(crs = 4326, seed = 12)
  )

  vdiffr::expect_doppelganger(
    "geom_spatial_label_repel()",
    p +
      geom_spatial_label_repel(crs = 4326, seed = 12)
  )


})

test_that("stat_spatial_identity function", {
  load_longlake_data(which = c("longlake_depthdf", "longlake_waterdf"))
  df <- df_spatial(longlake_depthdf)

  expect_message(
    vdiffr::expect_doppelganger(
      "stat_spatial_identity(crs = NULL)",
      ggplot() +
        annotation_spatial(longlake_waterdf, fill = "lightblue") +
        stat_spatial_identity(aes(LON, LAT, col = DEPTH_M), data = df)
    ),
    "Assuming `crs = 4326`"
  )

  vdiffr::expect_doppelganger(
    "stat_spatial_identity(crs = 4326)",
    ggplot() +
      annotation_spatial(longlake_waterdf, fill = "lightblue") +
      stat_spatial_identity(aes(LON, LAT, col = DEPTH_M), data = df, crs = 4326)
  )
})

test_that("create spatial stat class gets tested", {
  expect_is(
    create_spatial_stat_class(ggplot2::StatIdentity, "stat_spatial_identity"),
    "StatIdentity"
  )
})
