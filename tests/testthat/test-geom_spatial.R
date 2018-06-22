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
  expect_warning(xy_transform(c(NA, 2, 3), c(1, 2, 3), to = 3857, na.rm = FALSE), "non-finite points removed")
  expect_silent(xy_transform(c(NA, 2, 3), c(1, 2, 3), to = 3857, na.rm = TRUE))

  # zero-length cases
  expect_equal(nrow(xy_transform(numeric(0), numeric(0), to = 3857)), 0)
  expect_equal(colnames(xy_transform(numeric(0), numeric(0), to = 3857)), c("x", "y"))
})

test_that("geom_spatial_* geoms work properly", {
  load_longlake_data()

  point_df <- spatial_fortify(longlake_depthdf)

  point <- ggplot(point_df, aes(.long, .lat)) +
    geom_spatial_point(aes(col = DEPTH.M), crs = 26920) +
    coord_sf(crs = 3857)

  expect_is(point, "ggplot")
  expect_silent(print(point))

  path <- ggplot(point_df[order(point_df$WAYPOINT_I),], aes(.long, .lat)) +
    geom_spatial_path(aes(col = DEPTH.M), crs = 26920) +
    coord_sf(crs = 3857)

  expect_is(path, "ggplot")
  expect_silent(print(path))

  poly_df <- spatial_fortify(longlake_waterdf[2,])
  poly <- ggplot(poly_df, aes(.long, .lat)) +
    geom_spatial_polygon(crs = 26920) +
    coord_sf(crs = 3857)

  expect_is(poly, "ggplot")
  expect_silent(print(poly))

})

test_that("stat_spatial_identity function", {
  load_longlake_data()
  df <- df_spatial(longlake_depthdf)

  expect_message(
    print(
      ggplot() +
        annotation_spatial(longlake_waterdf, fill = "lightblue") +
        stat_spatial_identity(aes(LON, LAT, col = DEPTH.M), data = df) +
        labs(caption = "all the points should be in the lake!")
    ),
    "Assuming crs"
  )

  expect_silent(
    print(
      ggplot() +
        annotation_spatial(longlake_waterdf, fill = "lightblue") +
        stat_spatial_identity(aes(LON, LAT, col = DEPTH.M), data = df, crs = 4326) +
        labs(caption = "all the points should be in the lake!")
    )
  )

})

test_that("create spatial stat class gets tested", {
  expect_is(
    create_spatial_stat_class(ggplot2::StatIdentity, "stat_spatial_identity"),
    "StatIdentity"
  )
})
