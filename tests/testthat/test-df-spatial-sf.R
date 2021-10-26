
test_that("duplicate cols are warned for in sf objects", {
  load_longlake_data(which = "longlake_depthdf")
  longlake_depthdf$x <- "x value"
  expect_message(df_spatial(longlake_depthdf), "Renamed columns")
})

test_that("df_spatial() works with sf objects", {
  skip_if_not_installed("vdiffr")

  # load the long lake test data
  load_longlake_data()

  # point / sf
  df_points <- expect_df_spatial(longlake_depthdf, c("NOTES", "DEPTH_M"))
  expect_true(inherits(df_points$feature_id, "integer"))
  expect_true(inherits(df_points$part_id, "integer"))
  expect_equal(nrow(df_points), nrow(longlake_depthdf))

  df_points_sfc <- expect_df_spatial(longlake_depthdf$geometry)
  expect_identical(df_points_sfc, df_points[c("x", "y", "feature_id", "part_id")])

  expect_doppelganger(
    "df_spatial(), point",
    ggplot(df_points, aes(x, y, colour = DEPTH_M)) + ggplot2::geom_point()
  )


  # multipoint / sf
  multipoints_sf <- dplyr::summarise(dplyr::group_by(longlake_depthdf, NOTES), one = 1)
  df_multipoints <- expect_df_spatial(multipoints_sf, c("NOTES", "one"))
  expect_true(inherits(df_multipoints$feature_id, "integer"))
  expect_true(inherits(df_multipoints$part_id, "integer"))
  expect_setequal(df_multipoints$NOTES, c("mouth of inlet", "reeds", NA))
  expect_setequal(df_multipoints$feature_id, 1:3)
  expect_equal(nrow(df_multipoints), nrow(df_points))

  df_multipoints_sfc <- expect_df_spatial(multipoints_sf$geometry)
  expect_identical(df_multipoints_sfc, df_multipoints[c("x", "y", "feature_id", "part_id")])

  expect_doppelganger(
    "df_spatial(), multipoint",
    ggplot(df_multipoints, aes(x, y, colour = one)) + ggplot2::geom_point()
  )

  # linestring
  df_lines <- expect_df_spatial(longlake_roadsdf, c("z", "OBJECTID"))
  expect_true(inherits(df_lines$feature_id, "integer"))
  expect_true(inherits(df_lines$part_id, "integer"))
  expect_setequal(df_lines$feature_id, seq_len(nrow(longlake_roadsdf)))

  df_lines_sfc <- expect_df_spatial(longlake_roadsdf$geometry)
  expect_identical(df_lines_sfc, df_lines[c("x", "y", "z", "feature_id", "part_id")])

  expect_doppelganger(
    "df_spatial(), linestring",
    ggplot(df_lines, aes(x, y, group = interaction(feature_id, part_id))) + ggplot2::geom_path()
  )

  # multilinestring
  multilines_sf <- dplyr::summarise(longlake_roadsdf, one = 1)
  df_multilines <- expect_df_spatial(multilines_sf, c("z", "part_id", "one"))
  expect_true(inherits(df_multilines$feature_id, "integer"))
  expect_true(inherits(df_multilines$part_id, "integer"))

  df_multilines_sfc <- expect_df_spatial(multilines_sf$geometry, c("z", "part_id"))
  expect_identical(df_multilines_sfc, df_multilines[c("x", "y", "z", "part_id", "feature_id")])

  expect_doppelganger(
    "df_spatial(), multilinestring",
    ggplot(df_multilines, aes(x, y, group = interaction(feature_id, part_id))) + ggplot2::geom_path()
  )

  # polygon
  df_polygons <- expect_df_spatial(longlake_waterdf, c("part_id", "piece_id"))
  expect_true(inherits(df_polygons$feature_id, "integer"))
  expect_true(inherits(df_polygons$part_id, "integer"))
  expect_true(inherits(df_polygons$piece_id, "integer"))
  expect_length(unique(df_polygons$feature_id), nrow(longlake_waterdf))
  expect_length(unique(df_polygons$part_id), 1)
  expect_length(unique(df_polygons$piece_id), 7)

  df_polygons_sfc <- expect_df_spatial(longlake_waterdf$geometry, c("part_id", "piece_id"))
  expect_identical(
    df_polygons_sfc,
    df_polygons[c("x", "y", "z", "feature_id", "part_id", "piece_id")]
  )

  expect_doppelganger(
    "df_spatial(), polygon",
    ggplot(
      df_polygons,
      aes(x, y, group = interaction(feature_id, part_id), subgroup = piece_id, fill = label)
    ) +
      ggplot2::geom_polygon()
  )

  # multipolygon
  multipolygons_sf <- dplyr::summarise(longlake_waterdf, one = 1)
  df_multipolygons <- expect_df_spatial(multipolygons_sf, c("part_id", "piece_id"))
  expect_true(inherits(df_multipolygons$feature_id, "integer"))
  expect_true(inherits(df_multipolygons$part_id, "integer"))
  expect_true(inherits(df_multipolygons$piece_id, "integer"))
  expect_length(unique(df_multipolygons$feature_id), nrow(multipolygons_sf))
  expect_length(unique(df_multipolygons$part_id), nrow(longlake_waterdf))
  expect_length(unique(df_multipolygons$piece_id), 7)

  df_multipolygons_sfc <- expect_df_spatial(multipolygons_sf$geometry, c("part_id", "piece_id"))

  expect_identical(
    df_multipolygons_sfc,
    df_multipolygons[c("x", "y", "z", "feature_id", "part_id", "piece_id")]
  )

  expect_doppelganger_extra(
    "df_spatial(), multipolygon",
    ggplot(
      df_multipolygons,
      aes(x, y, group = interaction(feature_id, part_id), subgroup = piece_id)
    ) +
      ggplot2::geom_polygon()
  )
})
