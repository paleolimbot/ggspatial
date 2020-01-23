
context("test-df-spatial-sf")


test_that("sf objects are fortified correctly", {

  # load the long lake test data
  load_longlake_data()

  # point / sf
  df_points <- expect_df_spatial(longlake_depthdf, c("NOTES", "DEPTH_M"))
  expect_equal(nrow(df_points), nrow(longlake_depthdf))

  df_points_sfc <- expect_df_spatial(longlake_depthdf$geometry)
  expect_identical(df_points_sfc, df_points[c("x", "y", "feature_id")])

  # multipoint / sf
  multipoints_sf <- dplyr::summarise(dplyr::group_by(longlake_depthdf, NOTES), one = 1)
  df_multipoints <- expect_df_spatial(multipoints_sf, c("NOTES", "one"))
  expect_setequal(df_multipoints$NOTES, c("mouth of inlet", "reeds", NA))

  df_points_sfc <- expect_df_spatial(multipoints_sf$geometry)
  expect_identical(df_points_sfc, df_multipoints[c("x", "y", "feature_id")])

  # linestring
  df_lines <- expect_df_spatial(longlake_roadsdf, c("coordinate_id", "piece_id", "OBJECTID"))
  expect_is(df_lines$coordinate_id, "integer")
  expect_is(df_lines$piece_id, "factor")

  df_lines_sfc <- expect_df_spatial(longlake_roadsdf$geometry)
  # expect_identical(df_lines_sfc, df_lines[c("x", "y", "feature_id", "coordinate_id", "piece_id")])

  # multilinestring

  multilines_sf <- dplyr::summarise(longlake_depthdf, one = 1)
  #df_multilines <- expect_df_spatial(multilines_sf, c("coordinate_id", "piece_id", "one"))
  #expect_is(df_multilines$coordinate_id, "integer")
  #expect_is(df_multilines$piece_id, "factor")

  # df_multilines_sfc <- expect_df_spatial(multilines_sf$geometry, c("coordinate_id", "piece_id"))

  # manual check of success
  # ggplot2::ggplot(lines) + ggplot2::geom_path(ggplot2::aes(x, y, group = piece_id))

  # polygon
  df_polygons <- expect_df_spatial(longlake_waterdf, c("coordinate_id", "piece_id"))
  expect_is(df_polygons$coordinate_id, "integer")
  expect_is(df_polygons$piece_id, "factor")

  df_polygons_sfc <- expect_df_spatial(longlake_waterdf$geometry, c("coordinate_id", "piece_id"))
  # expect_identical(
  #   df_polygons_sfc,
  #   df_polygons[c("x", "y", "feature_id", "coordinate_id", "piece_id")]
  # )

  # multipolygon
  multipolygons_sf <- dplyr::summarise(longlake_waterdf, one = 1)
  df_multipolygons <- expect_df_spatial(multipolygons_sf, c("coordinate_id", "piece_id"))

  df_multipolygons_sfc <- expect_df_spatial(multipolygons_sf, c("coordinate_id", "piece_id"))

  # expect_identical(
  #   df_multipolygons_sfc,
  #   df_multipolygons[c("x", "y", "feature_id", "coordinate_id", "piece_id")]
  # )

  # manual check of success
  # ggplot(df_multipolygons) + geom_polypath(aes(x, y, group = piece_id))
  # ggplot(df_polygons) + geom_polypath(aes(x, y, group = piece_id))
})
