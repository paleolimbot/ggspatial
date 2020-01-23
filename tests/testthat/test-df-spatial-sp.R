
context("test-df-spatial-sp")

test_that("Spatial* objects are converted by df_spatial()", {
  skip_if_not_installed("sp")

  # load the long lake test data
  load_longlake_data()

  # SpatialPoints
  spoints_df <- sf::as_Spatial(longlake_depthdf)
  spoints <- sp::SpatialPoints(spoints_df, proj4string = spoints_df@proj4string)
  expect_df_spatial(spoints)
  expect_equal(nrow(df_spatial(spoints)), length(spoints))
  expect_df_spatial(spoints_df, c("NOTES", "DEPTH_M"))

  # SpatialMultiPoints
  spmultipoints_sf <- dplyr::summarise(dplyr::group_by(longlake_depthdf, NOTES), one = 1)
  spmultipoints_df <- sf::as_Spatial(spmultipoints_sf)
  spmultipoints <- as(spmultipoints_df, "SpatialMultiPoints")

  expect_df_spatial(spmultipoints)
  expect_df_spatial(spmultipoints_df, c("NOTES", "one"))
  expect_true(setequal(df_spatial(spmultipoints_df)$NOTES, c("mouth of inlet", "reeds", NA)))

  # SpatialLines
  splines_df <- sf::as_Spatial(sf::st_zm(longlake_roadsdf))
  splines <- sp::SpatialLines(splines_df@lines, proj4string = splines_df@proj4string)
  line <- splines@lines[[1]]@Lines[[1]]
  lines <- splines@lines[[1]]

  expect_df_spatial(line)

  expect_df_spatial(lines, c("coordinate_id", "piece_id"))
  expect_is(df_spatial(lines)$coordinate_id, "integer")
  expect_is(df_spatial(lines)$piece_id, "factor")

  expect_df_spatial(splines, c("coordinate_id", "piece_id"))
  expect_is(df_spatial(splines)$coordinate_id, "integer")
  expect_is(df_spatial(splines)$piece_id, "factor")

  expect_df_spatial(splines_df, c("coordinate_id", "piece_id", "FEAT_CODE"))
  expect_is(df_spatial(splines_df)$coordinate_id, "integer")
  expect_is(df_spatial(splines_df)$piece_id, "factor")

  # manual check of success
  # ggplot(df_spatial(splines)) + geom_path(aes(x, y, group = piece_id))
  # ggplot(df_spatial(splines_df)) + geom_path(aes(x, y, group = piece_id))

  # SpatialPolygons
  spoly_df <- sf::as_Spatial(sf::st_zm(longlake_waterdf))
  spoly <- as(spoly_df, "SpatialPolygons")
  polygon <- spoly@polygons[[1]]@Polygons[[1]]
  polygons <- spoly@polygons[[1]]

  expect_df_spatial(polygon, c("is_hole", "ring_direction"))

  expect_df_spatial(polygons, c("coordinate_id", "piece_id"))
  expect_is(df_spatial(polygons)$coordinate_id, "integer")
  expect_is(df_spatial(polygons)$piece_id, "factor")

  expect_df_spatial(spoly, c("coordinate_id", "piece_id"))
  expect_is(df_spatial(spoly)$coordinate_id, "integer")
  expect_is(df_spatial(spoly)$piece_id, "factor")

  expect_df_spatial(spoly_df, c("coordinate_id", "piece_id"))
  expect_is(df_spatial(spoly_df)$coordinate_id, "integer")
  expect_is(df_spatial(spoly_df)$piece_id, "factor")

  # manual check of success
  # ggplot(df_spatial(spoly)) + geom_polypath(aes(x, y, group = piece_id))
  # ggplot(df_spatial(spoly_df)) + geom_polypath(aes(x, y, group = piece_id))
})
