context("test-df-spatial.R")

expect_df_spatial <- function(expr, cols = character(0)) {
   expr_name <- paste("Expression:", deparse(substitute(expr)))
   force(expr)

   expect_true(all(c("x", "y", "feature_id", cols) %in% colnames(df_spatial(expr))), info = expr_name)
   expect_is(df_spatial(expr)$feature_id, "factor", info = expr_name)
   expect_is(df_spatial(expr), "data.frame", info = expr_name)
   expect_is(df_spatial(expr), "tbl_df", info = expr_name)
}

test_that("duplicate column name fixing works", {
  tbl <- tibble::as_tibble(list(x = 1:5, y = 1:5, x = letters[1:5]), validate = FALSE)
  expect_identical(
    expect_message(fix_duplicate_cols(tbl), "Renamed columns"),
    tibble::tibble(x = 1:5, y = 1:5, x..3 = letters[1:5])
  )

  tbl2 <- tibble::as_tibble(list(x = 1:5, y = 1:5, z = letters[1:5]))
  expect_identical(
    expect_silent(fix_duplicate_cols(tbl2)),
    tbl2
  )
})

test_that("Spatial* objects are fortified correctly", {
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

test_that("Raster* objects are converted properly by df_spatial", {
  skip_if_not_installed("raster")

  load_longlake_data(which = c("longlake_depth_raster", "longlake_osm"), raster_format = "raster")
  expect_df_spatial(longlake_depth_raster)
  expect_df_spatial(longlake_osm)
  expect_equal(
    nrow(df_spatial(longlake_depth_raster)),
    longlake_depth_raster@nrows * longlake_depth_raster@ncols
  )
  expect_equal(
    nrow(df_spatial(longlake_osm)),
    longlake_osm@nrows * longlake_osm@ncols
  )

  # manual check
  # ggplot(df_spatial(longlake_depth_raster)) + geom_raster(aes(x, y, fill = band1))
})

test_that("stars objects are converted properly by df_spatial", {
  stars_rast <- stars::read_stars(system.file("longlake/longlake_depth.tif", package = "ggspatial"))
  stars_rgb <- stars::read_stars(system.file("longlake/longlake.tif", package = "ggspatial"))

  expect_equal(colnames(df_spatial(stars_rast)), c("x", "y", "value_name", "band1"))
  expect_equal(colnames(df_spatial(stars_rgb)), c("x", "y", "value_name", "band1", "band2", "band3"))
})
