context("test-df_spatial.R")

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
  # load the long lake test data
  load_longlake_data()

  withr::with_package("sf", {
    # SpatialPoints
    spoints_df <- as(longlake_depthdf, "Spatial")
    spoints <- sp::SpatialPoints(spoints_df, proj4string = spoints_df@proj4string)
    expect_df_spatial(spoints)
    expect_equal(nrow(df_spatial(spoints)), length(spoints))
    expect_df_spatial(spoints_df, c("NOTES", "DEPTH_M"))

    # SpatialMultiPoints
    spmultipoints_sf <- dplyr::summarise(dplyr::group_by(longlake_depthdf, NOTES), one = 1)
    spmultipoints_df <- as(spmultipoints_sf, "Spatial")
    spmultipoints <- as(spmultipoints_df, "SpatialMultiPoints")

    expect_df_spatial(spmultipoints)
    expect_df_spatial(spmultipoints_df, c("NOTES", "one"))
    expect_true(setequal(df_spatial(spmultipoints_df)$NOTES, c("mouth of inlet", "reeds", NA)))

    # SpatialLines
    splines_df <- as(sf::st_zm(longlake_roadsdf), "Spatial")
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
    spoly_df <- as(sf::st_zm(longlake_waterdf), "Spatial")
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

    # sf points, multipoints, lines, polygons, and sfc
    expect_identical(df_spatial(longlake_depthdf), df_spatial(spoints_df))
    expect_identical(df_spatial(spmultipoints_sf), df_spatial(spmultipoints_df))
    expect_identical(df_spatial(longlake_roadsdf), df_spatial(splines_df))
    expect_identical(df_spatial(longlake_waterdf), df_spatial(spoly_df))
    expect_df_spatial(longlake_depthdf$geometry)
  })
})

test_that("Raster* objects are converted properly by df_spatial", {
  load_longlake_data()
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
