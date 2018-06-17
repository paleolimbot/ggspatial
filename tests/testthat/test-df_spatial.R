context("test-df_spatial.R")

expect_df_spatial <- function(expr) {
   expr_name <- paste("Expression:", deparse(substitute(expr)))
   force(expr)

   expect_true(all(c("x", "y", "feature_id") %in% colnames(df_spatial(expr))), info = expr_name)
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

  # SpatialPoints
  spoints_df <- as(longlake_depthdf, "Spatial")
  spoints <- sp::SpatialPoints(spoints_df, proj4string = spoints_df@proj4string)
  expect_df_spatial(spoints)
  expect_equal(nrow(df_spatial(spoints)), length(spoints))

  # SpatialPointsDataFrame
  expect_df_spatial(spoints_df)
  expect_equal(nrow(df_spatial(spoints_df)), nrow(spoints_df))

  # SpatialLines
  splines_df <- as(sf::st_zm(longlake_roadsdf), "Spatial")
  splines <- sp::SpatialLines(splines_df@lines, proj4string = splines_df@proj4string)
  line <- splines@lines[[1]]@Lines[[1]]
  lines <- splines@lines[[1]]


  expect_df_spatial(line)
  expect_df_spatial(lines)
  expect_df_spatial(splines)
  expect_df_spatial(splines_df)
  expect_true("FEAT_CODE" %in% colnames(df_spatial(splines_df)))
})
