context("test-geom-polypath.R")

test_that("polypath works as intended", {
  load_longlake_data(which = "longlake_waterdf")
  df <- df_spatial(longlake_waterdf[3,])
  vdiffr::expect_doppelganger(
    "polygon without the proper holes",
    ggplot() +
      ggplot2::geom_polygon(aes(x, y, group = piece_id), data = df)
  )

  vdiffr::expect_doppelganger(
    "polygon with the proper holes",
    ggplot() +
      geom_polypath(aes(x, y, group = piece_id), data = df)
  )

  expect_error(
    ggplot() + geom_polypath(aes(x, y, group = piece_id), data = df, rule = "fish"),
    "must be 'evenodd'"
  )
})
