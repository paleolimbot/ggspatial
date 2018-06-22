context("test-geom-polypath.R")

test_that("polypath works as intended", {
  load_longlake_data()
  df <- df_spatial(longlake_waterdf[3,])
  print(
    ggplot() +
      geom_polygon(aes(x, y, group = piece_id), data = df) +
      labs(caption = "this polygon doesn't have the holes right!")
  )

  print(
    ggplot() +
      geom_polypath(aes(x, y, group = piece_id), data = df) +
      labs(caption = "this polygon does have the holes right!")
  )

  expect_error(
    ggplot() + geom_polypath(aes(x, y, group = piece_id), data = df, rule = "fish"),
    "must be 'evenodd'"
  )

  # visual test
  expect_true(TRUE)
})
