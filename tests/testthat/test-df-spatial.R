
context("test-df-spatial")

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
