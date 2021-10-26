
test_that("duplicate column name fixing works", {
  tbl <- tibble::as_tibble(list(x = 1:5, y = 1:5, x = letters[1:5]), .name_repair = identity)
  expect_message(
    expect_identical(
      fix_duplicate_cols(tbl),
      tibble::tibble(x = 1:5, y = 1:5, x..3 = letters[1:5])
    ),
    "Renamed columns"
  )

  tbl2 <- tibble::as_tibble(list(x = 1:5, y = 1:5, z = letters[1:5]))
  expect_identical(
    expect_silent(fix_duplicate_cols(tbl2)),
    tbl2
  )
})
