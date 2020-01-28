
#' Create a ggplot-friendly data frame from a spatial object
#'
#' @param x A spatial object
#' @param ... Passed to specific methods
#'
#' @return A tibble with coordinates as `x` and `y`,
#'   features as `feature_id`, and parts as `part_id`.
#' @export
#'
#' @examples
#' load_longlake_data()
#' df_spatial(longlake_osm)
#' df_spatial(longlake_depthdf)
#' df_spatial(as(longlake_depthdf, "Spatial"))
#'
df_spatial <- function(x, ...) {
  UseMethod("df_spatial")
}

#' Fix duplicate column names
#'
#' This fixes possible masking of column names within df_spatial without mangling
#' the required column names.
#'
#' @param df A data.frame
#'
#' @return modified df
#' @noRd
#'
fix_duplicate_cols <- function(...) {
  df <- cbind(...)
  cols <- colnames(df)
  dup_cols <- duplicated(cols)
  cols[dup_cols] <- paste(cols[dup_cols], seq_along(cols)[dup_cols], sep = "..")
  renamed <- colnames(df) != cols

  if(any(renamed)) {
    message(
      "Renamed columns ",
      paste0("`", colnames(df)[renamed], "` => `", cols[renamed], "`", collapse = ", "),
      " in df_spatial()"
    )
  }

  colnames(df) <- cols
  tibble::as_tibble(df)
}

expect_df_spatial <- function(expr, cols = character(0)) {
  expr_name <- paste("Expression:", deparse(substitute(expr)))
  df <- df_spatial(expr)

  testthat::expect_true(
    all(c("x", "y", cols) %in% colnames(df)),
    info = expr_name
  )
  testthat::expect_is(df, "data.frame", info = expr_name)
  testthat::expect_is(df, "tbl_df", info = expr_name)

  invisible(df)
}
