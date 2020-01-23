
#' Create a ggplot-friendly data frame from a spatial object
#'
#' @param x A spatial object
#' @param ... Passed to specific methods
#'
#' @return A tibble with coordinates as .x and .y, and features as .feature
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

#' @export
df_spatial.Raster <- function(x, ...) {
  # get values in a data frame
  fused <- cbind(expand.grid(x=1:x@ncols, y=1:x@nrows), raster::values(x))
  fused$feature_id <- factor(seq_len(nrow(fused)))

  # set names to be long, lat, band1, band2, ...
  nbands <- ncol(fused) - 3
  names(fused) <- c("x", "y", paste0("band", 1:nbands), "feature_id")

  # fix x and y to be physical coordinates using the bbox
  bbox <- raster::as.matrix(x@extent)
  fused$x <- bbox[1,1]+(fused$x-1)/x@ncols*(bbox[1,2]-bbox[1,1])
  fused$y <- bbox[2,1]+(fused$y-x@nrows)/x@nrows*(bbox[2,1]-bbox[2,2])

  tibble::as_tibble(fused)
}

#' @export
#' @importFrom rlang !!
df_spatial.stars <- function(x, ...) {
  names <- names(x)

  df <- as.data.frame(x, ...)
  gathered <- tidyr::gather(df, key = "value_name", value = "band1", !!names)
  if("band" %in% colnames(gathered)) {
    gathered$band <- paste0("band", gathered$band)
    tibble::as_tibble(tidyr::spread(gathered, key = "band", value = "band1"))
  } else {
    tibble::as_tibble(gathered)
  }
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
  force(expr)

  testthat::expect_true(
    all(c("x", "y", "feature_id", cols) %in% colnames(df_spatial(expr))),
    info = expr_name
  )
  testthat::expect_is(df_spatial(expr)$feature_id, "factor", info = expr_name)
  testthat::expect_is(df_spatial(expr), "data.frame", info = expr_name)
  testthat::expect_is(df_spatial(expr), "tbl_df", info = expr_name)
}
