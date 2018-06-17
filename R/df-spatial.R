
#' Create a ggplot-friendly data frame from a spatial object
#'
#' @param x A spatial object
#' @param ... Passed to specific methods
#'
#' @return A tibble with coordinates as .x and .y, and features as .feature
#' @export
#'
df_spatial <- function(x, ...) {
  UseMethod("df_spatial")
}

#' @export
df_spatial.SpatialPoints <- function(x, ...) {
  coords <- sp::coordinates(x)
  tibble::tibble(
    x = coords[,1, drop = TRUE],
    y = coords[,2, drop = TRUE],
    feature_id = seq_len(nrow(coords))
  )
}

#' @export
df_spatial.SpatialPointsDataFrame <- function(x, ...) {
  coords <- sp::coordinates(x)
  coords_df <- tibble::tibble(
    x = coords[,1, drop = TRUE],
    y = coords[,2, drop = TRUE],
    feature_id = seq_len(nrow(coords))
  )

  fix_duplicate_cols(coords_df, as.data.frame(x))
}

#' @export
df_spatial.Line <- function(x, feature_id = 1L, piece_id = 1L, ...) {
  df <- tibble::as_tibble(x@coords[, c(1, 2), drop = TRUE])
  colnames(df) <- c("x", "y")
  df$feature_id <- feature_id
  df$coordinate_id <- seq_len(nrow(df))
  df
}

#' @export
df_spatial.Lines <- function(x, ...) {
  line_dfs <- mapply(x@Lines, seq_len(length(x)), df_spatial.Line)
  tibble::as_tibble(do.call(rbind, line_dfs))
}

#' @export
df_spatial.SpatialLines <- function(x, ...) {
  line_dfs <- lapply(x@lines, df_spatial.Line)
  tibble::as_tibble(do.call(rbind, line_dfs))
}

#' @export
df_spatial.SpatialLinesDataFrame <- function(x, ...) {
  line_dfs <- lapply(x@lines, df_spatial.Line)
  tibble::as_tibble(do.call(rbind, line_dfs))
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
