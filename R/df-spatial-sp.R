
#' @export
df_spatial.SpatialPoints <- function(x, ...) {
  coords <- sp::coordinates(x)
  tibble::tibble(
    x = coords[, 1, drop = TRUE],
    y = coords[, 2, drop = TRUE],
    feature_id = factor(seq_len(nrow(coords)))
  )
}

#' @export
df_spatial.SpatialPointsDataFrame <- function(x, ...) {
  coords <- sp::coordinates(x)
  coords_df <- tibble::tibble(
    x = coords[, 1, drop = TRUE],
    y = coords[, 2, drop = TRUE],
    feature_id = factor(seq_len(nrow(coords)))
  )

  fix_duplicate_cols(coords_df, x@data)
}

#' @export
df_spatial.SpatialMultiPoints <- function(x, ...) {
  coords <- sp::coordinates(x)
  tibble::tibble(
    x = coords[, 1, drop = TRUE],
    y = coords[, 2, drop = TRUE],
    feature_id = factor(rownames(coords))
  )
}

#' @export
df_spatial.SpatialMultiPointsDataFrame <- function(x, ...) {
  coords <- sp::coordinates(x)
  coords_df <- tibble::tibble(
    x = coords[,1, drop = TRUE],
    y = coords[,2, drop = TRUE],
    feature_id = factor(rownames(coords))
  )

  attrs <- x@data

  fix_duplicate_cols(coords_df, attrs[match(coords_df$feature_id, rownames(attrs)), , drop = FALSE])
}

#' @export
df_spatial.Line <- function(x, ...) {
  df_spatial_line(x, feature_id = factor(1))
}

df_spatial_line <- function(x, feature_id = 1L, ...) {
  mat <- x@coords[, c(1, 2), drop = FALSE]
  df <- tibble::tibble(x = mat[, 1, drop = TRUE], y = mat[, 2, drop = TRUE])
  df$feature_id <- feature_id
  df$coordinate_id <- seq_len(nrow(df))
  df
}

#' @export
df_spatial.Lines <- function(x, ...) {
  lines <- x@Lines
  pieces <- plyr::ldply(seq_along(lines), function(i) {
    df <- df_spatial_line(lines[[i]])
    df$piece_id <- i
    df
  })
  pieces$feature_id <- factor(x@ID)
  pieces$piece_id <- factor(pieces$piece_id)
  pieces$piece_id <- interaction(pieces$feature_id, pieces$piece_id)
  tibble::as_tibble(pieces)
}

#' @export
df_spatial.SpatialLines <- function(x, ...) {
  tibble::as_tibble(plyr::ldply(x@lines, df_spatial.Lines))
}

#' @export
df_spatial.SpatialLinesDataFrame <- function(x, ...) {
  df <- tibble::as_tibble(plyr::ldply(x@lines, df_spatial.Lines))
  attrs <- x@data
  attrs <- attrs[match(df$feature_id, rownames(attrs)), , drop = FALSE]
  fix_duplicate_cols(df, attrs)
}

#' @export
df_spatial.Polygon <- function(x, ...) {
  df_spatial_poly(x, feature_id = factor(1))
}

df_spatial_poly <- function(x, feature_id = 1L, ...) {
  mat <- x@coords[, c(1, 2), drop = FALSE]
  df <- tibble::tibble(x = mat[, 1, drop = TRUE], y = mat[, 2, drop = TRUE])
  df$feature_id <- feature_id
  df$coordinate_id <- seq_len(nrow(df))
  df$is_hole <- x@hole
  df$ring_direction <- x@ringDir
  df
}

#' @export
df_spatial.Polygons <- function(x, ...) {
  polygons <- x@Polygons
  pieces <- plyr::ldply(seq_along(polygons), function(i) {
    df <- df_spatial_line(polygons[[i]])
    df$piece_id <- i
    df
  })
  pieces$feature_id <- factor(x@ID)
  pieces$piece_id <- factor(pieces$piece_id)
  pieces$piece_id <- interaction(pieces$feature_id, pieces$piece_id)
  tibble::as_tibble(pieces)
}

#' @export
df_spatial.SpatialPolygons <- function(x, ...) {
  tibble::as_tibble(plyr::ldply(x@polygons, df_spatial.Polygons))
}

#' @export
df_spatial.SpatialPolygonsDataFrame <- function(x, ...) {
  df <- tibble::as_tibble(plyr::ldply(x@polygons, df_spatial.Polygons))
  attrs <- x@data
  attrs <- attrs[match(df$feature_id, rownames(attrs)), , drop = FALSE]
  fix_duplicate_cols(df, attrs)
}
