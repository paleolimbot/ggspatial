
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

#' @export
#' @importFrom sf st_zm
df_spatial.sfc <- function(x, ...) {
  if(!requireNamespace("sp", quietly = TRUE)) {
    stop("There is no package called 'sp'")
  }
  df_spatial(methods::as(sf::st_zm(x), "Spatial"))
}

#' @export
#' @importFrom sf st_zm
df_spatial.sf <- function(x, ...) {
  if(!requireNamespace("sp", quietly = TRUE)) {
    stop("There is no package called 'sp'")
  }
  df_spatial(methods::as(sf::st_zm(x), "Spatial"))
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
