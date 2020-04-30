
#' @export
df_spatial.sfc_POINT <- function(x, ...) {
  df <- tibble::as_tibble(sf::st_coordinates(x))
  names(df) <- tolower(names(df))
  df$feature_id <- seq_len(nrow(df))
  df$part_id <- rep_len(1L, nrow(df))
  df
}

#' @export
df_spatial.sfc_MULTIPOINT <- function(x, ...) {
  df <- tibble::as_tibble(sf::st_coordinates(x))
  names(df) <- gsub("^l1$", "feature_id", tolower(names(df)))
  df$feature_id <- as.integer(df$feature_id)
  lengths  <- rle(df$feature_id)$lengths
  df$part_id <- unlist(lapply(lengths, seq_len))
  df
}

#' @export
df_spatial.sfc_LINESTRING <- function(x, ...) {
  df <- tibble::as_tibble(sf::st_coordinates(x))
  names(df) <- gsub("^l1$", "feature_id", tolower(names(df)))
  df$feature_id <- as.integer(df$feature_id)
  df$part_id <- rep_len(1L, nrow(df))
  df
}

#' @export
df_spatial.sfc_MULTILINESTRING <- function(x, ...) {
  df <- tibble::as_tibble(sf::st_coordinates(x))
  names(df) <- gsub("^l2$", "part_id", gsub("^l1$", "feature_id", tolower(names(df))))
  df$feature_id <- as.integer(df$feature_id)
  df$part_id <- as.integer(df$part_id)
  df
}

#' @export
df_spatial.sfc_POLYGON <- function(x, ...) {
  df <- tibble::as_tibble(sf::st_coordinates(x))
  names(df) <- gsub(
    "^l2$", "feature_id",
    gsub(
      "^l1$", "piece_id",
      tolower(names(df))
    )
  )

  df$feature_id <- as.integer(df$feature_id)
  df$piece_id <- as.integer(df$piece_id)
  df$part_id <- 1L

  col_order <- c("x", "y", "z", "m", "feature_id", "part_id", "piece_id")
  df[intersect(col_order, names(df))]
}

#' @export
df_spatial.sfc_MULTIPOLYGON <- function(x, ...) {
  df <- tibble::as_tibble(sf::st_coordinates(x))
  names(df) <- gsub(
    "^l3$", "feature_id",
    gsub(
      "^l2$", "part_id",
      gsub(
        "^l1$", "piece_id",
        tolower(names(df))
      )
    )
)

  df$feature_id <- as.integer(df$feature_id)
  df$piece_id <- as.integer(df$piece_id)
  df$part_id <- as.integer(df$part_id)

  col_order <- c("x", "y", "z", "m", "feature_id", "part_id", "piece_id")
  df[intersect(col_order, names(df))]
}

#' @export
df_spatial.sfc_GEOMETRY <- function(x, ...) {
  df_spatial(sfc_cast_common(x))
}

#' @export
#' @importFrom sf st_zm
df_spatial.sfc <- function(x, ...) {
  cls <- paste0("'", class(x), "'", collapse = " / ")
  stop("Don't know how to convert object of class ", cls, " to a df")
}

#' @export
#' @importFrom sf st_zm
df_spatial.sf <- function(x, ...) {
  df_geom <- df_spatial(sf::st_geometry(x))
  x_without_geom <- tibble::as_tibble(sf::st_set_geometry(x, NULL))
  fix_duplicate_cols(
    df_geom,
    x_without_geom[df_geom$feature_id, ]
  )
}

sfc_cast_common <- function(x) {
  types <- unlist(lapply(x, sf::st_geometry_type))
  common_class <- sfg_type_common(types)
  sf::st_cast(x, common_class)
}

sfg_type_common <- function(types) {
  if ((length(unique(types)) == 1) && types != "GEOMETRY") {
    types
  } else if (all(types %in% c("POINT", "MULTIPOINT"))) {
    "MULTIPOINT"
  } else if(all(types %in% c("LINESTRING", "MULTILINESTRING"))) {
    "MULTILINESTRING"
  } else if (all(types %in% c("POLYGON", "MULTIPOLYGON"))) {
    "MULTIPOLYGON"
  } else {
    types <- paste0('"', unique(types), "'", collapse = ", ")
    stop(glue::glue("Can't find common type for geometry types {types}"))
  }
}
