
#' Spatial-aware ggplot2 layers
#'
#' These layers are much like their counterparts, [stat_identity][ggplot2::stat_identity],
#' [geom_point][ggplot2::geom_point], [geom_path][ggplot2::geom_path],
#' and [geom_polygon][ggplot2::geom_polygon], except they have a `crs` argument that
#' ensures they are projected when using [coord_sf][ggplot2::coord_sf]. Stats are applied to the x and y coordinates
#' that have been transformed.
#'
#' @param mapping An aesthetic mapping created with [ggplot2::aes()].
#' @param data A data frame or other object, coerced to a data.frame by [ggplot2::fortify()].
#' @param crs The crs of the x and y aesthetics, or NULL to use default lon/lat
#'   crs (with a message).
#' @param geom The geometry to use.
#' @param position The position to use.
#' @param ... Passed to the combined stat/geom as parameters or fixed aesthetics.
#' @param show.legend,inherit.aes See [ggplot2::layer()].
#'
#' @return A [ggplot2::layer()].
#' @export
#'
#' @examples
#' cities <- data.frame(
#'   x = c(-63.58595, 116.41214, 0),
#'   y = c(44.64862, 40.19063, 89.9),
#'   city = c("Halifax", "Beijing", "North Pole")
#' )
#'
#' library(ggrepel)
#' ggplot(cities, aes(x, y)) +
#'   geom_spatial_point(crs = 4326) +
#'   stat_spatial_identity(aes(label = city), geom = "label_repel") +
#'   coord_sf(crs = 3857)
#'
stat_spatial_identity <- function(
  mapping = NULL, data = NULL, crs = NULL, geom = "point",
  position = "identity", ..., show.legend = NA, inherit.aes = TRUE
) {
  ggplot2::layer(
    data = data, mapping = mapping, stat = StatSpatialIdentity,
    geom = geom, position = position, show.legend = show.legend,
    inherit.aes = inherit.aes, params = list(na.rm = FALSE, crs = crs, ...)
  )
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_point <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggplot2::geom_point(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_path <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggplot2::geom_path(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_polygon <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  geom_polypath(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_text <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggplot2::geom_text(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_label <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggplot2::geom_label(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_text_repel <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggrepel::geom_text_repel(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' @rdname stat_spatial_identity
#' @export
geom_spatial_label_repel <- function(mapping = NULL, data = NULL, crs = NULL, ...) {
  ggrepel::geom_label_repel(mapping = mapping, data = data, stat = StatSpatialIdentity, crs = crs, ...)
}

#' Coordinate transform
#'
#' Coordinate transform, propotating non-finite cases.
#'
#' @param x The x coordinate
#' @param y The y coordinate
#' @param from From CRS
#' @param to To CRS
#' @param na.rm Warn for non-finite cases?
#'
#' @return A data.frame with x and y components.
#' @export
#'
#' @examples
#' xy_transform(c(1, 2, 3), c(1, 2, 3), to = 3857)
#' xy_transform(c(1, 2, 3), c(NA, NA, NA), to = 3857)
#' xy_transform(c(1, 2, 3), c(NA, 2, 3), to = 3857)
#' xy_transform(c(1, 2, 3), c(1, 2, NA), to = 3857)
#'
xy_transform <- function(x, y, from = 4326, to = 4326, na.rm = FALSE) {

  from <- sf::st_crs(from)
  to <- sf::st_crs(to)

  finite <- is.finite(x) & is.finite(y)
  if(!all(finite) && !na.rm) warning(sum(!finite), " non-finite points removed by xy_transform()")

  # if none are finite, return none
  if(!any(finite)) {
    return(data.frame(x = rep(NA_real_, length(x)), y = rep(NA_real_, length(y))))
  }

  # no transform necessary if CRS is equal
  if(from == to) return(data.frame(X = x, Y = y))

  # create coordinates for finite, infinite cases
  df_finite <- data.frame(id = which(finite), X = x[finite], Y = y[finite])
  if(any(!finite)) {
    df_non_finite <- data.frame(id = which(!finite), X = NA_real_, Y = NA_real_)
  } else {
    df_non_finite <- data.frame(id = numeric(0), X = numeric(0), Y = numeric(0))
  }
  sf_finite <- sf::st_as_sf(df_finite, coords = c("X", "Y"), crs = from)

  # finite points get transformed
  sf_finite_trans <- sf::st_transform(sf_finite, crs = to)
  df_finite_trans <- as.data.frame(sf::st_coordinates(sf_finite_trans))
  df_finite_trans$id <- which(finite)

  # non-finite points get rbinded
  df_trans <- rbind(
    df_finite_trans,
    df_non_finite
  )

  # return arranged by id, without id column
  df_trans <- df_trans[order(df_trans$id), c("X", "Y")]
  names(df_trans) <- c("x", "y")
  rownames(df_trans) <- NULL
  df_trans
}


#' Create spatial-aware stat transformations
#'
#' @param ParentStat The parent Stat
#' @param class_name The class name
#'
#' @return A ggproto Stat subclass
#' @noRd
#'
create_spatial_stat_class <- function(ParentStat, class_name) {
  ggplot2::ggproto(
    class_name,
    ParentStat,
    extra_params = c(ParentStat$extra_params, "crs"),
    required_aes = unique(c("x", "y", ParentStat$required_aes)),
    compute_layer = function(self, data, params, layout) {

      if(is.null(params$crs)) {
        message("Assuming `crs = 4326` in ", class_name, "()")
        from_crs <- sf::st_crs(4326)
      } else {
        from_crs <- sf::st_crs(params$crs)
      }

      if(!is.null(layout$coord_params$crs)) {
        # project data XY coordinates
        if(!all(c("x", "y") %in% colnames(data))) {
          stop("Missing required aesthetics x, y in ", class_name, "()")
        }

        # project `x` and `y`
        data[c("x", "y")] <- xy_transform(
          data$x, data$y,
          from = from_crs,
          to = layout$coord_params$crs
        )
      } else {
        warning(
          "Ignoring transformation in ", class_name, "(). Use coord_sf() with a crs to project this layer.",
          call. = FALSE
        )
      }

      # do whatever the parent geom was going to do with it
      ggplot2::ggproto_parent(ParentStat, self)$compute_layer(data, params, layout)
    }
  )
}

# the workhorses of the above functions
StatSpatialIdentity <- create_spatial_stat_class(ggplot2::StatIdentity, "stat_spatial_identity")
