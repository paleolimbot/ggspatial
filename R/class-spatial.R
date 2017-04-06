
# ---------- method definitions for objects from sp ------------

#' Create a data.frame from Spatial* objects
#'
#' Methods to turn Spatial* objects into data.frames
#'
#' Several methods are missing from ggplot2, including \link[ggplot2]{fortify}
#' implementations for SpatialPoints, SpatialPointsDataFrame, and SpatialLines.
#'
#' @param model The object
#' @param data Not used by this method
#' @param ... Not used by this method
#'
#' @return A data.frame with colummns id, long and lat (mimics
#' behaviour in fortify.Spatial* from ggplot2.
#' @export
#'
#' @examples
#' fortify(longlake_depthdf)
#'
fortify.SpatialPoints <- function(model, data = NULL, ...) {
  coords <- sp::coordinates(model)
  data.frame(id=1:nrow(coords), long = coords[, 1], lat = coords[, 2])
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.SpatialPointsDataFrame <- function(model, data = NULL, ...) {
  coords <- sp::coordinates(model)
  data.frame(id=1:nrow(coords), long = coords[, 1], lat = coords[, 2])
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.SpatialLines <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

# ----------- spatial_fortify definitions ---------------

#' @rdname spatial_fortify
#' @export
spatial_fortify.SpatialPointsDataFrame <- function(x, attrs = NA, ...) {
  # override attribute table if present
  if(identical(attrs, NA)) {
    attrs <- x@data
  }

  if(!is.null(attrs)) {
    # add .id column
    attrs$.id <- 1:nrow(attrs)
  }

  # call default method
  spatial_fortify.default(x, attrs, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.SpatialLinesDataFrame <- function(x, attrs = NA, ...) {
  # override attribute table if present
  if(identical(attrs, NA)) {
    attrs <- x@data
  }
  # call default method
  spatial_fortify.default(x, attrs, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.SpatialPolygonsDataFrame <- function(x, attrs = NA, ...) {
  # same as spatial lines data frame with suppressMessages
  suppressMessages(spatial_fortify.SpatialLinesDataFrame(x, attrs = attrs, ...))
}


# --------- geometry definitions ----------------

#' @rdname spatial_geom
#' @export
spatial_geom.SpatialPoints <- function(x) ggplot2::GeomPoint
#' @rdname spatial_geom
#' @export
spatial_geom.Line <- function(x) ggplot2::GeomPath
#' @rdname spatial_geom
#' @export
spatial_geom.Lines <- function(x) ggplot2::GeomPath
#' @rdname spatial_geom
#' @export
spatial_geom.SpatialLines <- function(x) ggplot2::GeomPath
#' @rdname spatial_geom
#' @export
spatial_geom.Polygon <- function(x) GeomPolypath
#' @rdname spatial_geom
#' @export
spatial_geom.Polygons <- function(x) GeomPolypath
#' @rdname spatial_geom
#' @export
spatial_geom.SpatialPolygons <- function(x) GeomPolypath

# ----------- default aes definitions ------------
# for items whose fortify() method produceds a group

#' @rdname spatial_geom
#' @export
spatial_default_aes.Lines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

# spatial lines data frame extends spatial lines
#' @rdname spatial_geom
#' @export
spatial_default_aes.SpatialLines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

#' @rdname spatial_geom
#' @export
spatial_default_aes.Polygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

# spatial lines data frame extends spatial polygons
#' @rdname spatial_geom
#' @export
spatial_default_aes.SpatialPolygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

