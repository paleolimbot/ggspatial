
# method definitions for objects from sp

# add missing fortify methods

#' Create a data.frame from Spatial* objects
#'
#' Methods to turn Spatial* objects into data.frames.
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
#' fortify(longlake_depthsdf)
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

# add special spatial_fortify definitions for spdf classes

spatial_fortify.SpatialPointsDataFrame <- function(x, attrs = NULL, ...) {
  # override attribute table if present
  if(is.null(attrs)) {
    attrs <- x@data
  }
  # call default method
  spatial_fortify.default(x, attrs, ...)
}

spatial_fortify.SpatialLinesDataFrame <- function(x, attrs = NULL, ...) {
  # same as spatial points data frame
  spatial_fortify.SpatialPointsDataFrame(x, attrs = attrs, ...)
}

spatial_fortify.SpatialPolygonsDataFrame <- function(x, attrs = NULL, ...) {
  # same as spatial points data frame
  spatial_fortify.SpatialPointsDataFrame(x, attrs = attrs, ...)
}


# add geometry definitions

spatial_geom.SpatialPoints <- function(x) ggplot2::GeomPoint
spatial_geom.Line <- function(x) ggplot2::GeomPath
spatial_geom.Lines <- function(x) ggplot2::GeomPath
spatial_geom.SpatialLines <- function(x) ggplot2::GeomPath
spatial_geom.Polygon <- function(x) GeomPolypath2
spatial_geom.Polygons <- function(x) GeomPolypath2
spatial_geom.SpatialPolygons <- function(x) GeomPolypath2

# add default aes definitions for items whose fortify() method produceds a group
spatial_default_aes.Lines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

# spatial lines data frame extends spatial lines
spatial_default_aes.SpatialLines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

spatial_default_aes.Polygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

# spatial lines data frame extends spatial polygons
spatial_default_aes.SpatialPolygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

