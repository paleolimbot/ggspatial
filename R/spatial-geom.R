
# this is essentially an automatic geometry chooser, since the
# geom is often implied by the input type

#' Guess ggplot parameters based on a spatial object
#'
#' In most cases, the ggplot geometry that should be used with a
#' spatial object is suggested based on its type (e.g, SpatialPoints
#' should use \code{geom = "point"}). S3 implementations of this
#' method should return a ggplot2 \code{Geom} that will be used in
#' \link{geom_spatial} when \code{geom = NA}. The default is
#' \code{geom = "point"}. In almost all cases, the statistic
#' to be used should be \code{stat = "identity"}, but could
#' theoretically be a custom stat written for a specific
#' class of spatial object. Default aesthetics are by default
#' \code{aes(.long, .lat)}, but some spatial objects require
#' other aesthetics, for which the defaults are returned by
#' \code{spatial_default_aes()}.
#'
#' @param x A spatial object
#'
#' @return A ggplot2 \code{Geom}
#' @export
#'
#' @examples
#' spatial_geom(data.frame()) # default is GeomPoint
#' spatial_geom(longlake_waterdf) # GeomPolypath
#' spatial_geom(longlake_roadsdf) # GeomPath
#' spatial_geom(longlake_buildingsdf) # GeomPoint
#' spatial_geom(longlake_osm) # GeomRaster
#'
spatial_geom <- function(x) UseMethod("spatial_geom")
spatial_geom.default <- function(x) ggplot2::GeomPoint

#' @rdname spatial_geom
#' @export
spatial_stat <- function(x) UseMethod("spatial_stat")
spatial_stat.default <- function(x) ggplot2::StatIdentity

#' @rdname spatial_geom
#' @export
spatial_default_aes <- function(x) UseMethod("spatial_default_aes")
spatial_default_aes.default <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat")
}

# also need a method to combine aesthetics with overriding values
override_aesthetics <- function(user_mapping = NULL, default_mapping = NULL) {
  if(is.null(user_mapping) && is.null(default_mapping)) {
    ggplot2::aes()
  } else if(is.null(default_mapping)) {
    user_mapping
  } else if(is.null(user_mapping)) {
    default_mapping
  } else {
    new_aes <- c(user_mapping, default_mapping)
    class(new_aes) <- "uneval"
    new_aes
  }
}
