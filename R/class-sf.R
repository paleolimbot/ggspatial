
#' A ggplot2 geom for sf* objects
#'
#' This implementation of \link{geom_spatial} for the sf package currently converts
#' objects to a Spatial* object, although this will likely change in future versions.
#'
#' @inheritParams geom_spatial
#'
#' @return A ggplot2 Geom object
#' @export
#'
#' @examples
#' library(sf)
#' llsf <- st_as_sf(longlake_waterdf)
#' ggspatial(llsf)
#' llsfc <- st_as_sfc(longlake_waterdf)
#' ggspatial(llsfc)
#'
geom_spatial.sf <- function(data, mapping = NULL, show.legend = TRUE,
                            inherit.aes = FALSE, position = "identity", crsfrom = NA, crsto = NA,
                            attribute_table = NA, geom = NA, stat = NA, ...) {
  geom_spatial.default(methods::as(data, "Spatial"), mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position, crsfrom = crsfrom, crsto = crsto,
                       attribute_table = attribute_table, geom = geom, stat = stat, ...)
}

#' @rdname geom_spatial.sf
#' @export
geom_spatial.sfc <- function(data, mapping = NULL, show.legend = TRUE,
                            inherit.aes = FALSE, position = "identity", crsfrom = NA, crsto = NA,
                            attribute_table = NA, geom = NA, stat = NA, ...) {
  geom_spatial.default(methods::as(data, "Spatial"), mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position, crsfrom = crsfrom, crsto = crsto,
                       attribute_table = attribute_table, geom = geom, stat = stat, ...)
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.sf <- function(model, data, ...) {
  fortify(methods::as(model, "Spatial"), data = data, ...)
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.sfc <- function(model, data, ...) {
  fortify(methods::as(model, "Spatial"), data = data, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.sf <- function(x, attrs = NA, ...) {
  spatial_fortify(methods::as(x, "Spatial"), attrs = attrs, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.sfc <- function(x, attrs = NA, ...) {
  spatial_fortify(methods::as(x, "Spatial"), attrs = attrs, ...)
}

#' @rdname spatial_geom
#' @export
spatial_geom.sf <- function(x) spatial_geom(methods::as(x, "Spatial"))
#' @rdname spatial_geom
#' @export
spatial_geom.sfc <- function(x) spatial_geom(methods::as(x, "Spatial"))

#' @rdname spatial_geom
#' @export
spatial_default_aes.sf <- function(x) {
  spatial_default_aes(methods::as(x, "Spatial"))
}

#' @rdname spatial_geom
#' @export
spatial_default_aes.sfc <- function(x) {
  spatial_default_aes(methods::as(x, "Spatial"))
}

