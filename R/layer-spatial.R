
#' Turn a spatial object into a ggplot2 layer
#'
#' See also [layer_spatial.Raster()], [layer_spatial.stars()],
#' [layer_spatial.SpatRaster()] and [layer_spatial.bbox()] for implementations
#' for other types of spatial objects.
#'
#' @param data An object that can be coerced to an sf object using [st_as_sf][sf::st_as_sf].
#' @param mapping A mapping, created using [aes][ggplot2::aes].
#' @param sf_params Passed to [st_as_sf][sf::st_as_sf].
#' @param inherit.aes Inherit aesthetics from ggplot()?
#' @param ... Passed to [geom_sf][ggplot2::geom_sf]
#'
#' @return A ggplot2 [layer][ggplot2::layer].
#' @export
#' @importFrom ggplot2 aes
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' load_longlake_data(
#'   which = c(
#'     "longlake_roadsdf",
#'     "longlake_depthdf",
#'     "longlake_depth_raster"
#'   )
#' )
#'
#' ggplot() +
#'
#'   # annotation_spatial() layers don't train the scales, so data stays central
#'   annotation_spatial(longlake_roadsdf, size = 2, col = "black") +
#'   annotation_spatial(longlake_roadsdf, size = 1.6, col = "white") +
#'
#'   # raster layers train scales and get projected automatically
#'   layer_spatial(longlake_depth_raster, aes(alpha = after_stat(band1)), fill = "darkblue") +
#'   scale_alpha_continuous(na.value = 0) +
#'
#'   # layer_spatial() layers train the scales
#'   layer_spatial(longlake_depthdf, aes(col = DEPTH_M)) +
#'
#'   # spatial-aware automagic scale bar
#'   annotation_scale(location = "tl") +
#'
#'   # spatial-aware automagic north arrow
#'   annotation_north_arrow(location = "br", which_north = "true")
#' }
#'
layer_spatial <- function(data, mapping, ...) {
  UseMethod("layer_spatial")
}

#' @export
#' @rdname layer_spatial
annotation_spatial <- function(data, mapping, ...) {
  UseMethod("annotation_spatial")
}

#' @rdname layer_spatial
#' @export
layer_spatial.default <- function(data, mapping = aes(), inherit.aes = FALSE, sf_params = list(), ...) {
  ggplot2::geom_sf(
    mapping = mapping,
    data = do.call(sf::st_as_sf, c(list(data), sf_params)),
    inherit.aes = inherit.aes,
    ...
  )
}

#' @rdname layer_spatial
#' @export
annotation_spatial.default <- function(data, mapping = aes(), inherit.aes = FALSE, sf_params = list(), ...) {
  ggplot2::geom_sf(
    mapping = mapping,
    data = do.call(sf::st_as_sf, c(list(data), sf_params)),
    inherit.aes = inherit.aes,
    na.rm = TRUE,
    stat = StatSfAnnotation,
    ...
  )
}

#' @export
#' @rdname layer_spatial
shadow_spatial <- function(data, ...) {
  UseMethod("shadow_spatial")
}

#' @rdname layer_spatial
#' @export
shadow_spatial.default <- function(data, ...) {
  ggplot2::stat_sf(
    mapping = aes(),
    data = sf::st_as_sf(data),
    inherit.aes = FALSE,
    na.rm = FALSE,
    geom = GeomBlankSf
  )
}

StatSfAnnotation <- ggplot2::ggproto(
  "StatSfAnnotation",
  ggplot2::StatSf,
  compute_group = function(data, scales) {
    data$xmin <- NA_real_
    data$xmax <- NA_real_
    data$ymin <- NA_real_
    data$ymax <- NA_real_

    data
  }
)

GeomBlankSf <- ggplot2::ggproto(
  "GeomBlankSf",
  ggplot2::GeomBlank,
  extra_params = c(ggplot2::GeomBlank$extra_params, "legend")
)
