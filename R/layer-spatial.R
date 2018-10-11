
#' Turn a spatial object into a ggplot2 layer
#'
#' @param data An object that can be coerced to an sf object using \link[sf]{st_as_sf}.
#' @param mapping A mapping, created using \link[ggplot2]{aes}.
#' @param sf_params Passed to \link[sf]{st_as_sf}.
#' @param inherit.aes Inherit aesthetics from ggplot()?
#' @param ... Passed to \link[ggplot2]{geom_sf}
#'
#' @return A ggplot2 \link[ggplot2]{layer}.
#' @export
#' @importFrom ggplot2 aes
#'
#' @examples
#' load_longlake_data()
#'
#' ggplot() +
#'
#'   # annotation_spatial() layers don't train the scales, so data stays central
#'   annotation_spatial(longlake_roadsdf, size = 2, col = "black") +
#'   annotation_spatial(longlake_roadsdf, size = 1.6, col = "white") +
#'
#'   # raster layers train scales and get projected automatically
#'   layer_spatial(longlake_depth_raster, aes(alpha = stat(band1)), fill = "darkblue") +
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
    inherit.aes = FALSE,
    ...
  )
}

#' @rdname layer_spatial
#' @export
annotation_spatial.default <- function(data, mapping = aes(), inherit.aes = FALSE, sf_params = list(), ...) {
  ggplot2::geom_sf(
    mapping = mapping,
    data = do.call(sf::st_as_sf, c(list(data), sf_params)),
    inherit.aes = FALSE,
    na.rm = TRUE,
    stat = StatSfAnnotation,
    ...
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

