
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
layer_spatial <- function(data, mapping, ...) {
  UseMethod("layer_spatial")
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
