
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
