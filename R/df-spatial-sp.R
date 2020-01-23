
#' @export
df_spatial.Spatial <- function(x, ...) {
  if ("data" %in% methods::slotNames(x)) {
    # a Spatial*DataFrame
    df_spatial(sf::st_as_sf(x))
  } else {
    # a Spatial*
    df_spatial(sf::st_as_sfc(x))
  }
}
