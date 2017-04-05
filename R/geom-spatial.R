

#' A ggplot2 geom for Spatial* objects
#'
#' A function returning a geom_* object based on the Spatial* input. Also will
#' happily project a regular \code{data.frame} provided x and y aesthetics are
#' specified. The result is a \code{geom_*} for use with ggplot2, with aesthetics
#' and other argumets passed on to that geom.
#'
#' @param data A \code{Spatial*} object or \code{data.frame}.
#' @param mapping A mapping as created by \code{aes()} or \code{aes_string()}
#' @param show.legend Logical describing the legend visibility.
#' @param inherit.aes Logical describing if aesthetics are inherited
#' @param position Passed on to geom_*
#' @param crsfrom An object that can be coerced to a CRS using \link{as.CRS}; defaults
#'   to the CRS of the data or lat/lon if that does not exist
#' @param crsto An object that can be coerced to a CRS using \link{as.CRS}; defaults to
#'   lat/lon so that the plot can be projected using coord_map()
#' @param geom For data frames, the geometry to use
#' @param attribute_table For SpatialPoints, SpatialLines, and SpatialPolygons, an attribute
#'   table that matches the input object.
#' @param rule One of 'evenodd' or 'winding', if the Spatial object is a polygon layer.
#' @param ... Agruments passed on to the \code{geom_*} (e.g. \code{lwd}, \code{fill}, etc.)
#'
#' @return A ggplot2 'layer' object.
#'
#' @importFrom ggplot2 layer
#' @importFrom sp CRS
#' @export
#'
#' @examples
#' \donttest{
#' library(prettymapr)
#' ns <- searchbbox("Nova Scotia")
#' cities <- geocode(c("Wolfville, NS", "Windsor, NS", "Halifax, NS"))
#' ggplot(cities, aes(x=lon, y=lat)) + geom_spatial(crsto=26920)
#' # default projection is Spherical Mercator (EPSG:3857)
#' ggplot(cities, aes(x=lon, y=lat)) + geom_spatial() + coord_map()
#' }
#'
#' # plot a number of spatial objects
#' ggplot() +
#'   geom_spatial(longlake_waterdf, fill="lightblue") +
#'   geom_spatial(longlake_marshdf, fill="grey", alpha=0.5) +
#'   geom_spatial(longlake_streamsdf, col="lightblue") +
#'   geom_spatial(longlake_roadsdf, col="black") +
#'   geom_spatial(longlake_buildingsdf, pch=1, col="brown", size=0.25) +
#'   coord_map()
#'
geom_spatial <- function(data, mapping = NULL, ...) UseMethod("geom_spatial")

#' @export
#' @rdname geom_spatial
ggspatial <- function(data, mapping = NULL, ...) {
  ggplot() + geom_spatial(data, mapping = mapping, ...) + coord_map()
}

#' @export
#' @rdname geom_spatial
geom_spatial.default <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes = FALSE,
                                  position = "identity", crsfrom = NA, crsto = NA,
                                  attribute_table = NULL, geom = NA, stat = NA, ...) {
  # get projections
  projections <- get_projections(data, crsfrom, crsto)

  # fortify then project
  df <- spatial_fortify(data, attribute_table)
  check_spatial_fortify(data, df)

  df[, c(".long", ".lat")] <- xyTransform(df$.long, df$.lat,
                                          projections$crsfrom, projections$crsto)

  # find the geom, stat and default aesthetics
  if(identical(geom, NA)) {
    geom <- spatial_geom(data)
  }
  if(identical(stat, NA)) {
    stat <- spatial_stat(data)
  }

  final_mapping <- override_aesthetics(mapping, spatial_default_aes(data))

  # return layer
  layer(
    stat = stat, data = df, mapping = final_mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = position,
    params=list(...)
  )

}

get_projections <- function(data, crsfrom = NA, crsto = NA) {
  # process projection information before finding methods

  crsfrom <- as.CRS(crsfrom)
  crsto <- as.CRS(crsto)

  if(identical(crsfrom, NA)) {
    crsfrom <- as.CRS(data)
  }

  if(identical(crsfrom, NA)) {
    message("Assuming input coordinates are lat/lon")
    crsfrom_out <- as.CRS(4326)
  } else {
    crsfrom_out <- crsfrom
  }

  # assign defaults here
  if(identical(crsto, NA)) {
    if(!identical(crsfrom, NA)) message("Converting coordinates to lat/lon")
    crsto_out <- as.CRS(4326)
  } else {
    crsto_out <- crsto
  }

  # return list in the form used by StatProject
  list(crsfrom = crsfrom_out, crsto = crsto_out)
}
