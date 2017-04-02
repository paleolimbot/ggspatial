
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
geom_spatial <- function(data, ...) UseMethod("geom_spatial")


#' @rdname geom_spatial
#' @export
ggspatial <- function(data, mapping = NULL, ...) {
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) + ggplot2::coord_map()
}

#' @rdname geom_spatial
#' @export
geom_spatial.default <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=NULL,
                                 position = "identity", crsfrom = NA, crsto = NA,
                                 geom = "point", ...) {

  # allow missing data for inherited data
  if(missing(data)) {
    data <- NULL
  }

  # inherit aes by default
  if(is.null(inherit.aes)) {
    inherit.aes <- TRUE
  }

  # get projections
  projections <- get_projections(data = data, crsfrom, crsto)

  # return layer
  layer(
    stat = StatProject, data = data, mapping = mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = "identity",
    params=c(projections, list(...))
  )
}

#' @rdname geom_spatial
#' @export
geom_spatial.SpatialPoints <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=NULL,
                                       position = "identity", crsfrom = NA, crsto = NA,
                                       attribute_table = NULL, ...) {

  # get projections
  projections <- get_projections(data = data, crsfrom = crsfrom, crsto = crsto)

  # extract coordinates
  coords <- sp::coordinates(data)
  df <- data.frame(.x=coords[,1], .y=coords[,2])
  # create mapping
  final_mapping <- ggplot2::aes_string(".x", ".y")

  if(is.null(attribute_table)) {
    # warn if the user tried to pass a mapping
    if(!is.null(mapping)) message("Ignoring argument 'mapping' in geom_spatial.SpatialPoints")
    # warn if user tried to pass inherit.aes = TRUE
    if(!is.null(inherit.aes)) message("Ignoring argument 'inherit.aes' in geom_spatial.SpatialPoints")
  } else {
    # ensure mapping is an uneval object
    if(!is.null(mapping) && !inherits(mapping, "uneval")) {
      stop("mapping must be created with aes() or aes_string()")
    }
    # ensure data has same length as coordinates and join
    if(nrow(df) != nrow(attribute_table)) {
      stop("Number of points and number of rows in attribute_table are not identical")
    }
    df <- cbind(df, attribute_table)

    # add coordinates to mapping
    final_mapping <- c(mapping, ggplot2::aes_string(".x", ".y"))
    class(final_mapping) <- "uneval"
  }

  # return layer
  layer(
    stat = StatProject, data = df, mapping = final_mapping, geom = "point",
    show.legend = show.legend, inherit.aes = FALSE, position = position,
    params=c(projections, list(...))
  )
}

#' @rdname geom_spatial
#' @export
geom_spatial.SpatialPointsDataFrame <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=NULL,
                                                position = "identity", crsfrom = NA,
                                                crsto = NA, ...) {
  # use geom_spatial.SpatialPoints with attribute_table
  geom_spatial.SpatialPoints(sp::SpatialPoints(data, proj4string = data@proj4string),
                             mapping = mapping, show.legend = show.legend,
                             inherit.aes = inherit.aes, position = position, crsfrom = crsfrom,
                             crsto = crsto, attribute_table = data@data, ...)
}

#' @rdname geom_spatial
#' @export
geom_spatial.SpatialLines <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=NULL,
                                      position = "identity", crsfrom = NA, crsto = NA,
                                      attribute_table = NULL, ...) {
  # SpatialLines don't have a fortify function, so it's best just to create a
  # SpatialLinesDataFrame

  if(is.null(attribute_table)) {
    # warn if the user tried to pass a mapping
    if(!is.null(mapping)) message("Ignoring argument 'mapping' in geom_spatial.SpatialLines")
    # warn if user tried to pass inherit.aes = TRUE
    if(!is.null(inherit.aes)) message("Ignoring argument 'inherit.aes' in geom_spatial.SpatialLines")
    # create dummy attribute_table
    attribute_table <- data.frame(.dummy=1:length(data), row.names=row.names(data))
  }

  # create SpatialLinesDataFrame
  spldf <- sp::SpatialLinesDataFrame(data, attribute_table)

  # return result of geom_spatial.SpatialLinesDataFrame
  geom_spatial.SpatialLinesDataFrame(spldf, mapping = mapping, show.legend = show.legend,
                                     inherit.aes = FALSE, position = position,
                                     crsfrom = crsfrom, crsto = crsto, ...)
}

#' @rdname geom_spatial
#' @export
geom_spatial.SpatialLinesDataFrame <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=NULL,
                                               position = "identity", crsfrom = NA,
                                               crsto = NA, ...) {

  # get projections
  projections <- get_projections(data = data, crsfrom = crsfrom, crsto = crsto)

  # turn the SpatialLinesDataFrame into a data.frame, join with attribute table
  data@data$.id <- rownames(data@data)
  data.fort <- suppressMessages(fortify.SpatialLinesDataFrame(data, data@data))
  data <- suppressWarnings(merge(data.fort, data@data, by.x="id", by.y=".id"))
  mapping <- c(ggplot2::aes_string(x="long", y="lat", group="group"), mapping)
  class(mapping) <- "uneval"

  # return layer
  layer(
    stat = StatProject, data = data, mapping = mapping, geom = "path",
    show.legend = show.legend, inherit.aes = FALSE, position = position,
    params=c(projections, list(...))
  )

}

#' @rdname geom_spatial
#' @export
geom_spatial.SpatialPolygons <- function(data, mapping = NULL, show.legend = TRUE,
                                         inherit.aes=NULL, position = "identity",
                                         crsfrom = NA, crsto = NA,
                                         rule = "winding", attribute_table = NULL, ...) {

  # creating a SpatialPolygonsDataFrame and using
  # geom_spatial.SpatialPolygonsDataFrame

  if(is.null(attribute_table)) {
    # warn if the user tried to pass a mapping
    if(!is.null(mapping)) message("Ignoring argument 'mapping' in geom_spatial.SpatialPolygons")
    # warn if user tried to pass inherit.aes = TRUE
    if(!is.null(inherit.aes)) message("Ignoring argument 'inherit.aes' in geom_spatial.SpatialPolygons")
    # create dummy attribute_table
    attribute_table <- data.frame(.dummy=1:length(data), row.names=row.names(data))
  }

  # create SpatialPolygonsDataFrame
  sppdf <- sp::SpatialPolygonsDataFrame(data, attribute_table)

  # return result of geom_spatial.SpatialPolygonsDataFrame
  geom_spatial.SpatialPolygonsDataFrame(sppdf, mapping = mapping, show.legend = show.legend,
                                        inherit.aes = FALSE, position = position,
                                        crsfrom = crsfrom, crsto = crsto, rule = rule, ...)
}

#' @rdname geom_spatial
#' @export
geom_spatial.SpatialPolygonsDataFrame <- function(data, mapping = NULL, show.legend = TRUE,
                                                  inherit.aes=NULL, position = "identity",
                                                  crsfrom = NA, crsto = NA,
                                                  rule = "winding", ...) {
  # get projections
  projections <- get_projections(data = data, crsfrom = crsfrom, crsto = crsto)

  # turn the SpatialPolygonsDataFrame into a data.frame, join with attribute table
  data@data$.id <- rownames(data@data)
  data.fort <- suppressMessages(fortify.SpatialPolygonsDataFrame(data))
  data <- suppressWarnings(merge(data.fort, data@data, by.x="id", by.y=".id"))

  # add 'fortified' fields to mapping
  if(is.null(mapping)) {
    mapping <- ggplot2::aes()
  }
  mapping <- c(mapping, ggplot2::aes_string("long", "lat", group="group"))
  class(mapping) <- "uneval"

  # return layer
  layer(
    stat = StatProject, data = data, mapping = mapping, geom = GeomPolypath2,
    show.legend = show.legend, inherit.aes = FALSE, position = position,
    params=c(projections, list(na.rm = FALSE, rule=rule, ...))
  )
}

# this is used by most S3s to get projection info
# note that 'data' cannot be missing
get_projections <- function(data, crsfrom = NA, crsto = NA) {
  # process projection information before finding methods

  crsfrom <- as.CRS(crsfrom)
  crsto <- as.CRS(crsto)

  if(identical(crsfrom, NA)) {
    crsfrom <- as.CRS(data)
  }

  # return list in the form used by StatProject
  list(crsfrom = crsfrom, crsto = crsto)
}
