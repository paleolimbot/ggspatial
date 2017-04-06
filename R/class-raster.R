
#' Spatial Geometry for Raster* Objects
#'
#' Similar to the \link{geom_spatial} family for Spatial* objects, this method
#' plots a spatial raster as a spatial raster. Note that projecting (or un-projecting)
#' this layer will likely result in odd results (namely that geom_raster will not
#' function properly or at all). As a convenience, \code{ggraster()} is provided,
#' which replaces the call to \code{coord_map()} from \link{ggspatial} with a call
#' to \code{coord_fixed()}.
#'
#' @param data A Raster* object
#' @param mapping A mapping
#' @param geom The geometry to use. Defaults to raster (obviously), but could also
#'   be another value if used with a different stat (e.g. contour)
#' @param stat The stat to apply. Defaults to 'identity', but could be something else
#'   like 'contour', \link{stat_rgba} ("rgba"), or \link{stat_project} ("project").
#' @param position The position to apply (should probably always be 'identity')
#' @param show.legend Should the legend be shown for this layer?
#' @param inherit.aes Should aesthetics be inherited from the base plot?
#' @param crsfrom Override the source projection
#' @param crsto Override the source projection
#' @param ... Further arguments passed to the stat/geom
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' # standard ggplot syntax
#' ggplot() + geom_spatial(longlake_osm, aes(fill = band1)) + coord_fixed()
#' \donttest{
#' # or use ggraster()
#' ggraster(longlake_osm, aes(fill = band1))
#' }
#'
geom_spatial.Raster <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes = FALSE,
                                position = "identity", crsfrom = NA, crsto = NULL,
                                attribute_table = NA, geom = NA, stat = NA, ...) {
  # check number of bands
  nbands <- raster::nlayers(data)

  # throw a warning if there is more than one band (not in the default aesthetic)
  if(is.null(mapping) && (nbands > 1)) {
    message("geom_spatial.Raster: using aesthetic fill=band1 (nbands=", nbands, ")")
  }

  # throw a warning if crsto is not null
  if(!is.null(crsto) && identical(geom, NA)) {
    message("Attempting to use geom_raster with projected coordinates (use crsto = NULL",
            " to disable automatic projection)")
  }

  # call the default
  geom_spatial.default(data, mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position,
                       crsfrom = crsfrom, crsto = crsto, attribute_table = attribute_table,
                       geom = geom, stat = stat, ...)
}

#' @rdname geom_spatial.Raster
#' @export
ggraster <- function(data, mapping = NULL, ...) {
  if(!methods::is(data, "Raster")) stop("ggraster is only applicable to objects of class Raster")
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) +
    coord_fixed() +
    labs(x = "long", y = "lat")
}

#' Turn a Raster into a data.frame
#'
#' Like others in the \link[ggplot2]{fortify} family, this method coerces its
#' input into a \code{data.frame}. Each band in the raster is a column in the data frame,
#' alongside x and y coordinate columns. This is the format required for input to
#' \link[ggplot2]{geom_raster}, such that a \code{Raster} object can be passed directly
#' to geom_raster without a conversion function. Band columns are named band1, band2, band3,
#' etc., for use in creating a mapping.
#'
#' @param model A \code{Raster} object
#' @param data Unused
#' @param format Use 'long' to get values in a single column; otherwise
#'   values are in one column per band.
#' @param ... Not used by this method
#'
#' @return A data.frame with columns, x and y as coordinates in the projection
#'   of the Raster,
#'
#' @export
#'
#' @importFrom ggplot2 fortify
#'
#' @examples
#' # use with ggplot()
#' df <- fortify(longlake_osm)
#' ggplot(df, aes(x, y, fill = band1)) + geom_raster() +
#'   coord_fixed()
#' \donttest{
#' # identical usage with ggraster()
#' ggraster(longlake_osm, aes(fill = band1))
#'
#' # use long format to facet by band
#' dflong <- fortify(longlake_osm, format = "long")
#' ggplot(dflong, aes(long, lat, fill = value)) +
#'   geom_raster() + facet_wrap(~band) +
#'   coord_fixed()
#'
#'
#' # can use on other raster types as well
#' x <- rosm::osm.raster("gatineau qc")
#' ggraster(x, aes(fill = band1))
#' df <- fortify(x, format = "long")
#' ggplot(df, aes(long, lat, fill = value)) +
#'   geom_raster() + facet_wrap(~band) +
#'   coord_fixed()
#' }
#'
fortify.Raster <- function(model, data = NULL, format = c("wide", "long"), ...) {
  # match format arg
  format <- match.arg(format)

  # I used 'x', ggplot2 used 'model'...
  x <- model

  # get values in a data frame
  fused <- cbind(expand.grid(x=1:x@ncols, y=1:x@nrows),
                 raster::values(x))
  # set names to be long, lat, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c("long", "lat", paste0("band", 1:nbands))

  # fix x and y to be physical coordinates using the bbox
  bbox <- raster::as.matrix(x@extent)
  fused$long <- bbox[1,1]+(fused$long-1)/x@ncols*(bbox[1,2]-bbox[1,1])
  fused$lat <- bbox[2,1]+(fused$lat-x@nrows)/x@nrows*(bbox[2,1]-bbox[2,2])

  if(format == "wide") {
    # return the data frame
    fused
  } else {
    reshape2::melt(fused, id.vars = c("long", "lat"), variable.name = "band")
  }
}

# the spatial_fortify is *almost* the same, except band columns should not be
# renamed to .band1, .band2, etc.
#' @rdname spatial_fortify
#' @export
spatial_fortify.Raster <- function(x, ...) {
  fortified <- fortify(x, ...)
  plyr::rename(fortified, c(long = ".long", lat = ".lat"))
}

# add geometry definition
#' @rdname spatial_geom
#' @export
spatial_geom.Raster <- function(x) ggplot2::GeomRaster

# add default aes definition
#' @rdname spatial_geom
#' @export
spatial_default_aes.Raster <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", fill = "band1")
}
