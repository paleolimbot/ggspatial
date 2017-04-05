
# add method definitions for types in the raster package

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

# add geometry definition
spatial_geom.Raster <- function(x) ggplot2::GeomRaster

# add default aes definition
spatial_default_aes.Raster <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", fill = ".band1")
}
