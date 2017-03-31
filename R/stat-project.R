
#' Statisitc to project coordinates
#'
#' Projects coordinates using rgdal/sp. Takes params
#' \code{crsfrom} and \code{crsto}, both wrapped in \link{as.CRS}, such that you can
#' pass an epsg code, a CRS object or \code{NA} to guess the input (will be either lat/lon or
#' google mercator). If \code{NA}, \code{crsto} is assumed
#' to be EPSG:4326 (Lat/Lon), so data can be used with \link[ggplot2]{coord_map}.
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
#' @param ... Passed to the geom
#'
#' @export
#'
#' @examples
#' # longlake roads df is in UTM zone 20 (epsg:26920)
#' ggplot(longlake_roadsdf, aes(long, lat)) +
#'   stat_project(geom = "path", crsfrom = 26920) +
#'   coord_map()
#'
stat_project <- function(mapping = NULL, data = NULL, crsfrom = NA, crsto = NA,
                         position = "identity", show.legend = TRUE, inherit.aes = TRUE,
                         geom = "point", ...) {
  layer(
    stat = StatProject, data = data, mapping = mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = position,
    params=list(crsfrom = as.CRS(crsfrom), crsto = as.CRS(crsto), ...)
  )
}

# stat to transform coordinates
StatProject <- ggplot2::ggproto("StatProject", ggplot2::Stat,

    # TODO: this should really be in compute_layer, which has
    # slightly different arguments
    compute_panel = function(data, scales, crsfrom=NA, crsto=NA) {

      # guess missing coordinate systems
      if(identical(crsfrom, NA) || is.na(rgdal::CRSargs(crsfrom))) {
        epsg <- guess.epsg(sp::bbox(cbind(data$x, data$y)))
        crsfrom <- as.CRS(epsg)
      }

      # default is actually to "unproject", for use with ggmap/coord_map
      if(identical(crsto, NA) || is.na(rgdal::CRSargs(crsto))) {
        crsto <- as.CRS(4326)
        if(crsfrom@projargs != crsto@projargs) {
          message("Converting coordinates to lat/lon (epsg:4326)")
        }
      }

      # if crs from and to are equivalent, return the original data
      if(crsfrom@projargs == crsto@projargs) {
        return(data)
      } else {
        coords <- xyTransform(data$x, data$y, from = crsfrom, to = crsto)
        data$x <- coords[,1]
        data$y <- coords[,2]
        return(data)
      }
    },

    required_aes = c("x", "y")
)
