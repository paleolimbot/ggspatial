

#' Turn a Raster into a data.frame
#'
#' Like others in the \link[ggplot2]{fortify} family, this method coerces its
#' input into a \code{data.frame}. Each band in the raster is a column in the data frame,
#' alongside x and y coordinate columns. This is the format required for input to
#' \link[ggplot2]{geom_raster}, such that a \code{Raster} object can be passed directly
#' to geom_raster without a conversion function. Band columns are named band1, band2, band3,
#' etc., for use in creating a mapping.
#'
#' @param x A \code{Raster} object
#' @param ... Not used by this method
#'
#' @return A data.frame with columns, x and y as coordinates in the projection
#'   of the Raster,
#'
#' @export
#'
fortify.Raster <- function(x, ...) {

  # get values in a data frame
  fused <- cbind(expand.grid(x=1:x@ncols, y=1:x@nrows),
                 raster::values(x))
  # set names to be x, y, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c("x", "y", paste0("band", 1:nbands))

  # fix x and y to be physical coordinates using the bbox
  bbox <- raster::as.matrix(x@extent)
  fused$x <- bbox[1,1]+(fused$x-1)/x@ncols*(bbox[1,2]-bbox[1,1])
  fused$y <- bbox[2,1]+(fused$y-x@nrows)/x@nrows*(bbox[2,1]-bbox[2,2])

  # return the data frame
  fused
}


#' Spatial Geometry for Raster* Objects
#'
#' Similar to the \link{geom_spatial} family for Spatial* objects, this method
#' plots a spatial raster as a spatial raster. Note that projecting (or un-projecting)
#' this layer will likely result in odd results (namely that geom_raster will not
#' function properly or at all).
#'
#' @param data A Raster* object
#' @param mapping A mapping
#' @param geom The geometry to use. Defaults to raster (obviously), but could also
#'   be another value if used with a different stat (e.g. contour)
#' @param stat The stat to apply. Defaults to 'identity', but could be something else
#'   like 'contour' or \link{stat_rgba}.
#' @param position The position to apply (should probably always be 'identity')
#' @param ... Further arguments passed to the stat/geom
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' ggplot() + geom_spatial(longlake_osm)
#'
geom_spatial.Raster <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=FALSE,
                                position = "identity", fromepsg=NULL, toepsg=NULL,
                                fromprojection=NULL, toprojection=NULL, geom = "raster",
                                stat = "identity", ...) {

  if(stat == "project") {
    if(geom == "raster") {
      message("stat == 'project' and geom == 'raster' are unlikely to work properly")
    }

    # get projections
    projections <- get_projections(data = data,
                                   fromepsg = fromepsg, toepsg = toepsg,
                                   fromprojection = fromprojection,
                                   toprojection = toprojection)
  } else {
    # no projections
    projections <- list()
  }

  # set the defaults
  final_mapping <- ggplot2::aes_string("x", "y", fill = "band1")
  final_mapping <- c(mapping, final_mapping)
  class(final_mapping) <- "uneval"

  # return layer
  layer(
    stat = stat, data = fortify.Raster(data), mapping = final_mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = "identity",
    params=c(projections, list(...)))
}

rescale_item <- function(values, limits, to, identity = 0) {
  # need a zero range value (probably 0)
  if(scales::zero_range(range(values, finite = TRUE))) {
    # if single value, clamp to range
    val <- range(values, finite = TRUE)[1]
    if(val > to[2]) {
      val <- to[2]
    } else if(val < to[1]) {
      val <- to[1]
    }
    rep_len(val, length.out = length(values))
  } else if(is.null(limits)) {
    # null means leave alone, but still need to censor
    scales::censor(values, to)
  } else if(identical(limits, NA)) {
    # NA means scale to min/max
    scales::rescale(values, to = to)
  } else if(length(limits) == 2) {
    scales::rescale(values, from = limits, to = to)
  } else {
    stop("Don't know how to rescale with limits ", values)
  }
}


stat_rgba <- function(mapping = NULL, data = NULL, alpha = 1, red = 0, green = 0,
                      blue = 0, geom = "point", position = "identity", ...) {
  # TODO working on this
}

StatRgba <- ggplot2::ggproto("StatRgba", ggplot2::Stat,

  compute_group = function(data, scales, limits_red = NA, limits_green = NA,
                           limits_blue = NA, limits_alpha = NA) {

    # rescale data between 0 and 255 for RGB, 0 and 1 for alpha
    data$red <- rescale_item(data$red, limits_red, to = c(0, 255))
    data$green <- rescale_item(data$green, limits_green, to = c(0, 255))
    data$blue <- rescale_item(data$blue, limits_blue, to = c(0, 255))
    data$alpha <- rescale_item(data$alpha, limits_alpha, to = c(0, 1))

    not_finite <- !complete.cases(data[c("red", "green", "blue", "alpha")])

    data$fill <- paste0("#", do.call(paste0, lapply(data[c("red", "green", "blue")],
                                                    as.character.hexmode, 2)))
    data$fill[not_finite] <- NA
    return(data)
  },

  required_aes = c("red", "green", "blue", "alpha"),

  default_aes = aes(red = ..band1.., green = ..band2..,
                    blue = ..band3.., ..band4..)
)

