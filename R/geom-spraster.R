

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
#' ggplot(dflong, aes(x, y, fill = value)) +
#'   geom_raster() + facet_wrap(~band) +
#'   coord_fixed()
#'
#'
#' # can use on other raster types as well
#' x <- rosm::osm.raster("gatineau qc")
#' ggraster(x, aes(fill = band1))
#' df <- fortify(x, format = "long")
#' ggplot(df, aes(x, y, fill = value)) +
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
  # set names to be x, y, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c("x", "y", paste0("band", 1:nbands))

  # fix x and y to be physical coordinates using the bbox
  bbox <- raster::as.matrix(x@extent)
  fused$x <- bbox[1,1]+(fused$x-1)/x@ncols*(bbox[1,2]-bbox[1,1])
  fused$y <- bbox[2,1]+(fused$y-x@nrows)/x@nrows*(bbox[2,1]-bbox[2,2])

  if(format == "wide") {
    # return the data frame
    fused
  } else {
    reshape2::melt(fused, id.vars = c("x", "y"), variable.name = "band")
  }
}


#' Annotation raster using Raster* layers
#'
#' This is a thin wrapper around \link[ggplot2]{annotation_raster}, with the
#' bounds arguments filled in using the bounds of the raster. Like annotation_raster,
#' this will not adjust the extents of the plot. The coordinates used are in
#' the coordinate system of the raster, which are likely not lat/lon.
#'
#' @param raster A Raster* object
#' @param interpolate TRUE to interpolate rendering
#' @param na.value A value to represent NAs, since a transparency band
#'   may or may not exist for this raster. If na.value = NA, a transparency
#'   band will be created to remove missing values from display.
#'
#' @return An annotation layer or list of layers (geom_spraster_rgb)
#' @export
#'
#' @examples
#' ggplot() +
#'   annotation_spraster(longlake_osm) +
#'   geom_spatial(longlake_waterdf, toepsg = 26920) +
#'   coord_fixed()
#'
#'
annotation_spraster <- function(raster, interpolate = FALSE, na.value = NA) {
  if(!methods::is(raster, "Raster")) stop("Cannot use annotation_spraster with non Raster* object")
  bbox <- raster::as.matrix(raster@extent)
  raster <- raster::as.array(raster)

  # check dims
  dims <- dim(raster)
  if(length(dims) != 3) stop("Raster has incorrect dimensions: ", paste(dims, collapse = ", "))
  if(!(dims[3] %in% c(3, 4))) stop("Need a 3 or 4-band array to use annotation_spraster")

  # make values between 0 and 1, if they are not already
  vrange <- range(raster, finite = TRUE)
  if(!all(vrange >= 0 & vrange <= 1)) {
    if(all(vrange >= 0 & vrange <= 256)) {
      raster <- scales::rescale(raster, from = c(0, 256))
    } else{
      raster <- scales::rescale(raster)
    }
  }

  # eliminate NAs
  if(is.na(na.value) && any(is.na(raster))) {
    # find NA cells
    na_cells <- is.na(raster[, , 1]) | is.na(raster[, , 2]) |
      is.na(raster[, , 3])

    # grid doesn't do non-finite values, so we need to set the transparency band
    # for missing cells
    if(dim(raster)[3] == 4) {
      tband <- raster[ , , 4, drop = FALSE]
    } else {
      tband <- array(1, dim(raster)[1:2])
    }

    # set alpha to NA cells to 0
    tband[na_cells] <- 0

    # bind it to the original raster
    raster <- abind::abind(raster[, , 1:3, drop = FALSE], tband)

    # set NA values to 0
    raster[is.na(raster)] <- 0
  } else {
    raster[is.na(raster)] <- na.value
  }

  # call ggplot2's annotation_raster
  ggplot2::annotation_raster(raster, bbox[1, 1], bbox[1, 2], bbox[2, 1], bbox[2, 2],
                             interpolate = interpolate)
}

#' @rdname annotation_spraster
#' @export
geom_spraster_rgb <- function(raster, interpolate = FALSE, na.value = NA) {
  if(!methods::is(raster, "Raster")) stop("Cannot use geom_spraster_rgb with non Raster* object")

  # wraps around annotation_spraster using a blank geometry to set bounds
  bbox <- raster::as.matrix(raster@extent)
  data <- data.frame(x = bbox[1, ], y = bbox[2, ])

  # returns a list with both layers
  list(
    ggplot2::geom_point(ggplot2::aes_string("x", "y"), data = data, alpha = 0, inherit.aes = FALSE,
                        show.legend = FALSE),
    annotation_spraster(raster, interpolate = interpolate, na.value = na.value)
  )
}


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
geom_spatial.Raster <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes=FALSE,
                                position = "identity", crsfrom = crsfrom, crsto = crsto,
                                geom = "raster", stat = "identity", ...) {

  if(stat == "project") {
    if(geom == "raster") {
      message("stat == 'project' and geom == 'raster' are unlikely to work properly")
    }

    # get projections
    projections <- get_projections(data = data, crsfrom = crsfrom, crsto = crsto)

  } else {
    # no projections
    projections <- list()
  }

  # set the defaults
  final_mapping <- ggplot2::aes_string("x", "y")
  final_mapping <- c(mapping, final_mapping)
  class(final_mapping) <- "uneval"

  # create layer
  gglayer <- layer(
    stat = stat, data = fortify.Raster(data), mapping = final_mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = "identity",
    params=c(projections, list(...)))

  # if stat is rgba, we need to return a list, since this stat
  # makes no sense without scale_alpha_identity and scale_fill_identity
  if(stat == "rgba") {
    list(gglayer, ggplot2::scale_alpha_identity(), ggplot2::scale_fill_identity())
  } else {
    # return layer
    gglayer
  }
}

#' @rdname geom_spatial.Raster
#' @export
ggraster <- function(data, mapping = NULL, ...) {
  if(!methods::is(data, "Raster")) stop("ggraster is not applicable for class of type ", class(data))
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...)
}

#' Statistic to create RGB fill values
#'
#' Usually used on conjunction with \link[ggplot2]{geom_raster} or
#' \link{geom_spatial.Raster}. This function appears to work, however
#' it is slow, and in general, only good for very small datasets.
#'
#' @param mapping A mapping created with \link[ggplot2]{aes}
#' @param data A data.frame
#' @param ... Passed to \link[ggplot2]{geom_raster}
#' @param limits_red Data limits from which to scale red values. Use NULL to
#'   perform no transformation, or NA to use the data value without transformation.
#' @param limits_green Data limits from which to scale green values
#' @param limits_blue Data limits from which to scale blue values
#' @param limits_alpha Data limits from which to scale alpha values
#'
#' @return A ggplot2 layer object
#' @export
#'
#' @examples
#' # using stat_rgba()
#' ggplot(longlake_osm, aes(x, y)) +
#'   stat_rgba(aes(red = band1, green = band2, blue = band3, alpha = 1),
#'             limits_red = NULL, limits_green = NULL, limits_blue = NULL,
#'             limits_alpha = NULL, interpolate = TRUE) +
#'   coord_fixed()
#' \donttest{
#' # using ggraster() with stat = 'rgba'
#' ggraster(longlake_osm, aes(red = band1, green = band2, blue = band3, alpha = 1),
#'          stat = "rgba")
#' }
#'
stat_rgba <- function(mapping = NULL, data = NULL, ..., limits_red = NA, limits_green = NA,
                      limits_blue = NA, limits_alpha = NA) {
  # return a list, since this stat is meaningless without identity scales
  # for fill and alpha

  list(
    ggplot2::geom_raster(mapping = mapping, data = data, stat = StatRgba,
                         limits_red = limits_red,
                         limits_green = limits_green, limits_blue = limits_blue,
                         limits_alpha = limits_alpha, ...),
    ggplot2::scale_alpha_identity(),
    ggplot2::scale_fill_identity()
  )
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

    data$rgb <- paste0("#", do.call(paste0, lapply(data[c("red", "green", "blue")],
                                                    as.character.hexmode, 2)))
    data$rgb[not_finite] <- NA

    data
  },

  required_aes = c("red", "green", "blue", "alpha"),

  default_aes = aes(fill = ..rgb..)
)

# this rescale item appears in StatRgba
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
