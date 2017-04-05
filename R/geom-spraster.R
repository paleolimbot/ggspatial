




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
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) + coord_fixed()
}


