
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
  data <- data.frame(long = bbox[1, ], lat = bbox[2, ])

  # returns a list with both layers
  list(
    ggplot2::geom_point(ggplot2::aes_string("long", "lat"), data = data, alpha = 0, inherit.aes = FALSE,
                        show.legend = FALSE),
    annotation_spraster(raster, interpolate = interpolate, na.value = na.value)
  )
}
