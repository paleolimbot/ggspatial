
#' Spatial ggplot2 layer for raster objects
#'
#' @param data A Raster object
#' @param mapping A mapping, using band names or band1, band2, etc. to refer to band names
#' @param interpolate Interpolate resampling for rendered raster image
#' @param ... Passed to other methods
#'
#' @return A ggplot2 layer
#' @export
#'
#' @importFrom ggplot2 waiver
#'
layer_spatial.Raster <- function(data, mapping = waiver(), interpolate = TRUE, is_annotation = FALSE, ...) {
  ggplot2::layer(
    data = data.frame(x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_)),
    mapping = NULL, stat = StatSpatialRaster,
    geom = GeomSpatialRaster, position = "identity",
    inherit.aes = FALSE, show.legend = NA,
    params = list(raster_obj = data, interpolate = interpolate, is_annotation = is_annotation)
  )
}

StatSpatialRaster <-  ggproto(
  "StatSpatialRaster",
  Stat,
  extra_params = c("raster_obj", "is_annotation"),

  compute_layer = function(self, data, params, layout) {
    if(!params$is_annotation) {

      coord_crs <- layout$coord$crs

      if(!is.null(coord_crs)) {
        raster_trans <- raster::projectRaster(
          params$raster_obj,
          crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
        )
      } else {
        warning("Skipping projection in geom_spatial_raster. Use coord_sf() to project.")
        raster_trans <- raster_obj
      }

      ext <- raster_trans@extent
      data$x <- c(ext@xmin, ext@xmax)
      data$y <- c(ext@ymin, ext@ymax)
    }

    data
 }
)

GeomSpatialRaster <- ggplot2::ggproto(
  "GeomSpatialRaster",
  ggplot2::Geom,

  required_aesthetics = c("x", "y"),

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coordinates, raster_obj, interpolate = TRUE, crop = TRUE) {

    coord_crs <- coordinates$crs

    if(!is.null(coord_crs)) {
      raster_trans <- raster::projectRaster(
        raster_obj,
        crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
      )
    } else {
      warning("Skipping projection in geom_spatial_raster. Use coord_sf() to project.")
      raster_trans <- raster_obj
    }

    ext <- raster_trans@extent
    corners <- data.frame(x = c(ext@xmin, ext@xmax), y = c(ext@ymin, ext@ymax))
    data <- coordinates$transform(corners, panel_params)

    x_rng <- range(data$x, na.rm = TRUE)
    y_rng <- range(data$y, na.rm = TRUE)

    grid::rasterGrob(
      raster_as_array(raster_trans),
      x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate
    )
  }

)


raster_as_array <- function(raster_obj, na.value = NA) {
  if(!methods::is(raster_obj, "Raster")) stop("Cannot use raster_as_array with non Raster* object")

  raster <- raster::as.array(raster_obj)

  # check dims
  dims <- dim(raster)
  if(length(dims) != 3) stop("Raster has incorrect dimensions: ", paste(dims, collapse = ", "))
  if(!(dims[3] %in% c(3, 4))) stop("Need a 3 or 4-band array to use raster_as_array")

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

  raster
}

