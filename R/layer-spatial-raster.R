
#' Spatial ggplot2 layer for raster objects
#'
#' This is intended for use with RGB(A) rasters (e.g., georeferenced imagery or photos). To work with
#' bands as if they were columns, use \link{df_spatial} and \link{geom_raster}.
#'
#' @param data A Raster object
#' @param mapping Currently, only RGB or RGBA rasters are supported. In the future, one may be able to
#'   map specific bands to the fill and alpha aesthetics.
#' @param interpolate Interpolate resampling for rendered raster image
#' @param is_annotation Lets raster exist without modifying scales
#' @param ... Passed to other methods
#'
#' @return A ggplot2 layer
#' @export
#'
#' @importFrom ggplot2 waiver
#'
layer_spatial.Raster <- function(data, mapping = NULL, interpolate = TRUE, is_annotation = FALSE, ...) {


  is_rgb <- is.null(mapping) && (raster::nbands(data) %in% c(3, 4))
  if(is_rgb) {
    # RGB(A)
    if(is_annotation) {
      stat <- StatSpatialRasterAnnotation
    } else {
      stat <- StatSpatialRaster
    }

    geom <- GeomSpatialRaster
    mapping <- ggplot2::aes_string(raster = "raster")
  } else {
    # mapped aesthetics mode
    if(is_annotation) {
      stop("Non-RGBA rasters with is_annotation = TRUE is not implemented.")
    } else {
      stat <- StatSpatialRasterDf
    }

    geom <- ggplot2::GeomRaster
    mapping <- override_aesthetics(
      mapping,
      ggplot2::aes_string(raster = "raster")
    )
  }

  c(
    ggplot2::layer(
      data = tibble::tibble(raster = list(data)),
      mapping = mapping,
      stat = stat,
      geom = geom,
      position = "identity",
      inherit.aes = FALSE, show.legend = !is_rgb,
      params = list(interpolate = interpolate, ...)
    ),
    # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
    # using the first layer's CRS as the base CRS for coord_sf().
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(), crs = sf::st_crs(data@crs@projargs)),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @rdname layer_spatial.Raster
#' @export
annotation_spatial.Raster <- function(data, mapping = NULL, interpolate = TRUE, ...) {
  layer_spatial.Raster(data, mapping = mapping, interpolate = interpolate, is_annotation = TRUE, ...)
}

StatSpatialRaster <-  ggplot2::ggproto(
  "StatSpatialRaster",
  Stat,
  required_aes = "raster",
  extra_params = "",

  compute_layer = function(self, data, params, layout) {

    # project to target coordinate system
    coord_crs <- layout$coord_params$crs
    if(!is.null(coord_crs)) {
      data$raster <- lapply(
        data$raster,
        raster::projectRaster,
        crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
      )
    } else {
      warning("Spatial rasters may not be displayed correctly. Use coord_sf().", call. = FALSE)
    }

    # add extent so that scales are trained properly
    data$extent <- lapply(data$raster, raster::extent)
    data$xmin <- vapply(data$extent, function(x) x@xmin, numeric(1))
    data$xmax <- vapply(data$extent, function(x) x@xmax, numeric(1))
    data$ymin <- vapply(data$extent, function(x) x@ymin, numeric(1))
    data$ymax <- vapply(data$extent, function(x) x@ymax, numeric(1))

    data
 }
)

StatSpatialRasterAnnotation <- ggplot2::ggproto(
  "StatSpatialRaster",
  StatSpatialRaster,
  required_aes = "raster",

  compute_layer = function(self, data, params, layout) {
    data <- ggplot2::ggproto_parent(self, StatSpatialRaster)$compute_layer(data, params, layout)
    data$xmin <- NULL
    data$xmax <- NULL
    data$ymin <- NULL
    data$ymax <- NULL

    data
  }
)

StatSpatialRasterDf <- ggplot2::ggproto(
  "StatSpatialRasterDf",
  StatSpatialRasterAnnotation,
  required_aes = "raster",
  default_aes = ggplot2::aes(fill = stat(band1)),

  compute_layer = function(self, data, params, layout) {
    data <- ggplot2::ggproto_parent(self, StatSpatialRasterAnnotation)$compute_layer(data, params, layout)
    data$extent <- NULL

    # make all rasters data frames using df_spatial, if column still exists
    if("raster" %in% colnames(data)) {
      data$raster <- lapply(data$raster, df_spatial)
      tidyr::unnest(data, .data$raster)
    } else {
      data
    }
  }
)

GeomSpatialRaster <- ggplot2::ggproto(
  "GeomSpatialRaster",
  ggplot2::Geom,
  extra_params = "",

  required_aesthetics = c("raster", "extent"),

  default_aes = ggplot2::aes(alpha = 1),

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coordinates, interpolate = TRUE, crop = TRUE) {
    ext <- data$extent[[1]]
    corners <- data.frame(x = c(ext@xmin, ext@xmax), y = c(ext@ymin, ext@ymax))
    corners_trans <- coordinates$transform(corners, panel_params)

    x_rng <- range(corners_trans$x, na.rm = TRUE)
    y_rng <- range(corners_trans$y, na.rm = TRUE)

    grid::rasterGrob(
      raster_as_array(data$raster[[1]], alpha = data$alpha[1]),
      x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate
    )
  }
)


raster_as_array <- function(raster_obj, na.value = NA, alpha = 1) {
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
  if((alpha != 1) || (is.na(na.value) && any(is.na(raster)))) {
    # find NA cells
    na_cells <- is.na(raster[, , 1]) | is.na(raster[, , 2]) | is.na(raster[, , 3])

    # grid doesn't do non-finite values, so we need to set the transparency band
    # for missing cells
    if(dim(raster)[3] == 4) {
      tband <- raster[ , , 4, drop = FALSE]
    } else {
      tband <- array(1, dim(raster)[1:2])
    }

    # set alpha to NA cells to 0
    tband[na_cells] <- 0

    # multiply the alpha band by the requested alpha (clamping to 0,1)
    tband <- tband * max(0, min(1, alpha))

    # bind it to the original raster
    raster <- abind::abind(raster[, , 1:3, drop = FALSE], tband)

    # set NA values to 0
    raster[is.na(raster)] <- 0
  } else {
    raster[is.na(raster)] <- na.value
  }

  raster
}

