
#' Spatial ggplot2 layer for SpatRaster objects
#'
#' This is intended for use with RGB(A) rasters (e.g., georeferenced imagery
#' or photos). To work with bands as if they were columns, use [df_spatial]
#' and [geom_raster].
#'
#' @param data A SpatRaster object created with [terra::rast()].
#' @inheritParams layer_spatial.Raster
#'
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' load_longlake_data(
#'   which = c(
#'     "longlake_osm",
#'     "longlake_depth_raster"
#'   ),
#'   raster_format = "terra"
#' )
#' ggplot() +
#'   layer_spatial(longlake_osm)
#'
#' ggplot() +
#'   layer_spatial(longlake_depth_raster) +
#'   scale_fill_continuous(
#'     na.value = NA,
#'     type = "viridis"
#'   )
#' }
layer_spatial.SpatRaster <- function(data,
                                     mapping = NULL,
                                     interpolate = NULL,
                                     is_annotation = FALSE,
                                     lazy = FALSE,
                                     dpi = 150,
                                     ...) {
  is_rgb <- is.null(mapping) && (terra::nlyr(data) %in% c(3, 4))
  if (is_rgb) {
    # RGB(A)
    if (is_annotation) {
      stat <- StatSpatRasterAnnotation
    } else {
      stat <- StatSpatRaster
    }

    geom <- GeomSpatRaster
    mapping <- ggplot2::aes(raster = .data$raster)
  } else {
    # mapped aesthetics mode
    if (is_annotation) {
      stop("Non-RGBA rasters with is_annotation = TRUE is not implemented.")
    } else {
      stat <- StatSpatRasterDf
    }

    geom <- ggplot2::GeomRaster
    mapping <- override_aesthetics(
      mapping,
      ggplot2::aes(raster = .data$raster)
    )
  }

  c(
    ggplot2::layer(
      data = tibble::tibble(raster = list(data)),
      mapping = mapping,
      stat = stat,
      geom = geom,
      position = "identity",
      inherit.aes = FALSE, show.legend = if (is_rgb) FALSE else NA,
      params = list(
        interpolate = if (!is.null(interpolate)) interpolate else is_rgb,
        lazy = lazy,
        dpi = dpi,
        ...
      )
    ),
    # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
    # using the first layer's CRS as the base CRS for coord_sf().
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(),
        crs = sf::st_crs(terra::crs(data))
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @rdname layer_spatial.SpatRaster
#' @export
annotation_spatial.SpatRaster <- function(data, mapping = NULL,
                                          interpolate = NULL, ...) {
  layer_spatial.SpatRaster(data,
    mapping = mapping,
    interpolate = interpolate,
    is_annotation = TRUE, ...
  )
}

#' @rdname layer_spatial.SpatRaster
#' @export
# nocov start
StatSpatRaster <- ggplot2::ggproto(
  "StatSpatialRaster",
  ggplot2::Stat,
  required_aes = "raster",
  compute_layer = function(self, data, params, layout) {


    # raster extents
    extents <- lapply(data$raster, terra::ext)

    # project to target coordinate system, if one exists
    coord_crs <- layout$coord_params$crs
    if (!is.null(coord_crs)) {
      data$extent <- lapply(seq_along(extents), function(i) {
        x <- as.vector(extents[[i]])

        project_extent(
          xmin = x["xmin"],
          xmax = x["xmax"],
          ymin = x["ymin"],
          ymax = x["ymax"],
          from_crs = sf::st_crs(terra::crs(data$raster[[i]])),
          to_crs = sf::st_crs(coord_crs)
        )
      })

      # this needs to be directly in the data so that the position scales get trained
      data$xmin <- vapply(data$extent, function(x) x["xmin"], numeric(1))
      data$xmax <- vapply(data$extent, function(x) x["xmax"], numeric(1))
      data$ymin <- vapply(data$extent, function(x) x["ymin"], numeric(1))
      data$ymax <- vapply(data$extent, function(x) x["ymax"], numeric(1))
    } else {
      stop("Spatial rasters require coord_sf().", call. = FALSE)
    }

    data
  }
)
# nocov end

#' @rdname layer_spatial.SpatRaster
#' @export
# nocov start
StatSpatRasterAnnotation <- ggplot2::ggproto(
  "StatSpatRaster",
  StatSpatRaster,
  required_aes = "raster",
  compute_layer = function(self, data, params, layout) {
    data <- ggplot2::ggproto_parent(
      self,
      StatSpatRaster
    )$compute_layer(data, params, layout)
    data$xmin <- NULL
    data$xmax <- NULL
    data$ymin <- NULL
    data$ymax <- NULL

    data
  }
)
# nocov end
#' @rdname layer_spatial.SpatRaster
#' @export
# nocov start
StatSpatRasterDf <- ggplot2::ggproto(
  "StatSpatRasterDf",
  ggplot2::Stat,
  required_aes = "raster",
  extra_params = c("lazy", "dpi"),
  default_aes = ggplot2::aes(fill = after_stat(band1)),
  compute_layer = function(self, data, params, layout) {
    if (params$lazy) stop("Lazy rendering not implemented for mapped rasters")

    # project to target coordinate system
    # make all rasters data frames using df_spatial, if column still exists
    # (because this method gets called multiple times)

    if ("raster" %in% colnames(data)) {
      coord_crs <- layout$coord_params$crs
      if (!is.null(coord_crs)) {
        data$raster <- lapply(
          data$raster,
          project_terra_lazy,
          crs = sf::st_crs(coord_crs)$wkt
        )
      } else {
        warning("Spatial rasters may not be displayed correctly. Use coord_sf().", call. = FALSE)
      }

      data$raster <- lapply(data$raster, df_spatial)
      tidyr::unnest(data, "raster")
    } else {
      data
    }
  }
)
# nocov end

#' @rdname layer_spatial.SpatRaster
#' @export
# nocov start
GeomSpatRaster <- ggplot2::ggproto(
  "GeomSpatRaster",
  ggplot2::Geom,
  required_aesthetics = c("raster", "extent"),
  default_aes = ggplot2::aes(alpha = 1),
  handle_na = function(data, params) {
    data
  },
  draw_panel = function(data, panel_params,
                        coordinates,
                        interpolate = FALSE,
                        lazy = FALSE,
                        dpi = 150,
                        max_pixel_scale = 1) {
    rst <- data$raster[[1]]
    coord_crs <- panel_params$crs
    alpha <- data$alpha[1]

    if (lazy) {

      # this code makes sure that the resampled raster doesn't contain any more area than it needs to
      coord_bbox <- panel_params_as_bbox(panel_params)
      rst_bbox <- data$extent[[1]]
      if (bboxes_equal(coord_bbox, rst_bbox)) {
        target_bbox <- coord_bbox
      } else {
        target_bbox <- sf::st_bbox(
          sf::st_intersection(
            bbox_as_polygon(rst_bbox),
            bbox_as_polygon(coord_bbox)
          )
        )
      }
      bbox_scale_x <- (target_bbox["xmax"] - target_bbox["xmin"]) / (coord_bbox["xmax"] - coord_bbox["xmin"])
      bbox_scale_y <- (target_bbox["ymax"] - target_bbox["ymin"]) / (coord_bbox["ymax"] - coord_bbox["ymin"])

      # this code makes sure we aren't making more pixels in the resampled image than there was in the
      # original image (a bit hard because of reprojection but we can estimate)
      original_pixels_y <- dim(rst)[1]
      original_pixels_x <- dim(rst)[2]
      projected_aspect <- (rst_bbox["ymax"] - rst_bbox["ymin"]) / (rst_bbox["xmax"] - rst_bbox["xmin"])
      rst_bbox_scale_x <- (target_bbox["xmax"] - target_bbox["xmin"]) / (rst_bbox["xmax"] - rst_bbox["xmin"])
      rst_bbox_scale_y <- (target_bbox["ymax"] - target_bbox["ymin"]) / (rst_bbox["ymax"] - rst_bbox["ymin"])
      max_pixels_x <- sqrt(original_pixels_x * original_pixels_y / projected_aspect) * max_pixel_scale
      max_pixels_y <- projected_aspect * max_pixels_x * max_pixel_scale

      # return a gTree with the right params so that the raster can be rendered later
      grid::gTree(
        raster_obj = rst,
        coord_crs = coord_crs,
        coordinates = coordinates,
        target_bbox = target_bbox,
        bbox_scale = c(bbox_scale_x, bbox_scale_y),
        max_pixels = c(max_pixels_x, max_pixels_y),
        panel_params = panel_params,
        alpha = alpha,
        interpolate = interpolate,
        dpi = dpi,
        cache = new.env(parent = emptyenv()),
        cl = "geom_spatial_terra_lazy"
      )
    } else {
      raster_grob_from_terra(
        rst,
        coord_crs,
        coordinates,
        panel_params,
        alpha = alpha,
        interpolate = interpolate
      )
    }
  }
)
# nocov end

#' @importFrom grid makeContent
#' @export
makeContent.geom_spatial_terra_lazy <- function(x) {
  width_in <- grid::convertWidth(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  height_in <- grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)

  # until I figure out how to get DPI from the graphics device at load time,
  # use the user-specified value
  dpi_x <- x$dpi
  dpi_y <- x$dpi
  width_px <- min(ceiling(width_in * dpi_x * x$bbox_scale[1]), x$max_pixels[1])
  height_px <- min(ceiling(height_in * dpi_y * x$bbox_scale[2]), x$max_pixels[2])

  # check to see if the x$cache environment has a raster already
  raster_var <- paste0("raster_", width_px, "x", height_px)
  if (raster_var %in% names(x$cache)) {
    return(grid::setChildren(x, grid::gList(x$cache[[raster_var]])))
  }

  if (!is.null(x$coord_crs)) {
    template_raster <- terra::rast(
      xmin = x$target_bbox["xmin"],
      ymin = x$target_bbox["ymin"],
      xmax = x$target_bbox["xmax"],
      ymax = x$target_bbox["ymax"],
      ncols = width_px,
      nrows = height_px,
      crs = sf::st_crs(x$coord_crs)$wkt
    )
  } else {
    template_raster <- NULL
  }

  # add content to the gTree, cache the rasterGrob
  x$cache[[raster_var]] <- raster_grob_from_terra(
    x$raster_obj,
    x$coord_crs,
    x$coordinates,
    x$panel_params,
    alpha = x$alpha,
    interpolate = x$interpolate,
    template_raster = template_raster
  )

  grid::setChildren(x, grid::gList(x$cache[[raster_var]]))
}

raster_grob_from_terra <- function(rst, coord_crs, coordinates, panel_params,
                                   template_raster = NULL, alpha = 1,
                                   interpolate = TRUE) {
  if (interpolate) {
    raster_method <- "bilinear"
  } else {
    raster_method <- "near"
  }

  if (!is.null(template_raster) && !is.null(coord_crs)) {
    rst <- terra::project(
      rst,
      y = template_raster,
      method = raster_method
    )
  } else if (!is.null(coord_crs)) {
    # project
    # raster::projectRaster has very odd behaviour...it outputs the warning
    # "no non-missing arguments to max; returning -Inf"
    # but only when run with a calling handler
    rst <- project_terra_lazy(
      rst,
      crs = sf::st_crs(coord_crs)$wkt
    )
  } else if (!is.null(template_raster)) {
    # resample (& crop?)
    rst <- terra::resample(rst, template_raster, method = raster_method)
  }

  ext <- as.vector(terra::ext(rst))
  corners <- data.frame(x = c(ext["xmin"], ext["xmax"]), y = c(ext["ymin"], ext["ymax"]))
  corners_trans <- coordinates$transform(corners, panel_params)

  x_rng <- range(corners_trans$x, na.rm = TRUE)
  y_rng <- range(corners_trans$y, na.rm = TRUE)

  grid::rasterGrob(
    terra_as_array(rst, alpha = alpha),
    x_rng[1], y_rng[1],
    diff(x_rng), diff(y_rng),
    default.units = "native",
    just = c("left", "bottom"), interpolate = interpolate
  )
}

terra_as_array <- function(raster_obj, na.value = NA, alpha = 1) {
  if (!methods::is(raster_obj, "SpatRaster")) stop("Cannot use terra_as_array with non SpatRaster object")

  raster <- terra::as.array(raster_obj)

  # check dims
  dims <- dim(raster)
  if (length(dims) != 3) stop("Raster has incorrect dimensions: ", paste(dims, collapse = ", "))
  if (!(dims[3] %in% c(3, 4))) stop("Need a 3 or 4-band array to use terra_as_array")

  # make values between 0 and 1, if they are not already
  vrange <- range(raster, finite = TRUE)
  if (!all(vrange >= 0 & vrange <= 1)) {
    if (all(vrange >= 0 & vrange <= 256)) {
      raster <- scales::rescale(raster, from = c(0, 256))
    } else {
      raster <- scales::rescale(raster)
    }
  }

  # eliminate NAs
  if ((alpha != 1) || (is.na(na.value) && any(is.na(raster)))) {
    # find NA cells
    na_cells <- is.na(raster[, , 1]) | is.na(raster[, , 2]) | is.na(raster[, , 3])

    # grid doesn't do non-finite values, so we need to set the transparency band
    # for missing cells
    if (dim(raster)[3] == 4) {
      tband <- raster[, , 4, drop = FALSE]
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

project_terra_lazy <- function(x, crs, ...) {

  # We compare both crs using sf::st_crs()$wkt
  x_crs <- terra::crs(x)

  if (sf::st_crs(crs)$wkt != sf::st_crs(x_crs)$wkt) {
    terra::project(x, y = crs, ...)
  } else {
    x
  }
}
