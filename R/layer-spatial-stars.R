
#' Spatial ggplot2 layer for stars objects
#'
#' This is intended for use with RGB(A) rasters (e.g., georeferenced imagery or photos). To work with
#' bands as if they were columns, use [df_spatial] and [geom_raster].
#'
#' @param data A stars object
#' @param mapping Currently, only RGB or RGBA rasters are supported. In the future, one may be able to
#'   map specific bands to the fill and alpha aesthetics.
#' @param interpolate Interpolate resampling for rendered raster image
#' @param is_annotation Lets raster exist without modifying scales
#' @param lazy Delay projection and resample of raster until the plot is being rendered
#' @param dpi if lazy = TRUE, the dpi to which the raster should be resampled
#' @param options GDAL options for warping/resampling (see [st_warp][stars::st_warp])
#' @param ... Passed to other methods
#'
#' @return A ggplot2 layer
#' @export
#' @examples
#' \donttest{
#'
#' library(ggplot2)
#' load_longlake_data(
#'   which = c(
#'     "longlake_osm",
#'     "longlake_depth_raster"
#'   ),
#'   raster_format = "stars"
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
layer_spatial.stars <- function(data, mapping = NULL, interpolate = NULL, is_annotation = FALSE,
                                lazy = FALSE, dpi = 150, options = character(0), ...) {

  is_rgb <- is.null(mapping) && (dim(data)["band"] %in% c(3, 4))
  if (is_rgb) {
    # RGB(A)
    if (is_annotation) {
      stat <- StatSpatialStarsAnnotation
    } else {
      stat <- StatSpatialStars
    }

    geom <- GeomSpatialStars
    mapping <- ggplot2::aes_string(raster = "raster")
  } else {
    # mapped aesthetics mode
    if (is_annotation) {
      stop("Non-RGBA rasters with is_annotation = TRUE is not implemented.")
    } else {
      stat <- StatSpatialStarsDf
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
      inherit.aes = FALSE, show.legend = if (is_rgb) FALSE else NA,
      params = list(
        interpolate = if (!is.null(interpolate)) interpolate else is_rgb,
        lazy = lazy,
        dpi = dpi,
        options = options,
        ...
      )
    ),
    # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
    # using the first layer's CRS as the base CRS for coord_sf().
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(),
                        crs = sf::st_crs(data)
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @rdname layer_spatial.stars
#' @export
annotation_spatial.stars <- function(data, mapping = NULL, interpolate = NULL, ...) {
  layer_spatial.stars(data, mapping = mapping, interpolate = interpolate, is_annotation = TRUE, ...)
}

#' @rdname layer_spatial.stars
#' @export
# nocov start
StatSpatialStars <- ggplot2::ggproto(
  "StatSpatialStars",
  ggplot2::Stat,
  required_aes = "raster",
  compute_layer = function(self, data, params, layout) {


    # raster extents
    extents <- lapply(data$raster, sf::st_bbox)

    # project to target coordinate system, if one exists
    coord_crs <- layout$coord_params$crs
    if (!is.null(coord_crs)) {
      data$extent <- lapply(seq_along(extents), function(i) {
        x <- extents[[i]]

        project_extent(
          xmin = x["xmin"],
          xmax = x["xmax"],
          ymin = x["ymin"],
          ymax = x["ymax"],
          from_crs = sf::st_crs(data$raster[[i]]),
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

#' @rdname layer_spatial.stars
#' @export
# nocov start
StatSpatialStarsAnnotation <- ggplot2::ggproto(
  "StatSpatialStars",
  StatSpatialStars,
  required_aes = "raster",
  compute_layer = function(self, data, params, layout) {
    data <- ggplot2::ggproto_parent(
      self,
      StatSpatialStars
    )$compute_layer(data, params, layout)
    data$xmin <- NULL
    data$xmax <- NULL
    data$ymin <- NULL
    data$ymax <- NULL

    data
  }
)
# nocov end
#' @rdname layer_spatial.stars
#' @export
# nocov start
StatSpatialStarsDf <- ggplot2::ggproto(
  "StatSpatialStarsDf",
  ggplot2::Stat,
  required_aes = "raster",
  extra_params = c("lazy", "dpi", "options"),
  default_aes = ggplot2::aes(fill = stat(band1)),
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
          project_stars_lazy,
          crs = sf::st_crs(coord_crs)
        )
      } else {
        warning("Spatial rasters may not be displayed correctly. Use coord_sf().", call. = FALSE)
      }

      data$raster <- lapply(data$raster, df_spatial)
      tidyr::unnest(data, .data$raster)
    } else {
      data
    }
  }
)
# nocov end

#' @rdname layer_spatial.stars
#' @export
# nocov start
GeomSpatialStars <- ggplot2::ggproto(
  "GeomSpatialStars",
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
                        max_pixel_scale = 1,
                        options = character(0)) {
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
        options = options,
        cache = new.env(parent = emptyenv()),
        cl = "geom_spatial_stars_lazy"
      )
    } else {
      raster_grob_from_stars(
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
makeContent.geom_spatial_stars_lazy <- function(x) {
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

    # Create template from x$target_bbox
    bbox <- x$target_bbox
    sf::st_crs(bbox) <- sf::st_crs(x$coord_crs)
    template_raster <- stars::st_as_stars(
      bbox,
      # Adjust ncells to dpi
      # Needs to be integer
      nx = round(width_px, 0),
      ny = round(height_px, 0)
    )
  } else {
    template_raster <- NULL
  }

  # add content to the gTree, cache the rasterGrob
  x$cache[[raster_var]] <- raster_grob_from_stars(
    x$raster_obj,
    x$coord_crs,
    x$coordinates,
    x$panel_params,
    alpha = x$alpha,
    interpolate = x$interpolate,
    template_raster = template_raster,
    options = x$options
  )

  grid::setChildren(x, grid::gList(x$cache[[raster_var]]))
}

raster_grob_from_stars <- function(rst, coord_crs, coordinates, panel_params,
                                   template_raster = NULL, alpha = 1,
                                   interpolate = TRUE,
                                   options = character(0)) {
  if (interpolate) {
    raster_method <- "bilinear"
  } else {
    raster_method <- "near"
  }
  if (!is.null(template_raster) && !is.null(coord_crs)) {
    rst <- stars::st_warp(
      rst,
      template_raster,
      use_gdal = TRUE,
      method = raster_method,
      options = options
    )
  } else if (!is.null(coord_crs)) {
    # project
    rst <- project_stars_lazy(
      rst,
      crs = sf::st_crs(coord_crs)
    )
  } else if (!is.null(template_raster)) {
    # resample (& crop?)
    rst <- stars::st_warp(rst,
                          template_raster,
                          use_gdal = TRUE,
                          method = raster_method,
                          options = options
    )
  }

  ext <- sf::st_bbox(rst)
  corners <- data.frame(x = c(ext["xmin"], ext["xmax"]), y = c(ext["ymin"], ext["ymax"]))
  corners_trans <- coordinates$transform(corners, panel_params)

  x_rng <- range(corners_trans$x, na.rm = TRUE)
  y_rng <- range(corners_trans$y, na.rm = TRUE)

  grid::rasterGrob(
    stars_as_array(rst, alpha = alpha),
    x_rng[1], y_rng[1],
    diff(x_rng), diff(y_rng),
    default.units = "native",
    just = c("left", "bottom"), interpolate = interpolate
  )
}

stars_as_array <- function(raster_obj, na.value = NA, alpha = 1) {
  if (!methods::is(raster_obj, "stars")) stop("Cannot use stars_as_array with non stars object")

  raster <- stars::st_as_stars(raster_obj)[[1]]

  # check dims
  dims <- dim(raster)
  if (length(dims) != 3) stop("Raster has incorrect dimensions: ", paste(dims, collapse = ", "))
  if (!(dims[3] %in% c(3, 4))) stop("Need a 3 or 4-band array to use stars_as_array")

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
  # stars flips the X and Y dimensions from what grid expects
  aperm(raster, c(2, 1, 3))
}

project_stars_lazy <- function(x, crs, ...) {
  if (!sf::st_crs(crs) == sf::st_crs(x)) {
    stars::st_warp(x,
                   crs = sf::st_crs(crs),
                   ...
    )
  } else {
    x
  }
}
