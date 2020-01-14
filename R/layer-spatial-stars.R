
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
#'
layer_spatial.stars <- function(data, mapping = NULL, interpolate = TRUE, is_annotation = FALSE,
                                lazy = FALSE, dpi = 150, options = character(0), ...) {

  dims <- stars::st_dimensions(data)
  is_rgb <- ("band" %in% names(dims)) && (dims$band$to %in% c(3, 4))
  if(is_rgb) {
    # RGB(A)
    if(is_annotation) {
      stat <- StatSpatialStarsAnnotation
    } else {
      stat <- StatSpatialStars
    }

    geom <- GeomSpatialStars
    mapping <- ggplot2::aes_string(raster = "raster")
  } else {
    # mapped aesthetics mode
    if(is_annotation) {
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
      inherit.aes = FALSE, show.legend = !is_rgb,
      params = list(interpolate = interpolate, lazy = lazy, options = options, ...)
    ),
    # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
    # using the first layer's CRS as the base CRS for coord_sf().
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(), crs = sf::st_crs(data)),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @rdname layer_spatial.stars
#' @export
annotation_spatial.stars <- function(data, mapping = NULL, interpolate = TRUE, ...) {
  layer_spatial.stars(data, mapping = mapping, interpolate = interpolate, is_annotation = TRUE, ...)
}

#' @rdname layer_spatial.stars
#' @export
StatSpatialStars <-  ggplot2::ggproto(
  "StatSpatialStars",
  ggplot2::Stat,
  required_aes = "raster",

  compute_layer = function(self, data, params, layout) {

    # raster extents
    extents <- lapply(data$raster, sf::st_bbox)

    # project to target coordinate system, if one exists
    coord_crs <- layout$coord_params$crs
    if(!is.null(coord_crs)) {

      projected_bboxes <- lapply(seq_along(extents), function(i) {
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

      data$xmin <- vapply(projected_bboxes, function(x) x["xmin"], numeric(1))
      data$xmax <- vapply(projected_bboxes, function(x) x["xmax"], numeric(1))
      data$ymin <- vapply(projected_bboxes, function(x) x["ymin"], numeric(1))
      data$ymax <- vapply(projected_bboxes, function(x) x["ymax"], numeric(1))

    } else {
      warning("Spatial rasters may not be displayed correctly. Use coord_sf().", call. = FALSE)

      data$xmin <- vapply(extents, function(x) x["xmin"], numeric(1))
      data$xmax <- vapply(extents, function(x) x["xmax"], numeric(1))
      data$ymin <- vapply(extents, function(x) x["ymin"], numeric(1))
      data$ymax <- vapply(extents, function(x) x["ymax"], numeric(1))
    }

    data
  }
)

#' @rdname layer_spatial.stars
#' @export
StatSpatialStarsAnnotation <- ggplot2::ggproto(
  "StatSpatialStars",
  StatSpatialStars,
  required_aes = "raster",

  compute_layer = function(self, data, params, layout) {
    data <- ggplot2::ggproto_parent(self, StatSpatialStars)$compute_layer(data, params, layout)
    data$xmin <- NULL
    data$xmax <- NULL
    data$ymin <- NULL
    data$ymax <- NULL

    data
  }
)

#' @rdname layer_spatial.stars
#' @export
StatSpatialStarsDf <- ggplot2::ggproto(
  "StatSpatialStarsDf",
  ggplot2::Stat,
  required_aes = "raster",
  extra_params = "lazy",

  default_aes = ggplot2::aes(fill = stat(band1)),

  compute_layer = function(self, data, params, layout) {

    if(params$lazy) stop("Lazy rendering not implemented for mapped rasters")

    # project to target coordinate system
    # make all rasters data frames using df_spatial, if column still exists
    # (because this method gets called multiple times)

    if("raster" %in% colnames(data)) {

      coord_crs <- layout$coord_params$crs
      # FIXME this is a temporary fix until I can figure out how to do some of this using stars
      # basically, we need to return a representative subsample of the df version, which may
      # include some kind of mutate operation?
      # the temp fix bungles the NODATA values?

      # something like this would help
      # withr::with_output_sink("message.txt", {
      #   sf::gdal_utils("info", "longlake.tif", options = c("-approx_stats", "-json"))
      # })
      #
      data$raster <- lapply(data$raster, methods::as, "Raster")

      if(!is.null(coord_crs)) {
        data$raster <- lapply(
          data$raster,
          function(...) suppressWarnings(raster::projectRaster(...)),
          crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
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

#' @rdname layer_spatial.stars
#' @export
GeomSpatialStars <- ggplot2::ggproto(
  "GeomSpatialStars",
  ggplot2::Geom,

  required_aesthetics = c("raster", "extent"),

  default_aes = ggplot2::aes(alpha = 1),

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coordinates, interpolate = TRUE,
                        options = character(0), lazy = FALSE, dpi = 150) {
    rst <- data$raster[[1]]
    coord_crs <- panel_params$crs
    alpha <- data$alpha[1]

    if(lazy) {
      # return a gTree with the right params so that the raster can be rendered later
      grid::gTree(
        raster_obj = rst,
        coord_crs = coord_crs,
        coordinates = coordinates,
        panel_params = panel_params,
        alpha = alpha,
        interpolate = interpolate,
        dpi = dpi,
        cl = "geom_spatial_raster_lazy"
      )

    } else {
      raster_grob_from_stars(
        rst,
        coord_crs,
        coordinates,
        panel_params,
        alpha = alpha,
        options = options,
        interpolate = interpolate
      )
    }
  }
)

#' @importFrom grid makeContent
#' @export
makeContent.geom_spatial_stars_lazy <- function(x) {
  # FIXME this isn't implemented yet

  width_px <- grid::convertWidth(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  height_px <- grid::convertHeight(grid::unit(1, "npc"), "pt", valueOnly = TRUE)
  # no idea how to get DPI here...grDevices::dev.size() returns
  # incorrect pixel dimensions I think...
  message(sprintf("(%s x %s px) dpi: ??", width_px, height_px))

  if(identical(width_px, x$width_px) && identical(height_px, x$height_px) && !is.null(x$grob)) {
    grob <- x$grob
  } else {
    if(!is.null(x$coord_crs)) {

      template_stars <- stars::st_as_stars(
        dimensions = stars::st_dimensions(
          x = x$panel_params$x_range,
          y = x$panel_params$y_range
        )
      )

      sf::st_crs(template_stars) <- sf::st_crs(x$coord_crs)

    } else {
      template_raster <- NULL
    }

    grob <- grid::gList(
      raster_grob_from_raster(
        x$raster_obj,
        x$coord_crs,
        x$coordinates,
        x$panel_params,
        alpha = x$alpha,
        interpolate = x$interpolate,
        template_raster = template_raster
      )
    )
  }

  # add content to the gTree
  grid::setChildren(x, grob)
}

raster_grob_from_stars <- function(rst, coord_crs, coordinates, panel_params, template_raster = NULL, alpha = 1,
                                   options = character(0), interpolate = TRUE, height_px = 500, width_px = 500) {

  if(is.null(coord_crs)) {
    coord_crs <- sf::st_crs(rst)
  }

  # this is required for stars::st_warp
  if(is.null(template_raster)) {
    new_dim <- dim(rst)
    new_dim[1] <- width_px
    new_dim[2] <- height_px

    template_raster <- stars::st_as_stars(
      dimensions = stars::st_dimensions(
        x = seq(panel_params$x_range[1], panel_params$x_range[2], length.out = width_px),
        y = seq(panel_params$y_range[1], panel_params$y_range[2], length.out = height_px),
        band = seq_len(new_dim[3])
      )
    )

    sf::st_crs(template_raster) <- sf::st_crs(coord_crs)
  }

  rst <- stars::st_warp(rst, dest = template_raster, options = options)

  ext <- sf::st_bbox(rst)
  corners <- data.frame(x = c(ext["xmin"], ext["xmax"]), y = c(ext["ymin"], ext["ymax"]))
  corners_trans <- coordinates$transform(corners, panel_params)

  x_rng <- range(corners_trans$x, na.rm = TRUE)
  y_rng <- range(corners_trans$y, na.rm = TRUE)

  grid::rasterGrob(
    stars_as_array(rst, alpha = alpha),
    x_rng[1], y_rng[1],
    diff(x_rng), diff(y_rng), default.units = "native",
    just = c("left","bottom"), interpolate = interpolate
  )
}

stars_as_array <- function(raster_obj, na.value = NA, alpha = 1) {
  if(!inherits(raster_obj, "stars")) stop("Cannot use stars_as_array with non-stars object")

  raster <- stars::st_as_stars(raster_obj)[[1]]

  # check dims
  dims <- dim(raster)
  if(length(dims) != 3) stop("Raster has incorrect dimensions: ", paste(dims, collapse = ", "))
  if(!(dims[3] %in% c(3, 4))) stop("Need a 3 or 4-band array to use stars_as_array")

  # Inf values should trigger an error says @edzer

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

  # stars flips the X and Y dimensions from what grid expects
  # and Y values are in the reverse order?
  aperm(raster, c(2, 1, 3))[(dim(raster)[2]):1, , , drop = FALSE]
}
