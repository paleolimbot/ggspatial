
#' Spatial ggplot2 layer for raster objects
#'
#' This is intended for use with RGB(A) rasters (e.g., georeferenced imagery or photos). To work with
#' bands as if they were columns, use [df_spatial] and [geom_raster].
#'
#' @param data A Raster object
#' @param mapping Currently, only RGB or RGBA rasters are supported. In the future, one may be able to
#'   map specific bands to the fill and alpha aesthetics.
#' @param interpolate Interpolate resampling for rendered raster image
#' @param is_annotation Lets raster exist without modifying scales
#' @param lazy Delay projection and resample of raster until the plot is being rendered
#' @param dpi if lazy = TRUE, the dpi to which the raster should be resampled
#' @param ... Passed to other methods
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' load_longlake_data(which = c("longlake_osm", "longlake_depth_raster"))
#' ggplot() + layer_spatial(longlake_osm)
#' ggplot() + layer_spatial(longlake_depth_raster) + scale_fill_continuous(na.value = NA)
#' }
#'
layer_spatial.Raster <- function(data, mapping = NULL, interpolate = NULL, is_annotation = FALSE,
                                 lazy = FALSE, dpi = 150, ...) {


  is_rgb <- is.null(mapping) && (raster::nlayers(data) %in% c(3, 4))
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
      data = sf::st_sfc(sf::st_point(), crs = sf::st_crs(data@crs@projargs)),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @rdname layer_spatial.Raster
#' @export
annotation_spatial.Raster <- function(data, mapping = NULL, interpolate = NULL, ...) {
  layer_spatial.Raster(data, mapping = mapping, interpolate = interpolate, is_annotation = TRUE, ...)
}

#' @rdname layer_spatial.Raster
#' @export
StatSpatialRaster <-  ggplot2::ggproto(
  "StatSpatialRaster",
  ggplot2::Stat,
  required_aes = "raster",

  compute_layer = function(self, data, params, layout) {

    # raster extents
    extents <- lapply(data$raster, raster::extent)

    # project to target coordinate system, if one exists
    coord_crs <- layout$coord_params$crs
    if(!is.null(coord_crs)) {

      data$extent <- lapply(seq_along(extents), function(i) {
        x <- extents[[i]]
        project_extent(
          xmin = x@xmin,
          xmax = x@xmax,
          ymin = x@ymin,
          ymax = x@ymax,
          from_crs = sf::st_crs(raster::crs(data$raster[[i]])@projargs),
          to_crs = sf::st_crs(coord_crs)
        )
      })

      # this needs to be directly in the data so that the position scales get trained
      data$xmin <- vapply(data$extent, function(x) x["xmin"], numeric(1))
      data$xmax <- vapply(data$extent, function(x) x["xmax"], numeric(1))
      data$ymin <- vapply(data$extent, function(x) x["ymin"], numeric(1))
      data$ymax <- vapply(data$extent, function(x) x["ymax"], numeric(1))

      # this stat also generates band1....band[n] columns with the limits of each band
      # this allows aesthetics in the form fill = stat(band1), alpha = stat(band3)

      # in many cases this is cached or at the very least doesn't lead to the whole
      # raster being read into memory

      # the raster package doesn't have support for discrete values in bands (?),
      # so there are no discrete limits to worry about here (?)
      # if there were, crossing() would probably be the way to go

      # commenting this out until access to the scales is possible from the Geom

      # data$band_limits <- lapply(data$raster, function(rst) {
      #   min_values <- as.list(raster::minValue(rst))
      #   names(min_values) <- paste0("band", seq_along(min_values))
      #   max_values <- as.list(raster::maxValue(rst))
      #   names(max_values) <- paste0("band", seq_along(max_values))
      #   rbind(
      #     tibble::as_tibble(min_values),
      #     tibble::as_tibble(max_values)
      #   )
      # })
      #
      # data <- tidyr::unnest(data, .data$band_limits, .drop = FALSE)

    } else {
      stop("Spatial rasters require coord_sf().", call. = FALSE)
    }

    data
 }
)

#' @rdname layer_spatial.Raster
#' @export
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

#' @rdname layer_spatial.Raster
#' @export
StatSpatialRasterDf <- ggplot2::ggproto(
  "StatSpatialRasterDf",
  ggplot2::Stat,
  required_aes = "raster",
  extra_params = c("lazy", "dpi"),

  default_aes = ggplot2::aes(fill = stat(band1)),

  compute_layer = function(self, data, params, layout) {

    if(params$lazy) stop("Lazy rendering not implemented for mapped rasters")

    # project to target coordinate system
    # make all rasters data frames using df_spatial, if column still exists
    # (because this method gets called multiple times)

    if("raster" %in% colnames(data)) {

      coord_crs <- layout$coord_params$crs
      if(!is.null(coord_crs)) {
        data$raster <- lapply(
          data$raster,
          project_raster_lazy,
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

#' @rdname layer_spatial.Raster
#' @export
GeomSpatialRaster <- ggplot2::ggproto(
  "GeomSpatialRaster",
  ggplot2::Geom,

  required_aesthetics = c("raster", "extent"),

  default_aes = ggplot2::aes(alpha = 1),

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coordinates, interpolate = FALSE, lazy = FALSE, dpi = 150,
                        max_pixel_scale = 1) {
    rst <- data$raster[[1]]
    coord_crs <- panel_params$crs
    alpha <- data$alpha[1]

    if(lazy) {

      # this code makes sure that the resampled raster doesn't contain any more area than it needs to
      coord_bbox <- panel_params_as_bbox(panel_params)
      rst_bbox <- data$extent[[1]]
      if(bboxes_equal(coord_bbox, rst_bbox)) {
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
        cl = "geom_spatial_raster_lazy"
      )

    } else {
      raster_grob_from_raster(
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

#' @importFrom grid makeContent
#' @export
makeContent.geom_spatial_raster_lazy <- function(x) {

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
  if(raster_var %in% names(x$cache)) {
    return(grid::setChildren(x, grid::gList(x$cache[[raster_var]])))
  }

  if(!is.null(x$coord_crs)) {
    template_raster <- raster::raster(
      xmn = x$target_bbox["xmin"],
      ymn = x$target_bbox["ymin"],
      xmx = x$target_bbox["xmax"],
      ymx = x$target_bbox["ymax"],
      ncols = width_px,
      nrows = height_px,
      crs = raster::crs(sf::st_crs(x$coord_crs)$proj4string)
    )

  } else {
    template_raster <- NULL
  }

  # add content to the gTree, cache the rasterGrob
  x$cache[[raster_var]] <- raster_grob_from_raster(
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

raster_grob_from_raster <- function(rst, coord_crs, coordinates, panel_params,
                                    template_raster = NULL, alpha = 1, interpolate = TRUE) {
  if(interpolate) {
    raster_method <- "bilinear"
  } else {
    raster_method <- "ngb"
  }

  if(!is.null(template_raster) && !is.null(coord_crs)) {
    # project + resample
    # raster::projectRaster has very odd behaviour...it outputs the warning
    # "no non-missing arguments to max; returning -Inf"
    # but only when run with a calling handler
    rst <- suppressWarnings(
        raster::projectRaster(
          rst,
          to = template_raster,
          method = raster_method
      )
    )
  } else if(!is.null(coord_crs)) {
    # project
    # raster::projectRaster has very odd behaviour...it outputs the warning
    # "no non-missing arguments to max; returning -Inf"
    # but only when run with a calling handler
    rst <- project_raster_lazy(
      rst,
      crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
    )
  } else if(!is.null(template_raster)) {
    # resample (& crop?)
    rst <- raster::resample(rst, template_raster, method = raster_method)
  }

  ext <- raster::extent(rst)
  corners <- data.frame(x = c(ext@xmin, ext@xmax), y = c(ext@ymin, ext@ymax))
  corners_trans <- coordinates$transform(corners, panel_params)

  x_rng <- range(corners_trans$x, na.rm = TRUE)
  y_rng <- range(corners_trans$y, na.rm = TRUE)

  grid::rasterGrob(
    raster_as_array(rst, alpha = alpha),
    x_rng[1], y_rng[1],
    diff(x_rng), diff(y_rng), default.units = "native",
    just = c("left","bottom"), interpolate = interpolate
  )
}

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

project_extent <- function(xmin, ymin, xmax, ymax,
                           from_crs = 4326, to_crs = 4326, format = c("sf", "sp"),
                           n = 50) {
  format <- match.arg(format)

  proj_grid_poly <- sf_bbox_to_sf(
    sf::st_bbox(
      stats::setNames(
        c(xmin, ymin, xmax, ymax),
        c("xmin", "ymin", "xmax", "ymax")
      ),
      crs = from_crs
    ),
    detail = NULL
  )
  proj_grid <- sf::st_make_grid(proj_grid_poly, n = n, what = "corners")
  out_grid <- sf::st_transform(proj_grid, crs = to_crs)
  out_bbox <- sf::st_bbox(out_grid)

  if(format == "sp") {
    prettymapr::makebbox(
      n = out_bbox["ymax"],
      e = out_bbox["xmax"],
      s = out_bbox["ymin"],
      w = out_bbox["xmin"]
    )
  } else {
    out_bbox
  }
}

bboxes_equal <- function(bbox1, bbox2) {
  all(bbox1 == bbox2)
}

panel_params_as_bbox <- function(panel_params) {
  data <- c(
    xmin = panel_params$x_range[1],
    ymin = panel_params$y_range[1],
    xmax = panel_params$x_range[2],
    ymax = panel_params$y_range[2]
  )

  structure(data, crs = sf::st_crs(panel_params$crs), class = "bbox")
}

project_raster_lazy <- function(x, crs, ...) {
  if (sf::st_crs(crs) != sf::st_crs(x)) {
    suppressWarnings(raster::projectRaster(x, crs = crs, ...))
  } else {
    x
  }
}

# need a way to get an extent to an sf::st_polygon
bbox_as_polygon <- function(bbox) {
  sf::st_polygon(list(matrix(
    c(bbox["xmin"], bbox["ymin"],
      bbox["xmax"], bbox["ymin"],
      bbox["xmax"], bbox["ymax"],
      bbox["xmin"], bbox["ymax"],
      bbox["xmin"], bbox["ymin"]),
    ncol = 2,
    byrow = TRUE
  )), dim = "XY")
}

# also need a method to combine aesthetics with overriding values
override_aesthetics <- function(user_mapping = NULL, default_mapping = NULL) {
  if(is.null(user_mapping) && is.null(default_mapping)) {
    ggplot2::aes()
  } else if(is.null(default_mapping)) {
    user_mapping
  } else if(is.null(user_mapping)) {
    default_mapping
  } else {
    all_aes_names <- unique(c(names(user_mapping), names(default_mapping)))
    new_aes <- c(user_mapping, default_mapping)[all_aes_names]
    class(new_aes) <- "uneval"
    new_aes
  }
}
