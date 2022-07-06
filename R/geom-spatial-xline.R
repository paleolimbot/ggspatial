
#' Projected horizontal and vertical lines
#'
#' @inheritParams stat_spatial_identity
#' @param intercept The x or y value that should be constant in the given
#'   `crs`. Can also be passed as an aesthetic through `data` and `mapping`.
#' @param limits Use `NULL` to guess the minimum and maximum x or y value in
#'   the non-constant dimension, or specify a vector of length 2 to specify
#'   manually.
#' @param detail The number of points that should be used when converting the
#'   line into segments.
#' @param stat Statistical transformation to use on this layer. See [ggplot2::layer()].
#' @param show.legend Should the legend be shown?
#' @param na.rm Should missing aesthetic values be removed?
#'
#' @export
#' @examples
#' cities <- data.frame(
#'   x = c(-63.58595, 116.41214, 0),
#'   y = c(44.64862, 40.19063, 89.9),
#'   city = c("Halifax", "Beijing", "North Pole")
#' )
#'
#' p <- ggplot(cities, aes(x, y, label = city)) +
#'   geom_spatial_point(crs = 4326) +
#'   # view of the north pole
#'   coord_sf(crs = 3995)
#'
#' p +
#'   # longitude lines
#'   annotation_spatial_vline(
#'     intercept = seq(-180, 180, by = 10),
#'     crs = 4326
#'   ) +
#'   # latitude lines
#'   annotation_spatial_hline(
#'     intercept = seq(0, 90, by = 10),
#'     crs = 4326
#'   )
#'
annotation_spatial_hline <- function(mapping = NULL, data = NULL,
                                     stat = "identity",
                                     ...,
                                     intercept = waiver(),
                                     limits = NULL,
                                     detail = 100,
                                     crs = NULL,
                                     na.rm = FALSE,
                                     show.legend = NA) {
  annotation_spatial_xline(
    mapping = mapping,
    data = data,
    stat = stat,
    ...,
    intercept = intercept,
    direction = "h",
    limits = limits,
    detail = detail,
    crs = crs,
    na.rm = na.rm,
    show.legend = show.legend
  )
}

#' @rdname annotation_spatial_hline
#' @export
annotation_spatial_vline <- function(mapping = NULL, data = NULL,
                                     stat = "identity",
                                     ...,
                                     intercept = waiver(),
                                     limits = NULL,
                                     detail = 100,
                                     crs = NULL,
                                     na.rm = FALSE,
                                     show.legend = NA) {
  annotation_spatial_xline(
    mapping = mapping,
    data = data,
    stat = stat,
    ...,
    intercept = intercept,
    direction = "v",
    limits = limits,
    crs = crs,
    detail = detail,
    na.rm = na.rm,
    show.legend = show.legend
  )
}

annotation_spatial_xline <- function(mapping = NULL, data = NULL,
                                     stat = "identity",
                                     ...,
                                     intercept = waiver(),
                                     direction = c("h", "v"),
                                     limits = NULL,
                                     detail = 100,
                                     crs = NULL,
                                     na.rm = FALSE,
                                     show.legend = NA) {

  direction <- match.arg(direction)
  stopifnot(
    is.null(limits) || is.numeric(limits)
  )

  if (!is.null(crs)) {
    crs <- sf::st_crs(crs)
  }

  if (!inherits(intercept, "waiver")) {
    if (!is.null(mapping)) {
      warn_overwritten_args("geom_spatial_hline()", "mapping", "intercept")
    }
    if (!is.null(data)) {
      warn_overwritten_args("geom_spatial_hline()", "data", "intercept")
    }

    data <- tibble::tibble(intercept = intercept)
    mapping <- aes(intercept = intercept)
    show.legend <- FALSE
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSpatialXline,
    position = ggplot2::PositionIdentity,
    show.legend = show.legend,
    inherit.aes = FALSE,
    params = list(
      na.rm = na.rm,
      limits = limits,
      direction = direction,
      detail = detail,
      crs = crs,
      ...
    )
  )
}

#' @rdname annotation_spatial_hline
#' @export
GeomSpatialXline <- ggplot2::ggproto(
  "GeomSpatialXline", ggplot2::GeomHline,
  draw_panel = function(data, panel_params, coord,
                        detail = 100, direction = "h", limits = NULL, crs = NULL) {
    if (is.null(crs)) {
      message("Assuming `crs = 4326` in annotation_spatial_(h|v)line()")
      crs <- sf::st_crs(4326)
    }

    if (is.null(panel_params$crs) || identical(sf::st_crs(panel_params$crs), crs)) {

      if (is.null(panel_params$crs)) {
        warning(
          glue::glue(
            "
            Ignoring transformation in annotation_spatial_(h|v)line().
            Use `coord_sf()` with a crs to project this layer."
          ),
          call. = FALSE
        )
      }

      if (direction == "h") {
        data$yintercept <- data$intercept
        suppressWarnings(ggplot2::GeomHline$draw_panel(data, panel_params, coord))
      } else if (direction == "v") {
        data$xintercept <- data$intercept
        suppressWarnings(ggplot2::GeomVline$draw_panel(data, panel_params, coord))
      } else {
        stop(glue::glue("Unrecognized direction: '{direction}'")) # nocov
      }
    } else {
      bbox <- panel_params_as_bbox(panel_params)

      if (is.null(limits)) {
        projected_bbox <- project_extent(
          bbox["xmin"], bbox["ymin"],
          bbox["xmax"], bbox["ymax"],
          from_crs = panel_params$crs,
          to_crs = crs,
          n = 50
        )

        if (direction == "h") {
          limits <- projected_bbox[c("xmin", "xmax")]
        } else if(direction == "v") {
          limits <- projected_bbox[c("ymin", "ymax")]
        } else {
          stop(glue::glue("Unrecognized direction: '{direction}'")) # nocov
        }

        # expand limits slightly, since it's unlikely they were
        # guessed exactly at the edges
        limits <- scales::expand_range(limits, mul = 0.05)

        # if crs is lat/lon (most common) and direction == "h",
        # constraining limits to [-180, 180] makes the ends
        # intersect better (e.g., in a polar projection)
        if (identical(crs, sf::st_crs(4326)) && direction == "h") {
          limits[1] <- max(c(-180, limits[1]))
          limits[2] <- min(c(180, limits[2]))
        }
      } else {
        limits <- range(limits)
        if (!all(is.finite(limits))) {
          stop("Non-finite `limits` in annotation_spatial_(h|v)line()")
        }
      }

      data$geometry <- do.call(
        sf::st_sfc,
        c(
          lapply(data$intercept,
            geometry_xline,
            limits = limits,
            bbox = bbox,
            direction = direction,
            crs_source = crs,
            crs_dest = panel_params$crs,
            detail = detail
          ),
          list(crs = panel_params$crs)
        )
      )

      # don't include empty items
      data <- data[!sf::st_is_empty(data$geometry), , drop = FALSE]

      if (nrow(data) == 0) {
        ggplot2::zeroGrob()
      } else {
        # use GeomSf to draw the panel
        ggplot2::GeomSf$draw_panel(
          data, panel_params, coord,
          lineend = "butt",
          linejoin = "round",
          linemitre = 10
        )
      }
    }
  },
  draw_key = ggplot2::draw_key_path,
  required_aes = "intercept"
)

geometry_xline <- function(intercept, limits, bbox, direction, crs_source, crs_dest, detail) {
  coords_variable <- seq(limits[1], limits[2], length.out = detail)
  coords_constant <- rep_len(intercept, detail)
  if (direction == "h") {
    coords <- matrix(c(coords_variable, coords_constant), byrow = FALSE, ncol = 2)
  } else if (direction == "v") {
    coords <- matrix(c(coords_constant, coords_variable), byrow = FALSE, ncol = 2)
  } else {
    stop(glue::glue("Unrecognized direction: '{direction}'")) # nocov
  }

  geom <- sf::st_sfc(sf::st_linestring(coords), crs = crs_source)
  geom <- sf::st_transform(geom, crs = crs_dest)
  if (!is.null(bbox)) {
    geom <- suppressWarnings(sf::st_intersection(geom, sf_bbox_to_sf(bbox)))
  }

  if (length(geom) > 0) {
    geom[[1]]
  } else {
    sf::st_linestring()
  }
}

warn_overwritten_args <- function(fun_name, overwritten_arg, provided_args, plural_join = " and/or ") {
  overwritten_arg_text <- paste0("`", overwritten_arg, "`")

  n_provided_args <- length(provided_args)
  if (n_provided_args == 1) {
    provided_arg_text <- paste0("`", provided_args, "`")
    verb <- "was"
  } else if (n_provided_args == 2) {
    provided_arg_text <- paste0("`", provided_args, "`", collapse = plural_join)
    verb <- "were"
  } else {
    provided_arg_text <- paste0(
      paste0("`", provided_args[-n_provided_args], "`", collapse = ", "),
      ",", plural_join,
      "`", provided_args[n_provided_args], "`"
    )
    verb <- "were"
  }

  warning(
    glue::glue(
      "{fun_name}: Ignoring {overwritten_arg_text} because {provided_arg_text} {verb} provided."
    ),
    call. = FALSE
  )
}
