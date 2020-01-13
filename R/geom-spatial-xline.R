
#' Projected horizontal and vertical lines
#'
#' @param intercept The x or y value that should be constant in the given
#'   `crs`. Can also be passed as an aesthetic through `data` and `mapping`.
#' @param limits Use `NULL` to guess the minimum and maximum x or y value in
#'   the non-constant dimension, or specify a vector of length 2 to specify
#'   manually.
#' @param detail The number of points that should be used when converting the
#'   line into segments.
#'
#' @export
annotation_spatial_hline <- function(mapping = NULL, data = NULL,
                                     stat = "identity",
                                     ...,
                                     intercept = waiver(),
                                     limits = NULL,
                                     detail = 100,
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
                                     na.rm = FALSE,
                                     show.legend = NA) {

  direction <- match.arg(direction)
  stopifnot(is.null(limits) || is.numeric(limits))

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
      ...
    )
  )
}

#' @rdname annotation_spatial_hline
#' @export
GeomSpatialXline <- ggplot2::ggproto(
  "GeomSpatialXline", ggplot2::GeomHline,
  draw_panel = function(data, panel_params, coord,
                        detail = 100, direction = "h", limits = NULL) {
    if(is.null(panel_params$crs)) {
      warning(
        glue::glue(
          "
          Ignoring transformation in annotation_spatial_(h|v)line().
          Use coord_sf() with a crs to project this layer."
        ),
        call. = FALSE
      )

      if (direction == "h") {
        data$yintercept <- data$intercept
        ggplot2::GeomHline$draw_panel(data, panel_params, coord)
      } else if (direction == "v") {
        data$xintercept <- data$intercept
        ggplot2::GeomVline$draw_panel(data, panel_params, coord)
      } else {
        stop(glue::glue("Unrecognized direction: '{direction}'"))
      }
    } else {
      stop("Not implemented")
    }
  },
  draw_key = draw_key_path,
  required_aes = "intercept"
)

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
