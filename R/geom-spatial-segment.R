
#' Spatial line segments
#'
#' While the implementation is slightly differrent, this function is
#' intended to behave identically to [ggplot2::geom_segment()]. Use
#' `great_circle = FALSE` and `detail = NULL` if you wish ignore the fact
#' that the earth is round.
#'
#' @inheritParams stat_spatial_identity
#' @inheritParams layer_spatial.bbox
#' @inheritParams annotation_spatial_hline
#' @inheritParams geom_spatial_rect
#' @param great_circle If `TRUE`, use [lwgeom::st_geod_segmentize()]
#'   to connect the (x, y) and (xend, yend) with the shortest possible
#'   great circle along the earth.
#' @param wrap_dateline When using `great_circle = TRUE`, using
#'   `wrap_dateline = TRUE` splits the great circle along the dateline.
#'   You may want to pass `FALSE` here if using `arrow` and a projection
#'   that wraps the dateline.
#' @param arrow An arrow specification as a call to [grid::arrow()].
#' @param lineend See [ggplot2::geom_segment()].
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' # visualize flights from
#' # Halifax -> Anchorage -> Berlin -> Halifax
#' cities <- data.frame(
#'   lon = c(-63.58595, 116.41214, 13.50, -149.75),
#'   lat = c(44.64862, 40.19063, 52.51, 61.20),
#'   city = c("Halifax", "Beijing", "Berlin", "Anchorage"),
#'   city_to = c("Anchorage", "Beijing", "Berlin", "Halifax")
#' )
#'
#' cities$lon_end <- cities$lon[c(4, 3, 1, 2)]
#' cities$lat_end <- cities$lat[c(4, 3, 1, 2)]
#'
#' p <- ggplot(cities, aes(lon, lat, xend = lon_end, yend = lat_end)) +
#'   geom_spatial_point(crs = 4326)
#'
#' # by default, geom_spatial_segment() connects points
#' # using the shortest distance along the face of the earth
#' # wrapping at the date line
#' p +
#'   geom_spatial_segment(crs = 4326) +
#'   coord_sf(crs = 3857)
#'
#' # to let the projection handle the dateline,
#' # use `wrap_dateline = FALSE` (most useful for
#' # when using `arrow`)
#' p +
#'   geom_spatial_segment(
#'     wrap_dateline = FALSE,
#'     arrow = grid::arrow(),
#'     crs = 4326
#'   ) +
#'   coord_sf(crs = 3995)
#'
#' # to ignore the roundness of the earth, use
#' # `great_circle = FALSE`
#' p +
#'   geom_spatial_segment(
#'     great_circle = FALSE,
#'     arrow = grid::arrow(),
#'     crs = 4326
#'   ) +
#'   coord_sf(crs = 3995)
#'
geom_spatial_segment <- function(mapping = NULL, data = NULL,
                                 ...,
                                 crs = NULL,
                                 detail = waiver(),
                                 great_circle = TRUE,
                                 wrap_dateline = TRUE,
                                 arrow = NULL,
                                 lineend = "butt",
                                 linejoin = "round",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSpatialSegment,
    # using GeomPath so that `arrow` is supported
    geom = ggplot2::GeomPath,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      crs = crs,
      detail = detail,
      great_circle = great_circle,
      wrap_dateline = wrap_dateline,
      arrow = arrow,
      # arrow.fill is used with GeomSegment, and
      # doesn't work with GeomPath
      lineend = lineend,
      linejoin = linejoin,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_spatial_segment
#' @export
StatSpatialSegment <- ggplot2::ggproto(
  "StatSpatialSegment", StatSpatialRect,

  required_aes = c("x", "y", "xend", "yend"),

  compute_panel = function(self, data, scales, crs, crs_dest, detail = waiver(),
                           great_circle = TRUE, wrap_dateline = TRUE) {
    # source CRS
    if(is.null(crs)) {
      message("Assuming `crs = 4326` in stat_spatial_segment()")
      crs <- sf::st_crs(4326)
    } else {
      crs <- sf::st_crs(crs)
    }

    if (inherits(detail, "waiver"))  {
      if (great_circle)  {
        detail <- 100
      } else {
        detail <- NULL
      }
    }

    if (nrow(data) == 0) {
      return(data.frame())
    }

    geometry <- spatial_segment_geometry(
      data, crs, crs_dest, detail,
      great_circle, wrap_dateline
    )

    # convert geometry into a form that GeomPath can understand
    # not using df_spatial, since currently it relies on sp
    # for sf geometries
    geometry_df <- lapply(seq_along(geometry), function(i) {
      geom <- geometry[[i]]
      if (inherits(geom, "MULTILINESTRING")) {
        df <- sf::st_coordinates(geom)
        colnames(df) <- c("x", "y", "l1", "l2")
        df <- tibble::as_tibble(df)
        df$group <- paste(i, df$l1, sep = ".")
        df$l1 <- NULL
        df$l2 <- NULL
      } else if (inherits(geom, "LINESTRING")) {
        df <- unclass(geom)
        colnames(df) <- c("x", "y")
        df <- tibble::as_tibble(df)
        df$group <- paste0(i, ".1")
      } else {
        bad_class <- paste0("`", class(geom), "`", collapse = " / ")
        stop(glue::glue("Unrecognized geometry in stat_spatial_segment(): {bad_class}"))
      }

      df
    })

    # a stand-in for tidyr::unnest()
    geometry_nrow <- vapply(geometry_df, nrow, integer(1))
    # rep(, each = ) isn't vectorized
    data_indices <- lapply(seq_len(nrow(data)), function(i) rep(i, geometry_nrow[i]))
    data$xend <- NULL
    data$yend <- NULL
    data <- data[unlist(data_indices), , drop = FALSE]
    data$group <- NA
    data[c("x", "y", "group")] <- do.call(rbind, geometry_df)

    data
  }
)

spatial_segment_geometry <- function(data, crs, crs_dest, detail = NULL,
                                     great_circle = TRUE, wrap_dateline = TRUE) {

  # great circle only works if original data are in lat/lon
  if (great_circle) {
    if (is.null(detail)) {
      warning(
        "Ignoring `detail = NULL` because `great_circle = TRUE` in stat_spatial_segment()",
        call. = FALSE
      )
      detail <- 100
    }

    data[c("x", "y")] <- xy_transform(
      data$x, data$y,
      from = crs,
      to = 4326
    )
    data[c("xend", "yend")] <- xy_transform(
      data$xend, data$yend,
      from = crs,
      to = 4326
    )
    crs <- sf::st_crs(4326)
  }

  geometry <- lapply(seq_len(nrow(data)), function(i) {
    geom <- sf::st_linestring(
      matrix(
        c(data$x[i], data$y[i], data$xend[i], data$yend[i]),
        byrow = TRUE,
        ncol = 2
      )
    )

    # have to segmentize here because of the way `detail` is defined
    length <- sf::st_length(geom)
    if (!is.null(detail) && (length > 0)) {
      if (great_circle) {
        geom_sfc <- sf::st_sfc(geom, crs = crs)
        geom_sfc <- lwgeom::st_geod_segmentize(
          geom_sfc,
          lwgeom::st_geod_length(geom_sfc) / detail
        )

        if (wrap_dateline) {
          geom_sfc <- sf::st_wrap_dateline(geom_sfc)
        }
        geom <- geom_sfc[[1]]
      } else {
        dfMaxLength <- length / detail
        geom <- sf::st_segmentize(geom, dfMaxLength)
      }
    }

    geom
  })

  # create geometry column and project
  geometry <- do.call(sf::st_sfc, c(geometry, list(crs = crs)))
  sf::st_transform(geometry, crs = crs_dest)
}
