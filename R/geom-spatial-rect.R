
#' Projected rectangular regions
#'
#' If you need to plot a [sf::st_bbox()], use [layer_spatial()] instead.
#' While the implementation is slightly differrent, these functions are
#' intended to behave identically to [ggplot2::geom_rect()] and
#' [ggplot2::geom_tile()].
#'
#' @inheritParams stat_spatial_identity
#' @inheritParams layer_spatial.bbox
#' @inheritParams annotation_spatial_hline
#' @param linejoin How corners should be joined
#'
#' @export
#'
#' @examples
#' library(ggplot2)
#' tile_df <- expand.grid(
#'   x = seq(-140, -52, by = 20),
#'   y = seq(40, 70, by = 10)
#' )
#'
#' ggplot(tile_df, aes(x, y)) +
#'   geom_spatial_tile(crs = 4326) +
#'   coord_sf(crs = 3979)
#'
#' # the same plot using geom_spatial_rect()
#' ggplot(
#'   tile_df,
#'   aes(xmin = x - 10, xmax = x + 10, ymin = y - 5, ymax = y + 5)
#' ) +
#'   geom_spatial_rect(crs = 4326) +
#'   coord_sf(crs = 3979)
#'
geom_spatial_rect <- function(mapping = NULL, data = NULL,
                              ...,
                              crs = NULL,
                              detail = 30,
                              linejoin = "mitre",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSpatialRect,
    geom = ggplot2::GeomSf,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      crs = crs,
      detail = detail,
      linejoin = linejoin,
      legend = "polygon",
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_spatial_rect
#' @export
geom_spatial_tile <- function(mapping = NULL, data = NULL,
                              ...,
                              crs = NULL,
                              detail = 30,
                              linejoin = "mitre",
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = StatSpatialTile,
    geom = ggplot2::GeomSf,
    position = "identity",
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      crs = crs,
      detail = detail,
      linejoin = linejoin,
      legend = "polygon",
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_spatial_rect
#' @export
StatSpatialRect <- ggplot2::ggproto(
  "StatSpatialRect", ggplot2::Stat,

  required_aes = c("xmin", "xmax", "ymin", "ymax"),

  compute_layer = function(self, data, params, layout) {
    # add dest CRS to parameters
    params$crs_dest <- sf::st_crs(layout$coord_params$crs)
    ggplot2::ggproto_parent(ggplot2::Stat, self)$compute_layer(data, params, layout)
  },

  compute_panel = function(self, data, scales, crs, crs_dest, detail = 30) {
    # source CRS
    if(is.null(crs)) {
      message("Assuming `crs = 4326` in stat_spatial_rect()")
      crs <- sf::st_crs(4326)
    } else {
      crs <- sf::st_crs(crs)
    }

    bboxes <- lapply(seq_len(nrow(data)), function(i) {
      sf::st_bbox(
        c(
          xmin = data$xmin[i],
          ymin = data$ymin[i],
          xmax = data$xmax[i],
          ymax = data$ymax[i]
        ),
        crs = crs
      )
    })

    geometry <- lapply(bboxes, function(x) sf_bbox_to_sf(x, detail = detail)$geometry[[1]])
    data$geometry <- do.call(sf::st_sfc, c(geometry, list(crs = crs)))
    data$geometry <- sf::st_transform(data$geometry, crs = crs_dest)

    # update xmin/xmax/ymin/ymax for proper scale training
    projected_bbox <- sf::st_bbox(data$geometry)
    data$xmin <- projected_bbox["xmin"]
    data$ymin <- projected_bbox["ymin"]
    data$xmax <- projected_bbox["xmax"]
    data$ymax <- projected_bbox["ymax"]

    data
  }
)

#' @rdname geom_spatial_rect
#' @export
StatSpatialTile <- ggplot2::ggproto(
  "StatSpatialTile", StatSpatialRect,

  setup_data = function(data, params) {
    if (all(is.na(data$width))) {
      data$width <- ggplot2::resolution(data$x, FALSE)
    }

    if (all(is.na(data$height))) {
      data$height <- ggplot2::resolution(data$y, FALSE)
    }

    x <- NULL; rm(x)
    height <- NULL; rm(height)
    width <- NULL; rm(width)

    transform(
      data,
      xmin = x - width / 2,
      xmax = x + width / 2,
      width = NULL,
      ymin = y - height / 2,
      ymax = y + height / 2,
      height = NULL
    )
  },

  required_aes = c("x", "y"),
  default_aes = aes(height = NA_real_, width = NA_real_)
)
