
#' Add background OSM tiles
#'
#' Uses [rosm::osm.image()] to add background tiles. If you are publishing
#' a map using these tiles, make sure to use the proper attribution
#' (e.g., "Copyright OpenStreetMap contributors" when using an
#' OpenStreetMap-based tile set).
#'
#' @param type The map type (one of that returned by [rosm::osm.types])
#' @param zoom The zoom level (overrides zoomin)
#' @param zoomin Delta on default zoom. The default value is designed
#'   to download fewer tiles than you probably want. Use `-1` or `0` to
#'   increase the resolution.
#' @param forcedownload Re-download cached tiles?
#' @param cachedir Specify cache directory
#' @param progress Use `progress = "none"` to suppress progress and zoom output
#' @param quiet Use `quiet = FALSE` to see which URLs are downloaded
#' @param interpolate Passed to [grid::rasterGrob()]
#' @param alpha Use to make this layer semi-transparent
#' @param data,mapping Specify data and mapping to use this geom with facets
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' library(ggplot2)
#' load_longlake_data()
#'
#' ggplot() +
#'   annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
#'   geom_sf(data = longlake_waterdf, fill = NA, col = "grey50")
#'
annotation_map_tile <- function(type = "osm", zoom = NULL, zoomin = -2,
                                forcedownload = FALSE, cachedir = NULL,
                                progress = c("text", "none"), quiet = TRUE,
                                interpolate = TRUE, data = NULL, mapping = NULL, alpha = 1) {

  progress <- match.arg(progress)
  if(!is.null(zoom)) {
    zoomin <- 0
  }

  if(is.null(data)) {
    if(is.null(zoom)) {
      data <- data.frame(type = type, zoomin = zoomin, stringsAsFactors = FALSE)
      mapping <- ggplot2::aes(type = type, zoomin = zoomin)
    } else {
      data <- data.frame(type = type, zoom = zoom, zoomin = zoomin, stringsAsFactors = FALSE)
      mapping <- ggplot2::aes(type = type, zoom = zoom, zoomin = zoomin)
    }
  }

  c(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      geom = GeomMapTile,
      stat = "identity",
      position = "identity",
      params = list(
        forcedownload = forcedownload,
        cachedir = cachedir,
        progress = progress,
        quiet = quiet,
        interpolate = interpolate,
        alpha = alpha
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    ),
    # use an emtpy geom_sf() with same CRS as the raster to mimic behaviour of
    # using the first layer's CRS as the base CRS for coord_sf().
    ggplot2::geom_sf(
      data = sf::st_sfc(sf::st_point(), crs = sf::st_crs(3857)),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @export
#' @rdname annotation_map_tile
GeomMapTile <- ggplot2::ggproto(
  "GeomMapTile",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  default_aes = ggplot2::aes(
    type = "osm",
    zoomin = 0,
    zoom = NULL
  ),

  draw_panel = function(
    data, panel_params, coordinates,
    forcedownload = FALSE, cachedir = NULL,
    progress = c("none", "text"), quiet = TRUE, interpolate = TRUE, alpha = 1
  ) {
    progress <- match.arg(progress)

    coord_crs <- sf::st_crs(panel_params$crs)
    if(!is.null(coord_crs)) {
      sp_bbox <- project_extent(
        xmin = panel_params$x_range[1],
        ymin = panel_params$y_range[1],
        xmax = panel_params$x_range[2],
        ymax = panel_params$y_range[2],
        from_crs = coord_crs,
        to_crs = 4326,
        format = "sp"
      )

      # check for a bounding box that is too small (<5 m)
      # (may cause rosm_raster() to crash RStudio)
      if (.geodist(sp_bbox[, 1], sp_bbox[, 2]) < 5) {
        warning("annotation_map_tile(): bounding box is too small", call. = FALSE)
        return(ggplot2::zeroGrob())
      }


    } else {
      stop("geom_map_tile() requires coord_sf().", call. = FALSE)
    }

    if(coord_crs != sf::st_crs(3857)) {

      # have to use raster to reproject...
      # because this involves a call to projectRaster(), it has to be wrapped in
      # suppressWarnings to avoid the "no non-missing arguments to min; returning Inf" error
      # Suppress "Discarded datum ... in CRS definition" warning (not important at the scale
      # of an OSM map)
      raster <- suppressWarnings(
        rosm_raster(
          x = sp_bbox,
          zoomin = data[["zoomin"]][1],
          zoom = data[["zoom"]][1],
          type = as.character(data[["type"]][1]),
          forcedownload = forcedownload,
          cachedir = cachedir,
          progress = progress,
          quiet = quiet
        )
      )

      # raster::projectRaster has very odd behaviour...it outputs the warning
      # "no non-missing arguments to max; returning -Inf"
      # but only when run with a calling handler
      raster_proj <- suppressWarnings(
          raster::projectRaster(
          raster,
          crs = raster::crs(sf::st_crs(coord_crs)$proj4string)
        )
      )

      ext <- raster::extent(raster_proj)
      corners <- data.frame(x = c(ext@xmin, ext@xmax), y = c(ext@ymin, ext@ymax))
      img <- raster_as_array(raster_proj, alpha = alpha)

    } else {

      # Can use osm.image, which is much faster (and this is the most common case)
      # Suppress "Discarded datum ... in CRS definition" warning (not important at the scale
      # of an OSM map)
      img <- suppressWarnings(
        rosm_image(
          x = sp_bbox,
          zoomin = data[["zoomin"]][1],
          zoom = data[["zoom"]][1],
          type = as.character(data[["type"]][1]),
          forcedownload = forcedownload,
          cachedir = cachedir,
          progress = progress,
          quiet = quiet
        )
      )

      bbox_img <- attr(img, "bbox")
      corners <- data.frame(t(bbox_img))

      # apply the specified alpha
      if(alpha != 1) {
        # it could be that there is already a band 4, or it may not exist yet
        if(dim(img)[3] == 4) {
          tband <- img[ , , 4, drop = FALSE]
        } else {
          tband <- array(1, dim(img)[1:2])
        }

        # multiply the alpha band by the requested alpha (clamping to 0,1)
        tband <- tband * max(0, min(1, alpha))

        # bind it to the original raster
        img <- abind::abind(img[, , 1:3, drop = FALSE], tband)
      }
    }

    # transform corners to viewport cs
    corners_trans <- coordinates$transform(corners, panel_params)
    x_rng <- range(corners_trans$x, na.rm = TRUE)
    y_rng <- range(corners_trans$y, na.rm = TRUE)

    # return raster grob of the img
    grid::rasterGrob(
      img,
      x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate
    )
  }
)

# the CMD check doesn't detect the call to
# rosm in the ggproto object and complains
rosm_image <- function(...) {
  rosm::osm.image(...)
}

rosm_raster <- function(...) {
  rosm::osm.raster(...)
}
