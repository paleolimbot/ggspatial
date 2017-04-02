
#' An Open Street Map Tile Geometry for ggplot2
#'
#' This geometry is a lazy version of its conterparts in the ggmap and rosm
#' packages. All tile downloading/loading/drawing is done at plot draw, such
#' that a complete backdrop of tiles can be calculated. The arguments are
#' essentially a wrapper around \link[rosm]{osm.image} and
#' \link[ggplot2]{annotation_raster} that can plot a specific bounding box or
#' default to the extents of the plot. The \code{ggosm()}  function is a
#' shorthand for the common case of \code{ggplot() + geom_osm(...) +
#' coord_map()}.
#'
#' @param x An object that can be coerced to a bounding box using
#'   \link[rosm]{extract_bbox}, or NULL to use the plot extents to fetch tiles
#'   (probably what you want).
#' @param zoomin The amount by which to adjust the automatically calculated zoom
#'   (or manually specified if the \code{zoom} parameter is passed). Use +1 to
#'   zoom in, or -1 to zoom out.
#' @param zoom Manually specify the zoom level (not recommended; adjust
#'   \code{zoomin} instead.
#' @param type A map type; one of that returned by \link[rosm]{osm.types}. User
#'   defined types are possible by passing any object coercible to type
#'   tile_source (see \link[rosm]{as.tile_source}).
#' @param forcedownload \code{TRUE} if cached tiles should be re-downloaded.
#'   Useful if some tiles are corrupted.
#' @param cachedir The directory in which tiles should be cached. Defaults to
#'   \code{getwd()/rosm.cache}.
#' @param progress A progress bar to use, or "none" to suppress progress updates
#' @param quiet Pass \code{FALSE} to see more error messages, particularly if
#'   your tiles do not download/load properly.
#'
#' @return A ggplot2 layer object
#' @export
#'
#' @examples
#'
#' \donttest{
#' library(prettymapr)
#' # use as a backdrop for geographical data
#' cities <- geocode(c("Halifax, NS", "Moncton, NB", "Montreal QC"))
#' ggplot(cities, aes(lon, lat, shape = query)) + geom_osm() +
#'   geom_point() + coord_map()
#'
#' # use ggosm() shorthand
#' ggosm() + geom_point(aes(lon, lat), cities)
#'
#' # use on its own with a bounding box
#' ggosm(searchbbox("vermont, USA"))
#'
#' # use alternative map types (see rosm::osm.types())
#' ggosm(type = "stamenwatercolor") + geom_point(aes(lon, lat), cities)
#' }
#'
#'
geom_osm <- function(x = NULL, zoomin=0, zoom=NULL, type=NULL, forcedownload=FALSE, cachedir=NULL,
                     progress = c("text", "none"), quiet = TRUE) {
  # sanitze progress arg
  progress <- match.arg(progress)

  # sanitize type here to avoid errors that don't show up until later
  if(is.null(type)) {
    type <- rosm::get_default_tile_source()
  } else {
    type <- rosm::as.tile_source(type)
  }

  # if x in NULL, this geom shouldn't have any influence on scale training
  if(is.null(x)) {
    data <- data.frame(x=NA_real_, y=NA_real_)
  } else {
    # use the bounding box as the data
    bbox <- rosm::extract_bbox(x)
    data <- data.frame(x = bbox[1, ], y = bbox[2, ])
  }

  # return GeomTileSource layer
  layer(data = data, mapping = ggplot2::aes_string("x", "y"),
        position = "identity", geom = GeomTileSource, stat = "identity",
        show.legend = FALSE, inherit.aes = FALSE,
        params = list(na.rm = TRUE, zoomin = zoomin, zoom = zoom, type = type,
                      forcedownload = forcedownload,
                      cachedir = cachedir, progress = progress, quiet = quiet))
}

#' @rdname geom_osm
#' @export
ggosm <- function(x = NULL, zoomin=0, zoom=NULL, type=NULL, forcedownload=FALSE, cachedir=NULL,
                  progress = c("text", "none"), quiet = TRUE) {
  progress <- match.arg(progress)

  # return ggplot() + geom_osm()
  ggplot2::ggplot() + geom_osm(x = x, zoomin = zoomin, zoom = zoom, type = type,
                               forcedownload = forcedownload, cachedir = cachedir,
                               progress = progress, quiet = quiet) +
    ggplot2::coord_map(projection = "mercator")
}

# the ggproto version
GeomTileSource <- ggplot2::ggproto("GeomTileSource", Geom,

  required_aes = c("x", "y"),

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, scales, coordinates,
                       zoomin=-1, zoom=NULL, type=NULL, forcedownload=FALSE, cachedir=NULL,
                       progress = c("text", "none"), quiet = TRUE, crsto = NULL,
                       interpolate = TRUE) {

   # original coordinate bounding box
   bbox <- rbind(x = scales$x.range, y = scales$y.range)

   if(!is.null(coordinates$projection)) {
     # check that projection is a play-by-the-rules, cyllindrical projection
     if(coordinates$projection != "mercator") {
       stop("geom_osm requires a 'mercator' projection")
     }

     # check that there is no change in orientation
     if(!is.null(coordinates$orientation) && (coordinates$orientation != 0)) {
       stop("geom_osm requires an orientation of 0")
     }

     # source is lat/lon
     epsg <- 4326

   } else {
     # warn user
     message("Attemping to use geom_osm without coord_map()")

     # guess source coordinate system
     epsg <- guess.epsg(bbox)
     bbox <- bboxTransform(bbox, from = epsg)
   }

   # get OSM image
   img <- rosm::osm.image(bbox, zoomin = zoomin, zoom = zoom, type = type,
                          forcedownload = forcedownload, cachedir = cachedir,
                          progress = progress, quiet = quiet)

   # convert bounding box back to lat/lon, if not epsg 3857
   if(epsg == 4326) {
     bbox_img <- bboxTransform(attr(img, "bbox"), from = 3857)
   } else {
     bbox_img <- attr(img, "bbox")
   }

   # mimic GeomRasterAnn in ggplot2
   # https://github.com/tidyverse/ggplot2/blob/master/R/annotation-raster.r

   # find bbox extents in the coordinate system
   data <- coordinates$transform(data.frame(t(bbox_img)), scales)

   # get coordinate ranges
   x_rng <- range(data$x, na.rm = TRUE)
   y_rng <- range(data$y, na.rm = TRUE)

   # return the grid::rasterGrob object
   grid::rasterGrob(img, x_rng[1], y_rng[1],
                    diff(x_rng), diff(y_rng), default.units = "native",
                    just = c("left","bottom"), interpolate = interpolate)

  }
)

# not really an exportable function
guess.epsg <- function(extents, plotunit = NULL, plotepsg = NULL, quiet = FALSE) {

  if(is.null(plotepsg) && is.null(plotunit)) {
    # check for valid lat/lon in extents
    if(extents[1, 1] >= -180 &&
       extents[1, 1] <= 180 &&
       extents[1, 2] >= -180 &&
       extents[1, 2] <= 180 &&
       extents[2, 1] >= -90 &&
       extents[2, 1] <= 90 &&
       extents[2, 2] >= -90 &&
       extents[2, 2] <= 90) {
      if(!quiet) message("Autodetect projection: assuming lat/lon (epsg 4326)")
      plotepsg <- 4326
    } else {
      # else assume google mercator used by {OpenStreetMap} (epsg 3857)
      if(!quiet) message("Audotdetect projection: assuming Google Mercator (epsg 3857)")
      plotepsg <- 3857
    }
  } else if(!is.null(plotunit)) {
    if(plotunit=="latlon") {
      plotepsg <- 4326
    }
  }

  plotepsg
}



