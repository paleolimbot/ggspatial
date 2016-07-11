


# this stat takes the params passed to it and makes it into a dataframe with x, y, and colour vals
StatOSM <- ggplot2::ggproto("StatOSM", ggplot2::Stat,

   compute_group = function(data, scales, obj=NULL, zoomin=0, zoom=NULL,
                            type="osm", forcedownload=FALSE, cachedir=NULL,
                            projection=NULL) {
     if(is.null(obj)) {
       # create bbox from xrange, yrange
       obj <- rbind(scales$x$range$range, scales$y$range$range)
     }
     fused <- rosm::osm.raster(x=obj, zoomin=zoomin, zoom=zoom, type=type, forcedownload=forcedownload,
                         cachedir=cachedir, projection=sp::CRS("+init=epsg:3857"))
     fused <- raster2dataframe(fused, crsto=projection)
     return(fused[c("x", "y", "fill")])
   },

   required_aes = c()
)

#' A ggplot geometry for OSM imagery
#'
#' An experimental function returning a geom_raster representing the tile data such that it
#' can be plotted as a \code{ggplot2} layer. Should probably be used with \code{coord_fixed}.
#' Note that this does not scale the aspect like the \code{sp} package and assumes lat/lon
#' as default coordinates (specify with )
#'
#' @param obj An object like in \link{osm.raster}: a bounding box or Spatial* object.
#' @param epsg The epsg code of the projection of the coordinates being plotted by other geoms
#' @inheritParams rosm::osm.raster
#' @param ... Agruments passed on to \code{geom_raster()}
#'
#' @return A \code{geom_raster} with colour data from the tiles.
#' @export
#'
#' @examples
#' \donttest{
#' library(prettymapr)
#' library(ggplot2)
#' ns <- searchbbox("Nova Scotia")
#'
#' ggplot(data.frame(x=1)) + geom_osm(ns)
#'
#' ggplot(data.frame(t(ns)), aes(x=x, y=y)) +
#'   geom_osm(type="osm", zoomin=-1) +
#'   geom_point() + coord_fixed()
#' }
#'
geom_osm <- function(obj=NULL, zoomin=0, zoom=NULL, type="osm", forcedownload=FALSE, cachedir=NULL,
                     epsg=4326, ...) {
  if(!("ggplot2" %in% rownames(utils::installed.packages()))) {
    stop("package 'ggplot2' must be installed for call to geom_osm()")
  }
  rgdal::CRSargs(sp::CRS("+init=epsg:3857")) #hack to load rgdal namespace
  ggplot2::layer(
    stat = StatOSM, data = NULL, mapping = NULL, geom = "raster",
    show.legend = FALSE, inherit.aes = FALSE, position = "identity",
    params=list(obj=obj, zoomin=zoomin, zoom=zoom, type=type,
                forcedownload=forcedownload, cachedir=cachedir,
                projection=sp::CRS(paste0("+init=epsg:", epsg)), ...)
  )
}

#' A ggplot interface for OSM imagery
#'
#' An experimental function returning a \code{ggplot} object more or less equivalent to the
#' \link{osm.plot} function. This is considerably less efficient than using \link{osm.plot}
#' but may be desirable if 'ggplot' is the plotting framework of choice.
#'
#' @param epsg The epsg code of the projection of the coordinates being plotted by other geoms.
#'   Note that if this is not a cylindrical projection, this will not be entirely accurate.
#' @inheritParams rosm::osm.raster
#' @seealso geom_osm, rosm::osm.raster
#'
#' @return A \code{geom_raster} with colour data from the tiles.
#' @export
#'
#' @examples
#' \donttest{
#' library(prettymapr)
#' ns <- searchbbox("Nova Scotia")
#' osm.ggplot(ns)
#'
#' }
osm.ggplot <- function(x, zoomin=0, zoom=NULL, type="osm", forcedownload=FALSE, cachedir=NULL,
                       epsg=3857, ...) {
  if(!("ggplot2" %in% rownames(utils::installed.packages()))) {
    stop("package 'ggplot2' must be installed for call to geom_osm()")
  }
  if(!("reshape2" %in% rownames(utils::installed.packages()))) {
    stop("package 'reshape2' must be installed for call to geom_osm()")
  }
  rgdal::CRSargs(sp::CRS("+init=epsg:3857")) #hack to load rgdal namespace
  projection <- sp::CRS(paste0("+init=epsg:", epsg))
  if(methods::is(x, "Spatial")) {
    crop.bbox <- sp::bbox(x)
    if(!is.na(rgdal::CRSargs(x@proj4string))) {
      projection <- x@proj4string
      lookup.bbox <- sp::bbox(sp::spTransform(x, sp::CRS("+init=epsg:4326")))
    }
  } else if(!is.null(projection)) {
    lookup.bbox <- x
    crop.bbox <- .projectbbox(lookup.bbox, projection=projection)
  } else {
    lookup.bbox <- x
    crop.bbox <- .projectbbox(lookup.bbox, projection=sp::CRS("+init=epsg:3857"))
  }

  ggplot2::ggplot(data.frame(x=1)) +
    geom_osm(obj=lookup.bbox, zoomin=zoomin, zoom=zoom, type=type,
    forcedownload=forcedownload, cachedir=cachedir,
    epsg=epsg, ...) +
    ggplot2::lims(x=crop.bbox[1,], y=crop.bbox[2,]) +
    ggplot2::coord_fixed()
}
