

#' Extract a projection from an object
#'
#' Many functions in this package require projection information. This function
#' allows a CRS to be extracted from various objects, notably Spatial*,
#' Raster*, and integers (which are assumed to be EPSG codes). This allows
#' for less verbose syntax when dealing with projections in this package.
#'
#' @param x An object
#'
#' @return A CRS object, or NA if one cannot be extracted
#' @export
#'
#' @examples
#' as.CRS(4326) # integer
#' as.CRS(longlake_osm) # raster
#' as.CRS(longlake_waterdf) # spatial
#'
as.CRS <- function(x) {
  if(methods::is(x, "CRS")) {
    x
  } else if(methods::is(x, "Spatial")) {
    if(!is.na(rgdal::CRSargs(x@proj4string))) {
      x@proj4string
    } else {
      NA
    }
  } else if(methods::is(x, "Raster")) {
    x@crs
  } else if(is.numeric(x) && (length(x) == 1)) {
    requireNamespace("rgdal", quietly=TRUE)
    intx <- as.integer(x)
    sp::CRS(paste0("+init=epsg:", intx))
  } else {
    NA
  }
}

#' Project XY coordinates
#'
#' The sp package provides a powerful interface with easy syntax for projection Spatial* objects,
#' but raw coordinates are not as straightforward. Use this function to project raw
#' coordinates, and \link[sp]{spTransform} to project Spatial* objects.
#'
#' @param x The x values (or longitude)
#' @param y The y values (or latitude)
#' @param bbox The bounding box to transform. Note that bounding boxes are not truly
#'   transformed bounding boxes, but the bounding box of the transformed lower-left
#'   and upper-right coordinates. This is a perfect approximation in cylindrical
#'   systems but questionable in more complex systems.
#' @param from The source projection, or an object that can be coerced to one
#'   using \link{as.CRS}
#' @param to The destination projection, or an object that can be coerce to one
#'   using \link{as.CRS}
#' @param na.rm Currently xyTransform does not work with non-finite values.
#'   Pass na.rm = TRUE to remove them, or else a (more helpful) error will be
#'   thrown if non-finite values exist.
#'
#' @return A matrix with 2 columns (x and y)
#' @export
#'
#' @examples
#' all_latlons <- expand.grid(x=-180:180, y=-70:70)
#' xyTransform(all_latlons$x, all_latlons$y, from = 4326, to = 3857)
#' bboxTransform(bbox(longlake_osm), from = 26920)
#'
xyTransform <- function(x, y, from = 4326, to = 4326, na.rm = FALSE) {
  # create coordinates
  coords <- cbind(x, y)
  rownames(coords) <- NULL # causes warnings if this is not done

  # if there is nothing to be done, don't project
  # check before as.CRS
  if(identical(from, to)) return(coords)

  # load GDAL
  requireNamespace("rgdal", quietly=TRUE)

  # parse coordinate systems
  from <- as.CRS(from)
  to <- as.CRS(to)

  # deal with NA values
  finite <- !is.na(coords[, 1, drop = FALSE]) & !is.na(coords[, 2, drop = FALSE])
  if(na.rm) {
    coords <- coords[finite, , drop = FALSE]
  } else {
    # give a more helpful error message than what actually happens
    if(any(!finite)) stop("xyTransform cannot project non-finite values")
  }

  # do transform, return coordinates
  sp_out <- sp::spTransform(sp::SpatialPoints(coords, from), to)
  sp::coordinates(sp_out)
}

#' @rdname xyTransform
#' @export
bboxTransform <- function(bbox, from = 4326, to = 4326) {
  coords <- t(bbox)
  pbox <- t(xyTransform(coords[, 1], coords[, 2], from = from, to = to))
  rownames(pbox) <- c("x", "y")
  colnames(pbox) <- c("min", "max")
  pbox
}

