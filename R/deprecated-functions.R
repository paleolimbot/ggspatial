#####    R/class-raster.R    #####


#' Spatial Geometry for Raster* Objects
#'
#' Similar to the \link{geom_spatial} family for Spatial* objects, this method
#' plots a spatial raster as a spatial raster. Note that projecting (or un-projecting)
#' this layer will likely result in odd results (namely that geom_raster will not
#' function properly or at all). As a convenience, \code{ggraster()} is provided,
#' which replaces the call to \code{coord_map()} from \link{ggspatial} with a call
#' to \code{coord_fixed()}.
#'
#' @param data A Raster* object
#' @param mapping A mapping
#' @param geom The geometry to use. Defaults to raster (obviously), but could also
#'   be another value if used with a different stat (e.g. contour)
#' @param stat The stat to apply. Defaults to 'identity', but could be something else
#'   like 'contour', \link{stat_rgba} ("rgba"), or \link{stat_project} ("project").
#' @param position The position to apply (should probably always be 'identity')
#' @param show.legend Should the legend be shown for this layer?
#' @param inherit.aes Should aesthetics be inherited from the base plot?
#' @param crsfrom Override the source projection
#' @param crsto Override the source projection
#' @param attribute_table Not used by this method
#' @param ... Further arguments passed to the stat/geom
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' # standard ggplot syntax
#' ggplot() + geom_spatial(longlake_osm, aes(fill = band1)) + coord_fixed()
#' \donttest{
#' # or use ggraster()
#' ggraster(longlake_osm, aes(fill = band1))
#' }
#'
geom_spatial.Raster <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes = FALSE,
                                position = "identity", crsfrom = NA, crsto = NULL,
                                attribute_table = NA, geom = NA, stat = NA, ...) {
  # check number of bands
  nbands <- raster::nlayers(data)

  # throw a warning if there is more than one band (not in the default aesthetic)
  if(is.null(mapping) && (nbands > 1)) {
    message("geom_spatial.Raster: using aesthetic fill=band1 (nbands=", nbands, ")")
  }

  # throw a warning if crsto is not null
  if(!is.null(crsto) && identical(geom, NA)) {
    message("Attempting to use geom_raster with projected coordinates (use crsto = NULL",
            " to disable automatic projection)")
  }

  # call the default
  geom_spatial.default(data, mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position,
                       crsfrom = crsfrom, crsto = crsto, attribute_table = attribute_table,
                       geom = geom, stat = stat, ...)
}

#' @rdname geom_spatial.Raster
#' @export
ggraster <- function(data, mapping = NULL, ...) {
  if(!methods::is(data, "Raster")) stop("ggraster is only applicable to objects of class Raster")
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "long", y = "lat")
}

#' Turn a Raster into a data.frame
#'
#' Like others in the \link[ggplot2]{fortify} family, this method coerces its
#' input into a \code{data.frame}. Each band in the raster is a column in the data frame,
#' alongside x and y coordinate columns. This is the format required for input to
#' \link[ggplot2]{geom_raster}, such that a \code{Raster} object can be passed directly
#' to geom_raster without a conversion function. Band columns are named band1, band2, band3,
#' etc., for use in creating a mapping.
#'
#' @param model A \code{Raster} object
#' @param data Unused
#' @param format Use 'long' to get values in a single column; otherwise
#'   values are in one column per band.
#' @param ... Not used by this method
#'
#' @return A data.frame with columns, x and y as coordinates in the projection
#'   of the Raster,
#'
#' @export
#'
#' @importFrom ggplot2 fortify
#'
#' @examples
#' # use with ggplot()
#' df <- fortify(longlake_osm)
#' ggplot(df, aes(long, lat, fill = band1)) + geom_raster() +
#'   coord_fixed()
#' \donttest{
#' # identical usage with ggraster()
#' ggraster(longlake_osm, aes(fill = band1))
#'
#' # use long format to facet by band
#' dflong <- fortify(longlake_osm, format = "long")
#' ggplot(dflong, aes(long, lat, fill = value)) +
#'   geom_raster() + facet_wrap(~band) +
#'   coord_fixed()
#'
#'
#' # can use on other raster types as well
#' x <- rosm::osm.raster("gatineau qc")
#' ggraster(x, aes(fill = band1))
#' df <- fortify(x, format = "long")
#' ggplot(df, aes(long, lat, fill = value)) +
#'   geom_raster() + facet_wrap(~band) +
#'   coord_fixed()
#' }
#'
fortify.Raster <- function(model, data = NULL, format = c("wide", "long"), ...) {
  # match format arg
  format <- match.arg(format)

  # I used 'x', ggplot2 used 'model'...
  x <- model

  # get values in a data frame
  fused <- cbind(expand.grid(x=1:x@ncols, y=1:x@nrows),
                 raster::values(x))
  # set names to be long, lat, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c("long", "lat", paste0("band", 1:nbands))

  # fix x and y to be physical coordinates using the bbox
  bbox <- raster::as.matrix(x@extent)
  fused$long <- bbox[1,1]+(fused$long-1)/x@ncols*(bbox[1,2]-bbox[1,1])
  fused$lat <- bbox[2,1]+(fused$lat-x@nrows)/x@nrows*(bbox[2,1]-bbox[2,2])

  if(format == "wide") {
    # return the data frame
    fused
  } else {
    reshape2::melt(fused, id.vars = c("long", "lat"), variable.name = "band")
  }
}

# the spatial_fortify is *almost* the same, except band columns should not be
# renamed to .band1, .band2, etc.
#' @rdname spatial_fortify
#' @export
spatial_fortify.Raster <- function(x, ...) {
  fortified <- fortify(x, ...)
  plyr::rename(fortified, c(long = ".long", lat = ".lat"))
}

# add geometry definition
#' @rdname spatial_geom
#' @export
spatial_geom.Raster <- function(x) ggplot2::GeomRaster

# add default aes definition
#' @rdname spatial_geom
#' @export
spatial_default_aes.Raster <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", fill = "band1")
}



#####    R/class-sf.R    #####


#' A ggplot2 geom for sf* objects
#'
#' This implementation of \link{geom_spatial} for the sf package currently converts
#' objects to a Spatial* object, although this will likely change in future versions.
#'
#' @inheritParams geom_spatial
#'
#' @return A ggplot2 Geom object
#' @export
#'
#' @examples
#' library(sf)
#' llsf <- st_as_sf(longlake_waterdf)
#' ggspatial(llsf)
#' llsfc <- st_as_sfc(longlake_waterdf)
#' ggspatial(llsfc)
#'
geom_spatial.sf <- function(data, mapping = NULL, show.legend = TRUE,
                            inherit.aes = FALSE, position = "identity", crsfrom = NA, crsto = NA,
                            attribute_table = NA, geom = NA, stat = NA, ...) {
  geom_spatial.default(methods::as(data, "Spatial"), mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position, crsfrom = crsfrom, crsto = crsto,
                       attribute_table = attribute_table, geom = geom, stat = stat, ...)
}

#' @rdname geom_spatial.sf
#' @export
geom_spatial.sfc <- function(data, mapping = NULL, show.legend = TRUE,
                            inherit.aes = FALSE, position = "identity", crsfrom = NA, crsto = NA,
                            attribute_table = NA, geom = NA, stat = NA, ...) {
  geom_spatial.default(methods::as(data, "Spatial"), mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position, crsfrom = crsfrom, crsto = crsto,
                       attribute_table = attribute_table, geom = geom, stat = stat, ...)
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.sf <- function(model, data, ...) {
  fortify(methods::as(model, "Spatial"), data = data, ...)
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.sfc <- function(model, data, ...) {
  fortify(methods::as(model, "Spatial"), data = data, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.sf <- function(x, attrs = NA, ...) {
  spatial_fortify(methods::as(x, "Spatial"), attrs = attrs, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.sfc <- function(x, attrs = NA, ...) {
  spatial_fortify(methods::as(x, "Spatial"), attrs = attrs, ...)
}

#' @rdname spatial_geom
#' @export
spatial_geom.sf <- function(x) spatial_geom(methods::as(x, "Spatial"))
#' @rdname spatial_geom
#' @export
spatial_geom.sfc <- function(x) spatial_geom(methods::as(x, "Spatial"))

#' @rdname spatial_geom
#' @export
spatial_default_aes.sf <- function(x) {
  spatial_default_aes(methods::as(x, "Spatial"))
}

#' @rdname spatial_geom
#' @export
spatial_default_aes.sfc <- function(x) {
  spatial_default_aes(methods::as(x, "Spatial"))
}




#####    R/class-spatial.R    #####


# ---------- method definitions for objects from sp ------------

#' Create a data.frame from Spatial* objects
#'
#' Methods to turn Spatial* objects into data.frames
#'
#' Several methods are missing from ggplot2, including \link[ggplot2]{fortify}
#' implementations for SpatialPoints, SpatialPointsDataFrame, and SpatialLines.
#'
#' @param model The object
#' @param data Not used by this method
#' @param ... Not used by this method
#'
#' @return A data.frame with colummns id, long and lat (mimics
#' behaviour in fortify.Spatial* from ggplot2.
#' @export
#'
#' @examples
#' fortify(longlake_depthdf)
#'
fortify.SpatialPoints <- function(model, data = NULL, ...) {
  coords <- sp::coordinates(model)
  data.frame(id=1:nrow(coords), long = coords[, 1], lat = coords[, 2])
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.SpatialPointsDataFrame <- function(model, data = NULL, ...) {
  coords <- sp::coordinates(model)
  data.frame(id=row.names(model), long = coords[, 1], lat = coords[, 2])
}

#' @rdname fortify.SpatialPoints
#' @export
fortify.SpatialLines <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

# ----------- spatial_fortify definitions ---------------

#' @rdname spatial_fortify
#' @export
spatial_fortify.SpatialPointsDataFrame <- function(x, attrs = NA, ...) {
  # override attribute table if present
  if(identical(attrs, NA)) {
    attrs <- x@data
  }

  if(!is.null(attrs)) {
    # add .id column
    attrs$.id <- row.names(x)
  }

  # call default method
  spatial_fortify.default(x, attrs, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.SpatialLinesDataFrame <- function(x, attrs = NA, ...) {
  # override attribute table if present
  if(identical(attrs, NA)) {
    attrs <- x@data
    row.names(attrs) <- row.names(x)
  }
  # call default method
  spatial_fortify.default(x, attrs, ...)
}

#' @rdname spatial_fortify
#' @export
spatial_fortify.SpatialPolygonsDataFrame <- function(x, attrs = NA, ...) {
  # same as spatial lines data frame with suppressMessages
  suppressMessages(spatial_fortify.SpatialLinesDataFrame(x, attrs = attrs, ...))
}


# --------- geometry definitions ----------------

#' @rdname spatial_geom
#' @export
spatial_geom.SpatialPoints <- function(x) ggplot2::GeomPoint
#' @rdname spatial_geom
#' @export
spatial_geom.Line <- function(x) ggplot2::GeomPath
#' @rdname spatial_geom
#' @export
spatial_geom.Lines <- function(x) ggplot2::GeomPath
#' @rdname spatial_geom
#' @export
spatial_geom.SpatialLines <- function(x) ggplot2::GeomPath
#' @rdname spatial_geom
#' @export
spatial_geom.Polygon <- function(x) GeomPolypath
#' @rdname spatial_geom
#' @export
spatial_geom.Polygons <- function(x) GeomPolypath
#' @rdname spatial_geom
#' @export
spatial_geom.SpatialPolygons <- function(x) GeomPolypath

# ----------- default aes definitions ------------
# for items whose fortify() method produceds a group

#' @rdname spatial_geom
#' @export
spatial_default_aes.Lines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

# spatial lines data frame extends spatial lines
#' @rdname spatial_geom
#' @export
spatial_default_aes.SpatialLines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

#' @rdname spatial_geom
#' @export
spatial_default_aes.Polygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

# spatial lines data frame extends spatial polygons
#' @rdname spatial_geom
#' @export
spatial_default_aes.SpatialPolygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}




#####    R/geom-spatial.R    #####



#' A ggplot2 geom for Spatial* objects
#'
#' A function returning a geom_* object based on the Spatial* input. Also will
#' happily project a regular \code{data.frame} provided x and y aesthetics are
#' specified. The result is a \code{geom_*} for use with ggplot2, with aesthetics
#' and other argumets passed on to that geom.
#'
#' @param data A \code{Spatial*} object or \code{data.frame}.
#' @param mapping A mapping as created by \code{aes()} or \code{aes_string()}
#' @param show.legend Logical describing the legend visibility.
#' @param inherit.aes Logical describing if aesthetics are inherited
#' @param position Passed on to the layer
#' @param crsfrom An object that can be coerced to a CRS using \link{as.CRS}; defaults
#'   to the CRS of the data or lat/lon if that does not exist
#' @param crsto An object that can be coerced to a CRS using \link{as.CRS}; defaults to
#'   lat/lon so that the plot can be projected using coord_map()
#' @param geom The geometry to use for the object (NA to guess: see \link{spatial_geom})
#' @param stat The statistic to apply (NA to guess, is probably "identity": see \link{spatial_stat})
#' @param attribute_table For SpatialPoints, SpatialLines, and SpatialPolygons, an attribute
#'   table that matches the input object.
#' @param ... Agruments passed on to the \code{geom_*} (e.g. \code{lwd}, \code{fill}, etc.)
#'
#' @return A ggplot2 'layer' object.
#'
#' @importFrom ggplot2 layer
#' @importFrom sp CRS
#' @export
#'
#' @examples
#' # plot a number of spatial objects
#' ggplot() +
#'   geom_spatial(longlake_waterdf, fill="lightblue") +
#'   geom_spatial(longlake_marshdf, fill="grey", alpha=0.5) +
#'   geom_spatial(longlake_streamsdf, col="lightblue") +
#'   geom_spatial(longlake_roadsdf, col="black") +
#'   geom_spatial(longlake_buildingsdf, pch=1, col="brown", size=0.25) +
#'   coord_map()
#'
geom_spatial <- function(data, mapping = NULL, ...) UseMethod("geom_spatial")

#' @export
#' @rdname geom_spatial
ggspatial <- function(data, mapping = NULL, ...) {
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) + ggplot2::coord_map() +
    ggplot2::labs(x = "long", y = "lat")
}

#' @rdname geom_spatial
#' @export
geom_spatial.data.frame <- function(data, mapping = NULL, ...) {
  stop("Use stat_project to apply projections to data frame input")
}

#' @export
#' @rdname geom_spatial
geom_spatial.default <- function(data, mapping = NULL, show.legend = TRUE, inherit.aes = FALSE,
                                  position = "identity", crsfrom = NA, crsto = NA,
                                  attribute_table = NA, geom = NA, stat = NA, ...) {
  # get projections
  projections <- get_projections(data, crsfrom, crsto)

  # fortify then project
  df <- spatial_fortify(data, attribute_table)
  check_spatial_fortify(data, df)

  df[, c(".long", ".lat")] <- xyTransform(df$.long, df$.lat,
                                          projections$crsfrom, projections$crsto)

  # find the geom, stat and default aesthetics
  if(identical(geom, NA)) {
    geom <- spatial_geom(data)
  }
  if(identical(stat, NA)) {
    stat <- spatial_stat(data)
  }

  final_mapping <- override_aesthetics(mapping, spatial_default_aes(data))

  # return layer
  layer(
    stat = stat, data = df, mapping = final_mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = position,
    params=list(...)
  )

}

get_projections <- function(data, crsfrom = NA, crsto = NA) {

  # null crsfrom is ambiguous
  if(is.null(crsfrom)) stop("Value of 'crsfrom' cannot be NULL")

  crsfrom <- as.CRS(crsfrom)
  crsto <- as.CRS(crsto)

  if(identical(crsfrom, NA)) {
    crsfrom <- as.CRS(data)
  }

  if(identical(crsfrom, NA)) {
    message("Assuming input coordinates are lat/lon")
    crsfrom_out <- as.CRS(4326)
  } else {
    crsfrom_out <- crsfrom
  }

  # null crsto means don't project
  if(is.null(crsto)) {
    crsto_out <- crsfrom
  } else {

    # assign crs to here
    if(identical(crsto, NA)) {
      if(!identical(crsfrom, NA)) message("Converting coordinates to lat/lon")
      crsto_out <- as.CRS(4326)
    } else {
      crsto_out <- crsto
    }

  }

  # return list of crsfrom and crsto
  list(crsfrom = crsfrom_out, crsto = crsto_out)
}



#####    R/projections.R    #####



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
#' library(sf)
#' as.CRS(st_as_sf(longlake_waterdf)) # sf
#'
as.CRS <- function(x) UseMethod("as.CRS")

#' @rdname as.CRS
#' @export
as.CRS.default <- function(x) {
  if(is.null(x)) {
    NULL
  } else if(methods::is(x, "CRS")) {
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

#' @rdname as.CRS
#' @export
as.CRS.sf <- function(x) {
  sp::CRS(sf::st_crs(x)$proj4string)
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
#'   systems but questionable in more complex ones.
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
#' library(sp)
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




#####    R/spatial-fortify.R    #####


# this method performs the join between the fortified data frame
# and the original object. it starts by calling fortify() on
# the object


#' Create a data frame from a Spatial object with attributes
#'
#' The \link[ggplot2]{fortify} function returns a data frame with geometry information;
#' \code{spatial_fortify()} returns a data frame with the results of fortify (left) joined
#' with the attribute table. If there is no attribute table, the results of fortify are
#' returned with the column names preceeded by a \code{.}.
#'
#' @param x A spatial object
#' @param attrs An optional attribute table, NA for the default, or NULL for none.
#' @param ... Additional arguments passed to \link[ggplot2]{fortify}
#'
#' @return A data.frame with (at least) columns ".lat" and ".long".
#' @export
#'
#' @examples
#' head(spatial_fortify(longlake_depthdf))
#'
spatial_fortify <- function(x, attrs = NULL, ...) UseMethod("spatial_fortify")

#' @rdname spatial_fortify
#' @export
spatial_fortify.default <- function(x, attrs = NA, ...) {
  # check input
  if(!is.null(attrs) && !identical(attrs, NA) && !inherits(attrs, "data.frame")) {
    stop("Argument 'attrs' must be a data.frame")
  }

  # call fortify (will throw error if there is no method for type)
  fortified <- ggplot2::fortify(x, ...)

  # obscure names
  names(fortified) <- paste0(".", names(fortified))

  # determine attribute table
  if(!is.null(attrs) && !identical(attrs, NA)) {
    # add .id column
    attrs$.id <- rownames(attrs)

    # check for completeness
    if(any(!(attrs$.id %in% fortified$.id))) {
      stop("Some .id values are present in attrs and missing in fortified")
    }
    if(any(!(fortified$.id %in% attrs$.id))) {
      stop("Some .id values are present in fortified and missing in attrs")
    }

    # merge and return
    merge(fortified, attrs, by = ".id", all.x = TRUE)
  } else {
    fortified
  }
}

# this function checks the output of spatial_fortify
check_spatial_fortify <- function(x, df) {
  # check for data.frame
  if(!inherits(df, "data.frame")) {
    stop("spatial_fortify for class '", class(x)[1],
         "' did not return a data.frame")
  }

  # check for .long and .lat
  if(!(".long" %in% names(df))) {
    stop("spatial_fortify for class '", class(x)[1],
         "' did not return a data.frame with column '.long'")
  }
  if(!(".lat" %in% names(df))) {
    stop("spatial_fortify for class '", class(x)[1],
         "' did not return a data.frame with column '.lat'")
  }
}



#####    R/spatial-geom.R    #####


# this is essentially an automatic geometry chooser, since the
# geom is often implied by the input type

#' Guess ggplot parameters based on a spatial object
#'
#' In most cases, the ggplot geometry that should be used with a
#' spatial object is suggested based on its type (e.g, SpatialPoints
#' should use \code{geom = "point"}). S3 implementations of this
#' method should return a ggplot2 \code{Geom} that will be used in
#' \link{geom_spatial} when \code{geom = NA}. The default is
#' \code{geom = "point"}. In almost all cases, the statistic
#' to be used should be \code{stat = "identity"}, but could
#' theoretically be a custom stat written for a specific
#' class of spatial object. Default aesthetics are by default
#' \code{aes(.long, .lat)}, but some spatial objects require
#' other aesthetics, for which the defaults are returned by
#' \code{spatial_default_aes()}.
#'
#' @param x A spatial object
#'
#' @return A ggplot2 \code{Geom}
#' @export
#'
#' @examples
#' spatial_geom(data.frame()) # default is GeomPoint
#' spatial_geom(longlake_waterdf) # GeomPolypath
#' spatial_geom(longlake_roadsdf) # GeomPath
#' spatial_geom(longlake_buildingsdf) # GeomPoint
#' spatial_geom(longlake_osm) # GeomRaster
#'
spatial_geom <- function(x) UseMethod("spatial_geom")

#' @rdname spatial_geom
#' @export
spatial_geom.default <- function(x) ggplot2::GeomPoint

#' @rdname spatial_geom
#' @export
spatial_stat <- function(x) UseMethod("spatial_stat")
spatial_stat.default <- function(x) ggplot2::StatIdentity

#' @rdname spatial_geom
#' @export
spatial_default_aes <- function(x) UseMethod("spatial_default_aes")
spatial_default_aes.default <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat")
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



#####    R/stat-project.R    #####


#' Statisitc to project coordinates
#'
#' Projects coordinates using rgdal/sp. Takes params
#' \code{crsfrom} and \code{crsto}, both wrapped in \link{as.CRS}, such that you can
#' pass an epsg code, a CRS object or \code{NA} to guess the input (will be either lat/lon or
#' google mercator). If \code{NA}, \code{crsto} is assumed
#' to be EPSG:4326 (Lat/Lon), so data can be used with \link[ggplot2]{coord_map}.
#'
#' @param data A \code{Spatial*} object or \code{data.frame}.
#' @param mapping A mapping as created by \code{aes()} or \code{aes_string()}
#' @param show.legend Logical describing the legend visibility.
#' @param inherit.aes Logical describing if aesthetics are inherited
#' @param position Passed on to geom_*
#' @param crsfrom An object that can be coerced to a CRS using \link{as.CRS}; defaults
#'   to the CRS of the data or lat/lon if that does not exist
#' @param crsto An object that can be coerced to a CRS using \link{as.CRS}; defaults to
#'   lat/lon so that the plot can be projected using coord_map()
#' @param geom For data frames, the geometry to use
#' @param ... Passed to the geom
#'
#' @export
#'
#' @examples
#' # longlake roads df is in UTM zone 20 (epsg:26920)
#' ggplot(longlake_roadsdf, aes(long, lat)) +
#'   stat_project(geom = "path", crsfrom = 26920) +
#'   coord_map()
#'
stat_project <- function(mapping = NULL, data = NULL, crsfrom = NA, crsto = NA,
                         position = "identity", show.legend = TRUE, inherit.aes = TRUE,
                         geom = "point", ...) {
  layer(
    stat = StatProject, data = data, mapping = mapping, geom = geom,
    show.legend = show.legend, inherit.aes = inherit.aes, position = position,
    params=list(crsfrom = as.CRS(crsfrom), crsto = as.CRS(crsto), ...)
  )
}

# stat to transform coordinates
StatProject <- ggplot2::ggproto("StatProject", ggplot2::Stat,

    # TODO: this should really be in compute_layer, which has
    # slightly different arguments
    compute_panel = function(data, scales, crsfrom=NA, crsto=NA) {

      # guess missing coordinate systems
      if(identical(crsfrom, NA) || is.na(rgdal::CRSargs(crsfrom))) {
        epsg <- guess.epsg(sp::bbox(cbind(data$x, data$y)))
        crsfrom <- as.CRS(epsg)
      }

      # default is actually to "unproject", for use with ggmap/coord_map
      if(identical(crsto, NA) || is.na(rgdal::CRSargs(crsto))) {
        crsto <- as.CRS(4326)
        if(crsfrom@projargs != crsto@projargs) {
          message("Converting coordinates to lat/lon (epsg:4326)")
        }
      }

      # if crs from and to are equivalent, return the original data
      if(crsfrom@projargs == crsto@projargs) {
        return(data)
      } else {
        coords <- xyTransform(data$x, data$y, from = crsfrom, to = crsto)
        data$x <- coords[,1]
        data$y <- coords[,2]
        return(data)
      }
    },

    required_aes = c("x", "y")
)
