#####    R/class-raster.R    #####


#' Deprecated functions
#'
#' These functions are deprecated and should not be used.
#'
#' @param data,mapping,geom,stat,position,show.legend,inherit.aes deprecated
#' @param crsfrom,crsto,attribute_table deprecated
#' @param format,attrs,x,raster,interpolate,na.value deprecated
#' @param zoomin,zoom,type,forcedownload,cachedir,progress,quiet deprecated
#'
#' @param ... Passed to/from methods
#'
#' @rdname deprecated
#' @export
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

#' @rdname deprecated
#' @export
ggraster <- function(data, mapping = NULL, ...) {
  if(!methods::is(data, "Raster")) stop("ggraster is only applicable to objects of class Raster")
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) +
    ggplot2::coord_fixed() +
    ggplot2::labs(x = "long", y = "lat")
}

#' @rdname deprecated
#' @export
spatial_fortify.Raster <- function(x, ..., format = c("wide", "long")) {
  # match format arg
  format <- match.arg(format)

  # get values in a data frame
  fused <- cbind(expand.grid(x=1:x@ncols, y=1:x@nrows), raster::values(x))
  # set names to be long, lat, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c(".long", ".lat", paste0("band", 1:nbands))

  # fix x and y to be physical coordinates using the bbox
  bbox <- raster::as.matrix(x@extent)
  fused$.long <- bbox[1,1]+(fused$.long-1)/x@ncols*(bbox[1,2]-bbox[1,1])
  fused$.lat <- bbox[2,1]+(fused$.lat-x@nrows)/x@nrows*(bbox[2,1]-bbox[2,2])

  if(format == "wide") {
    # return the data frame
    fused
  } else {
    reshape2::melt(fused, id.vars = c(".long", ".lat"), variable.name = "band")
  }
}

#' @rdname deprecated
#' @export
spatial_geom.Raster <- function(x) ggplot2::GeomRaster

#' @rdname deprecated
#' @export
spatial_default_aes.Raster <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", fill = "band1")
}



#####    R/class-sf.R    #####


#' @rdname deprecated
#' @export
geom_spatial.sf <- function(data, mapping = NULL, show.legend = TRUE,
                            inherit.aes = FALSE, position = "identity", crsfrom = NA, crsto = NA,
                            attribute_table = NA, geom = NA, stat = NA, ...) {
  geom_spatial.default(methods::as(sf::st_zm(data), "Spatial"), mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position, crsfrom = crsfrom, crsto = crsto,
                       attribute_table = attribute_table, geom = geom, stat = stat, ...)
}

#' @rdname deprecated
#' @export
geom_spatial.sfc <- function(data, mapping = NULL, show.legend = TRUE,
                            inherit.aes = FALSE, position = "identity", crsfrom = NA, crsto = NA,
                            attribute_table = NA, geom = NA, stat = NA, ...) {
  geom_spatial.default(methods::as(sf::st_zm(data), "Spatial"), mapping = mapping, show.legend = show.legend,
                       inherit.aes = inherit.aes, position = position, crsfrom = crsfrom, crsto = crsto,
                       attribute_table = attribute_table, geom = geom, stat = stat, ...)
}

#' @rdname deprecated
#' @export
spatial_fortify.sf <- function(x, attrs = NA, ...) {
  spatial_fortify(methods::as(sf::st_zm(x), "Spatial"), attrs = attrs, ...)
}

#' @rdname deprecated
#' @export
spatial_fortify.sfc <- function(x, attrs = NA, ...) {
  spatial_fortify(methods::as(sf::st_zm(x), "Spatial"), attrs = attrs, ...)
}

#' @rdname deprecated
#' @export
spatial_geom.sf <- function(x) spatial_geom(methods::as(sf::st_zm(x), "Spatial"))
#' @rdname deprecated
#' @export
spatial_geom.sfc <- function(x) spatial_geom(methods::as(sf::st_zm(x), "Spatial"))

#' @rdname deprecated
#' @export
spatial_default_aes.sf <- function(x) {
  spatial_default_aes(methods::as(sf::st_zm(x), "Spatial"))
}

#' @rdname deprecated
#' @export
spatial_default_aes.sfc <- function(x) {
  spatial_default_aes(methods::as(sf::st_zm(x), "Spatial"))
}




#####    R/class-spatial.R    #####


# ---------- method definitions for objects from sp ------------

#' @importFrom ggplot2 fortify
#' @export
fortify.SpatialPoints <- function(model, data = NULL, ...) {
  coords <- sp::coordinates(model)
  data.frame(id=1:nrow(coords), long = coords[, 1], lat = coords[, 2])
}

#' @importFrom ggplot2 fortify
#' @export
fortify.SpatialPointsDataFrame <- function(model, data = NULL, ...) {
  coords <- sp::coordinates(model)
  data.frame(id=row.names(model), long = coords[, 1], lat = coords[, 2])
}

#' @importFrom ggplot2 fortify
#' @export
fortify.SpatialLines <- function(model, data, ...) {
  plyr::ldply(model@lines, fortify)
}

# ----------- spatial_fortify definitions ---------------

#' @rdname deprecated
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

#' @rdname deprecated
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

#' @rdname deprecated
#' @export
spatial_fortify.SpatialPolygonsDataFrame <- function(x, attrs = NA, ...) {
  # same as spatial lines data frame with suppressMessages
  suppressMessages(spatial_fortify.SpatialLinesDataFrame(x, attrs = attrs, ...))
}


# --------- geometry definitions ----------------

spatial_geom.SpatialPoints <- function(x) ggplot2::GeomPoint
spatial_geom.Line <- function(x) ggplot2::GeomPath
spatial_geom.Lines <- function(x) ggplot2::GeomPath
spatial_geom.SpatialLines <- function(x) ggplot2::GeomPath
spatial_geom.Polygon <- function(x) GeomPolypath
spatial_geom.Polygons <- function(x) GeomPolypath
spatial_geom.SpatialPolygons <- function(x) GeomPolypath

# ----------- default aes definitions ------------
# for items whose fortify() method produceds a group

spatial_default_aes.Lines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

spatial_default_aes.SpatialLines <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

spatial_default_aes.Polygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}

spatial_default_aes.SpatialPolygons <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat", group = ".group")
}




#####    R/geom-spatial.R    #####



#' @rdname deprecated
#' @export
geom_spatial <- function(data, mapping = NULL, ...) UseMethod("geom_spatial")

#' @export
#' @rdname deprecated
ggspatial <- function(data, mapping = NULL, ...) {
  ggplot2::ggplot() + geom_spatial(data, mapping = mapping, ...) + ggplot2::coord_map() +
    ggplot2::labs(x = "long", y = "lat")
}

#' @rdname deprecated
#' @export
geom_spatial.data.frame <- function(data, mapping = NULL, ...) {
  stop("Use stat_project to apply projections to data frame input")
}

#' @export
#' @rdname deprecated
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
  ggplot2::layer(
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

as.CRS <- function(x) UseMethod("as.CRS")

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

as.CRS.sf <- function(x) {
  sp::CRS(sf::st_crs(x)$proj4string)
}

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

bboxTransform <- function(bbox, from = 4326, to = 4326) {
  coords <- t(bbox)
  pbox <- t(xyTransform(coords[, 1], coords[, 2], from = from, to = to))
  rownames(pbox) <- c("x", "y")
  colnames(pbox) <- c("min", "max")
  pbox
}

#' @export
#' @rdname deprecated
spatial_fortify <- function(x, attrs = NULL, ...) UseMethod("spatial_fortify")

#' @rdname deprecated
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

spatial_geom <- function(x) UseMethod("spatial_geom")
spatial_geom.default <- function(x) ggplot2::GeomPoint
spatial_stat <- function(x) UseMethod("spatial_stat")
spatial_stat.default <- function(x) ggplot2::StatIdentity

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


#' @rdname deprecated
#' @export
stat_project <- function(mapping = NULL, data = NULL, crsfrom = NA, crsto = NA,
                         position = "identity", show.legend = TRUE, inherit.aes = TRUE,
                         geom = "point", ...) {
  ggplot2::layer(
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

#' @rdname deprecated
#' @export
annotation_spraster <- function(raster, interpolate = FALSE, na.value = NA) {
  if(!methods::is(raster, "Raster")) stop("Cannot use annotation_spraster with non Raster* object")
  bbox <- raster::as.matrix(raster@extent)
  raster <- raster::as.array(raster)

  # check dims
  dims <- dim(raster)
  if(length(dims) != 3) stop("Raster has incorrect dimensions: ", paste(dims, collapse = ", "))
  if(!(dims[3] %in% c(3, 4))) stop("Need a 3 or 4-band array to use annotation_spraster")

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
  if(is.na(na.value) && any(is.na(raster))) {
    # find NA cells
    na_cells <- is.na(raster[, , 1]) | is.na(raster[, , 2]) |
      is.na(raster[, , 3])

    # grid doesn't do non-finite values, so we need to set the transparency band
    # for missing cells
    if(dim(raster)[3] == 4) {
      tband <- raster[ , , 4, drop = FALSE]
    } else {
      tband <- array(1, dim(raster)[1:2])
    }

    # set alpha to NA cells to 0
    tband[na_cells] <- 0

    # bind it to the original raster
    raster <- abind::abind(raster[, , 1:3, drop = FALSE], tband)

    # set NA values to 0
    raster[is.na(raster)] <- 0
  } else {
    raster[is.na(raster)] <- na.value
  }

  # call ggplot2's annotation_raster
  ggplot2::annotation_raster(raster, bbox[1, 1], bbox[1, 2], bbox[2, 1], bbox[2, 2],
                             interpolate = interpolate)
}

#' @rdname deprecated
#' @export
geom_spraster_rgb <- function(raster, interpolate = FALSE, na.value = NA) {
  if(!methods::is(raster, "Raster")) stop("Cannot use geom_spraster_rgb with non Raster* object")

  # wraps around annotation_spraster using a blank geometry to set bounds
  bbox <- raster::as.matrix(raster@extent)
  data <- data.frame(long = bbox[1, ], lat = bbox[2, ])

  # returns a list with both layers
  list(
    ggplot2::geom_point(ggplot2::aes_string("long", "lat"), data = data, alpha = 0, inherit.aes = FALSE,
                        show.legend = FALSE),
    annotation_spraster(raster, interpolate = interpolate, na.value = na.value)
  )
}

#' @rdname deprecated
#' @export
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
    data <- data.frame(long = NA_real_, lat = NA_real_)
  } else {
    # use the bounding box as the data
    bbox <- rosm::extract_bbox(x)
    data <- data.frame(long = bbox[1, ], lat = bbox[2, ])
  }

  # return GeomTileSource layer
  ggplot2::layer(data = data, mapping = ggplot2::aes_string("long", "lat"),
                 position = "identity", geom = GeomTileSource, stat = "identity",
                 show.legend = FALSE, inherit.aes = FALSE,
                 params = list(na.rm = TRUE, zoomin = zoomin, zoom = zoom, type = type,
                               forcedownload = forcedownload,
                               cachedir = cachedir, progress = progress, quiet = quiet))
}

#' @rdname deprecated
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
GeomTileSource <- ggplot2::ggproto(
  "GeomTileSource",
  Geom,

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

