#####    R/class-raster.R    #####


#' Deprecated functions
#'
#' These functions are deprecated and should not be used.
#'
#' @param data,mapping,geom,stat,position,show.legend,inherit.aes deprecated
#' @param crsfrom,crsto,attribute_table deprecated
#' @param format,attrs,x deprecated
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
