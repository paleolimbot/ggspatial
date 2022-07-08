
#' Load longlake test data
#'
#' @param env The environment in which to assign the objects
#' @param vector_format,raster_format The format in which objects should be loaded
#' @param which An optional subset of objects to be loaded
#'
#' @export
#'
#' @source The Nova Scotia Topographic Database
#' (<https://geonova.novascotia.ca/>) and
#' Open Street Map (<https://www.openstreetmap.org/>).
#'
#' @examples
#' load_longlake_data(which = "longlake_waterdf")
#'
load_longlake_data <- function(env = parent.frame(), vector_format = c("sf", "sp"),
                               raster_format = c("raster", "stars", "stars_proxy", "terra"),
                               which = NULL) {
  raster_format <- match.arg(raster_format)
  vector_format <- match.arg(vector_format)

  longlake_folder <- system.file("longlake", package = "ggspatial")

  vector_layers <- c(
    "LongLakeDepthSurvey.shp" = "longlake_depthdf",
    "LongLakeMarshWaterPoly.shp" = "longlake_waterdf",
    "LongLakeMarshRoads.shp" = "longlake_roadsdf",
    "LongLakeMarshWetlands.gpkg" = " longlake_marshdf",
    "LongLakeMarshStreams.shp" = "longlake_streamsdf",
    "LongLakeMarshBuildings.shp" = "longlake_buildingsdf"
  )

  raster_layers <- c(
    "longlake_depth.tif" = "longlake_depth_raster",
    "longlake.tif" = "longlake_osm"
  )

  if(!is.null(which)) {
    vector_layers <- vector_layers[vector_layers %in% which]
    raster_layers <- raster_layers[raster_layers %in% which]
  }

  if(vector_format == "sf") {
    for(i in seq_along(vector_layers)) {
      env[[vector_layers[i]]] <- sf::read_sf(file.path(longlake_folder, names(vector_layers)[i]))
    }
  } else if(vector_format == "sp") {
    loadNamespace("sp")
    for(i in seq_along(vector_layers)) {
      env[[vector_layers[i]]] <- suppressWarnings(
        methods::as(
          sf::st_zm(sf::read_sf(file.path(longlake_folder, names(vector_layers)[i]))),
          "Spatial"
        )
      )
    }
  }

  if(raster_format == "raster") {
    if("longlake_osm" %in% raster_layers) {
      env$longlake_osm <- raster::brick(file.path(longlake_folder, "longlake.tif"))
    }

    if("longlake_depth_raster" %in% raster_layers) {
      env$longlake_depth_raster <- raster::raster(file.path(longlake_folder, "longlake_depth.tif"))
    }
  } else if(raster_format == "stars") {
    for(i in seq_along(raster_layers)) {
      env[[raster_layers[i]]] <- stars::read_stars(file.path(longlake_folder, names(raster_layers)[i]))
    }
  } else if(raster_format == "stars_proxy") {
    for(i in seq_along(raster_layers)) {
      env[[raster_layers[i]]] <- stars::read_stars(file.path(longlake_folder, names(raster_layers)[i]), proxy = TRUE)
    }
  } else if (raster_format == "terra"){
    for(i in seq_along(raster_layers)) {
      env[[raster_layers[i]]] <- terra::rast(file.path(longlake_folder, names(raster_layers)[i]))
    }

  }
}

