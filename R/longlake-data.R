
#' Load longlake test data
#'
#' @param env The environment in which to assign the objects
#'
#' @export
#'
#' @source The Nova Scotia Topographic Database
#' (\url{https://geonova.novascotia.ca/}) and
#' Open Street Map (\url{http://www.openstreetmap.org}).
#'
#' @examples
#' load_longlake_data()
#'
load_longlake_data <- function(env = parent.frame()) {

  env$longlake_depthdf <- sf::read_sf(system.file("longlake", package = "ggspatial"), "LongLakeDepthSurvey")
  env$longlake_waterdf <- sf::read_sf(system.file("longlake", package = "ggspatial"), "LongLakeMarshWaterPoly")
  env$longlake_roadsdf <- sf::read_sf(system.file("longlake", package = "ggspatial"), "LongLakeMarshRoads")
  env$longlake_marshdf <- sf::read_sf(system.file("longlake", package = "ggspatial"), "LongLakeMarshWetlands")
  env$longlake_streamsdf <- sf::read_sf(system.file("longlake", package = "ggspatial"), "LongLakeMarshStreams")
  env$longlake_buildingsdf <- sf::read_sf(system.file("longlake", package = "ggspatial"), "LongLakeMarshBuildings")
  env$longlake_osm <- raster::brick(system.file("longlake/longlake.tif", package = "ggspatial"))
  env$longlake_depth_raster <- raster::raster(system.file("longlake/longlake_depth.tif", package = "ggspatial"))

}

