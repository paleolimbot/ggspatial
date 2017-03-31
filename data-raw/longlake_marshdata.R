# marsh data acquisition
library(rgdal)
library(rosm)

files <- list.files("data_raw", full.names=TRUE)
file.rename(files, gsub(files, pattern = " ", replacement = "", fixed=TRUE))

longlake_depthdf <- readOGR("data-raw", "LongLakeDepthSurvey")
longlake_waterdf <- readOGR("data-raw", "LongLakeMarshWaterPoly")
longlake_roadsdf <- readOGR("data-raw", "LongLakeMarshRoads")
longlake_marshdf <- readOGR("data-raw", "LongLakeMarshWetlands")
longlake_streamsdf <- readOGR("data-raw", "LongLakeMarshStreams")
longlake_buildingsdf <- readOGR("data-raw", "LongLakeMarshBuildings")

# don't save raster to disk! this causes errors
longlake_osm <- osm.raster(longlake_depthdf, type = "hikebike",
                           zoomin=-1, crop=TRUE, overwrite=TRUE)
devtools::use_data(longlake_osm, overwrite=TRUE)

devtools::use_data(longlake_marshdf, longlake_roadsdf, longlake_waterdf, longlake_depthdf,
                   longlake_streamsdf, longlake_buildingsdf, overwrite=TRUE)

# test of this
ggplot() +
  geom_spatial(longlake_waterdf, fill="lightblue") +
  geom_spatial(longlake_marshdf, fill="grey", alpha=0.5) +
  geom_spatial(longlake_streamsdf, col="lightblue") +
  geom_spatial(longlake_roadsdf, col="black") +
  geom_spatial(longlake_buildingsdf, pch=1, col="brown", size=0.25) +
  geom_spatial(longlake_depthdf, aes(col=DEPTH.M)) +
  facet_wrap(~NOTES)+
  coord_fixed()
