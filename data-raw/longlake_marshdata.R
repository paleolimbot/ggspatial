# marsh data acquisition
library(rgdal)
library(rosm)

files <- list.files("data_raw", full.names=TRUE)
file.rename(files, gsub(files, pattern = " ", replacement = "", fixed=TRUE))

longlake_depthdf <- readOGR("data_raw", "LongLakeDepthSurvey")
longlake_waterdf <- readOGR("data_raw", "LongLakeMarshWater Poly")
longlake_roadsdf <- readOGR("data_raw", "LongLakeMarshRoads")
longlake_marshdf <- readOGR("data_raw", "LongLakeMarshWetlands")
longlake_streamsdf <- readOGR("data_raw", "LongLakeMarshStreams")
longlake_buildingsdf <- readOGR("data_raw", "LongLakeMarshBuildings")

longlake_osm<-osm.raster(longlake_depthdf, zoomin=-1,
                         filename="data-raw/longlake.tif", crop=TRUE, overwrite=TRUE)
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
