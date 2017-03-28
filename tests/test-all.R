
library(ggspatial)

data("longlake_buildingsdf")
data("longlake_depthdf")
data("longlake_marshdf")
data("longlake_waterdf")
data("longlake_roadsdf")
data("longlake_streamsdf")
data("longlake_osm")

# test a data.frame
depthsdf <- data.frame(longlake_depthdf)

# three cases
ggplot(depthsdf, aes(LON, LAT)) + geom_spatial()
ggplot() + geom_spatial(depthsdf, aes(LON, LAT))
ggplot(NULL, aes(LON, LAT)) + geom_spatial(depthsdf)

# check aesthetic mapping
ggplot(depthsdf, aes(LON, LAT, col=DEPTH)) + geom_spatial()
ggplot() + geom_spatial(depthsdf, aes(LON, LAT, col=DEPTH))

# check show.legend
ggplot(depthsdf, aes(LON, LAT, col=DEPTH)) + geom_spatial(show.legend = FALSE)

# check inherit.aes
ggplot(NULL, aes(col=DEPTH)) +
  geom_spatial(depthsdf, aes(LON, LAT), inherit.aes=FALSE)


# check SpatialPoints
spoints <- SpatialPoints(longlake_depthdf, proj4string = longlake_depthdf@proj4string)
ggplot() + geom_spatial(spoints)

# check SpatialPointsDataFrame
ggplot() + geom_spatial(longlake_depthdf)

# check SpatialLines
splines <- SpatialLines(longlake_roadsdf@lines, proj4string = longlake_roadsdf@proj4string)
ggplot() + geom_spatial(splines)

# check SpatialLinesDataFrame
ggplot() + geom_spatial(longlake_roadsdf)

# check SpatialPolygons
spoly <- SpatialPolygons(longlake_waterdf@polygons, proj4string = longlake_waterdf@proj4string)
ggplot() + geom_spatial(spoly)

# check SpatialPolygonsDataFrame
ggplot() + geom_spatial(longlake_waterdf)


 ggplot() +
   geom_spatial(longlake_waterdf, fill="lightblue") +
   geom_spatial(longlake_marshdf, fill="grey", alpha=0.5) +
   geom_spatial(longlake_streamsdf, col="lightblue") +
   geom_spatial(longlake_roadsdf, col="black") +
   geom_spatial(longlake_buildingsdf, pch=18, col="brown", size=0.25) +
   geom_spatial(longlake_depthdf, aes(col=DEPTH.M)) +
   facet_wrap(~NOTES)+
   coord_map()
