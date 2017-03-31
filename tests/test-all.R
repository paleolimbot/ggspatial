
library(ggspatial)

data("longlake_buildingsdf")
data("longlake_depthdf")
data("longlake_marshdf")
data("longlake_waterdf")
data("longlake_roadsdf")
data("longlake_streamsdf")
data("longlake_osm")

library(mapproj) # library this to keep R CMD check from yelling at me

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

# check shortcut ggspatial()
ggspatial(longlake_waterdf)

# check set/mapped aesthetic combinations

# points
ggspatial(longlake_depthdf, aes(col = DEPTH))
ggspatial(longlake_depthdf, aes(alpha = DEPTH))
ggspatial(longlake_depthdf, aes(col = DEPTH), alpha = 0.5)
ggspatial(longlake_depthdf, aes(alpha = DEPTH), col = "red")

# lines
ggspatial(longlake_streamsdf, aes(col = factor(OBJECTID)))
ggspatial(longlake_streamsdf, aes(alpha = OBJECTID))
ggspatial(longlake_streamsdf, aes(col = factor(OBJECTID)), lty = 2)
ggspatial(longlake_streamsdf, aes(alpha = OBJECTID), col = "red")

# polygons
ggspatial(longlake_waterdf, aes(col = label)) # outline
ggspatial(longlake_waterdf, aes(lty = label), col = "black", fill = "red") # outline
ggspatial(longlake_waterdf, aes(fill = label)) # fill
ggspatial(longlake_waterdf, aes(fill = label), col = "red") # fill with different outline color

# check mapped outlines
ggspatial(longlake_waterdf, aes(col = id), fill = "white") # outline with different fill color
ggspatial(longlake_waterdf, aes(lty = label), fill = "white", col = "black") # outline with different fill color
ggspatial(longlake_waterdf, aes(lwd = as.numeric(id)), fill = "white", col = "black")

# check mapped alpha
ggspatial(longlake_waterdf, aes(alpha = id), fill = "white") # outline with different fill color

# check fill = NA
ggspatial(longlake_waterdf, fill = NA, col = "black")

# check fill = NA with mapped outlines
ggspatial(longlake_waterdf, aes(col = label), fill = NA)
ggspatial(longlake_waterdf, aes(lty = label), fill = NA, col = "black")
ggspatial(longlake_waterdf, aes(lwd = as.numeric(id)), fill = NA, col = "black")


# check final plot
ggspatial(longlake_waterdf, fill="lightblue") +
   geom_spatial(longlake_marshdf, fill="grey", alpha=0.5) +
   geom_spatial(longlake_streamsdf, col="lightblue") +
   geom_spatial(longlake_roadsdf, col="black") +
   geom_spatial(longlake_buildingsdf, pch=18, col="brown", size=0.25) +
   geom_spatial(longlake_depthdf, aes(col=DEPTH.M)) +
   facet_wrap(~NOTES)+
   coord_map()
