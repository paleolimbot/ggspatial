
context("deprecated function tests")

test_that("deprecated tests run without error", {
  load_longlake_data()

  longlake_buildingsdf <- as(longlake_buildingsdf %>% sf::st_zm(), "Spatial")
  longlake_depthdf <- as(longlake_depthdf %>% sf::st_zm(), "Spatial")
  longlake_marshdf <- as(longlake_marshdf %>% sf::st_zm(), "Spatial")
  longlake_waterdf <- as(longlake_waterdf %>% sf::st_zm(), "Spatial")
  longlake_roadsdf <- as(longlake_roadsdf %>% sf::st_zm(), "Spatial")
  longlake_streamsdf <- as(longlake_streamsdf %>% sf::st_zm(), "Spatial")
  # long_lake_osm is already a raster

  # mapproj needs to be installed for
  mapproj::mapproject(0, 0)

  # check SpatialPoints
  spoints <- sp::SpatialPoints(longlake_depthdf, proj4string = longlake_depthdf@proj4string)
  print(ggplot() + geom_spatial(spoints))

  # check SpatialPointsDataFrame
  print(ggplot() + geom_spatial(longlake_depthdf))

  # check SpatialLines
  splines <- sp::SpatialLines(longlake_roadsdf@lines, proj4string = longlake_roadsdf@proj4string)
  print(ggplot() + geom_spatial(splines))

  # check SpatialLinesDataFrame
  print(ggplot() + geom_spatial(longlake_roadsdf))

  # check SpatialPolygons
  spoly <- sp::SpatialPolygons(longlake_waterdf@polygons, proj4string = longlake_waterdf@proj4string)
  print(ggplot() + geom_spatial(spoly))

  # check SpatialPolygonsDataFrame
  ggplot() + geom_spatial(longlake_waterdf)

  # check shortcut ggspatial()
  print(ggspatial(longlake_waterdf))

  # check set/mapped aesthetic combinations

  # points
  print(ggspatial(longlake_depthdf, aes(col = DEPTH)))
  print(ggspatial(longlake_depthdf, aes(alpha = DEPTH)))
  print(ggspatial(longlake_depthdf, aes(col = DEPTH), alpha = 0.5))
  print(ggspatial(longlake_depthdf, aes(alpha = DEPTH), col = "red"))

  # lines
  print(ggspatial(longlake_streamsdf, aes(col = factor(OBJECTID))))
  print(ggspatial(longlake_streamsdf, aes(alpha = as.numeric(OBJECTID))))
  print(ggspatial(longlake_streamsdf, aes(col = factor(OBJECTID)), lty = 2))
  print(ggspatial(longlake_streamsdf, aes(alpha = as.numeric(OBJECTID)), col = "red"))

  # polygons
  print(ggspatial(longlake_waterdf, aes(col = label))) # outline
  print(ggspatial(longlake_waterdf, aes(lty = label), col = "black", fill = "red")) # outline
  print(ggspatial(longlake_waterdf, aes(fill = label))) # fill
  print(ggspatial(longlake_waterdf, aes(fill = label), col = "red")) # fill with different outline color

  # check mapped outlines
  print(ggspatial(longlake_waterdf, aes(col = .id), fill = "white")) # outline with different fill color
  print(ggspatial(longlake_waterdf, aes(lty = label), fill = "white", col = "black")) # outline with different fill color
  print(ggspatial(longlake_waterdf, aes(lwd = as.numeric(.id)), fill = "white", col = "black"))

  # check mapped alpha
  print(ggspatial(longlake_waterdf, aes(alpha = as.numeric(.id)), fill = "white")) # outline with different fill color

  # check fill = NA
  print(ggspatial(longlake_waterdf, fill = NA, col = "black"))

  # check fill = NA with mapped outlines
  print(ggspatial(longlake_waterdf, aes(col = label), fill = NA))
  print(ggspatial(longlake_waterdf, aes(lty = label), fill = NA, col = "black"))
  print(ggspatial(longlake_waterdf, aes(lwd = as.numeric(.id)), fill = NA, col = "black"))


  # check final plot
  print(
    ggspatial(longlake_waterdf, fill="lightblue") +
      geom_spatial(longlake_marshdf, fill="grey", alpha=0.5) +
      geom_spatial(longlake_streamsdf, col="lightblue") +
      geom_spatial(longlake_roadsdf, col="black") +
      geom_spatial(longlake_buildingsdf, pch=18, col="brown", size=0.25) +
      geom_spatial(longlake_depthdf, aes(col=DEPTH.M)) +
      facet_wrap(~NOTES)+
      coord_map()
  )

  # check spatial fortify of spatial objects with non-standard row.names
  # SpatialPoints
  nonstandard_sp <- longlake_depthdf
  row.names(nonstandard_sp) <- sample(nrow(nonstandard_sp), replace = FALSE)
  head(spatial_fortify(nonstandard_sp))

  # SpatialPolygons
  nonstandard_sp <- longlake_waterdf
  row.names(nonstandard_sp) <- as.character(sample(nrow(nonstandard_sp), replace = FALSE))
  head(spatial_fortify(nonstandard_sp))
  print(ggspatial(nonstandard_sp, aes(fill = label)))

  # empty tests
  expect_true(TRUE)
})

test_that("old stat project works", {
  load_longlake_data()
  expect_silent(
    print(
      ggplot() +
        stat_project(aes(LON, LAT, col = DEPTH.M), data = longlake_depthdf, crsfrom = 4326, crsto = 26920) +
        labs(caption = "These coordinates should be in UTM not lat/lon")
    )
  )
})

test_that("deprecated geom_spatial works with sf objects", {
  load_longlake_data()
  print(ggplot() + geom_spatial(longlake_waterdf))
  print(ggplot() + geom_spatial(longlake_waterdf$geometry))
})

test_that("deprecated spraster functions work", {
  load_longlake_data()
  print(
    ggplot() +
      geom_spraster_rgb(longlake_osm) +
      coord_fixed() +
      labs(caption = "This should be a picture of long lake with grey around the edge")
  )

  print(
    ggplot() +
      geom_spatial(longlake_depth_raster, aes(fill = band1)) +
      coord_fixed() +
      labs(caption = "This should be a depth raster of long lake with a legend")
  )

  print(ggraster(longlake_depth_raster, aes(fill = band1)))

  expect_true(TRUE)
})

test_that("deprecated OSM tiles work", {
  load_longlake_data()
  expect_message(
    print(
      ggplot() +
        geom_osm(zoom = 15, type = "osm", cachedir = system.file("rosm.cache", package = "ggspatial")) +
        geom_spatial(longlake_depthdf, crsto = 4326) +
        coord_map()
    ),
    "Zoom: 15"
  )

  expect_message(
    print(
      ggosm(zoom = 13, type = "osm", cachedir = system.file("rosm.cache", package = "ggspatial")) +
        geom_spatial(longlake_waterdf, crsto = 4326) +
        geom_spatial(longlake_roadsdf, crsto = 4326) +
        geom_spatial(longlake_streamsdf, crsto = 4326) +
        geom_spatial(longlake_buildingsdf, crsto = 4326)
    ),
    "Zoom: 13"
  )
})
