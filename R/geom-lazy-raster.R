
geom_spraster <- function(rastobj, scale=1, rgb=NULL, band=NULL,
                          aesthetic="fill", show.legend=TRUE, ...) {
  layer(data = data.frame(x=1), mapping = NULL, position = "identity",
        geom = GeomSpatialRaster, stat = StatIdentity,
        params = list(raster = rastobj))
}

GeomSpatialRaster <- ggplot2::ggproto("GeomSpatialRaster", Geom,

  extra_params = c("na.rm"),

  draw_panel = function(data, scales, coordinates, raster=NULL) {

    if(!is.null(coordinates$projection)) {
      xlim <- scales$x.proj
      ylim <- scales$y.proj
      tocrs <- ggprojections[[coordinates$projection]]
      browser()
      cropped <- raster_proj(raster, tocrs, rbind(xlim, ylim))

      browser()
    } else {
      # xlim/ylim are in same coordinates as bounds
      xlim <- scales$x.range
      ylim <- scales$y.range

      bounds <- as.matrix(raster@extent)
      bounds_x <- bounds[1, ]
      bounds_y <- bounds[2, ]

      browser()
    }
  }
)

ggprojections <- list(mercator = sp::CRS("+init=epsg:3395"))


raster_proj <- function(osm.raster, projection, crop.bbox=NULL) {

  rstackproj <- raster::projectRaster(osm.raster, crs = projection)
  rstackproj@data@values[rstackproj@data@values > 255 ] <- 255
  rstackproj@data@values[rstackproj@data@values < 0 ] <- 0

  if(!is.null(crop.bbox)) {
    k <- min(c(0.052 * (crop.bbox[2,2] - crop.bbox[2,1]),
               0.052 * (crop.bbox[1,2] - crop.bbox[1,1])))
    crop.bbox[2,2] <- crop.bbox[2,2] + k
    crop.bbox[1,1] <- crop.bbox[1,1] - k
    crop.bbox[2,1] <- crop.bbox[2,1] - k
    crop.bbox[1,2] <- crop.bbox[1,2] + k

    return(raster::crop(rstackproj, crop.bbox))
  } else {
    return(rstackproj)
  }
}

bboxns <- prettymapr::zoombbox(prettymapr::searchbbox("long lake nova scotia"), 0.8)

ggplot(data.frame(x=bboxns[1,], y=bboxns[2,])) +
  geom_spraster(longlake_osm, raster = griddata) +
  geom_point(aes(x, y)) +
  coord_map()


