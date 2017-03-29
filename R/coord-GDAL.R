

coord_gdal <- function(projectionto=sp::CRS("+init=epsg:3857"),
                       projectionfrom=sp::CRS("+init=epsg:4326"), ...) {
  ggproto(NULL, CoordGDAL,
          crsfrom = projectionfrom,
          crsto = projectionto,
          params = list(...)
  )
}

CoordGDAL <- ggplot2::ggproto("CoordGDAL", ggplot2::Coord,

  transform = function(self, data, panel_params) {
    coords <- gdal_transform(data$x, data$y, panel_params$crsfrom, panel_params$crsto)
    data$x <- scales::rescale(coords[,1], 0:1, panel_params$x.proj)
    data$y <- scales::rescale(coords[,2], 0:1, panel_params$y.proj)
    browser()
    data
  },

  train = function(self, scales, params = list()) {
    # range in scale
    ranges <- list()
    for (n in c("x", "y")) {
      scale <- scales[[n]]
      limits <- self$limits[[n]]

      if (is.null(limits)) {
        range <- scale$dimension(ggplot2:::expand_default(scale))
      } else {
        range <- range(scale$transform(limits))
      }
      ranges[[n]] <- range
    }

    # Increase chances of creating valid boundary region
    grid <- expand.grid(
      x = seq(ranges$x[1], ranges$x[2], length.out = 50),
      y = seq(ranges$y[1], ranges$y[2], length.out = 50)
    )

    ret <- list(x = list(), y = list())

    # range in map
    proj <- sp::bbox(gdal_transform(grid$x, grid$y, self$crsfrom, self$crsto))
    ret$x$proj <- range(proj[,1])
    ret$y$proj <- range(proj[,2])

    for (n in c("x", "y")) {
      out <- scales[[n]]$break_info(ranges[[n]])
      ret[[n]]$range <- out$range
      ret[[n]]$major <- out$major_source
      ret[[n]]$minor <- out$minor_source
      ret[[n]]$labels <- out$labels
    }

    details <- list(
      crsfrom = self$crsfrom,
      crsto = self$crsto,
      x.range = ret$x$range, y.range = ret$y$range,
      x.proj = ret$x$proj, y.proj = ret$y$proj,
      x.major = ret$x$major, x.minor = ret$x$minor, x.labels = ret$x$labels,
      y.major = ret$y$major, y.minor = ret$y$minor, y.labels = ret$y$labels
    )
    details
  }
)

gdal_transform <- function(x, y, crsfrom, crsto) {
  # shortcut for identical crsfrom and crsto
  if(identical(crsfrom, crsto)) return(cbind(x, y))
  requireNamespace("rgdal", quietly=TRUE)
  # TODO may need to figure out NA values

  coordinates(sp::spTransform(
    sp::SpatialPoints(
      sp::coordinates(cbind(x, y)), crsfrom),
    crsto))
}


data <- expand.grid(x=-30:30, y=-10:10)

ggplot(data, aes(x, y)) + geom_point() + coord_gdal()
