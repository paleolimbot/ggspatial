# osm raster implementation

StatRGB <- ggplot2::ggproto("StatRGB", ggplot2::Stat,

  compute_group = function(data, scales) {
    data$fill <- paste0("#", do.call(paste0, lapply(data[c("r", "g", "b")], as.character.hexmode, 2)))
    return(data)
  },

  required_aes = c("r", "g", "b")
)


raster2dataframe <- function(obj, crsfrom=NULL, crsto=NULL, rm.na=TRUE, rgb=NULL, crop=NULL,
                             scale=1) {
  if(is.null(crsfrom) && !is.na(rgdal::CRSargs(obj@crs))) {
    crsfrom <- obj@crs
  }
  fused <- cbind(expand.grid(x=1:obj@ncols, y=1:obj@nrows), raster::values(obj)*scale)
  bbox <- rbind(c(obj@extent@xmin, obj@extent@xmax),
                c(obj@extent@ymin, obj@extent@ymax))
  # fix x and y to be physical coordinates
  fused$x <- bbox[1,1]+(fused$x-1)/obj@ncols*(bbox[1,2]-bbox[1,1])
  fused$y <- bbox[2,1]+(fused$y-obj@nrows)/obj@nrows*(bbox[2,1]-bbox[2,2])
  if(rm.na) {
    fused <- fused[stats::complete.cases(fused),]
  }
  # make colours from rgb value
  if((length(rgb)==3) || (is.null(rgb) && length(obj@data@names) >= 3)) {
    if(is.null(rgb)) {
      rgb <- 1:3
    }
    if(length(obj@data@names) < 3) stop("Need at least 3 bands to perform rgb conversion")
    fused$fill <- paste0("#", do.call(paste0, lapply(fused[obj@data@names[rgb]],as.character.hexmode, 2)))
  }
  if(!is.null(crsto) && !is.null(crsto)) {
    rgdal::CRSargs(CRS("+init=epsg:3857")) #hack to load rgdal namespace
    newcoords <- sp::coordinates(sp::spTransform(sp::SpatialPoints(sp::coordinates(cbind(fused$x, fused$y)), crsfrom), crsto))
    fused$x <- as.numeric(newcoords[,1])
    fused$y <- as.numeric(newcoords[,2])
  }
  if(!is.null(crop)) {
    fused <- fused[fused$x >= crop[1,1] & fused$x <= crop[1, 2] &
                       fused$y >= crop[2,1] & fused$y <= crop[2,2],]
  }
  return(fused)
}

#' Get a spatial raster as a geom_raster for ggplot2
#'
#' Note that rgb and alpha is not yet supported.
#'
#' @param rastobj A Raster, RasterStack, or RasterBrick
#' @param scale The scaling to apply to the raster before applying rgb conversion or
#'   aesthetics. May need to use 1/255 for alpha.
#' @param rgb The bands for which rgb should be computed (1:3 for bands >= 3). Pass
#'   FALSE if rgb should not be converted.
#' @param band The band to be used for aesthetic should more than one be available.
#' @param aesthetic Should probably always be "fill" but could be "alpha"
#' @param ... Agruments passed on to geom_raster.
#'
#' @return A geom_raster().
#'
#' @export
#'
#' @examples
#' data(longlake_osm)
#' ggplot() + geom_spraster(longlake_osm)
#' ggplot() + geom_spraster(longlake_osm, rgb=c(3,2,1))
#' ggplot() + geom_spraster(longlake_osm, band=2)
#' ggplot() + geom_spraster(longlake_osm, band=2, aesthetic="alpha")
#'
geom_spraster <- function(rastobj, scale=1, rgb=NULL, band=NULL,
                          aesthetic="fill", show.legend=TRUE, ...) {
  if(!("raster" %in% rownames(utils::installed.packages()))) {
    stop("package 'raster' must be installed for call to geom_spraster()")
  }
  df <- raster2dataframe(rastobj, rgb=FALSE, scale=scale)
  bands <- names(df)[!(names(df) %in% c("x", "y"))]
  message("Loaded ", class(rastobj), " with ", length(bands), " bands")
  if(is.null(band) && (length(bands) >= 3)) {
    if(is.null(rgb)) {
      rgb <- 1:3
    } else if(length(rgb) != 3) {
      stop("rgb must be a vector of length 3")
    }
    mapping <- aes_string(x="x", y="y", r=bands[rgb[1]], g=bands[rgb[2]], b=bands[rgb[3]])
    stat=StatRGB
    show.legend <- FALSE
  } else {
    if(is.null(band)) {
      band <- 1
    }
    args <- list(x="x", y="y")
    args[[aesthetic]] <- bands[band]
    mapping <- do.call(ggplot2::aes_string, args)
    stat="identity"
  }
  class(mapping) <- "uneval"
  ggplot2::layer(
    stat = stat, data = df, mapping = mapping, geom = "raster",
    show.legend = FALSE, inherit.aes = FALSE, position = "identity", ...)
}

