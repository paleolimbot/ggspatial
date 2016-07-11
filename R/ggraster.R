# osm raster implementation

raster2dataframe <- function(obj, crsfrom=NULL, crsto=NULL, rm.na=TRUE) {
  if(is.null(crsfrom) && !is.na(rgdal::CRSargs(obj@crs))) {
    crsfrom <- obj@crs
  }
  fused <- cbind(expand.grid(x=1:obj@ncols, y=1:obj@nrows), as.matrix(obj))
  bbox <- cbind(c(obj@extent@xmin, obj@extent@xmax),
                c(obj@extent@ymin, obj@extent@ymax))
  # fix x and y to be physical coordinates
  fused$x <- bbox[1,1]+(fused$x-1)/obj@ncols*(bbox[1,2]-bbox[1,1])
  fused$y <- bbox[2,1]+(fused$y-obj@nrows)/obj@nrows*(bbox[2,1]-bbox[2,2])
  if(rm.na) {
    fused <- fused[complete.cases(fused),]
  }
  # make colours from rgb value
  fused$fill <- paste0("#", do.call(paste0, lapply(fused[obj@data@names],as.character.hexmode, 2)))
  if(!is.null(crsto) && !is.null(crsto)) {
    rgdal::CRSargs(CRS("+init=epsg:3857")) #hack to load rgdal namespace
    newcoords <- sp::coordinates(
      sp::spTransform(sp::SpatialPoints(sp::coordinates(fused$x, fused$y), crsfrom),
                                 crsto))
    fused$x <- newcoords[,1]
    fused$y <- newcoords[,2]
  }

  return(fused)
}
