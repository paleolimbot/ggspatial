

# fortify for spatial polygons or spatial polygons data frame
# adapted from

fortify_pg_fixfeature <- function(df) {
  ringstarts <- which(!duplicated(df$group) & df$hole)
  if(length(ringstarts) < 2) {
    return(df)
  } else {
    ringstarts <- c(ringstarts, nrow(df))
    indicies <- c(1:(ringstarts[2]-1), do.call(c, lapply(2:(length(ringstarts)-1), function(x) {
        c(1, ringstarts[x]:(ringstarts[x+1]-1))
    })), nrow(df))
    return(df[indicies,])
  }
}


fortify_SpatialPolygons <- function(x, ...) {
  id <- NULL; rm(id); . <- NULL; rm(.)
  df <- ggplot2::fortify(x, ...)
  dplyr::do(dplyr::group_by(df, id), fortify_pg_fixfeature(.))
}
