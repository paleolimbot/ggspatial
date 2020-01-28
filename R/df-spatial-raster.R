
#' @export
df_spatial.Raster <- function(x, ..., hjust = 0.5, vjust = 0.5, na.rm = FALSE) {
  # get values in a data frame
  nrows <- x@nrows
  ncols <- x@ncols
  bbox <- raster::as.matrix(x@extent)

  fused <- cbind(
    expand.grid(x = seq_len(ncols), y = seq_len(nrows)),
    raster::values(x)
  )

  # set names to be x, y, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c("x", "y", paste0("band", seq_len(nbands)))

  if (na.rm) {
    for (i in seq_len(nbands)) {
      fused <- fused[!is.na(fused[[paste0("band", i)]]), ]
    }
  }

  # fix x and y to be physical coordinates using the bbox
  bbox_width <- bbox[1, 2] - bbox[1, 1]
  bbox_height <- bbox[2, 1] - bbox[2, 2]
  res <- raster::res(x)

  fused$x <- bbox[1, 1] + res[1] * hjust + (fused$x - 1) / ncols * bbox_width
  fused$y <- bbox[2, 1] + res[2] * vjust + (fused$y - nrows) / nrows * bbox_height

  tibble::as_tibble(fused)
}
