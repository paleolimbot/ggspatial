
#' @export
df_spatial.SpatRaster <- function(x, ..., na.rm = FALSE) {
  # get values in a data frame
  ext <- terra::ext(x)
  df <- terra::extract(x, ext, xy = TRUE, cells = TRUE, factors = TRUE)

  levels(df$cell)

  # Get bands with na.rm
  bands_narm <- terra::as.data.frame(x, cells = TRUE, na.rm = na.rm)

  # Clean df
  df <- df[df$cell %in% bands_narm$cell, ]

  # Prepare to rename
  xy <- df[, c("x", "y")]
  bands <- df[, !names(df) %in% c("x", "y", "cell")]


  fused <- cbind(
    xy,
    bands
  )

  # set names to be x, y, band1, band2, ...
  nbands <- ncol(fused) - 2
  names(fused) <- c("x", "y", paste0("band", seq_len(nbands)))
  # Ensure that a factor raster's values stay factors in `fused`
  # data.frame, taking levels from second column of RAT
  
  # Commented: is this needed on terra?
  # Anyway is adapted to terra funs 
  # if (any(terra::is.factor(x))) {
  #  band_names <- names(fused)[-1:-2]
  #  for (i in seq_along(band_names)) {
  #    if (terra::is.factor(x[[i]])) {
  #      rat <- terra::levels(x[[i]])[[1]]
  #      fused[[band_names[i]]] <-
  #        factor(fused[[band_names[i]]], levels = rat[, 1], labels = rat[, 2])
  #    }
  #  }
  # }

  tibble::as_tibble(fused)
}
