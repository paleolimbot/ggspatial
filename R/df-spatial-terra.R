
#' @export
df_spatial.SpatRaster <- function(x, ..., na.rm = FALSE) {
  # get values in a data frame
  ext <- terra::ext(x)
  df <- tibble::as_tibble(terra::extract(x, ext, xy = TRUE, cells = TRUE, factors = TRUE))


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

  tibble::as_tibble(fused)
}
