
#' @export
df_spatial.SpatRaster <- function(x, ..., na.rm = FALSE) {
  # get values in a data frame
  df <- tibble::as_tibble(
    terra::as.data.frame(x,
      xy = TRUE,
      na.rm = na.rm
    )
  )
  nbands <- terra::nlyr(x)
  names(df) <- c("x", "y", paste0("band", seq_len(nbands)))

  tibble::as_tibble(df)
}
