
#' @export
#' @importFrom rlang !!
df_spatial.stars <- function(x, ..., na.rm = FALSE) {

  # Get n bands first
  nband <- dim(x)[3]

  # On single band we get NA
  if (is.na(nband)) nband <- 1

  # To tibble
  df <- tibble::as_tibble(as.data.frame(x, ...))


  # Handle multi-layer stars objects
  if (nband > 1) {
    names(df) <- c("x", "y", "band", "values")

    # gather is superseeded, use pivot wider
    df <- tidyr::pivot_wider(df,
                             id_cols = c("x", "y"),
                             values_from = "values",
                             names_from = "band",
                             names_prefix = "band"
    )
  }

  # Now rename
  names(df) <- c("x", "y", paste0("band", seq_len(nband)))

  # Drop NA
  if (na.rm) {
    df <- tidyr::drop_na(df)
  }

  df
}
