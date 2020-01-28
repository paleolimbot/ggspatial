
#' @export
#' @importFrom rlang !!
df_spatial.stars <- function(x, ...) {
  names <- names(x)

  df <- as.data.frame(x, ...)
  gathered <- tidyr::gather(df, key = "value_name", value = "band1", !!names)
  if("band" %in% colnames(gathered)) {
    gathered$band <- paste0("band", gathered$band)
    tibble::as_tibble(tidyr::spread(gathered, key = "band", value = "band1"))
  } else {
    tibble::as_tibble(gathered)
  }
}
