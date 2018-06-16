
#' Statistic to create RGB fill values
#'
#' Usually used on conjunction with \link[ggplot2]{geom_raster} or
#' \link{geom_spatial.Raster}. This function appears to work, however
#' it is slow, and in general, only good for very small datasets.
#'
#' @param mapping A mapping created with \link[ggplot2]{aes}
#' @param data A data.frame
#' @param ... Passed to \link[ggplot2]{geom_raster}
#' @param limits_red Data limits from which to scale red values. Use NULL to
#'   perform no transformation, or NA to use the data value without transformation.
#' @param limits_green Data limits from which to scale green values
#' @param limits_blue Data limits from which to scale blue values
#' @param limits_alpha Data limits from which to scale alpha values
#'
#' @return A ggplot2 layer object
#' @export
#'
stat_rgba <- function(mapping = NULL, data = NULL, ..., limits_red = NA, limits_green = NA,
                      limits_blue = NA, limits_alpha = NA) {
  # return a list, since this stat is meaningless without identity scales
  # for fill and alpha

  list(
    ggplot2::geom_raster(mapping = mapping, data = data, stat = StatRgba,
                         limits_red = limits_red,
                         limits_green = limits_green, limits_blue = limits_blue,
                         limits_alpha = limits_alpha, ...),
    ggplot2::scale_alpha_identity(),
    ggplot2::scale_fill_identity()
  )
}


StatRgba <- ggplot2::ggproto("StatRgba", ggplot2::Stat,

   compute_group = function(data, scales, limits_red = NA, limits_green = NA,
                            limits_blue = NA, limits_alpha = NA) {

     # rescale data between 0 and 255 for RGB, 0 and 1 for alpha
     data$red <- rescale_item(data$red, limits_red, to = c(0, 255))
     data$green <- rescale_item(data$green, limits_green, to = c(0, 255))
     data$blue <- rescale_item(data$blue, limits_blue, to = c(0, 255))
     data$alpha <- rescale_item(data$alpha, limits_alpha, to = c(0, 1))

     not_finite <- !complete.cases(data[c("red", "green", "blue", "alpha")])

     data$rgb <- paste0("#", do.call(paste0, lapply(data[c("red", "green", "blue")],
                                                    as.character.hexmode, 2)))
     data$rgb[not_finite] <- NA

     data
   },

   required_aes = c("red", "green", "blue", "alpha"),

   default_aes = aes(fill = ..rgb..)
)

# this rescale item appears in StatRgba
rescale_item <- function(values, limits, to, identity = 0) {
  # need a zero range value (probably 0)
  if(scales::zero_range(range(values, finite = TRUE))) {
    # if single value, clamp to range
    val <- range(values, finite = TRUE)[1]
    if(val > to[2]) {
      val <- to[2]
    } else if(val < to[1]) {
      val <- to[1]
    }
    rep_len(val, length.out = length(values))
  } else if(is.null(limits)) {
    # null means leave alone, but still need to censor
    scales::censor(values, to)
  } else if(identical(limits, NA)) {
    # NA means scale to min/max
    scales::rescale(values, to = to)
  } else if(length(limits) == 2) {
    scales::rescale(values, from = limits, to = to)
  } else {
    stop("Don't know how to rescale with limits ", values)
  }
}
