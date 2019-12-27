
library(ggplot2)
library(grid)

# in real life, this would be a reference to a large (scary) raster file
big_scary_raster <- tibble::tibble(raster = list(matrix(1:9, nrow = 3)))

# -------- typical ggplot2 approach --------

StatMatrixList <- ggproto(
  "StatMatrixList",
  Stat,

  required_aes = "raster",
  default_aes = ggplot2::aes(fill = stat(z)),

  compute_layer = function(self, data, params, layout) {
    data$raster <- lapply(data$raster, function(x) {
      df <- reshape2::melt(x)
      names(df) <- c("x", "y", "z")
      df
    })

    tidyr::unnest(data, .data$raster)
  }
)

ggplot(big_scary_raster, aes(raster = raster)) +
  geom_raster(stat = StatMatrixList, hjust = 0, vjust = 0)


# ------ lazy ggplot2 approach ---------

StatLazyMatrixList <- ggproto(
  "StatLazyMatrixList",
  StatMatrixList,

  compute_layer = function(self, data, params, layout) {
    # only return limits in the stat (these are usually cached in the raster file,
    # so the raster doesn't need to be loaded). Scales get trained based on the
    # result of this function.
    data$limits <- lapply(data$raster, function(raster) {
      tibble::tibble(
        x = c(0, ncol(raster)),
        y = c(0, nrow(raster)),
        z = range(raster)
      )
    })

    tidyr::unnest(data, .data$limits, .drop = FALSE)
  }
)

GeomLazyRaster <- ggproto(
  "GeomLazyRaster",
  Geom,
  required_aesthetics = "raster",

  default_aes = ggplot2::aes(alpha = "__default_alpha__", fill = "__default_fill__"),

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coordinates) {
    # this is a super crazy hack
    # but there is no other way to scale objects from the draw function at build time (?)
    scales <- NULL
    for(i in 1:20) {
      env <- parent.frame(i)
      if("plot" %in% names(env) && "scales" %in% names(env$plot) && inherits(env$plot$scales, "ScalesList")) {
        scales <- env$plot$scales
        break
      }
    }
    if(is.null(scales)) stop("@paleolimbot's hack to get the ScalesList from Geom$draw_panel() has failed :'(")
    fill_scale <- scales$get_scales("fill")
    alpha_scale <- scales$get_scales("alpha")

    if(all(data$alpha == "__default_alpha__")) {
      # default
      alpha <- function(x) 1
    } else if(length(unique(data$alpha)) == 1) {
      # set (or mapped but constant)
      alpha <- function(x) unique(data$alpha)
    } else if(!is.null(alpha_scale)) {
      # mapped
      alpha <- alpha_scale$map
    } else {
      stop("Don't know how to compute 'alpha'")
    }

    if(all(data$fill == "__default_fill__")) {
      # default
      fill <- function(x) 1
    } else if(length(unique(data$fill)) == 1) {
      # set (or mapped but constant)
      fill <- function(x) unique(data$fill)
    } else if(!is.null(fill_scale)) {
      # mapped
      fill <- fill_scale$map
    } else {
      stop("Don't know how to compute 'fill'")
    }

    gTree(
      raster = data$raster[[1]],
      fill = fill,
      alpha = alpha,
      coordinates = coordinates,
      panel_params = panel_params,
      cl = "lazy_raster_grob"
    )
  }
)


geom_lazy_raster <- function(mapping = NULL, data = NULL, stat = StatLazyMatrixList,
                             ..., inherit.aes = TRUE, show.legend = NA) {
  layer(
    geom = GeomLazyRaster,
    stat = stat,
    data = data,
    mapping = mapping,
    position = "identity",
    params = list(...),
    inherit.aes = inherit.aes,
    show.legend = show.legend
  )
}

makeContext.lazy_raster_grob <- function(x) {
  # here it's possible to determine height and width in inches
  # getting DPI from the graphics device may not be possible,
  # but can always fall back on a user-specified minimum

  # projection + resampling would happen here

  # apply the aesthetics
  colors <- x$fill(x$raster)
  alpha <- x$alpha(x$raster)
  colors <- paste0(colors, as.character.hexmode(scales::rescale(alpha, from = c(0, 1), to = c(0, 255))))
  dim(colors) <- dim(x$raster)

  # map the coordinates
  corners <- data.frame(x = c(0, ncol(x$raster)), y = c(0, nrow(x$raster)))
  corners_trans <- x$coordinates$transform(corners, x$panel_params)
  x_rng <- range(corners_trans$x, na.rm = TRUE)
  y_rng <- range(corners_trans$y, na.rm = TRUE)

  setChildren(x, gList(rasterGrob(
    # there is an axis irregularity between what we think of as rows
    # and what grid thinks of as rows
    aperm(colors, c(2, 1))[nrow(colors):1,],
    x = x_rng[1], y = y_rng[1],
    height = diff(y_rng), width = diff(x_rng),
    default.units = "native",
    interpolate = FALSE,
    hjust = 0,
    vjust = 0
  )))
}

ggplot(big_scary_raster, aes(raster = raster)) +
  geom_lazy_raster()







