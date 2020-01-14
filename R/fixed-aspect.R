
#' Enforce a plot aspect ratio
#'
#' When using a fixed-aspect coordinate system, [fixed_plot_aspect()] expands
#' either the width or height of the plot to ensure that the output
#' has dimensions that make sense. This is a useful workaround for
#' getting reasonable-shaped plots when using [ggplot2::coord_sf()]
#' or [ggplot2::coord_fixed()] when the data happen to be
#' aligned vertically or horizontally.
#'
#' @param ratio The desired aspect ratio (width / height)
#'
#' @return A [ggplot2::layer()] that can be added to  a [ggplot2::ggplot()].
#' @export
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(x =  0:5, y =  seq(0, 10, length.out = 6))
#' ggplot(df, aes(x, y)) +
#'   geom_point() +
#'   fixed_plot_aspect(ratio = 1) +
#'   coord_fixed()
#'
fixed_plot_aspect <- function(ratio = 1) {
  structure(
    list(ratio = ratio),
    class = "fixed_plot_aspect"
  )
}

#' @export
#' @importFrom ggplot2 ggplot_add
ggplot_add.fixed_plot_aspect <- function(object, plot, object_name) {
  plot$fixed_aspect_ratio <- object$ratio
  class(plot) <- unique(c("gg_fixed_plot_aspect", class(plot)))
  plot
}

#' @export
#' @importFrom ggplot2 ggplot_build
ggplot_build.gg_fixed_plot_aspect <- function(plot) {
  # dynamically subclass the coordinate system to modify the scales prior to panel
  # params (the last possible moment)
  # you'd think this was possible some other way, but until layers have access to
  # all the scales, it isn't!
  coord_super <- plot$coordinates
  desired_aspect <- plot$fixed_aspect_ratio

  plot$coordinates <- ggplot2::ggproto(
    NULL, coord_super,
    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      new_bounds <- adjust_aspect(
        scale_x$get_limits(),
        scale_y$get_limits(),
        desired_aspect = desired_aspect
      )

      if (!is.null(new_bounds$xlim)) scale_x$train(new_bounds$xlim)
      if (!is.null(new_bounds$ylim)) scale_y$train(new_bounds$ylim)

      ggplot2::ggproto_parent(coord_super, self)$setup_panel_params(scale_x, scale_y, params)
    }
  )

  class(plot) <- setdiff(class(plot), "gg_fixed_plot_aspect")
  ggplot_build(plot)
}


adjust_aspect <- function(xlim, ylim, desired_aspect = 1) {
  # detect case for all non-finite data
  if (!all(is.finite(c(xlim, ylim)))) {
    return(list(xlim = NULL, ylim = NULL))
  }

  width <- diff(xlim)
  height <- diff(ylim)
  current_aspect <- width / height

  if (current_aspect > desired_aspect) {
    ymid <- ylim[1] + height / 2
    new_height <- height * current_aspect / desired_aspect
    plot_bounds <- list(
      xlim = xlim,
      ylim = c(
        ymid - new_height / 2,
        ymid + new_height / 2
      )
    )
  } else {
    xmid <- xlim[1] + width / 2
    new_width <- width * desired_aspect / current_aspect
    plot_bounds <- list(
      xlim = c(
        xmid - new_width / 2,
        xmid + new_width / 2
      ),
      ylim = ylim
    )
  }

  plot_bounds
}
