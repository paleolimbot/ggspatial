
# this is a revised version of GeomPolypath from the ggpolypath package
# see https://github.com/mdsumner/ggpolypath/issues/3



#' Polygons with holes in ggplot2
#'
#' This geometry used to plot polygons with holes in ggplot2 at the
#' more correctly than [geom_polygon][ggplot2::geom_polygon]; however,
#' in recent R and ggplot2 versions this is no longer needed.
#'
#' @param mapping An aesthetic mapping, created with [aes][ggplot2::aes]. The aesthetic
#'   will mostly likely need to contain a `group` mapping.
#' @param data A data.frame containing the coordinates to plot.
#' @param stat A statistic to apply (most likely "identity")
#' @param position A position to apply (most likely "identity")
#' @param na.rm Should missing coordinate be removed?
#' @param show.legend Should a legend be shown for mapped aesthetics?
#' @param inherit.aes Should aesthetics be inherited?
#' @param rule A fill rule to apply. One of "winding" or "evenodd".
#' @param ... Passed to the geom and/or stat.
#'
#' @return A ggplot2 layer
#' @export
#'
#' @examples
#' \donttest{
#' library(ggplot2)
#' load_longlake_data(which = "longlake_waterdf")
#' ggplot(df_spatial(longlake_waterdf), aes(x, y, group = piece_id)) +
#'   geom_polypath()
#' }
#'
geom_polypath <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
                           na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, rule = "winding",
                           ...) {
  if(!(rule %in% c("winding", "evenodd"))) {
    stop("geom_polypath: 'rule' must be 'evenodd', or 'winding'")
  }

  # No longer needed in recent R + ggplot2
  .Deprecated("ggplot2::geom_polygon()")

  ggplot2::layer(data = data, mapping = mapping, stat = stat, geom = GeomPolypath,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm , rule = rule, ...))
}


GeomPolypath <- ggplot2::ggproto(
  "GeomPolypath",
  ggplot2::GeomPolygon,
  extra_params = c("na.rm", "rule"),
  draw_panel = function(data, scales, coordinates, rule = "winding") {
    n <- nrow(data)
    if (n == 1) return(ggplot2::zeroGrob())

    munched <- ggplot2::coord_munch(coordinates, data, scales)
    munched <- munched[order(munched$group), ]

    # "size" became "linewidth" in ggplot 3.4.0
    if (packageVersion("ggplot2") >= "3.4.0") {
      lwd_var <- "linewidth"
    } else {
      lwd_var <- "size"
    }

    ## function to be applied to get a pathGrob for each "region"
    object_munch <- function(xmunch) {
      first_idx <- !duplicated(xmunch$group)
      first_rows <- xmunch[first_idx, ]
      grid::pathGrob(xmunch$x, xmunch$y, default.units = "native",
                     id = xmunch$group, rule = rule,
                     gp = grid::gpar(col = first_rows$colour,
                                     fill = scales::alpha(first_rows$fill, first_rows$alpha),
                                     lwd = first_rows[[lwd_var]] * ggplot2::.pt,
                                     lty = first_rows$linetype))
    }

    if (packageVersion("ggplot2") >= "3.4.0") {
      groups <- with(munched, paste(group, fill, colour, alpha, linewidth, linetype))
    } else {
      groups <- with(munched, paste(group, fill, colour, alpha, size, linetype))
    }

    ggplot2:::ggname(
      "geom_polypath",
      do.call(grid::grobTree, lapply(split(munched, groups), object_munch))
    )
  }
)
