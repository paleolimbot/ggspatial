
# this is a revised version of GeomPolypath from the ggpolypath package
# see https://github.com/mdsumner/ggpolypath/issues/3

GeomPolypath2 <- ggproto(
  "GeomPolypath",
  GeomPolygon,
  extra_params = c("na.rm", "rule"),
  draw_panel = function(data, scales, coordinates, rule) {
    n <- nrow(data)
    if (n == 1)
      return(zeroGrob())

    munched <- coord_munch(coordinates, data, scales)
    munched <- munched[order(munched$group), ]
    ## function to be applied to get a pathGrob for each "region"
    object_munch <- function(xmunch) {
      first_idx <- !duplicated(xmunch$group)
      first_rows <- xmunch[first_idx, ]
      grid::pathGrob(xmunch$x, xmunch$y, default.units = "native",
                     id = xmunch$group, rule = rule,
                     gp = grid::gpar(col = first_rows$colour,
                                     fill = alpha(first_rows$fill, first_rows$alpha),
                                     lwd = first_rows$size * .pt,
                                     lty = first_rows$linetype))
    }

    groups <- with(munched, paste(fill, colour, alpha, size, linetype))

    ggplot2:::ggname(
      "geom_holygon",
      do.call(grid::grobTree, lapply(split(munched, groups), object_munch))
    )
  }
)
