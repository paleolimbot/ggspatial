
#' Spatial-aware north arrow
#'
#' @param line_width,line_col,fill Parameters for north arrow polygons
#' @param text_col,text_family,text_face,text_angle Parameters for the "N" text
#' @param height,width Height and width of north arrow
#' @param pad_x,pad_y Padding between north arrow and edge of frame
#' @param which_north "grid" results in a north arrow always pointing up; "true" always points to the
#'   north pole from whichever corner of the map the north arrow is in.
#' @param rotation Override the rotation of the north arrow (degrees conterclockwise)
#' @param location Where to put the north arrow ("tl" for top left, etc.)
#' @param style Determines the style of North Arrow design you wish to use. Options are 'default', 'style_1', and 'style_2'

#'
#' @return A ggplot2 layer
#' @export
#' @importFrom grid unit
#'
annotation_north_arrow <- function(line_width = 1, line_col = "black", fill = c("white", "black"),
                                   text_col = "black", text_family = "", text_face = NULL,
                                   text_angle = NULL, text_size = 12,
                                   height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                                   pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"),
                                   which_north = c("grid", "true"), rotation = NULL,
                                   location = c("tr", "bl", "br", "tl"),
                                   style = c("default","style_1","style_2","nautical")){

  which_north <- match.arg(which_north)
  location <- match.arg(location)
  style <- match.arg(style)


  stopifnot(
    is.numeric(line_width), length(line_width) == 1,
    length(line_col) == 1, is.atomic(line_col),
    grid::is.unit(height), length(height) == 1,
    grid::is.unit(width), length(width) == 1,
    grid::is.unit(pad_x), length(pad_x) == 1,
    grid::is.unit(pad_y), length(pad_y) == 1,
    length(text_col) == 1, is.atomic(text_col),
    length(fill) == 2, is.atomic(fill)
  )

  ggplot2::layer(
    data = data.frame(x = NA),
    mapping = NULL,
    stat = ggplot2::StatIdentity,
    geom = GeomNorthArrow,
    position = ggplot2::PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      line_width = line_width,
      line_col = line_col,
      fill = fill,
      text_col = text_col,
      text_family = text_family,
      text_face = text_face,
      height = height,
      width = width,
      pad_x = pad_x,
      pad_y = pad_y,
      which_north = which_north,
      location = location,
      style = style
    )
  )
}

#' @rdname annotation_north_arrow
#' @export
GeomNorthArrow <- ggplot2::ggproto(
  "GeomNorthArrow",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  draw_panel = function(data, panel_params, coordinates,
                        line_width = 1, line_col = "black", fill = c("white", "black"),
                        text_col = "black", text_family = "", text_face = NULL,
                        text_angle = NULL,
                        height = unit(1.5, "cm"), width = unit(1.5, "cm"),
                        pad_x = unit(0.25, "cm"), pad_y = unit(0.25, "cm"), which_north = "grid",
                        rotation = NULL, location = "tr", style = "default") {

    if(is.null(rotation)) {
      rotation <- 0 # degrees anticlockwise

      if((which_north == "true") && inherits(coordinates, "CoordSf")) {
        # calculate bearing from centre of map to the north pole?
        bounds <- c(
          l = panel_params$x_range[1],
          r = panel_params$x_range[2],
          b = panel_params$y_range[1],
          t = panel_params$y_range[2]
        )

        rotation <- -1 * true_north(
          x = bounds[substr(location, 2, 2)],
          y = bounds[substr(location, 1, 1)],
          crs = sf::st_crs(panel_params$crs)
        )
      } else if(which_north == "true") {
        warning("True north is not meaningful without coord_sf()")
      }
    }

    if(is.null(text_angle)) {
      text_angle <- -rotation
    }

    # north arrow grob in npc coordinates
    sub_grob <- north_arrow_grob_default(
      line_width = line_width,
      line_col = line_col,
      fill = fill,
      text_col = text_col,
      text_family = text_family,
      text_face = text_face,
      text_angle = text_angle,
      style = style
    )

    # position of origin (centre of arrow) based on padding, width, height
    adj_x <- as.numeric(grepl("r", location))
    adj_y <- as.numeric(grepl("t", location))
    origin_x <- unit(adj_x, "npc") + (0.5 - adj_x) * 2 * (pad_x + 0.5 * width)
    origin_y <- unit(adj_y, "npc") + (0.5 - adj_y) * 2 * (pad_y + 0.5 * height)

    # gtree with a custom viewport
    grid::gTree(
      children = sub_grob,
      vp = grid::viewport(
        x = origin_x,
        y = origin_y,
        height = height,
        width = width,
        angle = rotation
      )
    )
  }
)

# I'm sure there is an easier way to do this...
true_north <- function(x, y, crs, delta_crs = 0.1, delta_lat = 0.1) {

  pt_crs <- sf::st_sfc(sf::st_point(c(x, y)), crs = crs)
  pt_crs_coords <- as.data.frame(sf::st_coordinates(pt_crs))

  pt_latlon <- sf::st_transform(pt_crs, crs = 4326)
  pt_latlon_coords <- as.data.frame(sf::st_coordinates(pt_latlon))


  # point directly grid north of x, y
  pt_grid_north <- sf::st_sfc(sf::st_point(c(x, y + delta_crs)), crs = crs)
  pt_grid_north_coords <- as.data.frame(sf::st_coordinates(pt_grid_north))

  # point directly true north of x, y
  pt_true_north <- sf::st_transform(
    sf::st_sfc(
      sf::st_point(c(pt_latlon_coords$X, pt_latlon_coords$Y + delta_lat)),
      crs = 4326
    ),
    crs = crs
  )
  pt_true_north_coords <- as.data.frame(sf::st_coordinates(pt_true_north))

  a <- c(
    x = pt_true_north_coords$X - pt_crs_coords$X,
    y = pt_true_north_coords$Y - pt_crs_coords$Y
  )

  b <- c(
    x = pt_grid_north_coords$X - pt_crs_coords$X,
    y = pt_grid_north_coords$Y - pt_crs_coords$Y
  )

  # https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
  theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )

  # use sign of cross product to indicate + or - rotation
  cross_product <- a[1]*b[2] - a[2]*b[1]

  # return in degrees
  rot_degrees <- theta * 180 / pi * sign(cross_product)[1]

  rot_degrees
}



# this creates a grob with N arrow and text (using 0...1 coordinates)
# must return a gList()
north_arrow_grob_default <- function(line_width = 1, line_col = "black", fill = c("white", "black"),
                                     text_col = "black", text_family = "", text_face = NULL,
                                     arrow_x = c(0, 0.5, 0.5, 1, 0.5, 0.5),
                                     arrow_y = c(0.1, 1, 0.5, 0.1, 1, 0.5),
                                     arrow_id = c(1, 1, 1, 2, 2, 2),
                                     text_x = 0.5, text_y = 0.1, text_size = 10, text_adj = c(0.5, 0.5),
                                     text_label = "N", text_angle = 0, style = "default"){
  # use default style
  if(style == "default"){
    grid::gList(
      grid::polygonGrob(
        x = arrow_x,
        y = arrow_y,
        id = arrow_id,
        default.units = "npc",
        gp = grid::gpar(
          linewidth = line_width,
          col = line_col,
          fill = fill
          )
        ),
      grid::textGrob(
        label = "N",
        x = text_x,
        y = text_y,
        hjust = text_adj[0],
        vjust = text_adj[1],
        # rot = text_angle,
        gp = grid::gpar(
          fontfamily = text_family,
          fontface = text_face,
          fontsize = text_size + 2
          )
        )
      )
    }
     # use 'style_1'
     else if(style == "style_1"){

      # add Grobs
      grid::gList(
        grid::polygonGrob(
          x = arrow_x[1:3] + c(0.65, 0, 0),
          y = arrow_y[1:3] + c(0.55, 0, 0.2),
          id = arrow_id[1:3],
          default.units = "npc",
          gp = grid::gpar(
            lwd = line_width,
            col = line_col,
            fill = fill[2]
            )
          ),
        grid::linesGrob(
          x = c(0.5,0.5),
          y = c(0,1),
          default.units = "npc",
          gp = grid::gpar(
            lwd = line_width,
            col = line_col
            )
          ),
        grid::textGrob(
          label = "N",
          x = text_x,
          y = text_y + 0.2,
          rot = text_angle,
          gp = grid::gpar(
            fontfamily = text_family,
            fontface = text_face,
            fontsize = text_size
            )
          )
        )
      }
       # use 'style_2'
       else if(style == "style_2"){
        # adjust default params to fit new style
        arrow_x_new  <- arrow_x + c(0.75, 0, 0, (-0.75), 0, 0)
        arrow_y_new  <- arrow_y - c(0, 0.2, 0.2, 0, 0.2, 0.2)
        text_y_new   <- text_y + 0.85
        text_size_new <- text_size

        # add Grobs
        grid::gList(
          grid::circleGrob(
            x = 0.505,
            y = 0.4,
            r = 0.3,
            default.units = "npc",
            gp = grid::gpar(
              fill = NA,
              col = line_col,
              lwd = line_width
              )
            ),
          grid::polygonGrob(
            x = arrow_x_new,
            y = arrow_y_new,
            id = arrow_id,
            default.units = "npc",
            gp = grid::gpar(
              lwd = line_width,
              col = line_col,
              fill = fill
              )
            ),
          grid::textGrob(
            label = "N",
            x = text_x,
            y = text_y_new,
            rot = text_angle,
            gp = grid::gpar(
              fontfamily = text_family,
              fontface = text_face,
              fontsize = text_size_new
              )
            )
          )

       }
  else if(style == "nautical"){
    nautical <- data.frame(x = c(0.5,0.45,0.5,0.5,0.55,0.5,  #North
                                 0.5,0.55,0.5,0.5,0.45,0.5,  #South
                                 0.5,0.6,0.8,0.5,0.6,0.8,    #East
                                 0.5,0.4,0.2,0.5,0.4,0.2,    #West
                                 0.5,0.55,0.65,0.5,0.6,0.65,  #NE
                                 0.5,0.6,0.65,0.5,0.55,0.65,  #SE
                                 0.5,0.4,0.35,0.5,0.45,0.35,  #NW
                                 0.5,0.45,0.35,0.5,0.4,0.35), #SW
                           y = c(0.5,0.6,0.8,0.5,0.6,0.8,
                                 0.5,0.4,0.2,0.5,0.4,0.2,
                                 0.5,0.55,0.5,0.5,0.45,0.5,
                                 0.5,0.45,0.5,0.5,0.55,0.5,
                                 0.5,0.6,0.65,0.5,0.55,0.65,
                                 0.5,0.45,0.35,0.5,0.4,0.35,
                                 0.5,0.55,0.65,0.5,0.6,0.65,
                                 0.5,0.4,0.35,0.5,0.45,0.35),
                           id = c(1,1,1,2,2,2,
                                  3,3,3,4,4,4,
                                  5,5,5,6,6,6,
                                  7,7,7,8,8,8,
                                  9,9,9,10,10,10,
                                  11,11,11,12,12,12,
                                  13,13,13,14,14,14,
                                  15,15,15,16,16,16))


    # add Grobs
    grid::gList(
      grid::circleGrob(
        x = 0.5,
        y = 0.5,
        r = 0.01,
        default.units = "npc",
        gp = grid::gpar(
          fill = fill[2],
          col = line_col,
          lwd = line_width
        )
      ),
      grid::circleGrob(
        x = 0.5,
        y = 0.5,
        r = 0.25,
        default.units = "npc",
        gp = grid::gpar(
          col = col,
          lty = 1,
          fill = NA,
          lwd = line_width
        )
      ),
      grid::circleGrob(
        x = 0.5,
        y = 0.5,
        r = 0.255,
        default.units = "npc",
        gp = grid::gpar(
          col = line_col,
          fill = NA,
          lty = 1,
          lwd = line_width
        )
      ),
      grid::polygonGrob(
        name = "nautical",
        x = nautical$x,
        y = nautical$y,
        id = nautical$id,
        default.units = "npc",
        gp = grid::gpar(
          fill = fill,
          col = line_col,
          lwd = line_width - 0.8
        )
      ),
      grid::textGrob(
        label = "N",
        x = unit(0.5, "npc"),
        y = unit(0.9, "npc"),
        just = "centre",
        hjust = NULL,
        vjust = NULL,
        rot = 0,
        check.overlap = FALSE,
        default.units = "npc",
        name = NULL,
        gp = gpar(
          fontsize = text_size,
          fontface = text_face,
          fontfamily = text_family,
          col = text_col
        ),
        vp = NULL
      )
    )
  }
}
