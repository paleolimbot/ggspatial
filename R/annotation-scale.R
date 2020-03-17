
#' Spatial-aware scalebar annotation
#'
#' @param plot_unit For non-coord_sf applications, specify the unit for x and y coordinates.
#'   Must be one of km, m, cm, mi, ft, or in.
#' @param bar_cols Colours to use for the bars
#' @param line_width Line width for scale bar
#' @param height Height of scale bar
#' @param pad_x,pad_y Distance between scale bar and edge of panel
#' @param text_pad,text_cex,text_face,text_family Parameters for label
#' @param tick_height Height of ticks relative to height of scale bar
#' @param mapping,data,... See Aesthetics
#'
#' @section Aesthetics:
#' The following can be used as parameters or aesthetics. Using them as
#' aesthetics is useful when facets are used to display multiple panels,
#' and a different (or missing) scale bar is required in different panels.
#' Otherwise, just pass them as arguments to `annotation_scale`.
#'   \itemize{
#'     \item width_hint: The (suggested) proportion of the plot area which the scalebar should occupy.
#'     \item unit_category: Use "metric" or "imperial" units.
#'     \item style: One of "bar" or "ticks"
#'     \item location: Where to put the scale bar ("tl" for top left, etc.)
#'     \item line_col and text_col: Line and text colour, respectively
#'   }
#'
#' @return A ggplot2 layer.
#' @export
#'
#' @importFrom grid unit
#'
#' @examples
#' cities <- data.frame(
#'   x = c(-63.58595, 116.41214),
#'   y = c(44.64862, 40.19063),
#'   city = c("Halifax", "Beijing")
#' )
#'
#' ggplot(cities) +
#'   geom_spatial_point(aes(x, y), crs = 4326) +
#'   annotation_scale() +
#'   coord_sf(crs = 3995)
#'
annotation_scale <- function(mapping = NULL, data = NULL,
                             ...,
                             plot_unit = NULL,
                             bar_cols = c("black", "white"),
                             line_width = 1,
                             height = unit(0.25, "cm"),
                             pad_x = unit(0.25, "cm"),
                             pad_y = unit(0.25, "cm"),
                             text_pad = unit(0.15, "cm"),
                             text_cex = 0.7,
                             text_face = NULL,
                             text_family = "",
                             tick_height = 0.6) {

  if(is.null(data)) {
    data <- data.frame(x = NA)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = ggplot2::StatIdentity,
    geom = GeomScaleBar,
    position = ggplot2::PositionIdentity,
    show.legend = FALSE,
    inherit.aes = FALSE,
    params = list(
      ...,
      plot_unit = plot_unit,
      bar_cols = bar_cols,
      line_width = line_width,
      height = height,
      pad_x = pad_x,
      pad_y = pad_y,
      text_pad = text_pad,
      text_cex = text_cex,
      text_face = text_face,
      text_family = text_family,
      tick_height = tick_height
    )
  )
}

#' @rdname annotation_scale
#' @export
GeomScaleBar <- ggplot2::ggproto(
  "GeomScaleBar",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  default_aes = ggplot2::aes(
    width_hint = 0.25,
    style = "bar",
    location = "bl",
    unit_category = "metric",
    text_col = "black",
    line_col = "black"
  ),

  draw_panel = function(self, data, panel_params, coordinates, plot_unit = NULL,
                        bar_cols = c("black", "white"),
                        line_width = 1,
                        height = unit(0.25, "cm"),
                        pad_x = unit(0.25, "cm"),
                        pad_y = unit(0.25, "cm"),
                        text_pad = unit(0.15, "cm"),
                        text_cex = 0.7,
                        text_face = NULL,
                        text_family = "",
                        tick_height = 0.6) {

    width_hint <- data$width_hint[1]
    style <- data$style[1]
    location = data$location[1]
    unit_category <- data$unit_category[1]
    text_col <- data$text_col[1]
    line_col <- data$line_col[1]

    stopifnot(
      is.null(plot_unit) || plot_unit %in% c("mi", "ft", "in", "km", "m", "cm"),
      length(unit_category) == 1, unit_category %in% c("metric", "imperial"),
      is.numeric(width_hint), length(width_hint) == 1,
      is.atomic(bar_cols),
      is.numeric(line_width), length(line_width) == 1,
      length(line_col) == 1,
      grid::is.unit(height), length(height) == 1,
      grid::is.unit(pad_x), length(pad_x) == 1,
      grid::is.unit(pad_y), length(pad_y) == 1,
      grid::is.unit(text_pad), length(text_pad) == 1,
      length(text_col) == 1,
      is.numeric(tick_height), length(tick_height) == 1
    )

    # ranges have to be unnamed because when given
    # xlim or ylim, these values have names that c()
    # "helpfully" appends
    if(inherits(coordinates, "CoordSf")) {
      sf_bbox <- c(
        xmin = unname(panel_params$x_range[1]),
        xmax = unname(panel_params$x_range[2]),
        ymin = unname(panel_params$y_range[1]),
        ymax = unname(panel_params$y_range[2])
      )
    } else if(coordinates$is_linear()) {
      sf_bbox <- c(
        xmin = unname(panel_params$x.range[1]),
        xmax = unname(panel_params$x.range[2]),
        ymin = unname(panel_params$y.range[1]),
        ymax = unname(panel_params$y.range[2])
      )
    } else {
      stop("Don't know how to create scalebar using ", paste(class(coordinates), collapse = "/"))
    }

    scalebar_params <- scalebar_params(
      sf_bbox = sf_bbox,
      plotunit = plot_unit,
      widthhint = width_hint,
      unitcategory = unit_category,
      sf_crs = panel_params$crs
    )

    scalebar_grobs(
      scalebar_params,
      style = style,
      location = location,
      bar_cols = bar_cols,
      line_width = line_width,
      line_col = line_col,
      height = height,
      pad_x = pad_x,
      pad_y = pad_y,
      text_pad = text_pad,
      text_cex = text_cex,
      text_col = text_col,
      text_face = text_face,
      text_family = text_family,
      tick_height = tick_height
    )
  }
)

scalebar_grobs <- function(
  params,
  style = c("ticks", "bar"),
  location = c("bl", "br", "tr", "tl"),
  bar_cols = c("black", "white"),
  line_width = 1,
  line_col = "black",
  height = unit(0.25, "cm"),
  pad_x = unit(0.25, "cm"),
  pad_y = unit(0.25, "cm"),
  text_pad = unit(0.15, "cm"),
  text_cex = 0.7,
  text_col = "black",
  text_face = NULL,
  text_family = "",
  tick_height = 0.6
) {
  style <- match.arg(style)

  location <- match.arg(location)

  adj_x <- as.numeric(grepl("r", location))
  adj_y <- as.numeric(grepl("t", location))
  width <- unit(params$widthnpc, "npc")

  origin_x <- unit(adj_x, "npc") - adj_x * width + (0.5 - adj_x) * 2 * pad_x
  origin_y <- unit(adj_y, "npc") - adj_y * height + (0.5 - adj_y) * 2 * pad_y
  text_origin_x <- unit(adj_x, "npc") + (0.5 - adj_x) * 2 * (pad_x + text_pad + width)
  text_origin_y <- unit(adj_y, "npc") + (0.5 - adj_y) * 2 * (pad_y + 0.5 * height)

  if(style == "bar") {
    bar_grob <- grid::rectGrob(
      x = origin_x + unit((seq_len(params$majordivs) - 1) * params$majordivnpc, "npc"),
      y = origin_y,
      width = unit(params$majordivnpc, "npc"),
      height = height,
      hjust = 0,
      vjust = 0,
      gp = grid::gpar(
        fill = rep(bar_cols, lengh.out = params$majordivs),
        col = line_col,
        lwd = line_width
      )
    )
  } else if(style == "ticks") {
    bar_grob <- grid::gList(
      grid::segmentsGrob(
        x0 = origin_x + unit((seq_len(params$majordivs + 1) - 1) * params$majordivnpc, "npc"),
        y0 = origin_y,
        x1 = origin_x + unit((seq_len(params$majordivs + 1) - 1) * params$majordivnpc, "npc"),
        y1 = origin_y + grid::unit.c(height, rep(height * tick_height, params$majordivs - 1), height),
        gp = grid::gpar(
          lwd = line_width,
          col = line_col
        )
      ),
      grid::segmentsGrob(
        x0 = origin_x,
        y0 = origin_y,
        x1 = origin_x + width,
        y1 = origin_y,
        gp = grid::gpar(
          lwd = line_width,
          col = line_col
        )
      )
    )
  } else {
    stop("not implemented")
  }

  grid::gList(
    bar_grob,
    grid::textGrob(
      label = params$labeltext,
      x = text_origin_x,
      y = text_origin_y,
      hjust = adj_x,
      vjust = 0.5,
      gp = grid::gpar(
        cex = text_cex,
        col = text_col,
        fontfamily = text_family,
        fontface = text_face
      )
    )
  )
}



# this is a rewritten version of prettymapr::scalebarparams()
# that uses sf projections rather than epsg codes
scalebar_params <- function(
  sf_bbox,
  plotunit = NULL,
  sf_crs = NULL,
  widthhint = 0.25,
  unitcategory = c("metric", "imperial")
) {
  # params check
  unitcategory <- match.arg(unitcategory)

  if(!is.null(sf_crs) && is.null(plotunit)) {

    point_coords <- expand.grid(
      x = c(sf_bbox["xmin"], sf_bbox["xmax"]),
      y = c(sf_bbox["ymin"], mean(c(sf_bbox["ymin"], sf_bbox["ymax"])), sf_bbox["ymax"])
    )
    latlon_coords <- sf::st_coordinates(
      sf::st_transform(
        sf::st_as_sf(point_coords, coords = c("x", "y"), crs = sf_crs),
        4326
      )
    )

    widthbottom <- .geodist(latlon_coords[1,], latlon_coords[2,])
    widthmiddle <- .geodist(latlon_coords[3,], latlon_coords[4,])
    widthtop <- .geodist(latlon_coords[5,], latlon_coords[6,])
    percentdiff <- (max(widthbottom, widthmiddle, widthtop) -
                      min(widthbottom, widthmiddle, widthtop)) / min(widthbottom, widthmiddle, widthtop)

    if(percentdiff > 0.1) {
      message("Scale on map varies by more than 10%, scale bar may be inaccurate")
    }

    widthm <- unname(widthmiddle)
    mperplotunit <- unname(widthmiddle/(sf_bbox["xmax"]-sf_bbox["xmin"]))
  } else {

    if(is.null(plotunit)) {
      message("Using plotunit = 'm'")
      plotunit <- "m"
    }

    plotunit <- match.arg(plotunit, choices = c("km", "m", "cm", "mi", "ft", "in"))

    heightm <- .tosi(sf_bbox["ymax"] - sf_bbox["ymin"], plotunit)
    widthm <- unname(.tosi(sf_bbox["xmax"] - sf_bbox["xmin"], plotunit))
    mperplotunit <- unname(.tosi(1.0, plotunit))
  }

  geowidthm <- unname(widthm * widthhint)

  if(geowidthm < 1) {
    scaleunits <- c("cm", "in")
  } else if(geowidthm < 1600) {
    scaleunits <- c("m", "ft")
  } else {
    scaleunits <- c("km", "mi")
  }

  #   String unit = units[unitCategory] ;
  if(unitcategory == "metric") {
    unit <- scaleunits[1]
  } else {
    unit <- scaleunits[2]
  }
  #   double widthHintU = Units.fromSI(geoWidthM, unit) ;
  widthhintu <- .fromsi(geowidthm, unit)
  #   double tenFactor = Math.floor(Math.log10(widthHintU)) ;
  tenfactor <- floor(log10(widthhintu))
  #   double widthInTens = Math.floor(widthHintU / Math.pow(10, tenFactor)) ;
  widthintens <- floor(widthhintu / (10^tenfactor))
  if(widthintens == 1) {
    widthintens <- 10
    tenfactor = tenfactor - 1 ;
  } else if(widthintens == 7) {
    widthintens <- 6
  } else if(widthintens == 9) {
    widthintens <- 8
  }

  if(widthintens < 6) {
    majdivtens <- 1
  } else {
    majdivtens <- 2
  }

  #   double widthU = widthInTens * Math.pow(10, tenFactor) ;
  widthu <- widthintens * 10^tenfactor
  #   double majorDiv = majDivTens * Math.pow(10, tenFactor) ;
  majordiv <- majdivtens * 10^tenfactor
  #   long majorDivs = Math.round(widthU / majorDiv) ;
  majordivs <- round(widthu / majordiv)
  #   double widthPx = Units.toSI(widthU, unit) / mPerPixel ;
  widthplotunit <- .tosi(widthu, unit) / mperplotunit
  #   double majorDivPx = widthPx / majorDivs ;
  majordivplotunit <- widthplotunit / majordivs
  #   this.scaleParameters = new double[] {widthU, majorDiv, widthPx, majorDivPx} ;
  params = list()
  params$plotwidthu <- .fromsi(widthm, unit)
  params$widthu <- widthu
  params$widthnpc <- params$widthu / params$plotwidthu
  params$unit <- unit
  params$majordivu <- majordiv
  params$majordivnpc <- params$majordivu / params$plotwidthu
  params$majordivs <- majordivs
  params$widthplotunit <- widthplotunit
  params$majordivplotunit <- majordivplotunit
  params$labeltext <- paste(as.integer(widthu), unit)
  params$extents <- sf_bbox
  #   this.labelText = String.valueOf(Math.round(widthU)) + " " + unit ;
  params

}

.geodist <- function(lonlat1, lonlat2) {

  long1 <- .torad(lonlat1[1])
  lat1 <- .torad(lonlat1[2])
  long2 <- .torad(lonlat2[1])
  lat2 <- .torad(lonlat2[2])
  R <- 6371009 # Earth mean radius [m]
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  d = R * c
  return(d) # Distance in m
}

.torad <- function(deg) {
  deg*pi/180.0
}

.fromsi <- function(sivalue, unit) {
  if(unit == "km") {
    sivalue / 1000.0
  } else if(unit == "m") {
    sivalue
  } else if(unit =="ft") {
    sivalue * 3.28084
  } else if(unit == "mi") {
    sivalue / 1609.344051499
  } else if(unit == "in") {
    sivalue * 39.370079999999809672
  } else if(unit == "cm") {
    sivalue * 100.0
  } else {
    stop("Unrecognized unit: ", unit)
  }
}

.tosi <- function(unitvalue, unit) {
  if(unit == "km") {
    unitvalue * 1000.0
  } else if(unit == "m") {
    unitvalue
  } else if(unit =="ft") {
    unitvalue / 3.28084
  } else if(unit == "mi") {
    unitvalue * 1609.344051499
  } else if(unit == "in") {
    unitvalue / 39.370079999999809672
  } else if(unit == "cm") {
    unitvalue / 100.0
  } else {
    stop("Unrecognized unit: ", unit)
  }
}

