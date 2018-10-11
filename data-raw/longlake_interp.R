
library(tidyverse)
library(sf)

# create a depth raster of longlake using depth_df and outline
ggspatial::load_longlake_data()

# helper functions from the lake mophometry paper

# this gets used to create a regular grid for GAM interpolation
create_grid <- function(xy, ...) {
  expand.grid(
    X = seq(floor(min(xy$X)), ceiling(max(xy$X)), ...),
    Y = seq(floor(min(xy$Y)), ceiling(max(xy$Y)), ...)
  ) %>% as_tibble()
}

interpolate_raster <- function(sp, values, boundary = NULL, boundary_values = 0,
                               method = gam_repl, ...) {

  # create nodes object for processing
  points <- sp %>%
    st_zm() %>%
    transmute(z = !!enquo(values), point_type = "input") %>%
    st_cast("POINT", warn = FALSE)

  if(!is.null(boundary)) {
    boundary <- boundary %>%
      st_transform(st_crs(points)) %>%
      st_zm()
    points <- points %>%
      rbind(
        boundary %>%
          transmute(z = !!enquo(boundary_values), point_type = "boundary") %>%
          st_cast("POINT", warn = FALSE)
      )
  }

  rast <- method(points, ...)

  if(!is.null(boundary)) {
    rast <- raster::mask(rast, as(boundary, "Spatial"))
  }

  rast
}

gam_repl <- function(points, k = 60, quiet = TRUE, by = NULL, length.out = 100) {
  # significant inspiration from here:
  # https://www.fromthebottomoftheheap.net/2016/03/27/soap-film-smoothers/

  if(is.null(by)) {
    grid_gen <- function(xy) {
      create_grid(xy, length.out = length.out)
    }
  } else {
    grid_gen <- function(xy) {
      create_grid(xy, by = by)
    }
  }

  if(!quiet) message("Creating grid...")

  # get z points, z as a data frame
  z_values <- bind_cols(
    points %>% unclass() %>% as_tibble() %>% select(-geometry),
    points %>% st_coordinates() %>% as_tibble()
  )

  if(!quiet) message("Fitting GAM...")
  s <- mgcv::s
  tprs <- mgcv::gam(z ~ s(X, Y, k = k), data = z_values, method = "REML")

  if(!quiet) message("Interpolating...")

  # create grid, predict values at regularly-spaced grid locations
  grid <- grid_gen(z_values)
  grid$z <- predict(tprs, grid, type = "response")

  # return raster with correct points
  raster::rasterFromXYZ(grid, crs = st_crs(points)$proj4string)
}

longlake_depth_raster <- interpolate_raster(
  longlake_depthdf,
  values = DEPTH_M,
  boundary = longlake_waterdf %>% slice(2)
)

raster::plot(longlake_depth_raster)

raster_fname <- "inst/longlake/longlake_depth.tif"
unlink(raster_fname)
raster::writeRaster(longlake_depth_raster, raster_fname)

