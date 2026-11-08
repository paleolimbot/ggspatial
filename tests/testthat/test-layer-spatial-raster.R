# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_that("layer-spatial works for raster objects", {
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("raster")

    load_longlake_data(
      which = c("longlake_osm", "longlake_depthdf", "longlake_depth_raster"),
      raster_format = "raster"
    )

    # should have little grey thing around it
    expect_doppelganger_extra(
      "layer_spatial.Raster()",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # should not have little grey thing around it
    expect_doppelganger_extra(
      "annotation_spatial.Raster()",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # grey thing
    expect_doppelganger_extra(
      "layer_spatial.Raster() project",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # no grey thing
    expect_doppelganger_extra(
      "annotation_spatial.Raster() project",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with alpha
    expect_doppelganger_extra(
      "annotation_spatial.Raster() alpha",
      ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.7) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with aesthetics
    expect_doppelganger_extra(
      "layer_spatial.Raster() aes()",
      ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    expect_doppelganger_extra(
      "layer_spatial.Raster() aes(band1)",
      ggplot() +
        layer_spatial(
          longlake_osm,
          aes(alpha = after_stat(band1), fill = NULL)
        ) +
        layer_spatial(longlake_depthdf)
    )

    # dpi works
    vdiffr::expect_doppelganger(
      "layer_spatial.Raster() dpi",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE, dpi = 2)
    )

    # lazy works
    vdiffr::expect_doppelganger(
      "layer_spatial.Raster() lazy",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE)
    )

    # interpolation works
    vdiffr::expect_doppelganger(
      "layer_spatial.Raster() no interpolation",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE, interpolate = FALSE, dpi = 5)
    )
  })
}

test_that("RGB RasterBrick renders with a geographic default CRS", {
  skip_if_not_installed("raster")
  withr::local_pdf(NULL)

  rgb <- raster::brick(
    nrows = 12,
    ncols = 20,
    nl = 3,
    xmn = 60,
    xmx = 145,
    ymn = 10,
    ymx = 57,
    crs = "+proj=longlat +datum=WGS84"
  )
  raster::values(rgb) <- cbind(
    180,
    rep(c(100, 160), length.out = raster::ncell(rgb)),
    230
  )

  plot <- ggplot() +
    layer_spatial(rgb) +
    coord_sf(
      crs = "+proj=aeqd +lat_0=36 +lon_0=104 +datum=WGS84 +units=m",
      default_crs = sf::st_crs(4326),
      xlim = c(76, 132),
      ylim = c(18, 54)
    )

  expect_warning(grob <- ggplot2::ggplotGrob(plot), NA)
  panel <- grob$grobs[[which(grob$layout$name == "panel")]]
  rasters <- Filter(function(x) inherits(x, "rastergrob"), panel$children)
  expect_length(rasters, 1)
  raster <- rasters[[1]]
  expect_true(all(is.finite(as.numeric(c(
    raster$x,
    raster$y,
    raster$width,
    raster$height
  )))))
  expect_gt(as.numeric(raster$width), 0)
  expect_gt(as.numeric(raster$height), 0)
})
