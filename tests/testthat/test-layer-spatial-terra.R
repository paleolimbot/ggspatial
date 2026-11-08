
# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_that("layer-spatial works for terra objects", {
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("terra")

    load_longlake_data(
      which = c(
        "longlake_osm", "longlake_depthdf",
        "longlake_depth_raster"
      ),
      raster_format = "terra"
    )

    expect_s4_class(longlake_osm, "SpatRaster")
    expect_s4_class(longlake_depth_raster, "SpatRaster")


    # should have little grey thing around it
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster()",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # should not have little grey thing around it
    expect_doppelganger_extra(
      "annotation_spatial.SpatRaster()",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # grey thing
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() project",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # no grey thing
    expect_doppelganger_extra(
      "annotation_spatial.SpatRaster() project",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with alpha
    expect_doppelganger_extra(
      "annotation_spatial.SpatRaster() alpha 0.3",
      ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.3) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with aesthetics
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() aes()",
      ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() aes(band1)",
      ggplot() +
        layer_spatial(
          longlake_osm,
          aes(alpha = after_stat(band1), fill = NULL)
        ) +
        layer_spatial(longlake_depthdf)
    )

    # dpi works
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() dpi",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE, dpi = 2)
    )

    # lazy works
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() lazy",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE)
    )

    # interpolation works
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() no interpolation",
      ggplot() +
        layer_spatial(longlake_osm,
          lazy = TRUE,
          interpolate = FALSE,
          dpi = 5
        )
    )

    # Test examples
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() example 1",
      ggplot() +
        layer_spatial(longlake_osm)
    )
    expect_doppelganger_extra(
      "layer_spatial.SpatRaster() example 2",
      # Not removing NAs to avoid warning
      ggplot() +
        layer_spatial(longlake_depth_raster) +
        ggplot2::scale_fill_continuous(type = "viridis")
    )
  })
}

test_that("Full lifecycle", {
  skip_if_not_installed("terra")
  skip_on_cran()
  # Read
  terra <- terra::rast(
    system.file("longlake/longlake.tif", package = "ggspatial")
  )
  expect_s4_class(terra, "SpatRaster")

  plot <- ggplot() +
    layer_spatial(terra)
  expect_s3_class(plot, "ggplot")

  # Write to tempfile without warning
  tmp <- tempfile(fileext = ".png")

  expect_silent(ggplot2::ggsave(tmp, plot, width = 4, height = 4))

  unlink(tmp)
})

test_that("RGB SpatRaster renders with a geographic default CRS", {
  skip_if_not_installed("terra")
  withr::local_pdf(NULL)

  rgb <- terra::rast(
    nrows = 12, ncols = 20, nlyrs = 3,
    xmin = 60, xmax = 145, ymin = 10, ymax = 57,
    crs = "EPSG:4326"
  )
  terra::values(rgb) <- cbind(
    180, rep(c(100, 160), length.out = terra::ncell(rgb)), 230
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
    raster$x, raster$y, raster$width, raster$height
  )))))
  expect_gt(as.numeric(raster$width), 0)
  expect_gt(as.numeric(raster$height), 0)
})
