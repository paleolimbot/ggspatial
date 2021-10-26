
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
          aes(alpha = stat(band1), fill = NULL)
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
