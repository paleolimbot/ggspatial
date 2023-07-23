
# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  test_that("layer-spatial works for raster objects", {
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("raster")

    load_longlake_data(which = c("longlake_osm", "longlake_depthdf", "longlake_depth_raster"), raster_format = "raster")

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
        layer_spatial(longlake_osm, aes(alpha = after_stat(band1), fill = NULL)) +
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
        layer_spatial(longlake_osm,
                      lazy = TRUE,
                      interpolate = FALSE,
                      dpi = 5
        )
    )
  })

}
