context("test-layer-spatial-raster.R")

# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  test_that("layer-spatial works for raster objects", {
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("raster")
    skip_if_not_installed("rgdal")

    load_longlake_data(which = c("longlake_osm", "longlake_depthdf", "longlake_depth_raster"), raster_format = "raster")

    # should have little grey thing around it
    vdiffr::expect_doppelganger(
      "layer_spatial.Raster()",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # should not have little grey thing around it
    vdiffr::expect_doppelganger(
      "annotation_spatial.Raster()",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # grey thing
    vdiffr::expect_doppelganger(
      "layer_spatial.Raster() project",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # no grey thing
    vdiffr::expect_doppelganger(
      "annotation_spatial.Raster() project",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with alpha
    vdiffr::expect_doppelganger(
      "annotation_spatial.Raster() alpha",
      ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.7) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with aesthetics
    vdiffr::expect_doppelganger(
      "layer_spatial.Raster() aes()",
      ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    vdiffr::expect_doppelganger(
      "layer_spatial.Raster() aes(band1)",
      ggplot() +
        layer_spatial(longlake_osm, aes(alpha = stat(band1), fill = NULL)) +
        layer_spatial(longlake_depthdf)
    )
  })

}
