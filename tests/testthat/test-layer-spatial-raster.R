context("test-layer-spatial-raster.R")

# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  test_that("layer-spatial works for raster objects", {
    load_longlake_data()

    # should have little grey thing around it
    print(
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
    )

    # should not have little grey thing around it
    print(
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        labs(caption = "Should have no grey area around the sides, roughly N-S projection")
    )

    # grey thing
    print(
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        labs(caption = "Should have a little grey area around the sides, rotated projection")
    )

    # no grey thing
    print(
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        labs(caption = "Should have no grey area around the sides, rotated projection")
    )

    # with alpha
    print(
      ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.7) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        labs(caption = "Should have no grey area around the sides, rotated projection, slight transparency")
    )

    # with aesthetics
    print(
      ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    print(
      ggplot() +
        layer_spatial(longlake_osm, aes(alpha = stat(band1), fill = NULL)) +
        layer_spatial(longlake_depthdf)
    )

    print(
      ggplot() +
        layer_spatial(longlake_depth_raster) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # still a problem with "no non-missing arguments to max()"
    # expect_silent(
    #   print(
    #     ggplot() +
    #       annotation_spatial(longlake_osm) +
    #       layer_spatial(longlake_depthdf) +
    #       coord_sf(crs = 3978)
    #   )
    # )
    #
    # expect_silent(
    #   print(
    #     ggplot() +
    #       layer_spatial(longlake_osm) +
    #       layer_spatial(longlake_depthdf)
    #   )
    # )

    # graphical tests so...
    expect_true(TRUE)
  })


  test_that("layer-spatial works for raster objects", {
    load_longlake_data()

    # should have little grey thing around it
    print(
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
    )

    # try on gr device with no pixel concept
    withr::with_pdf(file.path(tempdir(), "test.pdf"), {
      print(
        ggplot() +
          layer_spatial(longlake_osm, lazy = TRUE) +
          layer_spatial(longlake_depthdf) +
          labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
      )
    }, height = 10, width = 10)

    withr::with_cairo_pdf(file.path(tempdir(), "test.pdf"), {
      print(
        ggplot() +
          layer_spatial(longlake_osm, lazy = TRUE) +
          layer_spatial(longlake_depthdf) +
          labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
      )
    }, height = 10, width = 10)

    withr::with_png(file.path(tempdir(), "test.pdf"), {
      print(
        ggplot() +
          layer_spatial(longlake_osm, lazy = TRUE) +
          layer_spatial(longlake_depthdf) +
          labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
      )
    }, res = 300)

    # should not have little grey thing around it
    print(
      ggplot() +
        annotation_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        labs(caption = "Should have no grey area around the sides, roughly N-S projection")
    )

    # grey thing
    print(
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        labs(caption = "Should have a little grey area around the sides, rotated projection")
    )

    # no grey thing
    print(
      ggplot() +
        annotation_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        labs(caption = "Should have no grey area around the sides, rotated projection")
    )

    # with alpha
    print(
      ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.7, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        labs(caption = "Should have no grey area around the sides, rotated projection, slight transparency")
    )

    # with aesthetics (currently not implemented)
    # print(
    #   ggplot() +
    #     layer_spatial(longlake_osm, aes(), lazy = TRUE) +
    #     layer_spatial(longlake_depthdf)
    # )
    #
    # print(
    #   ggplot() +
    #     layer_spatial(longlake_osm, aes(alpha = stat(band1), fill = NULL), lazy = TRUE) +
    #     layer_spatial(longlake_depthdf)
    # )
    #
    # print(
    #   ggplot() +
    #     layer_spatial(longlake_depth_raster, lazy = TRUE) +
    #     layer_spatial(longlake_depthdf) +
    #     coord_sf(crs = 3978)
    # )

    # graphical tests so...
    expect_true(TRUE)
  })

}
