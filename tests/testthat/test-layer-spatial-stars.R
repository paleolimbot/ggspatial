context("test-layer-spatial-stars")

# only a few of these work, so skipping
test_that("stars tests", {
  skip("stars aren't implemented")
  expect_true(FALSE)
})

if (FALSE) {

  test_that("layer-spatial works for raster objects", {
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("stars")

    load_longlake_data(which = c("longlake_depthdf", "longlake_osm", "longlake_depth_raster"), raster_format = "stars")

    # should have little grey thing around it
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        ggplot2::labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
    )

    # should not have little grey thing around it
    print(
      ggplot2::ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        ggplot2::labs(caption = "Should have no grey area around the sides, roughly N-S projection")
    )

    # grey thing
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        ggplot2::coord_sf(crs = 3978) +
        ggplot2::labs(caption = "Should have a little grey area around the sides, rotated projection")
    )

    # no grey thing
    print(
      ggplot2::ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        ggplot2::labs(caption = "Should have no grey area around the sides, rotated projection")
    )

    # with alpha
    print(
      ggplot2::ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.7) +
        layer_spatial(longlake_depthdf) +
        ggplot2::coord_sf(crs = 3978) +
        ggplot2::labs(caption = "Should have no grey area around the sides, rotated projection, slight transparency")
    )

    # with aesthetics
    # currently, this is not working (also, unclear what it should do)
    expect_true(FALSE)
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    # currently, this is not mapped at all
    expect_true(FALSE)
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_osm, aes(alpha = stat(band1), fill = NULL)) +
        layer_spatial(longlake_depthdf)
    )

    # currently the NA values are not mapped correctly
    expect_true(FALSE)
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_depth_raster) +
        layer_spatial(longlake_depthdf) +
        ggplot2::coord_sf(crs = 3978)
    )

    # graphical tests so...
    expect_true(TRUE)
  })


  test_that("layer-spatial works for stars_proxy objects with lazy = TRUE", {
    load_longlake_data(
      which = c("longlake_depthdf", "longlake_osm", "longlake_depth_raster"),
      raster_format = "stars_proxy"
    )

    # should have little grey thing around it
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        ggplot2::labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
    )

    # try on gr device with no pixel concept
    withr::with_pdf(file.path(tempdir(), "test.pdf"), {
      print(
        ggplot2::ggplot() +
          layer_spatial(longlake_osm, lazy = TRUE) +
          layer_spatial(longlake_depthdf) +
          ggplot2::labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
      )
    }, height = 10, width = 10)

    withr::with_cairo_pdf(file.path(tempdir(), "test.pdf"), {
      print(
        ggplot2::ggplot() +
          layer_spatial(longlake_osm, lazy = TRUE) +
          layer_spatial(longlake_depthdf) +
          ggplot2::labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
      )
    }, height = 10, width = 10)

    withr::with_png(file.path(tempdir(), "test.pdf"), {
      print(
        ggplot2::ggplot() +
          layer_spatial(longlake_osm, lazy = TRUE) +
          layer_spatial(longlake_depthdf) +
          ggplot2::labs(caption = "Should have a little grey area around the sides, roughly N-S projection")
      )
    }, res = 300)

    # should not have little grey thing around it
    print(
      ggplot2::ggplot() +
        annotation_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        ggplot2::labs(caption = "Should have no grey area around the sides, roughly N-S projection")
    )

    # grey thing
    print(
      ggplot2::ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        ggplot2::labs(caption = "Should have a little grey area around the sides, rotated projection")
    )

    # no grey thing
    print(
      ggplot2::ggplot() +
        annotation_spatial(longlake_osm, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        ggplot2::coord_sf(crs = 3978) +
        ggplot2::labs(caption = "Should have no grey area around the sides, rotated projection")
    )

    # with alpha
    print(
      ggplot2::ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.7, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978) +
        ggplot2::labs(caption = "Should have no grey area around the sides, rotated projection, slight transparency")
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

  test_that("layer-spatial works for stars objects", {
    stars_rast <- stars::read_stars(system.file("longlake/longlake_depth.tif", package = "ggspatial"))
    stars_rgb <- stars::read_stars(system.file("longlake/longlake.tif", package = "ggspatial"))
    print(ggplot2::ggplot() + layer_spatial(stars_rast) + labs(caption = "longlake raster read by stars"))
    print(ggplot2::ggplot() + layer_spatial(stars_rgb) + labs(caption = "longlake rgb read by stars"))

    load_longlake_data()
    print(
      ggplot2::ggplot() +
        annotation_spatial(stars_rgb) +
        layer_spatial(longlake_depthdf) +
        ggplot2::labs(caption = "annotation stars rgb")
    )

    print(
      ggplot2::ggplot() +
        annotation_spatial(stars_rgb, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        ggplot2::labs(caption = "annotation stars rgb with lazy=TRUE")
    )

    # graphical tests so...
    expect_true(TRUE)
  })
}
