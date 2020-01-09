context("test-layer-spatial-stars")

# only a few of these work, so skpping
test_that("stars tests", {
  skip("stars aren't implemented fully")
  expect_true(FALSE)
})

if (FALSE) {

  test_that("layer-spatial works for raster objects", {
    load_longlake_data(which = c("longlake_depthdf", "longlake_osm", "longlake_depth_raster"), raster_format = "stars")

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
    # currently, this is not working (also, unclear what it should do)
    expect_true(FALSE)
    print(
      ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    # currently, this is not mapped at all
    expect_true(FALSE)
    print(
      ggplot() +
        layer_spatial(longlake_osm, aes(alpha = stat(band1), fill = NULL)) +
        layer_spatial(longlake_depthdf)
    )

    # currently the NA values are not mapped correctly
    expect_true(FALSE)
    print(
      ggplot() +
        layer_spatial(longlake_depth_raster) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # graphical tests so...
    expect_true(TRUE)
  })


  test_that("layer-spatial works for stars_proxy objects", {
    load_longlake_data(
      which = c("longlake_depthdf", "longlake_osm", "longlake_depth_raster"),
      raster_format = "stars_proxy"
    )

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

  test_that("layer-spatial works for stars objects", {
    stars_rast <- stars::read_stars(system.file("longlake/longlake_depth.tif", package = "ggspatial"))
    stars_rgb <- stars::read_stars(system.file("longlake/longlake.tif", package = "ggspatial"))
    print(ggplot() + layer_spatial(stars_rast) + labs(caption = "longlake raster read by stars"))
    print(ggplot() + layer_spatial(stars_rgb) + labs(caption = "longlake rgb read by stars"))

    load_longlake_data()
    print(
      ggplot() +
        annotation_spatial(stars_rgb) +
        layer_spatial(longlake_depthdf) +
        labs(caption = "annotation stars rgb")
    )

    print(
      ggplot() +
        annotation_spatial(stars_rgb, lazy = TRUE) +
        layer_spatial(longlake_depthdf) +
        labs(caption = "annotation stars rgb with lazy=TRUE")
    )

    # graphical tests so...
    expect_true(TRUE)
  })

  test_that("layer-spatial raster does not throw warnings", {
    load_longlake_data()

    # this error doesn't show up unless there's a wrapper around the call
    # such as in testthat or RMarkdown. this is replicated below
    # the culprit is raster::projectRaster()
    #
    # withCallingHandlers(
    #   print(
    #     ggplot() +
    #       annotation_map_tile(type = "osm", progress = "none") +
    #       layer_spatial(longlake_depthdf) +
    #       coord_sf(crs = 26920) +
    #       labs(caption = "just checking whether it prints without any messages")
    #   ),
    #   warning = function(w) stop(w)
    # )

    expect_silent(
      print(
        ggplot() +
          layer_spatial(longlake_osm) +
          labs(caption = "just checking whether it prints without any messages")
      )
    )

    expect_silent(
      print(
        ggplot() +
          layer_spatial(longlake_depth_raster) +
          labs(caption = "just checking whether it prints without any messages")
      )
    )

    expect_silent(
      print(
        ggplot() +
          annotation_map_tile(type = "osm", progress = "none") +
          layer_spatial(longlake_depthdf) +
          coord_sf(crs = 26920) +
          labs(caption = "just checking whether it prints without any messages")
      )
    )
  })
}
