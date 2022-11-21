
# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_that("layer-spatial works for stars objects", {
    skip_if_not_installed("vdiffr")
    skip_if_not_installed("stars")

    load_longlake_data(
      which = c(
        "longlake_osm", "longlake_depthdf",
        "longlake_depth_raster"
      ),
      raster_format = "stars"
    )

    expect_s3_class(longlake_osm, "stars")
    expect_s3_class(longlake_depth_raster, "stars")


    # should have little grey thing around it
    expect_doppelganger_extra(
      "layer_spatial.stars()",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # should not have little grey thing around it
    expect_doppelganger_extra(
      "annotation_spatial.stars()",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf)
    )

    # grey thing
    expect_doppelganger_extra(
      "layer_spatial.stars() project",
      ggplot() +
        layer_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # no grey thing
    expect_doppelganger_extra(
      "annotation_spatial.stars() project",
      ggplot() +
        annotation_spatial(longlake_osm) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with alpha
    expect_doppelganger_extra(
      "annotation_spatial.stars() alpha 0.3",
      ggplot() +
        annotation_spatial(longlake_osm, alpha = 0.3) +
        layer_spatial(longlake_depthdf) +
        coord_sf(crs = 3978)
    )

    # with aesthetics
    expect_doppelganger_extra(
      "layer_spatial.stars() aes()",
      ggplot() +
        layer_spatial(longlake_osm, aes()) +
        layer_spatial(longlake_depthdf)
    )

    expect_doppelganger_extra(
      "layer_spatial.stars() aes(band1)",
      ggplot() +
        layer_spatial(
          longlake_osm,
          aes(alpha = after_stat(band1), fill = NULL)
        ) +
        layer_spatial(longlake_depthdf)
    )

    # dpi works
    expect_doppelganger_extra(
      "layer_spatial.stars() dpi",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE, dpi = 5)
    )

    # lazy works
    expect_doppelganger_extra(
      "layer_spatial.stars() lazy",
      ggplot() +
        layer_spatial(longlake_osm, lazy = TRUE)
    )

    # interpolation works
    expect_doppelganger_extra(
      "layer_spatial.stars() no interpolation",
      ggplot() +
        layer_spatial(longlake_osm,
                      lazy = TRUE,
                      interpolate = FALSE,
                      dpi = 5
        )
    )

    # Test examples
    expect_doppelganger_extra(
      "layer_spatial.stars() example 1",
      ggplot() +
        layer_spatial(longlake_osm)
    )
    expect_doppelganger_extra(
      "layer_spatial.stars() example 2",
      # Not removing NAs to avoid warning
      ggplot() +
        layer_spatial(longlake_depth_raster) +
        ggplot2::scale_fill_continuous(type = "viridis")
    )
  })
}

test_that("Full lifecycle", {
  skip_if_not_installed("stars")
  skip_on_cran()
  # Read
  stars <- stars::read_stars(
    system.file("longlake/longlake.tif", package = "ggspatial")
  )
  expect_s3_class(stars, "stars")

  plot <- ggplot() +
    layer_spatial(stars)
  expect_s3_class(plot, "ggplot")

  # Write to tempfile without warning
  tmp <- tempfile(fileext = ".png")

  expect_silent(ggplot2::ggsave(tmp, plot, width = 4, height = 4))

  unlink(tmp)
})

test_that("Test that stars array (unprojected) is identical to terra and raster", {
  skip_if_not_installed("stars")
  skip_if_not_installed("raster")
  skip_on_cran()


  test_raster <- new.env(parent = emptyenv())
  load_longlake_data(test_raster, raster_format = "raster")


  test_stars <- new.env(parent = emptyenv())
  load_longlake_data(test_stars, raster_format = "stars")

  # Passing raster to stars
  # Expect error here
  expect_s4_class(test_raster$longlake_osm, "Raster")
  expect_error(stars_as_array(test_raster$longlake_osm))


  # arrays are identical
  expect_identical(
    raster_as_array(test_raster$longlake_osm),
    stars_as_array(test_stars$longlake_osm)
  )


  # With alpha
  expect_identical(
    raster_as_array(test_raster$longlake_osm, alpha = 0.4),
    stars_as_array(test_stars$longlake_osm, alpha = 0.4)
  )


  # Compare now stars vs terra
  skip_if_not_installed("terra")

  test_terra <- new.env(parent = emptyenv())
  load_longlake_data(test_terra, raster_format = "terra")

  # arrays are identical
  expect_identical(
    terra_as_array(test_terra$longlake_osm),
    stars_as_array(test_stars$longlake_osm)
  )


  # With alpha
  expect_identical(
    terra_as_array(test_terra$longlake_osm, alpha = 0.4),
    stars_as_array(test_stars$longlake_osm, alpha = 0.4)
  )
})
