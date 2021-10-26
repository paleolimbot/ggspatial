
# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  test_that("annotation_map_tile() works as intended", {
    skip_if_not_installed("vdiffr")

    load_longlake_data(which = "longlake_waterdf")

    expect_message(
      expect_doppelganger_extra(
        "default EPSG",
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50")
      ),
      "Zoom: 13"
    )

    expect_message(
      expect_doppelganger_extra(
        "non-default EPSG",
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 26920)
      ),
      "Zoom: 13"
    )

    expect_message(
      expect_doppelganger_extra(
        "specified default EPSG",
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3857)
      ),
      "Zoom: 13"
    )

    expect_message(
      expect_doppelganger_extra(
        "extreme rotated EPSG",
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3978)
      ),
      "Zoom: 13"
    )

    expect_message(
      expect_doppelganger_extra(
        "faceted type",
        ggplot() +
          annotation_map_tile(
            data = tibble::tibble(
              type = c("osm", "stamenbw", "cartolight", "cartodark"),
              zoom = 13
            ),
            cachedir = system.file("rosm.cache", package = "ggspatial"),
            mapping = aes(type = type, zoom = zoom)
          ) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3857) +
          ggplot2::facet_wrap(~type)
      ),
      "Zoom: 13"
    )

    df_alta <- data.frame(lon = c(-122.974, -123.0042), lat = c(50.1232, 50.1035))
    p <- ggplot(df_alta, aes(lon, lat)) + geom_spatial_point(crs = 4326)
    expect_doppelganger_extra(
      "hillshade",
      p +
        annotation_map_tile(type = "hillshade", alpha = 1)
    )

    expect_doppelganger_extra(
      "hillshade with alpha",
      p +
        annotation_map_tile(type = "hillshade", alpha = 0.5)
    )
    expect_doppelganger_extra(
      "projected hillshade",
      p +
        annotation_map_tile(type = "hillshade", alpha = 1) +
        coord_sf(crs = 26910)
    )
    expect_doppelganger_extra(
      "projected hillshade w/alpha",
      p +
        annotation_map_tile(type = "hillshade", alpha = 0.5) +
        coord_sf(crs = 26910)
    )


    expect_doppelganger_extra(
      "rgb tile",
      p +
        annotation_map_tile(alpha = 1)
    )

    expect_doppelganger_extra(
      "rgb tile with alpha",
      p +
        annotation_map_tile(alpha = 0.5)
    )
    expect_doppelganger_extra(
      "rgb tile projected",
      p +
        annotation_map_tile(alpha = 1) +
        coord_sf(crs = 26910)
    )
    expect_doppelganger_extra(
      "rgb tile projected with alpha",
      p +
        annotation_map_tile(alpha = 0.5) +
        coord_sf(crs = 26910)
    )

  })

  test_that("annotation_map_tile() does not fail when handed a single point", {
    p <- sf::st_sf(sf::st_sfc(sf::st_point(c(219200, 629650)), crs = 2039)) %>%
      ggplot() +
      annotation_map_tile() +
      geom_sf()

    expect_warning(ggplot2::ggplotGrob(p), "bounding box is too small")
  })
}
