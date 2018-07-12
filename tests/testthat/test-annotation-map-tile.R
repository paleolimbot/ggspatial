context("test-geom-osm.R")

# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  test_that("geom_osm works as intended", {
    load_longlake_data()

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          labs("This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 26920) +
          labs("This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3857) +
          labs("This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3978) +
          labs("This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
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
          facet_wrap(~type) +
          labs(caption = "this should have the maptypes correspond to the backdrop")
      ),
      "Zoom: 13"
    )

  })

}
