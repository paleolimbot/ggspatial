context("test-geom-osm.R")

# max test length was exceeded on CRAN, so these tests are skipped
if (identical(Sys.getenv("NOT_CRAN"), "true")) {

  test_that("annotation_map_tile() works as intended", {
    load_longlake_data(which = "longlake_waterdf")

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          ggplot2::labs(caption = "This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 26920) +
          ggplot2::labs(caption = "This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3857) +
          ggplot2::labs(caption = "This plot sould have they grey outlines line up with the OSM map beneath")
      ),
      "Zoom: 13"
    )

    expect_message(
      print(
        ggplot() +
          annotation_map_tile(zoom = 13, cachedir = system.file("rosm.cache", package = "ggspatial")) +
          geom_sf(data = longlake_waterdf, fill = NA, col = "grey50") +
          coord_sf(crs = 3978) +
          ggplot2::labs(caption = "This plot sould have they grey outlines line up with the OSM map beneath")
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
          ggplot2::facet_wrap(~type) +
          ggplot2::labs(caption = "this should have the maptypes correspond to the backdrop")
      ),
      "Zoom: 13"
    )

    df_alta <- data.frame(lon = c(-122.974, -123.0042), lat = c(50.1232, 50.1035))
    p <- ggplot(df_alta, aes(lon, lat)) + geom_spatial_point(crs = 4326)
    print(
      p +
        annotation_map_tile(type = "hillshade", alpha = 1) +
        ggplot2::labs(caption = "RGBA tile with 1 alpha")
    )
    print(
      p +
        annotation_map_tile(type = "hillshade", alpha = 0.5) +
        ggplot2::labs(caption = "RGBA tile with 0.5 alpha")
    )
    print(
      p +
        annotation_map_tile(type = "hillshade", alpha = 1) +
        ggplot2::labs(caption = "RGBA tile with 1 alpha") +
        coord_sf(crs = 26910)
    )
    print(
      p +
        annotation_map_tile(type = "hillshade", alpha = 0.5) +
        ggplot2::labs(caption = "RGBA tile with 0.5 alpha") +
        coord_sf(crs = 26910)
    )


    print(
      p +
        annotation_map_tile(alpha = 1) +
        ggplot2::labs(caption = "RGB tile with 1 alpha")
    )
    print(
      p +
        annotation_map_tile(alpha = 0.5) +
        ggplot2::labs(caption = "RGB tile with 0.5 alpha")
    )
    print(
      p +
        annotation_map_tile(alpha = 1) +
        ggplot2::labs(caption = "RGB tile with 1 alpha") +
        coord_sf(crs = 26910)
    )
    print(
      p +
        annotation_map_tile(alpha = 0.5) +
        ggplot2::labs(caption = "RGB tile with 0.5 alpha") +
        coord_sf(crs = 26910)
    )

  })

}
