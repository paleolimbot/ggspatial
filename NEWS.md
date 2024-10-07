# ggspatial (development version)

* Updated `load_longlake_data()` to use terra package by default instead of the deprecated raster package
* Updated example for `layer_spatial()` to avoid error when plotting raster layer
* Added `"line"` style and `text_pos` argument to `annotatation_scale()`.  
`annotation_scale(style = "line", width_hint = 0.2)` yields a scale bar line
with the bar length text above it. 

# ggspatial 1.1.9

* Fix donttest examples for updated raster/terra stack.

# ggspatial 1.1.8

* Fix `annotation_spatial()` for the latest ggplot2 release (#114, #115).

# ggspatial 1.1.7

* Fixed deprecated behaviour advanced in the latest ggplot2
  release (#106).

# ggspatial 1.1.6

* Added terra package support (@dieghernan, #92, #94)
* Fix north arrow rotation (@potash, #82)
* Add support for factor/categorical rasters (@JoshOBrien, #78)
* Better raster support for stars (@dieghernan, #95)
* Fix example data for updated sf/GDAL (@rsbivand, #101)

# ggspatial 1.1.5

* Added a `NEWS.md` file to track changes to the package.
* Suppressed discarded datum warnings that resulted from loading
  of test data.
* Suppressed discarded datum warnings that resulted from
  'rosm' package operations.
* Fixed an error that caused RStudio to crash from excessive
  memory allocation resulting when trying to plot a single point
  with `annotation_map_tile()` (#74).
* Ensured that packages in 'Suggests' are used conditionally in
  tests and examples.
