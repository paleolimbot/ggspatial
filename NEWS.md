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
