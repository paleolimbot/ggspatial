ggspatial: Spatial data framework for ggplot2
================

[![ggspatial on CRAN](https://cranlogs.r-pkg.org/badges/ggspatial)](https://cran.r-project.org/package=ggspatial) [![Travis-CI Build Status](https://travis-ci.org/paleolimbot/ggspatial.svg?branch=master)](https://travis-ci.org/paleolimbot/ggspatial) [![Coverage Status](https://img.shields.io/codecov/c/github/paleolimbot/ggspatial/master.svg)](https://codecov.io/github/paleolimbot/ggspatial?branch=master)

Spatial data plus the power of the `ggplot2` framework means easier mapping.

Installation
------------

The package is available on CRAN, and can be installed using `install.packages("ggspatial")`. The development version can be installed via **devtools**. Currently, the package depends on the not-quite-releasted version of ggplot2, which can also be installed using **devtools**.

``` r
install.packages("devtools") # if devtools isn't installed
devtools::install_github("paleolimbot/ggspatial")
```

Introduction
------------

This package is a framework for interacting with spatial data using **ggplot2** as a plotting backend. The package supports **sf** package objects, **sp** package objects, and **raster** package objects, and uses `geom_sf()` and `coord_sf()` to do most of the heavy lifting with respect to coordinate transformation.

``` r
library(ggspatial)
load_longlake_data()

ggplot() +
  # loads background map tiles from a tile source
  annotation_map_tile(zoomin = -1) +
  
  # annotation_spatial() layers don't train the scales, so data stays central
  annotation_spatial(longlake_roadsdf, size = 2, col = "black") +
  annotation_spatial(longlake_roadsdf, size = 1.6, col = "white") +

  # raster layers train scales and get projected automatically
  layer_spatial(longlake_depth_raster, aes(fill = NULL, alpha = stat(band1)), fill = "darkblue") +
  scale_alpha_continuous(na.value = 0) +
  
  # layer_spatial trains the scales
  layer_spatial(longlake_depthdf, aes(col = DEPTH.M)) +
  
  # spatial-aware automagic scale bar
  annotation_scale(location = "tl")
```

![](README_files/figure-markdown_github/fig-layer-spatial-sf-1.png)
