
expect_doppelganger_extra <- function(what, x) {
  if (identical(Sys.getenv("GGSPATIAL_RUN_EXTRA_VDIFFR"), "true")) {
    vdiffr::expect_doppelganger(what, {{ x }})
  } else {
    ggplot2::ggplot_gtable(ggplot2::ggplot_build(x))
    testthat::expect_true(TRUE)
    x
  }
}

expect_doppelganger <- function(what, x) {
  vdiffr::expect_doppelganger(what, {{ x }})
}
