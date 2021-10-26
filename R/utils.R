
expect_doppelganger_extra <- function(what, x) {
  if (identical(Sys.getenv("GGSPATIAL_RUN_EXTRA_VDIFFR"), "true")) {
    vdiffr::expect_doppelganger(what, {{ x }})
  } else {
    force(x)
  }
}

expect_doppelganger <- function(what, x) {
  vdiffr::expect_doppelganger(what, {{ x }})
}
