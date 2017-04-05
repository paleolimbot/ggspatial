
# this is essentially an automatic geometry chooser, since the
# geom is often implied by the input type
spatial_geom <- function(x) UseMethod("spatial_geom")
spatial_geom.default <- function(x) ggplot2::GeomPoint

# this is an automatic stat chooser, which is normally
# statIdentity but could theoreticaly be something else
spatial_stat <- function(x) UseMethod("spatial_stat")
spatial_stat.default <- function(x) ggplot2::StatIdentity

# this is the automatic aes chooser, which picks default x and y
# (or other) aesthetics
spatial_default_aes <- function(x) UseMethod("spatial_default_aes")
spatial_default_aes.default <- function(x) {
  ggplot2::aes_string(x = ".long", y = ".lat")
}

# also need a method to combine aesthetics with overriding values
override_aesthetics <- function(user_mapping = NULL, default_mapping = NULL) {
  if(is.null(user_mapping) && is.null(default_mapping)) {
    ggplot2::aes()
  } else if(is.null(default_mapping)) {
    user_mapping
  } else if(is.null(user_mapping)) {
    default_mapping
  } else {
    new_aes <- c(user_mapping, default_mapping)
    class(new_aes) <- "uneval"
    new_aes
  }
}
