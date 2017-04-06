
# this method performs the join between the fortified data frame
# and the original object. it starts by calling fortify() on
# the object


#' Create a data frame from a Spatial object with attributes
#'
#' The \link[ggplot2]{fortify} function returns a data frame with geometry information;
#' \code{spatial_fortify()} returns a data frame with the results of fortify (left) joined
#' with the attribute table. If there is no attribute table, the results of fortify are
#' returned with the column names preceeded by a \code{.}.
#'
#' @param x A spatial object
#' @param attrs An optional attribute table, NA for the default, or NULL for none.
#' @param ... Additional arguments passed to \link[ggplot2]{fortify}
#'
#' @return A data.frame with (at least) columns ".lat" and ".long".
#' @export
#'
#' @examples
#' head(spatial_fortify(longlake_depthsdf))
#'
spatial_fortify <- function(x, attrs = NULL, ...) UseMethod("spatial_fortify")

#' @rdname spatial_fortify
#' @export
spatial_fortify.default <- function(x, attrs = NA, ...) {
  # check input
  if(!is.null(attrs) && !identical(attrs, NA) && !inherits(attrs, "data.frame")) {
    stop("Argument 'attrs' must be a data.frame")
  }

  # call fortify (will throw error if there is no method for type)
  fortified <- ggplot2::fortify(x, ...)

  # obscure names
  names(fortified) <- paste0(".", names(fortified))

  # determine attribute table
  if(!is.null(attrs) && !identical(attrs, NA)) {
    # add .id column
    attrs$.id <- rownames(attrs)

    # check for completeness
    if(any(!(attrs$.id %in% fortified$.id))) {
      stop("Some .id values are present in attrs and missing in fortified")
    }
    if(any(!(fortified$.id %in% attrs$.id))) {
      stop("Some .id values are present in fortified and missing in attrs")
    }

    # merge and return
    merge(fortified, attrs, by = ".id", all.x = TRUE)
  } else {
    fortified
  }
}

# this function checks the output of spatial_fortify
check_spatial_fortify <- function(x, df) {
  # check for data.frame
  if(!inherits(df, "data.frame")) {
    stop("spatial_fortify for class '", class(x)[1],
         "' did not return a data.frame")
  }

  # check for .long and .lat
  if(!(".long" %in% names(df))) {
    stop("spatial_fortify for class '", class(x)[1],
         "' did not return a data.frame with column '.long'")
  }
  if(!(".lat" %in% names(df))) {
    stop("spatial_fortify for class '", class(x)[1],
         "' did not return a data.frame with column '.lat'")
  }
}
