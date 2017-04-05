
# this method performs the join between the fortified data frame
# and the original object. it starts by calling fortify() on
# the object
spatial_fortify <- function(x, attrs = NULL, ...) {
  UseMethod("spatial_fortify")
}

spatial_fortify.default <- function(x, attrs = NULL, ...) {
  # check input
  if(!is.null(attrs) && !inherits(attrs, "data.frame")) {
    stop("Argument 'attrs' must be a data.frame")
  }

  # call fortify (will throw error if there is no method for type)
  fortified <- ggplot2::fortify(x, ...)

  # obscure names
  names(fortified) <- paste0(".", names(fortified))

  # determine attribute table
  if(!is.null(attrs)) {
    attrs <- x@data
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
