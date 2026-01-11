check_sf <- function(
  x,
  ...,
  allow_empty = TRUE,
  allow_na_crs = FALSE,
  allow_geom_type = NULL,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (inherits(x, "sf")) {
      if (!allow_empty && any(sf::st_is_empty(x))) {
        abort(
          sprintf("`%s` can't contain empty geometries.", arg),
          arg = arg,
          call = call
        )
      }
      if (!allow_na_crs && is.na(sf::st_crs(x))) {
        abort(
          sprintf("`%s` can't have an empty crs.", arg),
          arg = arg,
          call = call
        )
      }
      if (!is.null(allow_geom_type) && !all(sf::st_is(x, allow_geom_type))) {
        abort(
          sprintf(
            "`%s`'s geometry type must be %s.",
            arg,
            oxford_comma(allow_geom_type)
          ),
          arg = arg,
          call = call
        )
      }

      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "an <sf> object",
    ...,
    arg = arg,
    call = call
  )
}

check_numeric <- function(
  x,
  ...,
  min = NULL,
  max = NULL,
  allow_infinite = TRUE,
  allow_na = FALSE,
  allow_null = FALSE,
  arg = caller_arg(x),
  call = caller_env()
) {
  if (!missing(x)) {
    if (is.numeric(x)) {
      if (!allow_na && any(is.na(x))) {
        abort(
          sprintf("`%s` can't contain NA values.", arg),
          arg = arg,
          call = call
        )
      }
      if (!allow_infinite && any(is.infinite(x))) {
        abort(
          sprintf("`%s` can't contain Inf values.", arg),
          arg = arg,
          call = call
        )
      }
      if (!is.null(min) && any(x < min)) {
        abort(
          sprintf("`%s` can't contain values less than %s.", arg, min),
          arg = arg,
          call = call
        )
      }
      if (!is.null(max) && any(x > max)) {
        abort(
          sprintf("`%s` can't contain values more than %s.", arg, max),
          arg = arg,
          call = call
        )
      }

      return(invisible(NULL))
    }

    if (allow_null && is_null(x)) {
      return(invisible(NULL))
    }
  }

  stop_input_type(
    x,
    "a numeric vector",
    ...,
    allow_null = allow_null,
    arg = arg,
    call = call
  )
}

check_column_exists <- function(
  x,
  column_name,
  ...,
  x_arg = caller_arg(x),
  column_name_arg = caller_arg(column_name),
  call = call
) {
  if (missing(x) || !is.data.frame(x)) {
    return(invisible(NULL))
  }

  if (!(column_name %in% names(x))) {
    abort(
      sprintf("Can't find `%s` column in `%s`.", column_name_arg, x_arg),
      call = call
    )
  }
}
