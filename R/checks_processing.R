lonlat_cartesian <- function(X, Y) {
  phi <- ((90 - Y) * pi) / 180
  theta <- (X * pi) / 180

  x_cart <- sin(phi) * cos(theta)
  y_cart <- sin(phi) * sin(theta)
  z_cart <- cos(phi)

  list(x = x_cart, y = y_cart, z = z_cart)
}

cartesian_lonlat <- function(x, y, z) {
  lat <- 90 - ((180 * acos(z)) / pi)
  lon <- ifelse(
    x > 0,
    atan(y / x) * (180 / pi),
    ifelse(
      y > 0,
      atan(y / x) * (180 / pi) + 180,
      atan(y / x) * (180 / pi) - 180
    )
  )

  list(X = lon, Y = lat)
}

x_checks <- function(x, x_name) {
  if (!(inherits(x, "sf") || inherits(x, "SpatVector"))) {
    stop("`", x_name, "` is not a simple features or spatial vector object")
  }
  if (!is_point_or_poly(x)) {
    stop("`", x_name, "` does not have only point or polygon geometries")
  }
  if (has_null_crs(x)) {
    stop("`", x_name, "` does not have defined coordinate reference system")
  }
  if (has_empty_geom(x)) {
    stop("`", x_name, "` contains empty geometries")
  }
}

weight_checks <- function(x, x_name, weight) {
  if (is.null(weight)) {
    return(NULL)
  }
  if (!(weight %in% names(x))) {
    stop("`", weight, "` does not exist within ", x_name)
  }

  if (!is.numeric(pluck(x, weight))) {
    stop("`", weight, "` is not numeric")
  }
  if (any(is.na(pluck(x, weight)))) {
    stop("`", weight, "` contains at least one missing value")
  }
  if (any(pluck(x, weight) < 0)) {
    stop("`", weight, "` contains at least one negative value")
  }
}

group_checks <- function(x, x_name, group) {
  if (is.null(group)) {
    return(NULL)
  }
  if (!(group %in% names(x))) {
    stop("`", group, "` does not exist within ", x_name)
  }

  if (any(is.na(pluck(x, group)))) {
    stop("`", group, "` contains at least one missing value")
  }
}

x_processing <- function(x, group, weight) {
  coordinates <- centroids(x) |>
    coordinates()

  x_weighted <- coordinates |>
    when(is_lonlat(x), do.call, what = lonlat_cartesian) |>
    as.data.frame() |>
    when(!is.null(weight), cbind, wts = pluck(x, weight))
  
  if (is.null(group)) {
    x_split <- list(x_weighted)
  } else {
    x_split <- split(x_weighted, factor(pluck(x, group), unique(pluck(x, group))))
  }

  x_split
}

output_processing <- function(centers, x, group) {
  output <- do.call(centers, what = rbind) |>
    as.data.frame() |>
    when(!is.null(group), cbind, group = unique(pluck(x, group)))

  if (inherits(x, "sf")) {
    output <- sf::st_as_sf(
      output,
      coords = c("X", "Y"), 
      crs = sf::st_crs(x), 
      na.fail = FALSE
    )
  } else {
    output <- terra::vect(
      output,
      geom = c("X", "Y"), 
      crs = terra::crs(x)
    )
  }
  
  class(output) <- class(x)

  output
}

# simplified version of purrr function (depreciated)
when <- function(.x, .p, .f, ...) {
  if (!.p) {
    return(.x)
  }
  .f(..., .x)
}
