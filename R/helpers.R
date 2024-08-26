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

x_checks <- function(x, x_name, allowed_geom) {
  if (!inherits(x, "sf")) {
    stop("`", x_name, "` is not an simple features object")
  }
  if (any(!(as.character(sf::st_geometry_type(x)) %in% allowed_geom))) {
    stop("`", x_name, "` does not have only point or polygon geometries")
  }
  if (is.na(sf::st_crs(x))) {
    stop("`", x_name, "` does not have defined coordinate reference system")
  }
  if (any(sf::st_is_empty(x))) {
    stop("`", x_name, "` contains empty geometries")
  }
}

x_processing <- function(x, is_lonlat, group, weight) {
  coordinates <- suppressWarnings(sf::st_centroid(x)) |>
    sf::st_coordinates() |>
    as.data.frame()

  x_weighted <- coordinates |>
    when(is_lonlat, do.call, what = lonlat_cartesian) |>
    as.data.frame() |>
    when(!is.null(weight), cbind, wts = x[[weight]])
  
  if (is.null(group)) {
    x_split <- list(x_weighted)
  } else {
    x_split <- split(x_weighted, factor(x[[group]], unique(x[[group]])))
  }

  x_split
}

weight_checks <- function(x, x_name, weight) {
  if (is.null(weight)) {
    return(NULL)
  }
  if (!(weight %in% colnames(x))) {
    stop("`", weight, "` does not exist within ", x_name)
  }

  if (!is.numeric(x[[weight]])) {
    stop("`", weight, "` is not numeric")
  }
  if (any(is.na(x[[weight]]))) {
    stop("`", weight, "` contains at least one missing value")
  }
  if (any(x[[weight]] < 0)) {
    stop("`", weight, "` contains at least one negative value")
  }
}

group_checks <- function(x, x_name, group) {
  if (is.null(group)) {
    return(NULL)
  }
  if (!(group %in% colnames(x))) {
    stop("`", group, "` does not exist within ", x_name)
  }

  if (any(is.na(x[[group]]))) {
    stop("`", group, "` contains at least one missing value")
  }
}

output_processing <- function(centers, x, group) {
  output <- do.call(centers, what = rbind) |>
    as.data.frame() |>
    when(!is.null(group), cbind, group = unique(x[[group]])) |>
    sf::st_as_sf(coords = c("X", "Y"), crs = sf::st_crs(x), na.fail = FALSE)

  rownames(output) <- NULL
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
