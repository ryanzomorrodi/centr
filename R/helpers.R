lonlat_cartesian <- function(x, y) {
  phi <- ((90 - y) * pi) / 180
  theta <- (x * pi) / 180

  x_cart <- sin(phi) * cos(theta)
  y_cart <- sin(phi) * sin(theta)
  z_cart <- cos(phi)

  list(x = x_cart, y = y_cart, z = z_cart)
}

cartesian_lonlat <- function(x, y, z) {
  lat <- 90 - ((180 * acos(z)) / pi)
  lon <-
    ifelse(x > 0,
      atan(y / x) * (180 / pi),
    ifelse(y > 0,
      atan(y / x) * (180 / pi) + 180,
      atan(y / x) * (180 / pi) - 180))

  list(x = lon, y = lat)
}

x_checks <- function(x, x_name, allowed_geom) {
  if (!inherits(x, "sf")) {
    stop(x_name, " must be an simple features object")
  }
  if (any(!(as.character(sf::st_geometry_type(x)) %in% allowed_geom))) {
    # probably should make more extendable
    stop(x_name, " must contain only point or polygon geometries")
  }
  if (is.na(sf::st_crs(x))) {
    stop(x_name, " must have a defined projection")
  }
  suppressWarnings(sf::st_centroid(x))
}

weight_checks <- function(x, x_name, weight) {
  if (is.null(weight)) {
    rep(1, nrow(x))
  } else {
    if (!(weight %in% colnames(x))) {
      stop(weight, "` doesn't exist within ", x_name)
    }
    if (!is.numeric(x[[weight]])) {
      stop(weight, " is not numeric")
    } else if (any(is.na(x[[weight]]))) {
      stop(weight, " contains at least one missing value")
    } else if (any(x[[weight]] < 0)) {
      stop(weight, " contains at least one negative value")
    }
    x[[weight]]
  }
}

group_checks <- function(x, x_name, group) {
  if (is.null(group)) {
    rep("1", nrow(x))
  } else {
    if (!(group %in% colnames(x))) {
      stop("Column `", group, "` doesn't exist within ", x_name)
    }
    if (any(is.na(x[[group]]))) {
      stop(group, " contains at least one missing value")
    } else if (!is.character(x[[group]])) {
      stop(group, " is not a character")
    }
    x[[group]]
  }
}

# simplified version of purrr function (depreciated)
when <- function(.x, .p, .f, ...) {
  if (!.p) {
    return(.x)
  }
  .f(..., .x)
}

# simplified version of tibble function
rownames_to_column <- function(x, colname) {
  x <- cbind(rownames(x), x)
  colnames(x)[1] <- colname
  rownames(x) <- NULL
  x
}
