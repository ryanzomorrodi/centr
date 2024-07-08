lonlat_cartesian <- function(lon, lat) {
  phi <- ((90 - lat) * pi) / 180
  theta <- (lon * pi) / 180

  x <- sin(phi) * cos(theta)
  y <- sin(phi) * sin(theta)
  z <- cos(phi)

  list(x = x, y = y, z = z)
}

cartesian_lonlat <- function(x, y, z) {
  lat <- 90 - ((180 * acos(z)) / pi)
  lon <- 
    ifelse(x > 0 ,
      atan(y / x) * (180 / pi),
    ifelse(y > 0,
      atan(y / x) * (180 / pi) + 180,
      atan(y / x) * (180 / pi) - 180))

  list(lon = lon, lat = lat)
}

x_checks <- function(x, x_name, allowed_geom) {
  if (!inherits(x, "sf")) {
    stop(x_name, " must be an simple features object")
  }
  if (any(!(as.character(sf::st_geometry_type(x)) %in% allowed_geom))) {
    stop(x_name, " must contain only point or polygon geometries") # probably should make more extendable
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
    if (any(is.na(x[[weight]]))) {
      stop(weight, " contains at least one missing value")
    } else if (!is.numeric(x[[weight]])) {
      stop(weight, " is not numeric")
    }
    x[[weight]]
  }
}

group_checks <- function(x, x_name, group) {
  if (is.null(group)) {
    rep("a", nrow(x))
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
