lonLat2cartesian <- function(lon, lat) {
  phi <- ((90 - lat) * pi)/180
  theta <- (lon * pi)/180

  x <- sin(phi) * cos(theta)
  y <- sin(phi) * sin(theta)
  z <- cos(phi)

  list(x = x, y = y, z = z)
}

cartesian2lonLat <- function(x, y, z) {
  phi <- acos(z)
  theta <- asin(y/(sin(phi)))

  lat <- 90 - ((180 * phi)/pi)
  lon <- (180 * theta)/pi

  list(lon = lon, lat = lat)
}

cartesianMeanCenter <- function(x, y, z, wts) {
  total <- sum(wts)

  x_mean <- sum(x * wts)/total
  y_mean <- sum(y * wts)/total
  z_mean <- sum(z * wts)/total

  L <- sqrt(sum((c(x_mean, y_mean, z_mean))^2))

  surface_x_mean <- x_mean/L
  surface_y_mean <- y_mean/L
  surface_z_mean <- z_mean/L

  list(x = surface_x_mean, y = surface_y_mean, z = surface_z_mean)
}

planarMeanCenter <- function (x, y, wts) {
  total <- sum(wts)

  x_mean <- sum(x * wts)/total
  y_mean <- sum(y * wts)/total

  list(x = x_mean, y = y_mean)
}

#' Mean Center
#'
#' @description
#'  `meanCenter()` returns the mean center for each group within
#'  a point or polygon simple features object.
#' 
#' @section Unprojected data:
#'  If `st_is_longlat(x)`, mean center is calculated
#'  assuming a spherical Earth. 
#' 
#' @section Projected data:
#'  If `!st_is_longlat(x)`, mean center is calculated assuming a
#' "flat" Earth.
#' 
#' @param x Input POINT or POLYGON simple features
#' @param group specifies groups to calculate individual mean centers
#'  for
#' @param weight numeric; weight specifying an individual point's 
#'  contribution to the mean center
#'  
#' 
#' @returns An sf object with a mean center for each group
#' @examples
#' df <- data.frame(
#'   lon = c(20, 50, 30, 80, 10),
#'   lat = c(25, 70, 30, 50, 30),
#'   grp = c("a", "b", "a", "b", "a"),
#'   wt = c(1,5,1,3,2))
#' x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
#' meanCenter(x, group = "grp", weight = "wt")
#' @export
meanCenter <- function (x, group = NULL, weight = NULL) {
  if (!inherits(x, "sf")) {
    stop(deparse(substitute(x)), " must be an simple features object")
  }
  if(any(!(as.character(sf::st_geometry_type(x)) %in% c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))) {
    stop(deparse(substitute(x)), " must contain only point or polygon geometries")
  }
  if(is.na(sf::st_crs(x))) {
    stop(deparse(substitute(x)), " must have a defined projection")
  }
  if (is.null(weight)) {
    wts <- rep(1, nrow(x))
  } else {
    if (!(weight %in% colnames(x))) {
      stop(weight, "` doesn't exist within ", deparse(substitute(x)))
    }
    wts <- x[[weight]]
    if (any(is.na(wts))) {
      stop(weight, " contains at least one missing value")
    } else if (!is.numeric(wts)) {
      stop(weight, " is not numeric")
    }
  }
  if (is.null(group)) {
    grps <- rep("a", nrow(x))
  } else {
    if (!(group %in% colnames(x))) {
      stop("Column `",  group, "` doesn't exist within ", deparse(substitute(x)))
    }
    grps <- x[[group]]
    if (any(is.na(grps))) {
      stop(group, " contains at least one missing value")
    } else if (!is.character(grps)) {
      stop(group, " is not a character")
    }
  }

  x <- suppressWarnings(sf::st_centroid(x))

  unique_groups <- unique(grps)
  geometry <- vector(mode = "list", length(unique_groups))
  names(geometry) <- unique_groups

  if (sf::st_is_longlat(x)) {
    lon <- sf::st_coordinates(x)[,1]
    lat <- sf::st_coordinates(x)[,2]
    cartesian <- lonLat2cartesian(lon, lat)
  
    centerArgs <- as.data.frame(cartesian)
    centerArgs$wt <- wts
    centerArgs_grped <- split(centerArgs, f = grps)
  
    for (grp in unique_groups) {
      meansCart <- do.call(cartesianMeanCenter, centerArgs_grped[[grp]])
      meansLonLat <- do.call(cartesian2lonLat, meansCart)
      geometry[[grp]] <- sf::st_point(unlist(meansLonLat))
    }
  } else {
    centerArgs <- data.frame(
      x = sf::st_coordinates(x)[,1], 
      y = sf::st_coordinates(x)[,2],
      wts = wts)
    centerArgs_grped <- split(centerArgs, f = grps)

    for (grp in unique_groups) {
      meansLonLat <- do.call(planarMeanCenter, centerArgs_grped[[grp]])
      geometry[[grp]] <- sf::st_point(unlist(meansLonLat))
    }
  }

  output <- sf::st_as_sf(data.frame(
    geometry = sf::st_sfc(geometry, crs = sf::st_crs(x))
  ))
  if (!is.null(group)) {
    output[[group]] = unique_groups
    output <- output[, c(2,1)]
  }
  output
}