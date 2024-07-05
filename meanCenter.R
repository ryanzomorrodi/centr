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

cartesianMeanCenter <- function(x, y, z, wt) {
  wt <- ifelse(is.null(wt), rep(1, length(x)), wt)

  total <- sum(wt)

  x_mean <- sum(x * wt)/total
  y_mean <- sum(y * wt)/total
  z_mean <- sum(z * wt)/total

  L <- sqrt(sum((c(x_mean, y_mean, z_mean))^2))

  surface_x_mean <- x_mean/L
  surface_y_mean <- y_mean/L
  surface_z_mean <- z_mean/L

  list(x = surface_x_mean, y = surface_y_mean, z = surface_z_mean)
}

meanCenter <- function (x, group = NULL, weight = NULL) {
  if (is.null(weight)) {
    weight = rep(1, nrow(x))
  } else {
    stopifnot(weight %in% colnames(x))
    weight = x[[weight]]
  }

  lon <- sf::st_coordinates(x)[,1]
  lat <- sf::st_coordinates(x)[,2]
  cartesian <- lonLat2cartesian(lon, lat)

  centerArgs <- as.data.frame(cartesian)
  centerArgs$wt <- weight

  if (is.null(group)) {
    meanCart <- do.call(cartesianMeanCenter, centerArgs)
    meanLonLat <- do.call(cartesian2lonLat, meanCart)
    point <- sf::st_point(unlist(meanLonLat))

    return(sf::st_as_sf(sf::st_sfc(point), crs = sf::st_crs(x)))
  }

  stopifnot(group %in% colnames(x))
  centerArgs_grped <- split(centerArgs, f = x[[group]])
  unique_groups <- unique(x[[group]])
  geometry <- vector(mode = "list", length(unique_groups))
  names(geometry) <- unique_groups

  for (group in unique_groups) {
    meansCart <- do.call(cartesianMeanCenter, centerArgs_grped[[group]])
    meansLonLat <- do.call(cartesian2lonLat, meansCart)
    geometry[[group]] <- sf::st_point(unlist(meansLonLat))
  }

  sf::st_as_sf(data.frame(
    group = unique_groups,
    geometry = sf::st_sfc(geometry, crs = st_crs(x))
  ))
}