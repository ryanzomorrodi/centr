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
    wts <- rep(1, nrow(x))
  } else {
    if (!(weight %in% colnames(x))) {
      stop(paste0("Column `",  weight, "` doesn't exist"))
    }
    wts <- x[[weight]]
  }

  if (is.null(group)) {
    grps <- rep(1, nrow(x))
  } else {
    if (!(group %in% colnames(x))) {
      stop(paste0("Column `",  group, "` doesn't exist"))
    }
    grps <- x[[group]]
  }

  if (sf::st_is_longlat(x)) {
    lon <- sf::st_coordinates(x)[,1]
    lat <- sf::st_coordinates(x)[,2]
    cartesian <- lonLat2cartesian(lon, lat)
  
    centerArgs <- as.data.frame(cartesian)
    centerArgs$wt <- wts

    centerArgs_grped <- split(centerArgs, f = grps)
    unique_groups <- unique(grps)
    geometry <- vector(mode = "list", length(unique_groups))
    names(geometry) <- unique_groups
  
    for (grp in unique_groups) {
      meansCart <- do.call(cartesianMeanCenter, centerArgs_grped[[grp]])
      meansLonLat <- do.call(cartesian2lonLat, meansCart)
      geometry[[grp]] <- sf::st_point(unlist(meansLonLat))
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
}