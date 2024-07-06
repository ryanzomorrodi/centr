#' Median Center
#' 
#' `medianCenter()` returns the Euclidean median within
#' a specified tolerance for each group using the method 
#' developed by Kuhn and Kuenne 1962.
#' 

euclideanXYDistance <- function(x, y, x_t, y_t) {
  sqrt((x - x_t)^2 + (y - y_t)^2)
}

planarMedianCenterEstimate <- function(x, y, x_t, y_t, wts) {
  d_t <- euclideanXYDistance(x, y, x_t, y_t)
  K_t <- wts/d_t

  x_estimate <- sum(K_t * x)/sum(K_t)
  y_estimate <- sum(K_t * y)/sum(K_t)

  list(x = x_estimate, y = y_estimate)
}

planarMedianCenter <- function(x, y, wts, tol = 0.01) {
  estimate <- planarMeanCenter(x, y, wts)
  new_estimate <- planarMedianCenterEstimate(x, y, estimate$x, estimate$y, wts)

  while(abs(estimate$x - new_estimate$x) > tol | abs(estimate$y - new_estimate$y) > tol) {
    estimate <- new_estimate
    new_estimate <- planarMedianCenterEstimate(x, y, estimate$x, estimate$y, wts)
  }
  new_estimate
}

medianCenter <- function (x, group = NULL, weight = NULL) {
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
    stop("Median centroid for unprojected data has not been implemented")
  } else {
    centerArgs <- data.frame(
      x = sf::st_coordinates(x)[,1], 
      y = sf::st_coordinates(x)[,2],
      wts = wts)
    centerArgs_grped <- split(centerArgs, f = grps)

    for (grp in unique_groups) {
      meansLonLat <- do.call(planarMedianCenter, centerArgs_grped[[grp]])
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