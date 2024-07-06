euclid_xy_dist <- function(x, y, x_t, y_t) {
  sqrt((x - x_t)^2 + (y - y_t)^2)
}

planar_median_est <- function(x, y, x_t, y_t, wts) {
  d_t <- euclid_xy_dist(x, y, x_t, y_t)
  k_t <- wts / d_t

  x_estimate <- sum(k_t * x) / sum(k_t)
  y_estimate <- sum(k_t * y) / sum(k_t)

  list(x = x_estimate, y = y_estimate)
}

planar_median <- function(x, y, wts, tol = 0.01) {
  estimate <- planar_mean(x, y, wts)
  new_estimate <- planar_median_est(x, y, estimate$x, estimate$y, wts)

  while (abs(estimate$x - new_estimate$x) > tol || abs(estimate$y - new_estimate$y) > tol) {
    estimate <- new_estimate
    new_estimate <- planar_median_est(x, y, estimate$x, estimate$y, wts)
  }
  new_estimate
}

#' Median Center
#' `median_center()` returns the Euclidean median within
#' a specified tolerance for each group using the method
#' developed by Kuhn and Kuenne 1962.
median_center <- function(x, group = NULL, weight = NULL) {
  x_name <- deparse(substitute(x))
  allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")

  x <- x_checks(x, x_name, allowed_geom)
  grps <- group_checks(x, x_name, group)
  wts <- weight_checks(x, x_name, weight)

  unique_grps <- unique(grps)
  geometry <- vector(mode = "list", length(unique_grps))
  names(geometry) <- unique_grps

  if (sf::st_is_longlat(x)) {
    stop("Median centroid for unprojected data has not been implemented")
  } else {
    ctr_args <- data.frame(
      x = sf::st_coordinates(x)[, 1],
      y = sf::st_coordinates(x)[, 2],
      wts = wts)
    ctr_args_split <- split(ctr_args, f = grps)

    for (grp in unique_grps) {
      mean_xy <- do.call(planar_median, ctr_args_split[[grp]])
      geometry[[grp]] <- sf::st_point(unlist(mean_xy))
    }
  }

  output <- sf::st_as_sf(data.frame(
    geometry = sf::st_sfc(geometry, crs = sf::st_crs(x))
  ))
  if (!is.null(group)) {
    output[[group]] <- unique_grps
    output <- output[, c(2, 1)]
  }
  output
}
