euclid_xy_dist <- function(X, Y, X_t, Y_t) {
  dist <- sqrt((X - X_t)^2 + (Y - Y_t)^2)
  dist[dist == 0] <- .Machine$double.eps

  dist
}

planar_median_est <- function(X, Y, X_t, Y_t, wts = NULL) {
  d_t <- euclid_xy_dist(X, Y, X_t, Y_t)
  k_t <- wts / d_t

  x_estimate <- sum(k_t * X) / sum(k_t)
  y_estimate <- sum(k_t * Y) / sum(k_t)

  list(X = x_estimate, Y = y_estimate)
}

planar_median <- function(X, Y, tol, wts = NULL) {
  if (is.null(wts)) {
    wts <- rep(1, length(X))
  } else if (sum(wts) == 0) {
    warning("Empty point returned for groups with zero total weight")
    return(list(x = NA_real_, y = NA_real_))
  }

  estimate <- planar_mean(X, Y, wts)
  new_estimate <- planar_median_est(X, Y, estimate$X, estimate$Y, wts)

  while (any(abs(unlist(estimate) - unlist(new_estimate)) > tol)) {
    estimate <- new_estimate
    new_estimate <- planar_median_est(X, Y, estimate$X, estimate$Y, wts)
  }
  new_estimate
}

#' Median Center
#'
#' @description
#' Median center iteratively calculates the point that minimizes
#' distance to all features. One can specify the groups to calculate
#' individual centers for and weights for each individual point. It
#' is analagous to the [ArcGIS Pro Median Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/median-center.htm)
#' tool.
#'
#' It uses the methodology introduced by Kuhn and Kuenne (1962).
#'
#' Currently, median center is only implemenented for projected data.
#' @param x Input POINT, MULTIPOINT, POLYGON, or MULTIPOLYGON
#'  simple features
#' @param group name of character column specifying groups
#'  to calculate individual median centers for
#' @param weight name of numeric weight column specifying an
#'  individual point's contribution to the median center
#' @param tolerance numeric threshold determining when an
#'  estimate improvement is sufficiently small enough to stop
#'  iterating (smaller = slower, but more precision)
#' @returns An sf object with a median center for each group
#' @examples
#' df <- data.frame(
#'   lon = c(-88, -90, -92, -89, -90),
#'   lat = c(42, 40, 30, 32, 42),
#'   grp = c("a", "b", "a", "b", "a"),
#'   wt = c(1, 1, 1, 1, 1)
#' )
#' x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
#' x_transformed <- sf::st_transform(x, crs = "ESRI:102003")
#' median_center(x_transformed, group = "grp", weight = "wt")
#' @export
median_center <- function(x, group = NULL, weight = NULL, tolerance = 0.0001) {
  x_name <- deparse(substitute(x))
  is_lonlat <- sf::st_is_longlat(x)
  allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")

  x_checks(x, x_name, allowed_geom)
  if (is_lonlat) {
    stop("`", x_name, "` does not have a defined projection")
  }

  group_checks(x, x_name, group)
  weight_checks(x, x_name, weight)

  x_processed <- x_processing(x, is_lonlat, group, weight)

  centers <- x_processed |>
    lapply(\(x) do.call(planar_median, c(x, tolerance)))

  output_processing(centers, x, group)
}
