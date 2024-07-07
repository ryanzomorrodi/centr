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

planar_median <- function(x, y, wts, tol = 0.0001) {
  estimate <- planar_mean(x, y, wts)
  new_estimate <- planar_median_est(x, y, estimate$x, estimate$y, wts)

  while (any(abs(unlist(estimate) - unlist(new_estimate)) > tol)) {
    estimate <- new_estimate
    new_estimate <- planar_median_est(x, y, estimate$x, estimate$y, wts)
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
#' It uses the [methodology](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/h-how-median-center-spatial-statistics-works.htm) 
#' introduced by Kuhn and Kuenne (1962).
#' 
#' Currently, median center is only implemenented for projected data.
#' @param x Input POINT, MULTIPOINT, POLYGON, or MULTIPOLYGON
#'  simple features
#' @param group specifies groups to calculate individual mean
#'  centers for
#' @param weight numeric; weight specifying an individual point's
#'  contribution to the mean center
#' @returns An sf object with a mean center for each group
#' @examples
#' df <- data.frame(
#'   lon = c(-88, -90, -92, -89, -90),
#'   lat = c(42, 40, 30, 32, 42),
#'   grp = c("a", "b", "a", "b", "a"),
#'   wt = c(1,1,1,1,1))
#' x_sf <- sf::st_as_sf(x, coords = c("lon", "lat"), crs = 4326)
#' x_transformed <- sf::st_transform(x, crs = "ESRI:102003")
#' median_center(x_transformed, group = "grp", weight = "wt")
#' @export
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
    stop(xname, " projection is not defined")
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
