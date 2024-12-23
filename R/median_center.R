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
  tibble::as_tibble(new_estimate)
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
#' @param x Input POINT or POLYGON simple features
#' @param group column name(s) specifying groups
#'  to calculate individual mean centers for
#' @param weight name of numeric weight column specifying an
#'  individual point's contribution to the mean center
#' @param tolerance numeric threshold determining when an
#'  estimate improvement is sufficiently small enough to stop
#'  iterating (smaller = slower, but more precision)
#' @param ... expressions passed to `dplyr::summarise()`
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
#' 
#' x_transformed |>
#'   dplyr::group_by(grp) |>
#'   median_center(weight = "wt")
#' @export
median_center <- function(x, group, weight, tolerance = 0.0001, ...) {
  chk::chk_s3_class(x, "sf")
  chk_not_any_empty_sf(x)
  chk_only_allowed_sf(x)
  chk_not_na_crs(x)
  chk_is_not_lonlat(x)
  
  if (!missing(group)) {
    chk::chk_character(group)
    chk_columns_exist(x, group)
  } else {
    group <- dplyr::group_cols(data = x)
  }
  if (!missing(weight)) {
    chk::chk_string(weight)
    chk_columns_exist(x, weight)
    chk::chk_numeric(x[[weight]])
    chk::chk_not_any_na(x[[weight]])
    chk_not_any_infinite(x[[weight]])
    chk::chk_gte(x[[weight]], 0)
  }

  crs <- sf::st_crs(x)
  coordinates <- suppressWarnings(sf::st_centroid(x)) |>
    sf::st_coordinates() |>
    tibble::as_tibble()
  sf_column <- attr(x, "sf_column")
  x <- tibble::tibble(x)
  if (!missing(weight)) {
    x[[sf_column]] <- dplyr::bind_cols(coordinates, wts = x[[weight]])
  } else {
    x[[sf_column]] <- coordinates
  }

  x <- dplyr::group_by(x, dplyr::pick({{ group }}))
  x <- dplyr::summarise(
    x, 
    ..., 
    geometry = do.call(planar_median, c(as.list(dplyr::pick({{ sf_column }})[[1]]), tol = tolerance))
  )

  x[[sf_column]] <- sf::st_as_sfc(sf::st_as_sf(x[[sf_column]], coords = c("X", "Y"), crs = crs, na.fail = FALSE))
  x <- dplyr::ungroup(sf::st_as_sf(x))

  center_is_empty <- sf::st_is_empty(x)
  if (any(center_is_empty)) {
    chk::wrn(
      "Empty point%s returned for %n group%s with zero total weight", 
      n = sum(center_is_empty)
    )
  }

  x
}
