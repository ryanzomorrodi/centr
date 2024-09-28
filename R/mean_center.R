cartesian_mean <- function(x, y, z, wts = NULL) {
  if (sum(wts) == 0) {
    warning("Empty point returned for groups with zero total weight")
    x_mean <- NA
    y_mean <- NA
    z_mean <- NA
  } else {
    total <- sum(wts)
  
    x_mean <- sum(x * wts) / total
    y_mean <- sum(y * wts) / total
    z_mean <- sum(z * wts) / total
  }

  l <- sqrt(sum((c(x_mean, y_mean, z_mean))^2))

  surface_x_mean <- x_mean / l
  surface_y_mean <- y_mean / l
  surface_z_mean <- z_mean / l

  list(x = surface_x_mean, y = surface_y_mean, z = surface_z_mean)
}

planar_mean <- function(X, Y, wts = NULL) {
  if (sum(wts) == 0) {
    warning("Empty point returned for groups with zero total weight")
    x_mean <- NA
    y_mean <- NA
  } else {
    total <- sum(wts)

    x_mean <- sum(X * wts) / total
    y_mean <- sum(Y * wts) / total
  }

  list(X = x_mean, Y = y_mean)
}

#' Mean Center
#'
#' @description
#' Mean center calculates the geographic average center.
#' One can specify the groups to calculate individual centers
#' for groups and weights for each individual point. It is
#' analagous to the [ArcGIS Pro Mean Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/mean-center.htm)
#' tool.
#'
#' If `st_is_longlat(x)`, mean center is calculated assuming a
#' spherical Earth. Projected data is calculated assuming a
#' "flat" Earth.
#' @param x Input POINT or POLYGON simple features
#' @param group name of character column specifying groups
#'  to calculate individual mean centers for
#' @param weight name of numeric weight column specifying an
#'  individual point's contribution to the mean center
#' @returns An sf object with a mean center for each group
#' @examples
#' df <- data.frame(
#'   lon = c(20, 50, 30, 80, 10),
#'   lat = c(25, 70, 30, 50, 30),
#'   grp = c("a", "b", "a", "b", "a"),
#'   wt = c(1, 5, 1, 3, 2)
#' )
#' x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
#' mean_center(x, group = "grp", weight = "wt")
#' @export
mean_center <- function(x, group = NULL, weight = NULL) {
  x_name <- deparse(substitute(x))

  x_checks(x, x_name)
  group_checks(x, x_name, group)
  weight_checks(x, x_name, weight)

  x_w_coords <- x_processing(x)

  if (is.null(weight)) {
    x_w_coords$.weight <- rep(1, nrow(x))
  } else {
    colnames(x_w_coords)[colnames(x_w_coords) == weight] <- ".weight"
  }
  
  if (sf::st_is_longlat(x)) {
    centers <- x_w_coords |>
      dplyr::mutate(
        geometry = do_call(tibble::tibble, do_call(lonlat_cartesian, geometry))
      ) |>
      dplyr::summarise(
        geometry = do_call(tibble::tibble, do_call(cartesian_mean, geometry, wts = .weight)),
        .by = dplyr::all_of(group)
      ) |>
      dplyr::mutate(
        geometry = do_call(tibble::tibble, do_call(cartesian_lonlat, geometry))
      )
  } else {
    centers <- x_w_coords  |>
      dplyr::summarise(
        geometry = do_call(tibble::tibble, do_call(planar_mean, geometry, wts = .weight)),
        .by = dplyr::all_of(group)
      )
  }

  output_processing(centers, x)
}
