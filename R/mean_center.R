cartesian_mean <- function(x, y, z, wts) {
  total <- sum(wts)

  x_mean <- sum(x * wts) / total
  y_mean <- sum(y * wts) / total
  z_mean <- sum(z * wts) / total

  l <- sqrt(sum((c(x_mean, y_mean, z_mean))^2))

  surface_x_mean <- x_mean / l
  surface_y_mean <- y_mean / l
  surface_z_mean <- z_mean / l

  list(x = surface_x_mean, y = surface_y_mean, z = surface_z_mean)
}

planar_mean <- function(x, y, wts) {
  total <- sum(wts)

  x_mean <- sum(x * wts) / total
  y_mean <- sum(y * wts) / total

  list(x = x_mean, y = y_mean)
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
  allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")

  x <- x_checks(x, x_name, allowed_geom)
  grps <- group_checks(x, x_name, group)
  wts <- weight_checks(x, x_name, weight)

  unique_grps <- unique(grps)
  geometry <- vector(mode = "list", length(unique_grps))
  names(geometry) <- unique_grps

  if (sf::st_is_longlat(x)) {
    lon <- sf::st_coordinates(x)[, 1]
    lat <- sf::st_coordinates(x)[, 2]
    cartesian <- lonlat_cartesian(lon, lat)
    ctr_args <- as.data.frame(cartesian)
    ctr_args$wt <- wts
    ctr_args_split <- split(ctr_args, f = grps)

    for (grp in unique_grps) {
      means_cartesian <- do.call(cartesian_mean, ctr_args_split[[grp]])
      means_lonlat <- do.call(cartesian_lonlat, means_cartesian)
      geometry[[grp]] <- sf::st_point(unlist(means_lonlat))
    }
  } else {
    ctr_args <- data.frame(
      x = sf::st_coordinates(x)[, 1],
      y = sf::st_coordinates(x)[, 2],
      wts = wts
    )
    ctr_args_split <- split(ctr_args, f = grps)

    for (grp in unique_grps) {
      mean_xy <- do.call(planar_mean, ctr_args_split[[grp]])
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
