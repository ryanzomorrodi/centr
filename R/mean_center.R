cartesian_mean <- function(x, y, z, wts) {
  if (all(wts == 0)) {
    return(list(x = NA_real_, y = NA_real_, z = NA_real_))
  }

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
  if (all(wts == 0)) {
    return(list(x = NA_real_, y = NA_real_))
  }

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
  x_is_lonlat <- sf::st_is_longlat(x)
  allowed_geom <- c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")

  x <- x_checks(x, x_name, allowed_geom)
  grps <- group_checks(x, x_name, group)
  wts <- weight_checks(x, x_name, weight)

  coords <- sf::st_coordinates(x) |>
    as.data.frame() |> as.list()
  names(coords) <- c("x", "y")

  x_split <- coords |>
    when(x_is_lonlat, do.call, what = lonlat_cartesian) |>
    c(wts = list(wts)) |>
    do.call(what = data.frame) |>
    split(factor(grps, unique(grps)))

  if (x_is_lonlat) {
    centers <- x_split |>
      lapply(\(x) do.call(cartesian_mean, x)) |>
      lapply(\(x) do.call(cartesian_lonlat, x))
  } else {
    centers <- x_split |>
      lapply(\(x) do.call(planar_mean, x))
  }

  output <- do.call(centers, what = rbind) |>
    as.data.frame() |>
    when(!is.null(group), rownames_to_column, colname = group) |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(x), na.fail = FALSE)

  if (any(sf::st_is_empty(output))) {
    warning("Empty point returned for groups with zero total weight")
  }

  output
}
