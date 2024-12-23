cartesian_mean <- function(x, y, z, wts = NULL) {
  if (is.null(wts)) {
    total <- length(x)

    x_mean <- sum(x) / total
    y_mean <- sum(y) / total
    z_mean <- sum(z) / total
  } else if (sum(wts) == 0) {
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

  tibble::tibble(x = surface_x_mean, y = surface_y_mean, z = surface_z_mean)
}

planar_mean <- function(X, Y, wts = NULL) {
  if (is.null(wts)) {
    total <- length(X)

    x_mean <- sum(X) / total
    y_mean <- sum(Y) / total
  } else if (sum(wts) == 0) {
    x_mean <- NA
    y_mean <- NA
  } else {
    total <- sum(wts)

    x_mean <- sum(X * wts) / total
    y_mean <- sum(Y * wts) / total
  }

  tibble::tibble(X = x_mean, Y = y_mean)
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
#' @param group column name(s) specifying groups
#'  to calculate individual mean centers for
#' @param weight name of numeric weight column specifying an
#'  individual point's contribution to the mean center
#' @param ... expressions passed to `dplyr::summarise()`
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
#' 
#' x |>
#'   dplyr::group_by(grp) |>
#'   mean_center(weight = "wt")
#' @export
mean_center <- function(x, group, weight, ...) {
  chk::chk_s3_class(x, "sf")
  chk_not_any_empty_sf(x)
  chk_only_allowed_sf(x)
  chk_not_na_crs(x)
  
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
  
  is_lonlat <- sf::st_is_longlat(x)
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

  if (is_lonlat) {
    x[[sf_column]] <- do.call(lonlat_cartesian, x[[sf_column]])
    x <- dplyr::group_by(x, dplyr::pick({{ group }}))
    x <- dplyr::summarise(x, ..., geometry = do.call(cartesian_mean, dplyr::pick({{ sf_column }})[[1]]))
    x[[sf_column]] <- do.call(cartesian_lonlat, x[[sf_column]])
  } else {
    x <- dplyr::group_by(x, dplyr::pick({{ group }}))
    x <- dplyr::summarise(x, ..., geometry = do.call(planar_mean, dplyr::pick({{ sf_column }})[[1]]))
  }
  
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
