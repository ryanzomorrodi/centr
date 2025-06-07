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
  } else {
    weight <- NULL
  }

  is_lonlat <- sf::st_is_longlat(x)
  crs <- sf::st_crs(x)
  sf_column <- attr(x, "sf_column")

  centers <- x |>
    sf::st_centroid() |>
    suppressWarnings() |>
    tibble::tibble() |>
    dplyr::mutate(!!sf_column := sf::st_coordinates(.data[[sf_column]])) |>
    dplyr::group_by(dplyr::pick({{ group }})) |>
    dplyr::summarise(
      "geometry" = {
        coords <- .data[[sf_column]]
        coords <- if (is_lonlat) lonlat_cartesian(coords) else coords

        coords <- if (is.null(weight)) {
          mean_center_matrix(coords)
        } else {
          mean_center_matrix(coords, weight = .data[[weight]])
        }

        coords <- if (is_lonlat) cartesian_lonlat(coords) else coords
        list(sf::st_point(coords))
      },
      ...
    ) |>
    dplyr::mutate("geometry" = sf::st_as_sfc(.data[["geometry"]])) |>
    sf::st_as_sf(crs = crs)

  center_is_empty <- sf::st_is_empty(centers)
  if (any(center_is_empty)) {
    chk::wrn(
      "Empty point%s returned for %n group%s with zero total weight",
      n = sum(center_is_empty)
    )
  }
  centers
}

mean_center_matrix <- function(points, weight = NULL) {
  if (is.null(weight)) {
    means <- colMeans(points)
  } else if (sum(weight) == 0) {
    means <- c(X = NA_real_, Y = NA_real_, Z = NA_real_)
  } else {
    means <- colSums(points * (weight / sum(weight)))
  }
  if (length(means) == 3) {
    means <- means / sqrt(sum(means^2))
  }

  t(means)
}

lonlat_cartesian <- function(points) {
  phi <- ((90 - points[, 2]) * pi) / 180
  theta <- (points[, 1] * pi) / 180

  x_cart <- sin(phi) * cos(theta)
  y_cart <- sin(phi) * sin(theta)
  z_cart <- cos(phi)

  cbind(X = x_cart, Y = y_cart, Z = z_cart)
}

cartesian_lonlat <- function(points) {
  lat <- 90 - ((180 * acos(points[, 3])) / pi)
  lon <- ifelse(
    points[, 1] > 0,
    atan(points[, 2] / points[, 1]) * (180 / pi),
    ifelse(
      points[, 2] > 0,
      atan(points[, 2] / points[, 1]) * (180 / pi) + 180,
      atan(points[, 2] / points[, 1]) * (180 / pi) - 180
    )
  )

  cbind(X = lon, Y = lat)
}
