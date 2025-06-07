#' Median Center
#'
#' @description
#' Median center iteratively calculates the point that minimizes
#' distance to all features. One can specify the groups to calculate
#' individual centers for and weights for each individual point. It
#' is analagous to the [ArcGIS Pro Median Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/median-center.htm)
#' tool.
#'
#' @param x Input POINT or POLYGON simple features
#' @param group column name(s) specifying groups
#'  to calculate individual mean centers for
#' @param weight name of numeric weight column specifying an
#'  individual point's contribution to the mean center
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
median_center <- function(x, group, weight, ...) {
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
    dplyr::group_by(dplyr::pick({{ group }})) |>
    dplyr::summarise(
      "geometry" = {
        coords <- .data[[sf_column]]

        coords <- if (is.null(weight)) {
          median_center_sfc(coords)
        } else {
          median_center_sfc(coords, weight = .data[[weight]])
        }

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

criteria <- function(par, points, weight) {
  distances <- sf::st_point(par) |>
    sf::st_sfc(crs = sf::st_crs(points)) |>
    sf::st_distance(points)

  if (is.null(weight)) {
    sum(distances)
  } else {
    sum(distances * weight)
  }
}

median_center_sfc <- function(points, weight = NULL) {
  if (!is.null(weight) & sum(weight) == 0) {
    means <- c(X = NA_real_, Y = NA_real_)
  } else {
    means <- stats::optim(
      par = as.vector(mean_center_matrix(sf::st_coordinates(points))),
      \(par) criteria(par, points, weight)
    )$par
  }
  t(means)
}
