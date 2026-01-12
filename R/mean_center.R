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
#'  individual point's contribution to the mean center OR a
#'  single layer SpatRaster whose pixels represent contributions
#'  to the mean center.
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
  check_sf(
    x,
    allow_empty = FALSE,
    allow_na_crs = FALSE,
    allow_geom_type = c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")
  )
  if (!missing(group)) {
    check_character(group)
    for (grp in group) {
      check_column_exists(x, grp, column_name_arg = grp)
    }
  } else {
    group <- dplyr::group_cols(data = x)
  }
  if (missing(weight)) {
    weight <- NULL
  } else {
    check_is_string_or_raster(weight)
  }

  result <- if (methods::is(weight, "SpatRaster")) {
    is_installed("terra")
    is_installed("exactextractr")
    check_raster(
      weight,
      allow_multilayer = FALSE,
      allow_non_numeric = FALSE
    )
    check_identical_crs(x, weight)

    mean_center_from_raster(x, group, weight, ...)
  } else {
    if (is_string(weight)) {
      check_column_exists(x, weight)
      check_numeric(
        x[[weight]],
        min = 0,
        allow_infinite = FALSE,
        allow_na = FALSE,
        allow_null = FALSE
      )
    }
    mean_center_from_col(x, group, weight, ...)
  }

  center_is_empty <- sf::st_is_empty(result)
  if (any(center_is_empty)) {
    warning(
      sprintf(
        "Empty point returned for %s groups with 0 total weight.",
        sum(center_is_empty)
      )
    )
  }

  result
}
