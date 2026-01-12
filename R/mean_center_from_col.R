mean_center_from_col <- function(x, group, weight, ...) {
  is_lonlat <- sf::st_is_longlat(x)
  crs <- sf::st_crs(x)
  sf_column <- attr(x, "sf_column")

  x |>
    sf::st_centroid() |>
    suppressWarnings() |>
    tibble::tibble() |>
    dplyr::mutate(!!sf_column := sf::st_coordinates(.data[[sf_column]])) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::summarise(
      ...,
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
      }
    ) |>
    dplyr::mutate("geometry" = sf::st_as_sfc(.data[["geometry"]])) |>
    sf::st_as_sf(crs = crs)
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
