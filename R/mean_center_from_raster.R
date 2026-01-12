mean_center_from_raster <- function(x, group, weight, ...) {
  sf_column <- attr(x, "sf_column")

  coords <- x |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::summarise(
      !!sf_column := sf::st_combine(.data[[sf_column]])
    ) |>
    exactextractr::coverage_fraction(x = weight) |>
    lapply(\(coverage_frac) {
      no_coverage <- terra::global(coverage_frac, "sum") == 0
      na_coverage <- anyNA(weight[coverage_frac != 0])

      if (no_coverage || na_coverage) {
        return(sf::st_point())
      }

      terra::centroids(weight * coverage_frac, weighted = TRUE) |>
        as.matrix() |>
        sf::st_point()
    }) |>
    sf::st_sfc(crs = sf::st_crs(x))

  x |>
    sf::st_drop_geometry() |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    dplyr::summarise(
      ...
    ) |>
    dplyr::mutate("geometry" = coords) |>
    sf::st_as_sf()
}
