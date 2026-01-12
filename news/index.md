# Changelog

## centr (development version)

- Nelder-Mead is now used to calculate
  [`median_center()`](https://ryanzomorrodi.github.io/centr/reference/median_center.md).
  This means that lon lat centers are now able to calculated.
- [`mean_center()`](https://ryanzomorrodi.github.io/centr/reference/mean_center.md)
  accepts a `SpatRaster` weight argument to support raster-weighted
  centroids.

## centr 0.2.4

CRAN release: 2025-04-17

- [`mean_center()`](https://ryanzomorrodi.github.io/centr/reference/mean_center.md)
  and
  [`median_center()`](https://ryanzomorrodi.github.io/centr/reference/median_center.md)
  now work even if the geometry column is not named geometry
  ([\#20](https://github.com/ryanzomorrodi/centR/issues/20)).

## centr 0.2.3

CRAN release: 2025-03-16

- Adjust `testthat` tolerance to 4 decimal points due cran checks.

## centr 0.2.2

CRAN release: 2025-02-07

- Specify R dependency of \>= 4.1.0

## centr 0.2.1

CRAN release: 2024-12-23

- [`mean_center()`](https://ryanzomorrodi.github.io/centr/reference/mean_center.md)
  and
  [`median_center()`](https://ryanzomorrodi.github.io/centr/reference/median_center.md)
  now support multiple group columns
  ([\#15](https://github.com/ryanzomorrodi/centR/issues/15)).
- [`mean_center()`](https://ryanzomorrodi.github.io/centr/reference/mean_center.md)
  and
  [`median_center()`](https://ryanzomorrodi.github.io/centr/reference/median_center.md)
  now support passing
  [`dplyr::summarise`](https://dplyr.tidyverse.org/reference/summarise.html)
  `...` arguments. This makes it possible to summarise of tabular data
  as well as geometry.
- [`mean_center()`](https://ryanzomorrodi.github.io/centr/reference/mean_center.md)
  and
  [`median_center()`](https://ryanzomorrodi.github.io/centr/reference/median_center.md)
  now support grouped `tibbles`.
- [`mean_center()`](https://ryanzomorrodi.github.io/centr/reference/mean_center.md)
  and
  [`median_center()`](https://ryanzomorrodi.github.io/centr/reference/median_center.md)
  now always return `sf` `tibbles`.

## centr 0.2

- Warning given if empty geometries returned due to zero total weight
  groups
- Groups can be more than just characters
- Support for `data.table` `sf` objects tested
- Vignette added for a walkthrough of typical usage

## centr 0.1

- Inital release
- Published to CRAN
