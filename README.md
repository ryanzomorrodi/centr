
<!-- README.md is generated from README.Rmd. Please edit that file -->

# centr

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/centr)](https://CRAN.R-project.org/package=centr)
[![centr status
badge](https://ryanzomorrodi.r-universe.dev/badges/centr)](https://ryanzomorrodi.r-universe.dev/centr)
[![R-CMD-check](https://github.com/ryanzomorrodi/centR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ryanzomorrodi/centR/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ryanzomorrodi/centR/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ryanzomorrodi/centR?branch=main)
<!-- badges: end -->

A Package for Weighted and Unweighted Spatial Centers

## Installation

``` r
# Install centr from CRAN
install.packages("centr")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("ryanzomorrodi/centr")
```

## Usage

The main functions are `mean_center` and `median_center`. They were
designed for calculation of population weighted centroids, but can be
extended to other forms of analyses.

Mean center calculates the geographic average center. One can specify
the groups to calculate individual centers for groups and weights for
each individual point. It is analagous to the [ArcGIS Pro Mean
Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/mean-center.htm)
tool.

``` r
library(centr)
df <- data.frame(
  lon = c(20, 50, 30, 80, 10),
  lat = c(25, 70, 30, 50, 30),
  group1 = c("a", "b", "a", "b", "a"),
  group2 = c(1, 1, 1, 1, 2),
  wt = c(1, 5, 1, 3, 2)
)
x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

mean_center(x, group = c("group1", "group2"), weight = "wt")
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 10 ymin: 27.58952 xmax: 65.92087 ymax: 63.32603
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 3
#>   group1 group2            geometry
#>   <chr>   <dbl>         <POINT [°]>
#> 1 a           1 (24.88607 27.58952)
#> 2 a           2             (10 30)
#> 3 b           1 (65.92087 63.32603)
```

Median center iteratively calculates the point that minimizes distance
to all features. One can specify the groups to calculate individual
centers for and weights for each individual point. It is analagous to
the [ArcGIS Pro Median
Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/median-center.htm)
tool.

``` r
x_transformed <- sf::st_transform(x, crs = "ESRI:102003")

median_center(x_transformed, group = c("group1", "group2"), weight = "wt")
#> Simple feature collection with 3 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4950281 ymin: 4293605 xmax: 9003834 ymax: 8151342
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 3 × 3
#>   group1 group2          geometry
#>   <chr>   <dbl>       <POINT [m]>
#> 1 a           1 (9003834 5545860)
#> 2 a           2 (8226081 4293605)
#> 3 b           1 (4950281 8151342)
```

Summaries of other attributes can be calculated by passing the summary
expressions to `...` just as in `dplyr::summarise()`.

``` r
mean_center(
  x, 
  group = c("group1", "group2"), 
  weight = "wt",
  total_weight = sum(wt)
)
#> Simple feature collection with 3 features and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 10 ymin: 27.58952 xmax: 65.92087 ymax: 63.32603
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 4
#>   group1 group2 total_weight            geometry
#>   <chr>   <dbl>        <dbl>         <POINT [°]>
#> 1 a           1            2 (24.88607 27.58952)
#> 2 a           2            2             (10 30)
#> 3 b           1            8 (65.92087 63.32603)
```
