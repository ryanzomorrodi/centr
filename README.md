
<!-- README.md is generated from README.Rmd. Please edit that file -->

# centr

<!-- badges: start -->

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
  grp = c("a", "b", "a", "b", "a"),
  wt = c(1, 5, 1, 3, 2)
)
x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)

mean_center(x, group = "grp", weight = "wt")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 17.51482 ymin: 29.00224 xmax: 65.92087 ymax: 63.32603
#> Geodetic CRS:  WGS 84
#>   grp                  geometry
#> 1   a POINT (17.51482 29.00224)
#> 2   b POINT (65.92087 63.32603)
```

Median center iteratively calculates the point that minimizes distance
to all features. One can specify the groups to calculate individual
centers for and weights for each individual point. It is analagous to
the [ArcGIS Pro Median
Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/median-center.htm)
tool.

``` r
library(centr)
df <- data.frame(
  lon = c(20, 50, 30, 80, 10),
  lat = c(25, 70, 30, 50, 30),
  grp = c("a", "b", "a", "b", "a"),
  wt = c(1, 5, 1, 3, 2)
)
x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
x_transformed <- sf::st_transform(x, crs = "ESRI:102003")

median_center(x_transformed, group = "grp", weight = "wt")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 4950281 ymin: 4293605 xmax: 8226081 ymax: 8151342
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#>   grp                geometry
#> 1   a POINT (8226081 4293605)
#> 2   b POINT (4950281 8151342)
```
