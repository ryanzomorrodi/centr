# Mean Center

Mean center calculates the geographic average center. One can specify
the groups to calculate individual centers for groups and weights for
each individual point. It is analagous to the [ArcGIS Pro Mean
Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/mean-center.htm)
tool.

If `st_is_longlat(x)`, mean center is calculated assuming a spherical
Earth. Projected data is calculated assuming a "flat" Earth.

## Usage

``` r
mean_center(x, group, weight, ...)
```

## Arguments

- x:

  Input POINT or POLYGON simple features

- group:

  column name(s) specifying groups to calculate individual mean centers
  for

- weight:

  name of numeric weight column specifying an individual point's
  contribution to the mean center OR a single layer SpatRaster whose
  pixels represent contributions to the mean center.

- ...:

  expressions passed to
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)

## Value

An sf object with a mean center for each group

## Examples

``` r
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
#> # A tibble: 2 × 2
#>   grp              geometry
#> * <chr>         <POINT [°]>
#> 1 a     (17.51482 29.00224)
#> 2 b     (65.92087 63.32603)

x |>
  dplyr::group_by(grp) |>
  mean_center(weight = "wt")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 17.51482 ymin: 29.00224 xmax: 65.92087 ymax: 63.32603
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 2
#>   grp              geometry
#> * <chr>         <POINT [°]>
#> 1 a     (17.51482 29.00224)
#> 2 b     (65.92087 63.32603)
```
