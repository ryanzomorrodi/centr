# Median Center

Median center iteratively calculates the point that minimizes distance
to all features. One can specify the groups to calculate individual
centers for and weights for each individual point. It is analagous to
the [ArcGIS Pro Median
Center](https://pro.arcgis.com/en/pro-app/latest/tool-reference/spatial-statistics/median-center.htm)
tool.

## Usage

``` r
median_center(x, group, weight, ...)
```

## Arguments

- x:

  Input POINT or POLYGON simple features

- group:

  column name(s) specifying groups to calculate individual mean centers
  for

- weight:

  name of numeric weight column specifying an individual point's
  contribution to the mean center

- ...:

  expressions passed to
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)

## Value

An sf object with a median center for each group

## Examples

``` r
df <- data.frame(
  lon = c(-88, -90, -92, -89, -90),
  lat = c(42, 40, 30, 32, 42),
  grp = c("a", "b", "a", "b", "a"),
  wt = c(1, 1, 1, 1, 1)
)
x <- sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
x_transformed <- sf::st_transform(x, crs = "ESRI:102003")
median_center(x_transformed, group = "grp", weight = "wt")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 542595.8 ymin: -147278.5 xmax: 582550.3 ymax: 482712.9
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 2 × 2
#>   grp               geometry
#> * <chr>          <POINT [m]>
#> 1 a      (542595.8 482712.9)
#> 2 b     (582550.3 -147278.5)

x_transformed |>
  dplyr::group_by(grp) |>
  median_center(weight = "wt")
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 542595.8 ymin: -147278.5 xmax: 582550.3 ymax: 482712.9
#> Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic
#> # A tibble: 2 × 2
#>   grp               geometry
#> * <chr>          <POINT [m]>
#> 1 a      (542595.8 482712.9)
#> 2 b     (582550.3 -147278.5)
```
