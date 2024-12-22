# centr 0.2.1

# version 0.2.1

* `mean_center()` and `median_center()` now support multiple group columns 
  (#15).
* `mean_center()` and `median_center()` now support passing `dplyr::summarise` 
  `...` arguments. This makes it possible to summarise of tabular data as well 
  as geometry.
* `mean_center()` and `median_center()` now support grouped `tibbles`.
* `mean_center()` and `median_center()` now always return `sf` `tibbles`.

# version 0.2

* Warning given if empty geometries returned due to zero total weight groups
* Groups can be more than just characters
* Support for `data.table` `sf` objects tested
* Vignette added for a walkthrough of typical usage 

# version 0.1

* Inital release
* Published to CRAN
