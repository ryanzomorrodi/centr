test_that("x checks", {
  "non-sf class"
  expect_error(mean_center(df))

  "non point/polygon"
  expect_error(mean_center(sf::st_cast(x, "LINESTRING")))

  "no CRS"
  x_noCRS <- sf::st_set_crs(x, NA)
  expect_error(mean_center(x_noCRS))

  "no geometry"
  x_emptyGeom <- sf::st_as_sf(
    data.frame(x = c(1, NA), y = c(1, NA)),
    coords = c("x", "y"),
    na.fail = FALSE
  )
  expect_error(mean_center(x_emptyGeom))

  "pass"
  expect_no_error(mean_center(x))
})

test_that("weight checks", {
  "non existent column"
  expect_error(mean_center(x, weight = "x"))

  "non numeric column"
  expect_error(mean_center(x, weight = "grp"))

  "missing value"
  x_missing_wt <- x
  x_missing_wt$wts[10] <- NA
  expect_error(mean_center(x_missing_wt, weight = "wts"))

  "negative value"
  x_neg_wt <- x
  x_neg_wt$wts[10] <- -1
  expect_error(mean_center(x_neg_wt, weight = "wts"))

  "infinite value"
  x_neg_wt <- x
  x_neg_wt$wts[10] <- Inf
  expect_error(mean_center(x_neg_wt, weight = "wts"))

  "existent column"
  expect_no_error(mean_center(x, weight = "wts"))
})

test_that("group checks", {
  "non existent column"
  expect_error(mean_center(x, "x"))

  "existent column"
  expect_no_error(mean_center(x, "grp"))

  "multiple columns"
  expect_no_error(mean_center(x, c("grp", "wts")))
})

test_that("From column - default behavior", {
  "unprojected"
  expect_equal(
    sf::st_coordinates(mean_center(x)),
    sf::st_coordinates(sf::st_centroid(sf::st_union(x)))
  )

  "projected"
  expect_equal(
    sf::st_coordinates(mean_center(x_proj)),
    sf::st_coordinates(sf::st_centroid(sf::st_union(x_proj)))
  )
})

test_that("From column - weighted behavior", {
  "unprojected"
  expect_equal(
    sf::st_coordinates(mean_center(x, weight = "wts")),
    sf::st_coordinates(sf::st_centroid(sf::st_combine(x_rep)))
  )

  "projected"
  expect_equal(
    sf::st_coordinates(mean_center(x_proj, weight = "wts")),
    sf::st_coordinates(sf::st_centroid(sf::st_combine(x_rep_proj)))
  )
})

test_that("From column - group behavior", {
  "unprojected"
  expect_equal(
    sf::st_coordinates(mean_center(x, group = "grp")),
    sf::st_coordinates(sf::st_centroid(x_grp))
  )

  "projected"
  expect_equal(
    sf::st_coordinates(mean_center(x_proj, group = "grp")),
    sf::st_coordinates(sf::st_centroid(x_grp_proj))
  )
})

test_that("From column - weights and group behavior", {
  "unprojected"
  expect_equal(
    sf::st_coordinates(mean_center(x, group = "grp", weight = "wts")),
    sf::st_coordinates(sf::st_centroid(x_rep_grp))
  )

  "projected"
  expect_equal(
    sf::st_coordinates(mean_center(x_proj, group = "grp", weight = "wts")),
    sf::st_coordinates(sf::st_centroid(x_rep_grp_proj))
  )
})

test_that("From column - column not named geometry should still work", {
  x_geom_col <- x
  colnames(x_geom_col)[colnames(x_geom_col) == "geometry"] <- "geom"
  sf::st_geometry(x_geom_col) <- "geom"

  "unprojected"
  expect_equal(
    sf::st_coordinates(mean_center(x_geom_col, group = "grp", weight = "wts")),
    sf::st_coordinates(sf::st_centroid(x_rep_grp))
  )

  x_geom_col_proj <- x_proj
  colnames(x_geom_col_proj)[colnames(x_geom_col_proj) == "geometry"] <- "geom"
  sf::st_geometry(x_geom_col_proj) <- "geom"

  "projected"
  expect_equal(
    sf::st_coordinates(mean_center(
      x_geom_col_proj,
      group = "grp",
      weight = "wts"
    )),
    sf::st_coordinates(sf::st_centroid(x_rep_grp_proj))
  )
})

test_that("From raster - weighted", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("exactextractr")

  # 5x5 unit raster of random values
  matrix_weights <- matrix(1:25, nrow = 5, ncol = 5)
  raster_weights <- terra::rast(matrix_weights, crs = "EPSG:4326")

  # 5x5 sf object like
  # |‾‾‾|‾‾‾‾|‾‾|
  # |   |    |  |
  # |   |----|  |
  # |   |       |
  # |___|_______|
  rect_a <- matrix(c(0, 0, 2, 0, 2, 5, 0, 5, 0, 0), ncol = 2, byrow = TRUE)
  coverage_a <- matrix(c(rep(1, 10), rep(0, 15)), ncol = 5, byrow = FALSE)
  rect_b <- matrix(
    c(2, 2.5, 4, 2.5, 4, 5, 2, 5, 2, 2.5),
    ncol = 2,
    byrow = TRUE
  )
  coverage_b <- matrix(
    c(rep(0, 10), rep(c(1, 1, 0.5, 0, 0), 2), rep(0, 5)),
    ncol = 5,
    byrow = FALSE
  )
  rect_c <- matrix(
    c(2, 0, 5, 0, 5, 5, 4, 5, 4, 2.5, 2, 2.5, 2, 0),
    ncol = 2,
    byrow = TRUE
  )
  coverage_c <- matrix(
    c(rep(0, 10), rep(c(0, 0, 0.5, 1, 1), 2), rep(1, 5)),
    ncol = 5,
    byrow = FALSE
  )
  sf_column <- sf::st_sfc(
    sf::st_polygon(list(rect_a)),
    sf::st_polygon(list(rect_b)),
    sf::st_polygon(list(rect_c))
  )
  sf_object <- sf::st_sf(
    ID = c("A", "B", "C"),
    geometry = sf_column,
    crs = "EPSG:4326"
  )

  mean_center_from_coverage <- function(coverage) {
    weights <- coverage * matrix_weights
    idx_matrix <- as.matrix(expand.grid(
      x = 1:nrow(weights),
      y = 1:ncol(weights)
    ))
    pts_matrix <- as.matrix(expand.grid(
      x = 1:nrow(weights),
      y = ncol(weights):1
    )) -
      0.5

    mean_center_matrix(
      pts_matrix,
      weights[idx_matrix[, 2:1]]
    )
  }

  ungrouped_center <- mean_center_from_coverage(matrix(rep(1, 25), ncol = 5))
  colnames(ungrouped_center) <- c("X", "Y")
  grouped_centers <- lapply(
    list(coverage_a, coverage_b, coverage_c),
    mean_center_from_coverage
  ) |>
    do.call(what = rbind)
  colnames(grouped_centers) <- c("X", "Y")

  "ungrouped"
  expect_equal(
    ungrouped_center,
    mean_center(sf_object, weight = raster_weights) |>
      sf::st_coordinates()
  )

  "grouped"
  expect_equal(
    grouped_centers,
    mean_center(sf_object, group = "ID", weight = raster_weights) |>
      sf::st_coordinates()
  )
})
