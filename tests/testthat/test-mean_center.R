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

# these are terrible tests, but at least for now this shouldn't error out
test_that("From raster - weighted", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("exactextractr")

  pop_count_raster <- terra::rast(
    system.file(
      'sao_miguel/gpw_v411_2020_count_2020.tif',
      package = 'exactextractr'
    )
  )

  concelhos_sf <- sf::st_read(
    system.file(
      'sao_miguel/concelhos.gpkg',
      package = 'exactextractr'
    ),
    quiet = TRUE
  )
  concelhos_sf$grp <- 1:2

  "grouped"
  expect_equal(
    nrow(mean_center(concelhos_sf, group = "grp", weight = pop_count_raster)),
    2
  )

  "ungrouped"
  expect_equal(
    nrow(mean_center(concelhos_sf, weight = pop_count_raster)),
    1
  )
})
