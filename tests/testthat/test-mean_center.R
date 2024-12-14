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

test_that("default behavior", {
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

test_that("weighted behavior", {
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

test_that("group behavior", {
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

test_that("weights and group behavior", {
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
