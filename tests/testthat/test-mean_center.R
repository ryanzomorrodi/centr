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

test_that("zero weight group", {
  x_zero_wts <- x
  x_zero_wts$wts[x$grp == "a"] <- 0
  expect_warning(
    mean_center(x_zero_wts, group = "grp", weight = "wts")
  )

  x_zero_wts_proj <- x_proj
  x_zero_wts_proj$wts[x$grp == "a"] <- 0
  expect_warning(
    mean_center(x_zero_wts_proj, group = "grp", weight = "wts")
  )
})