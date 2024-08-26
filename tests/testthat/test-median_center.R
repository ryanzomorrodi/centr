test_that("euclidean planar distance", {
  expect_equal(euclid_xy_dist(2, 2, 0, 0), 2 * sqrt(2))
  expect_equal(euclid_xy_dist(c(3, 5), c(4, 12), c(0, 0), c(0, 0)), c(5, 13))
})

test_that("median_center", {
  "projected; checked with ArcGIS Pro"
  df_triangle <- data.frame(
    lon = c(-99, -110, -70),
    lat = c(20, 40, 30)
  )
  x_triangle <- sf::st_as_sf(df_triangle, coords = c("lon", "lat"), crs = 4326)
  x_triangle_proj <- sf::st_transform(x_triangle, crs = "ESRI:102003")

  expect_equal(
    sf::st_coordinates(sf::st_transform(median_center(x_triangle_proj), 4326)),
    as.matrix(data.frame(X = -96.0135231, Y = 28.6822725))
  )

  df_rand <- data.frame(
    lon = c(-70, -80, -123, -79, -90, -121, -68, -93, -78, -108, -113, -94, -114),
    lat = c(45, 36, 44, 48, 34, 48, 27, 27, 29, 33, 48, 26, 36)
  )
  x_rand <- sf::st_as_sf(df_rand, coords = c("lon", "lat"), crs = 4326)
  x_rand_proj <- sf::st_transform(x_rand, crs = "ESRI:102003")
  expect_equal(
    round(sf::st_coordinates(sf::st_transform(median_center(x_rand_proj), 4326)), 4),
    round(as.matrix(data.frame(X = -91.2887347, Y = 35.3991316)), 4)
  )

  expect_equal(
    round(sf::st_coordinates(sf::st_transform(median_center(x_proj), 4326)), 4),
    round(as.matrix(data.frame(X = -96.4593993, Y = 38.0191323)), 4)
  )
})

test_that("zero weight group", {
  x_zero_wts_proj <- x_proj
  x_zero_wts_proj$wts[x$grp == "a"] <- 0
  expect_warning(
    mean_center(x_zero_wts_proj, group = "grp", weight = "wts")
  )
})
