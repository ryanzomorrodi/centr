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

test_that("weighted behavior", {
  "projected; checked with ArcGIS Pro"
  expect_equal(
    round(sf::st_coordinates(sf::st_transform(median_center(x_proj, weight = "wts"), 4326)), 4),
    round(as.matrix(data.frame(X = -96.1599666, Y = 37.9123573)), 4)
  )
})

test_that("group behavior", {
  "projected; checked with ArcGIS Pro"
  expect_equal(
    round(sf::st_coordinates(sf::st_transform(median_center(x_proj, group = "grp"), 4326)), 4),
    round(as.matrix(data.frame(
      X = c(-95.9452692, -96.2108539, -97.0269173),
      Y = c(38.6609282, 37.0473917, 38.2692852)
    )), 4)
  )
})

test_that("weights and group behavior", {
  "projected; checked with ArcGIS Pro"
  expect_equal(
    round(sf::st_coordinates(sf::st_transform(median_center(x_proj, weight = "wts", group = "grp"), 4326)), 4),
    round(as.matrix(data.frame(
      X = c(-95.9243719, -95.3915581, -96.9201130),
      Y = c(38.5073800, 36.8660540, 38.2864761)
    )), 4)
  )
})
