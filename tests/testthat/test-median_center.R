test_that("median_center", {
  "projected; checked with ArcGIS Pro"
  df_triangle <- data.frame(
    lon = c(-99, -110, -70),
    lat = c(20, 40, 30)
  )
  x_triangle <- sf::st_as_sf(df_triangle, coords = c("lon", "lat"), crs = 4326)
  x_triangle_proj <- sf::st_transform(x_triangle, crs = "ESRI:102003")

  data.frame(X = -96.0135231, Y = 28.6822725) |>
    sf::st_as_sf(crs = 4326, coords = c("X", "Y")) |>
    sf::st_transform(x_triangle, crs = "ESRI:102003") |>
    sf::st_distance(x_triangle_proj) |>
    sum()

  expect_equal(
    median_center(x_triangle_proj) |>
      sf::st_distance(x_triangle_proj) |>
      sum(),
    data.frame(X = -96.0135231, Y = 28.6822725) |>
      sf::st_as_sf(crs = 4326, coords = c("X", "Y")) |>
      sf::st_transform(crs = "ESRI:102003") |>
      sf::st_distance(x_triangle_proj) |>
      sum(),
    tolerance = 1e-5
  )

  df_rand <- data.frame(
    # fmt: skip
    lon = c(-70, -80, -123, -79, -90, -121, -68, -93, -78, -108, -113, -94, -114),
    lat = c(45, 36, 44, 48, 34, 48, 27, 27, 29, 33, 48, 26, 36)
  )
  x_rand <- sf::st_as_sf(df_rand, coords = c("lon", "lat"), crs = 4326)
  x_rand_proj <- sf::st_transform(x_rand, crs = "ESRI:102003")
  expect_equal(
    median_center(x_rand_proj) |>
      sf::st_distance(x_rand_proj) |>
      sum(),
    data.frame(X = -91.2887347, Y = 35.3991316) |>
      sf::st_as_sf(crs = 4326, coords = c("X", "Y")) |>
      sf::st_transform(crs = "ESRI:102003") |>
      sf::st_distance(x_rand_proj) |>
      sum(),
    tolerance = 1e-5
  )

  expect_equal(
    median_center(x_proj) |>
      sf::st_distance(x_proj) |>
      sum(),
    data.frame(X = -96.4593993, Y = 38.0191323) |>
      sf::st_as_sf(crs = 4326, coords = c("X", "Y")) |>
      sf::st_transform(crs = "ESRI:102003") |>
      sf::st_distance(x_proj) |>
      sum(),
    tolerance = 1e-5
  )
})
