test_that("lonlat cartesian conversions", {
  set.seed(1)

  lon <- runif(1000, min = -180, max = 180)
  lat <- runif(1000, min = -90, max = 90)
  lonlat_mat <- cbind(X = lon, Y = lat)

  "convert back and forth"
  expect_equal(
    lonlat_mat |>
      lonlat_cartesian() |>
      cartesian_lonlat(),
    lonlat_mat
  )
})
