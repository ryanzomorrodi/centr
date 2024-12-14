test_that("lonlat cartesian conversions", {
  set.seed(1)

  lon <- runif(100, min = -180, max = 180)
  lat <- runif(100, min = -90, max = 90)

  "convert back and forth"
  expect_equal(
    do.call(cartesian_lonlat, lonlat_cartesian(lon, lat)),
    tibble::tibble(X = lon, Y = lat)
  )
})
