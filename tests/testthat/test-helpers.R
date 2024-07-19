test_that("lonlat cartesian conversions", {
  set.seed(1)

  lon <- runif(100, min = -180, max = 180)
  lat <- runif(100, min = -90, max = 90)

  "convert back and forth"
  expect_equal(
    do.call(cartesian_lonlat, lonlat_cartesian(lon, lat)),
    list(x = lon, y = lat)
  )
})

test_that("x checks", {
  "non-sf class"
  expect_error(x_checks(df, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "non point/polygon"
  expect_error(x_checks(sf::st_cast(x, "LINESTRING"), "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "no CRS"
  x_noCRS <- x
  sf::st_crs(x_noCRS) <- NA
  expect_error(x_checks(x_noCRS, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "pass"
  expect_equal(x_checks(x, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")), sf::st_centroid(x))

})

test_that("weight checks", {
  "NULL weight"
  expect_equal(weight_checks(x, "x", NULL), rep(1, nrow(x)))

  "non existent column"
  expect_error(weight_checks(x, "x", "random"))

  "non numeric column"
  expect_error(weight_checks(x, "x", "grp"))

  "missing value"
  x_missing_wt <- x
  x_missing_wt$wts[10] <- NA_real_
  expect_error(weight_checks(x_missing_wt, "x", "wts"))

  "negative value"
  x_neg_wt <- x
  x_neg_wt$wts[10] <- -1
  expect_error(weight_checks(x_neg_wt, "x", "wts"))

  "existent column"
  expect_equal(weight_checks(x, "x", "wts"), x$wts)
})

test_that("group checks", {
  "NULL group"
  expect_equal(group_checks(x, "x", NULL), rep("1", nrow(x)))

  "non existent column"
  expect_error(group_checks(x, "x", "random"))

  "non numeric column"
  expect_error(group_checks(x, "x", "wts"))

  "existent column"
  expect_equal(group_checks(x, "x", "grp"), x$grp)
})
