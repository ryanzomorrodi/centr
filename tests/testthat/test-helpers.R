test_that("lonlat cartesian conversions", {
  set.seed(1)

  lon <- runif(100, min = -180, max = 180)
  lat <- runif(100, min = -90, max = 90)

  "convert back and forth"
  expect_equal(
    do.call(cartesian_lonlat, lonlat_cartesian(lon, lat)),
    list(X = lon, Y = lat)
  )
})

test_that("x checks", {
  "non-sf class"
  expect_error(x_checks(df, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "non point/polygon"
  expect_error(x_checks(sf::st_cast(x, "LINESTRING"), "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "no CRS"
  x_noCRS <- sf::st_set_crs(x, NA)
  expect_error(x_checks(x_noCRS, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "no geometry"
  x_emptyGeom <- sf::st_as_sf(
    data.frame(x = c(1, NA), y = c(1, NA)), 
    coords = c("x", "y"), 
    na.fail = FALSE
  )
  expect_error(x_checks(x_emptyGeom, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))

  "pass"
  expect_no_error(x_checks(x, "x", c("POINT", "POLYGON", "MULTIPOINT", "MULTIPOLYGON")))
})

test_that("weight checks", {
  "non existent column"
  expect_error(weight_checks(x, "x", "random"))

  "non numeric column"
  expect_error(weight_checks(x, "x", "grp"))

  "missing value"
  x_missing_wt <- x
  x_missing_wt$wts[10] <- NA
  expect_error(weight_checks(x_missing_wt, "x", "wts"))

  "negative value"
  x_neg_wt <- x
  x_neg_wt$wts[10] <- -1
  expect_error(weight_checks(x_neg_wt, "x", "wts"))

  "existent column"
  expect_no_error(weight_checks(x, "x", "wts"))
})

test_that("group checks", {
  "non existent column"
  expect_error(group_checks(x, "x", "random"))

  "missing value"
  x_missing_grp <- x
  x_missing_grp$grp[10] <- NA
  expect_error(weight_checks(x_missing_wt, "x", "wts"))

  "existent column"
  expect_no_error(group_checks(x, "x", "grp"))
})

test_that("output", {
  "tibble"
  x_tibble <- tibble::tibble(x) |> sf::st_as_sf()
  expect_s3_class(mean_center(x_tibble), "tbl_df")

  "data.table"
  x_dt <- data.table::data.table(x) |> sf::st_as_sf()
  expect_s3_class(mean_center(x_dt), "data.table")

  "crs"
  expect_equal(sf::st_crs(mean_center(x)), sf::st_crs(x))
})


