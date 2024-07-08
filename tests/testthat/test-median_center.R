test_that("euclidean distance", {
  
  expect_equal(euclid_xy_dist(2, 2, 0, 0), 2 * sqrt(2))
  expect_equal(euclid_xy_dist(c(3, 5), c(4, 12), c(0, 0), c(0, 0)), c(5, 13))

})

test_that("euclidean distance", {
  
  expect_equal(euclid_xy_dist(2, 2, 0, 0), 2 * sqrt(2))
  expect_equal(euclid_xy_dist(c(3, 5), c(4, 12), c(0, 0), c(0, 0)), c(5, 13))

})