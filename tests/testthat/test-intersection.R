test_that("Boolean intersection between two intervals", {
  # TRUE
  expect_true(intersection(c(1,5), c(2,6)))
  expect_true(intersection(c(2,6), c(1,5)))
  expect_true(intersection(c(1,6), c(2,4)))
  expect_true(intersection(c(2,4), c(1,6)))
  # FALSE
  expect_false(intersection(c(1,2), c(3,4)))
  expect_false(intersection(c(3,4), c(1,2)))
})


test_that("Resulting interval from intersection between two intervals", {
  expect_equal(intersection(c(1,5), c(2,6), interval = TRUE), c(2,5))
  expect_equal(intersection(c(2,6), c(1,5), interval = TRUE), c(2,5))
  expect_equal(intersection(c(1,6), c(2,4), interval = TRUE), c(2,4))
  expect_equal(intersection(c(2,4), c(1,6), interval = TRUE), c(2,4))
  expect_equal(intersection(c(1,2), c(3,4), interval = TRUE), NA)
  expect_equal(intersection(c(3,4), c(1,2), interval = TRUE), NA)
})
