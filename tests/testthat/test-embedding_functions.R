test_that("Width", {
  # a inside b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_w(c(.2, .3), c(0.4, 1)), 0)
  # a right of b

  # [a = [b and b] > a]
  # [a = [b and b] < a]
  # a] = b] and [b > [a
  # a] = b] and [b < [a

  # a has width 0
  # b has width 0

  # a and b are completely equal

})

test_that("Lukasiewicz", {
  #a dentro de b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  #a fuera de b
  expect_equal(emb_w(c(.2, .3), c(0.4, 1)), 0)
})

test_that("Fodor", {
  #a dentro de b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  #a fuera de b
  expect_equal(emb_w(c(.2, .3), c(0.4, 1)), 0)
  })

test_that("Godel", {
  #a dentro de b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  #a fuera de b
  expect_equal(emb_w(c(.2, .3), c(0.4, 1)), 0)
})

test_that("Goguen", {
  #a dentro de b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  #a fuera de b
  expect_equal(emb_w(c(.2, .3), c(0.4, 1)), 0)
})

test_that("Rescher", {
  #a dentro de b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  #a fuera de b
  expect_equal(emb_w(c(.2, .3), c(0.4, 1)), 0)
})
