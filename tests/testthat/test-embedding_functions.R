test_that("Width", {
  # a inside b
  expect_equal(emb_w(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_w(c(.2, .3), c(.4, 1)), 0)
  # a right of b
  expect_equal(emb_w(c(.6, .7), c(.4, .5)), 0)

  # [a = [b and b] > a]
  expect_equal(emb_w(c(.6, .7), c(.6, .8)), 1)
  # [a = [b and b] < a]
  expect_equal(emb_w(c(.6, .8), c(.6, .7)), 0.5)

  # a] = b] and [b > [a
  expect_equal(emb_w(c(.3, .7), c(.5, .7)), 0.5)
  # a] = b] and [b < [a
  expect_equal(emb_w(c(.6, .7), c(.3, .7)), 1)

  # a has width 0 out
  expect_equal(emb_w(c(.6, .6), c(.4, .5)), 0)
  # b has width 0 out
  expect_equal(emb_w(c(.6, .7), c(.5, .5)), 0)

  # a has width 0 in
  expect_equal(emb_w(c(.6, .6), c(.4, .7)), 1)
  # b has width 0 in
  expect_equal(emb_w(c(.4, .7), c(.5, .5)), 0)

  # a and b are completely equal
  expect_equal(emb_w(c(.2, .3),c(.2, .3)), 1)

})

test_that("Lukasiewicz", {
  # a inside b
  expect_equal(emb_lk(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_lk(c(.2, .3), c(.4, 1)), 0)
  # a right of b
  expect_equal(emb_lk(c(.6, .7), c(.4, .5)), 0)

  # [a = [b and b] > a]
  expect_equal(emb_lk(c(.6, .7), c(.6, .8)), 1)
  # [a = [b and b] < a]
  expect_equal(emb_lk(c(.6, .8), c(.6, .7)), 0.9)

  # a] = b] and [b > [a
  expect_equal(emb_lk(c(.3, .7), c(.5, .7)), 0.8)
  # a] = b] and [b < [a
  expect_equal(emb_lk(c(.6, .7), c(.3, .7)), 1)

  # a has width 0 out
  expect_equal(emb_lk(c(.6, .6), c(.4, .5)), 0)
  # b has width 0 out
  expect_equal(emb_lk(c(.6, .7), c(.5, .5)), 0)

  # a has width 0 in
  expect_equal(emb_lk(c(.6, .6), c(.4, .7)), 1)
  # b has width 0 in
  expect_equal(emb_lk(c(.4, .7), c(.5, .5)), 0.8)

  # a and b are completely equal
  expect_equal(emb_lk(c(.2, .3),c(.2, .3)), 1)
})

test_that("Fodor", {
  # a inside b
  expect_equal(emb_fd(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_fd(c(.2, .3), c(.4, 1)), 0)
  # a right of b
  expect_equal(emb_fd(c(.6, .7), c(.4, .5)), 0)

  # [a = [b and b] > a]
  expect_equal(emb_fd(c(.6, .7), c(.6, .8)), 1)
  # [a = [b and b] < a]
  expect_equal(emb_fd(c(.6, .8), c(.6, .7)), 0.7) #.6=.6<.7<.8 max(1-a2,b2) max(1-0.8,0.7)

  # a] = b] and [b > [a
  expect_equal(emb_fd(c(.3, .7), c(.5, .7)), 0.5) #.3<.5<.7=.7 max(1-b1,a1) max(1-0.5,0.3)
  # a] = b] and [b < [a
  expect_equal(emb_fd(c(.6, .7), c(.3, .7)), 1)

  # a has width 0 out
  expect_equal(emb_fd(c(.6, .6), c(.4, .5)), 0)
  # b has width 0 out
  expect_equal(emb_fd(c(.6, .7), c(.5, .5)), 0)

  # a has width 0 in
  expect_equal(emb_fd(c(.6, .6), c(.4, .7)), 1)
  # b has width 0 in
  expect_equal(emb_fd(c(.4, .7), c(.5, .5)), 0.5) #min{max(1−a2,b2),max(1−b1,a1)} min{max(1−.7,.5), max(1−.5,.4)}=min {.5,.5}

  # a and b are completely equal
  expect_equal(emb_fd(c(.2, .3),c(.2, .3)), 1)

  })

test_that("Godel", {
  # a inside b
  expect_equal(emb_gd(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_gd(c(.2, .3), c(.4, 1)), 0)
  # a right of b
  expect_equal(emb_gd(c(.6, .7), c(.4, .5)), 0)

  # [a = [b and b] > a]
  expect_equal(emb_gd(c(.6, .7), c(.6, .8)), 1)
  # [a = [b and b] < a]
  expect_equal(emb_gd(c(.6, .8), c(.6, .7)), 0.7)

  # a] = b] and [b > [a
  expect_equal(emb_gd(c(.3, .7), c(.5, .7)), 0.3)
  # a] = b] and [b < [a
  expect_equal(emb_gd(c(.6, .7), c(.3, .7)), 1)

  # a has width 0 out
  expect_equal(emb_gd(c(.6, .6), c(.4, .5)), 0)
  # b has width 0 out
  expect_equal(emb_gd(c(.6, .7), c(.5, .5)), 0)

  # a has width 0 in
  expect_equal(emb_gd(c(.6, .6), c(.4, .7)), 1)
  # b has width 0 in
  expect_equal(emb_gd(c(.4, .7), c(.5, .5)), 0.4)

  # a and b are completely equal
  expect_equal(emb_gd(c(.2, .3),c(.2, .3)), 1)

})

test_that("Goguen", {
  # a inside b
  expect_equal(emb_gg(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_gg(c(.2, .3), c(.4, 1)), 0)
  # a right of b
  expect_equal(emb_gg(c(.6, .7), c(.4, .5)), 0)

  # [a = [b and b] > a]
  expect_equal(emb_gg(c(.6, .7), c(.6, .8)), 1)
  # [a = [b and b] < a]
  expect_equal(emb_gg(c(.6, .8), c(.6, .7)), 0.875) #min{b2/a2,a1/b1} min{.7/.8,.6/.6}

  # a] = b] and [b > [a
  expect_equal(emb_gg(c(.3, .7), c(.5, .7)), 0.6) #min{b2/a2,a1/b1} min{.7/.7,.3/.5}
  # a] = b] and [b < [a
  expect_equal(emb_gg(c(.6, .7), c(.3, .7)), 1)

  # a has width 0 out
  expect_equal(emb_gg(c(.6, .6), c(.4, .5)), 0)
  # b has width 0 out
  expect_equal(emb_gg(c(.6, .7), c(.5, .5)), 0)

  # a has width 0 in
  expect_equal(emb_gg(c(.6, .6), c(.4, .7)), 1)
  # b has width 0 in
  expect_equal(round(emb_gg(c(.4, .7), c(.5, .5)),4), round(0.7142857,4))  #min{b2/a2,a1/b1} min{.5/.7,.4/.5}

  # a and b are completely equal
  expect_equal(emb_gg(c(.2, .3),c(.2, .3)), 1)

})

test_that("Rescher", {
  # a inside b
  expect_equal(emb_rs(c(.2, .3), c(0, 1)), 1)
  # a left of b
  expect_equal(emb_rs(c(.2, .3), c(.4, 1)), 0)
  # a right of b
  expect_equal(emb_rs(c(.6, .7), c(.4, .5)), 0)

  # [a = [b and b] > a]
  expect_equal(emb_rs(c(.6, .7), c(.6, .8)), 1)
  # [a = [b and b] < a]
  expect_equal(emb_rs(c(.6, .8), c(.6, .7)), 0)

  # a] = b] and [b > [a
  expect_equal(emb_rs(c(.3, .7), c(.5, .7)), 0)
  # a] = b] and [b < [a
  expect_equal(emb_rs(c(.6, .7), c(.3, .7)), 1)

  # a has width 0 out
  expect_equal(emb_rs(c(.6, .6), c(.4, .5)), 0)
  # b has width 0 out
  expect_equal(emb_rs(c(.6, .7), c(.5, .5)), 0)

  # a has width 0 in
  expect_equal(emb_rs(c(.6, .6), c(.4, .7)), 1)
  # b has width 0 in
  expect_equal(emb_rs(c(.4, .7), c(.5, .5)), 0)

  # a and b are completely equal
  expect_equal(emb_rs(c(.2, .3),c(.2, .3)), 1)

})
