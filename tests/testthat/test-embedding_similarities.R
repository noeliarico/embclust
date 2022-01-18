test_that("Width", {

  expect_equal(sim_emb_bt_two_obj(c(.1,.4, .3,.5), c(.3,.8, .1,.9), sim_w),
               c(0.2666667, 0.6250000),
               tolerance = 3)


})
