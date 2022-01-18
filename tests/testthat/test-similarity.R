test_that("Similariy matrix", {

  pretty_print_interval_data <- function(data) {
    obj <- 1
    out <- sapply(1:nrow(data), function(obj) {
      paste("Obj", obj, "\t", paste(sapply(seq(1, ncol(data), 2), function(i) {
        paste0("[", as.numeric(data[obj, i]), ",", as.numeric(data[obj, i+1]), "]")
      }), collapse = "  "))
    })
    cat(paste(out, collapse = "\n"))
  }
  # Define data
  data <- tibble::tribble(~v1L,~v1R,~v2L,~v2R,~v3L,~v3R)
  data <- dplyr::bind_rows(data,
                           tibble::tibble(
                             v1L = c(.2,.4,.1),
                             v1R = c(.5,.7,.5),
                             v2L = c(.1,.4,.2),
                             v2R = c(.6,.7,.6),
                             v3L = c(.2,.1,.8),
                             v3R = c(.3,.4,.9)
                           ))
  pretty_print_interval_data(data)

  #width
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w, mean),0.5111111,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_w)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_w, mean),0.5916667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_w)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_w, mean),0.2916667,
               tolerance = 3)
  #Lukasiewicz
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_lk, mean),0.85,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_lk, mean),0.66667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_lk, mean),0.5333333,
               tolerance = 3)
  #fodor
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_fd, mean),0.65,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_fd, mean),0.6,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_fd, mean),0.3833333,
               tolerance = 3)
  #godel
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gd, mean),0.4166667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gd, mean),0.3666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gd, mean),0.2333333,
               tolerance = 3)
  #goguen
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gg, mean),0.6369048,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gg, mean),0.5,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gg, mean),0.3869048,
               tolerance = 3)
  #rescher
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_rs, mean),0.1666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_rs, mean),0.3333333,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_rs, mean),0,
               tolerance = 3)


  sim_emb_matrix(data, sim_w, mean)

  #########
  # Define data
  data <- tibble::tribble(~v1L,~v1R,~v2L,~v2R,~v3L,~v3R)
  data <- dplyr::bind_rows(data,
                           tibble::tibble(
                             v1L = c(.2,0,.4),
                             v1R = c(.3,1,1),
                             v2L = c(.3,.6,.6),
                             v2R = c(.7,.7,.8),
                             v3L = c(.5,.4,.4),
                             v3R = c(.5,.6,.7)
                           ))
  pretty_print_interval_data(data)

  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w, mean),0.5583333,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_w)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_w, mean),0.2916667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_w)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_w, mean),0.7944444,
               tolerance = 3)
  #Lukasiewicz
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_lk, mean),0.8166667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_lk, mean),0.5666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_lk, mean),0.9,
               tolerance = 3)
  #fodor
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_fd, mean),0.7,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_fd, mean),0.4333333,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_fd, mean),0.8166667,
               tolerance = 3)
  #godel
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gd, mean),0.6166667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gd, mean),0.4,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gd, mean),0.71666667,
               tolerance = 3)
  #goguen
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gg, mean),0.7166667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gg, mean),0.514881,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gg, mean),0.7886905,
               tolerance = 3)
  #rescher
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_rs, mean),0.5,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_rs, mean),0.1666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_rs, mean),0.5,
               tolerance = 3)


  #########
  # Define data
  data <- tibble::tribble(~v1L,~v1R,~v2L,~v2R,~v3L,~v3R)
  data <- dplyr::bind_rows(data,
                           tibble::tibble(
                             v1L = c(0,0,.6),
                             v1R = c(.3,0,1),
                             v2L = c(.3,0,0),
                             v2R = c(1,.7,0),
                             v3L = c(0,.4,.0),
                             v3R = c(.5,1,.1)
                           ))
  pretty_print_interval_data(data)
  #width
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w) #comprobado
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w, mean),0.418254,
               tolerance = 3) #comprobado
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_w) #comprobado
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_w, mean),0.2,
               tolerance = 3) #comprobado
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_w) #comprobado
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_w, mean),0.1666667,
               tolerance = 3) #comprobado
  #Lukasiewicz
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_lk, mean),0.7,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_lk, mean),0.2666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_lk)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_lk, mean),0.2166667,
               tolerance = 3)
  #fodor
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_fd, mean),0.7,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_fd, mean),0.25,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_fd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_fd, mean),0.21666667,
               tolerance = 3)
  #godel
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gd, mean),0.3666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gd, mean),0.1833333,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gd)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gd, mean),0.1666667,
               tolerance = 3)
  #goguen
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_gg, mean),0,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_gg, mean),0,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gg)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_gg, mean),0,
               tolerance = 3)
  #rescher
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_rs, mean),0.1666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[3,]), sim_rs, mean),0.1666667,
               tolerance = 3)
  sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_rs)
  expect_equal(sim_emb_bt_two_obj(as.numeric(data[2,]), as.numeric(data[3,]), sim_rs, mean),0.1666667,
               tolerance = 3)

 })


