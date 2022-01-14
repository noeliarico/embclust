test_that("Similariy matrix", {

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

  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w)
  sim_emb_bt_two_obj(as.numeric(data[1,]), as.numeric(data[2,]), sim_w, mean)

  sim_emb_matrix(data, sim_w, mean)
})

pretty_print_interval_data <- function(data) {
  obj <- 1
  out <- sapply(1:nrow(data), function(obj) {
    paste("Obj", obj, "\t", paste(sapply(seq(1, ncol(data), 2), function(i) {
      paste0("[", as.numeric(data[obj, i]), ",", as.numeric(data[obj, i+1]), "]")
    }), collapse = "  "))
  })
  cat(paste(out, collapse = "\n"))
}
