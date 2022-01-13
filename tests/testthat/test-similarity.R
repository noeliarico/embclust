test_that("Similariy matrix", {

  # Define data
  data <- tibble::tribble(~v1L,~v1R,~v2L,~v2R,~v3L,~v3R)
  data <- dplyr::bind_rows(data,
              tibble::tibble(
                      v1L = c(2,4,1),
                      v1R = c(5,7,5),
                      v2L = c(1,4,2),
                      v2R = c(6,7,6),
                      v3L = c(2,1,8),
                      v3R = c(3,4,9)
                    ))

})
