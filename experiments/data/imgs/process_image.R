get_median_area <- function(data, pixel_row, pixel_col, window = 1) {
  data <- data %>%
    mutate(row = as.numeric(row),
           column = as.numeric(column)) %>%
    # get the area (and the pixel itself)
    filter(row <= pixel_row+1 &
           row >= pixel_row-1 &
           column <= pixel_col+1 &
           column >= pixel_col-1
           ) %>%
    # to remove the pixel itself
    filter(row != pixel_row | column != pixel_col) %>%
    select(-row, -column)
  data <- apply(data, 2, median)
  names(data) <- c("rMedian", "gMedian", "bMedian")
  return(data)
}
