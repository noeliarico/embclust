#' Create and interval
#'
#' @param left
#' @param right
#' @param open
#'
#' @return
#' @export
#'
#' @examples
interval <- function(left, right, open = TRUE) {
  i <- list(interval = c(left, right),
            type = "open")
  class(i) <- "interval"
}

format.interval <- function(interval) {
  if(interval$type == "open")
    cat("(", interval$interval[1], ",", interval$interval[2], ")\n")
  else
    cat("[", interval$interval[1], ",", interval$interval[2], "]\n")
}
