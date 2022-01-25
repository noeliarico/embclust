#' Get the index of the k nearest neighbours
#'
#' @param distance_matrix matrix of distance between each pair of objects
#' @param i row to obtain the nearest neighbors
#' @param k Number of neighbours
#' @param traintest FALSE if rows and columns refer to the same objects
#'
#' @return
#'
#' @examples
get_nn <- function (distance_matrix, i = 1, k = 3, traintest = T)
{
  if(is.matrix(m)) {
    ordered_neighbors <- order(distance_matrix[i, ])
    if(traintest)
      return(ordered_neighbors[1:k])
    else # skip the first object
      return(ordered_neighbors[2:(k+1)])
  }
  else { # only one objects, distance from one object to others
    order(distance_matrix)[1:k]
  }
}
