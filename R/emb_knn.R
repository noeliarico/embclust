# train <- matrix(runif(18,0,1),ncol=3) # n x n
# test <- matrix(runif(12,0,1),ncol=3) # n x n
# m <- Distance_for_KNN_test(test, train)
#
get_nn <- function (i, distance_matrix, k = 5, traintest = T)
{
  if(is.matrix(m)) {
    ordered_neighbors <- order(distance_matrix[i, ])
    if(traintest)
      return(ordered_neighbors[1:k])
    else
      return(ordered_neighbors[2:(k+1)])
  }
  else {
    order(distance_matrix)[1:k]
  }

}

# get_class <- function(data, nn) {
#
# }
