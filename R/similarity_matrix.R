#' Similarity between two objects
#'
#' Compute the similarity between two vectors using similarity measures
#' based on embedding functions.
#'
#' @param obj1 Object defined by a vector with a pair number of elements.
#' Consecutive elements represent lower (odd elements) and upper (even elements)
#' bounds of an interval.
#' @param obj2 The object to measure the similarity with obj1.
#' @param emb_sim Function that given two intervals a,b returns a value of similarity
#'
#' @return A vector with the similariy if agg is NULL and the value aggregated using agg otherwise
#' @export
#'
#' @examples
sim_emb_bt_two_obj <- function(obj1, obj2, emb_sim, agg = NULL) {
  emb_vector <- sapply(seq(1, length(obj1), 2), function(i) {
    emb_sim(obj1[c(i,i+1)], obj2[c(i,i+1)])
  })
  if(is.null(agg))
    return(emb_vector)
  else
    return(agg(emb_vector))
}

#' Matrix of similarities
#'
#' Given a dataset this computes a matrix of similarities using an embe
#'
#' @param data Dataset where each row is one of the objects. The data must have
#' an even pair of variables where the i-th column such that i is an odd number
#' is the lower bound of the interval representing the variable and the i+1 column
#' the corresponding upper bound.
#' @param emb_sim Embedding similarity
#' @param agg Aggregation function e.g. sum, mean
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
sim_emb_matrix <- function(data, emb_sim, agg, verbose = F) {

  nobjs <- nrow(data) # Number of objects in the data

  # Pairwise comparison of the objects
  obj1 <- 1
  obj2 <- 2
  smat <- # similarity matrix
    sapply(1:(nobjs-1), function(obj1) { # For each object
      # Compare pairwisely each object
      m <- (sapply((obj1+1):nobjs, function(obj2) {
        # Compare each pair of variables and get a vector with one element
        # for each variable (i.e. ncol(data)/2 elements) where each element
        # is the value of the similarity function that based on an embedding
        # function, which gives the similarity between the two objects in
        # this variables according to this function
        emb_vector <- sapply(seq(1, ncol(data), 2), function(i) {
          emb_sim(as.numeric(data[obj1, c(i,i+1)]),
                as.numeric(data[obj2, c(i,i+1)]))
        })
      }))
      # At this point m is a matrix of 3 rows (r,g,b) and as many columns
      # as objects left to compare with obj1.
      t(m) # vertical so transpose to have the columns as (r,g,b) and each row
            # for a comparison between a pris of objects
    })

  # at this point smat is a list where each element contains a matrix.
  # this matrix corresponds to the comparison of the object i, being i the
  # index of the element of the list, with the objects after it in the dataset.

  # Apply the aggregation function to obtain for each pairwise comparison
  # a single value
  smat <- lapply(smat, function(x) {
    apply(x, 1, agg)
  })

  # Pad with zeros for the matrix and concatenate to create a matrix
  smat <- t(sapply(smat, function(x, n) {
    c(rep(0, n-length(x)), x)
  }, nobjs))

  return(smat)
}


#' Matrix of similarities
#'
#' Given a dataset this computes a matrix of similarities using an embe
#'
#' @param data Dataset where each row is one of the objects. The data must have
#' an even pair of variables where the i-th column such that i is an odd number
#' is the lower bound of the interval representing the variable and the i+1 column
#' the corresponding upper bound.
#' @param emb_sim Embedding similarity
#' @param agg Aggregation function e.g. sum, mean
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
sim_emb_test_train <- function(data_test, data_train, emb_sim, agg, verbose = F) {

  nobjsTest <- nrow(data_test) # Number of objects to classify
  nobjsTrain <- nrow(data_train) # Number of objects in the training set
  nvariables <- ncol(data_test)

  # Pairwise comparison of the objects
  smat <- # similarity matrix
   t(sapply(1:nobjsTest, function(objTest) { # For each object in the test set

      # For each object in the training set
      m <- (sapply(1:nobjsTrain, function(objTrain) {
        # Given two objects: objTest and objTrain
        # Compare each pair of variables and get a vector with one element
        # for each variable (i.e. ncol(data)/2 elements) where each element
        # is the value of the similarity function that based on an embedding
        # function, which gives the similarity between the two objects in
        # this variables according to this function

        # print(data_train[objTrain, ])
        emb_vector <- sapply(seq(1, nvariables, 2), function(i) {
          emb_sim(as.numeric(data_test[objTest, c(i,i+1)]),
                  as.numeric(data_train[objTrain, c(i,i+1)]))
        })
        # emb stores a vector with the results of comparing two objects
        # variable by variable using an embedding function
      }))

      # Apply the aggregation function
      apply(m, 2, agg)
    }))

  return(smat)
}



