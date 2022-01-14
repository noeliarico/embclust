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
      # Compare with the remaining objects i.e. the ones that are in the
      # following rows of the dataset
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
      t(m) # vertical so transpote to be horizontal and create the matrix
    })

  # at this point smat is a list where each element corresponds to the pairwise
  # comparison of two objects. Each element of the list contains the vector
  # with the similarity for each variable between the two objects

  # Apply the aggregation function
  smat <- lapply(smat, function(x) {
    apply(x, 1, agg)
  })

  # Pad with zeros for the matrix and concatenate to create a matrix
  smat <- t(sapply(smat, function(x, n) {
    c(rep(0, n-length(x)), x)
  }, nobjs))

  return(smat)
}

