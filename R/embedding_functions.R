ok_interval <- function(interval) {
  if(length(interval)!=2 || !is.numeric(interval)) {
    stop("Invalid interval")
  }
  if(interval[1] > interval[2]) {
    stop("Invalid interval")
  }
}

# Aux functions ------------------------------------------------------------------

width <- function(interval) {
  return(interval[2] - interval[1])
}

#' Intersection between two intervals
#'
#' @param a first interval
#' @param b second interval
#'
#' @return TRUE if there is intersection, FALSE otherwise
#' @export
#'
#' @examples
intersection <- function(a, b, interval = FALSE) {
  if(!interval)
    return(max(a[1],b[1]) <= min(a[2],b[2]))
  else {
    left <- max(a[1],b[1])
    right <- min(a[2],b[2])
    # cat(paste0("Left: ", left, " right: ", right))
    if(left <= right) {
      return(c(left, right))
    }
    else {
      return(NA)
    }
  }
}

#' Union between two intervals
#'
#' @param a first interval
#' @param b second interval
#'
#' @return
#' @export
#'
#' @examples
union <- function(a, b, interval = FALSE) {
  left <- min(a[1],b[1])
  right <- max(a[2],b[2])
  return(c(left, right))
}


#' A is subset or equal to B
#'
#' @param a first interval
#' @param b second interval
#'
#' @return TRUE if there is intersection, FALSE otherwise
#' @export
#'
#' @examples
a_subeq_b <- function(a, b) {
  return(b[1] <= a[1] && a[2] <= b[2])
}

#' A is subset of B
#'
#' @param a first interval
#' @param b second interval
#'
#' @return TRUE if there is intersection, FALSE otherwise
#' @export
#'
#' @examples
a_sub_b <- function(a, b) {
  return(b[1] < a[1] && a[2] < b[2])
}

# Width ------------------------------------------------------------------------

#' Embedding based on the width of the interval
#'
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
emb_w <- function(a, b){
  #a y b intervalos
  #width funcion, en este caso ancho del intervalo
  if(width(a) == 0) {
    if (intersection(a, b)) {
      return(1)
    }
    else {
      return(0)
    }
  }
  else {
    i <- intersection(a, b, interval = TRUE)
    if(length(i) == 1 && is.na(i)) {
      return(0)
    }
    else {
      return(width(i)/width(a))
    }
  }
}

# Lukasiewicz ------------------------------------------------------------------

#' Lukasiewicz embedding function
#'
#' @param a interval
#' @param b interval
#'
#' @return Embedding value
#' @export
emb_lk <- function(a, b) {
  if(!intersection(a,b))
    return(0)
  else
    return(min(1-b[1]+a[1], 1+b[2]-a[2], 1))

}

# Fodor ------------------------------------------------------------------------

#' Fodor embedding function
#'
#' @param a interval
#' @param b interval
#'
#' @return Embedding value
#' @export
emb_fd <- function(a,b){
  if(!intersection(a, b)) {
    return(0)
  }
  if(a_subeq_b(a, b)) {
    return(1)
  }
  if(b[1] <= a[1] && a[1] <= b[2] && b[2] <= a[2]) {
    return(max(1-a[2], b[2]))
  }
  if(a[1] <= b[1] && b[1] <= a[2] && a[2] <= b[2]) {
    return(max(1-b[1], a[1]))
  }
  if(a_sub_b(b, a)) {
    return(min(max(1-a[2],b[2]),max(1-b[1],a[1])))
  }
  stop("Error in emb_fd")
}

# Godel ------------------------------------------------------------------------

#' Godel embedding function
#'
#' @param a interval
#' @param b interval
#'
#' @return Embedding value
#' @export
emb_gd <- function(a,b){
  if(!intersection(a, b)) {
    return(0)
  }
  else if(a_subeq_b(a, b)) {
    return(1)
  }
  else if(b[1] <= a[1] && a[1] <= b[2] && b[2] <= a[2]) {
    return(b[2])
  }
  else {
    return(a[1])
  }
}

# Goguen -----------------------------------------------------------------------

#' Goguen embedding function
#'
#' @param a interval
#' @param b interval
#'
#' @return Embedding value
#' @export
emb_gg <- function(a,b){
  if(!intersection(a, b)) {
    return(0)
  }
  else if(a_subeq_b(a, b)) {
    return(1)
  }
  else if(a[2] == 0) {
    return(a[1]/b[1])
  }
  else if(b[1] == 0) {
    return(b[2]/a[2])
  }
  else {
    return(min((b[2]/a[2]), a[1]/b[1]))
  }
}

# Rescher -----------------------------------------------------------------------

#' Rescher embedding function
#'
#' @param a interval
#' @param b interval
#'
#' @return Embedding value
#' @export
emb_rs <- function(a,b){
  if(a_subeq_b(a, b)) {
    return(1)
  }
  else {
    return(0)
  }
}

# Parametrized -----------------------------------------------------------------

#' Similarity functions
#'
#' @param a interval
#' @param b interval
#'
#' @return Value of the similarity
#' @export
similarity <- function(a, b, method){
  i <- intersection(a,b, interval = TRUE)
  if(all(is.na(i))) {
    return(0)
  }
  if(method == "dice") {
    return( width(i) / ((1/2) * (width(a)+width(b)) ) )
  }
  if(method == "jaccard") {
    return( width(i) / width(union(a,b)) )
  }
  if(method == "mean") {
    return( ( (width(i)/width(a)) + (width(i)/width(b)) ) / 2 )
  }
  if(method == "min") {
    return( min((width(i)/width(a)) , (width(i)/width(b)) ))
  }
  if(method == "product") {
    return( (width(i)/width(a)) * (width(i)/width(b)) )
  }
  if(method == "gmean") {
    return( width(i) / sqrt(width(a) * width(b)) )
  }
}
