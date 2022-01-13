#' Similarity given by embeddings
#'
#' @param emb
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
sim_emb <- function(emb, a, b) {
  return((emb(a,b) + emb(b,a))/2)
}
