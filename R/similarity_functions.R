#' Similarity given by embeddings
#'
#' @param emb
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @rdname sim_emb
#' @examples
sim_emb <- function(emb, a, b) {
  return((emb(a,b) + emb(b,a))/2)
}

#' @export
#' @rdname sim_emb
sim_w <- function(a, b) {
  sim_emb(emb_w, a, b)
}


#' @export
#' @rdname sim_emb
sim_lk <- function(a, b) {
  sim_emb(emb_lk, a, b)
}


#' @export
#' @rdname sim_emb
sim_fd <- function(a, b) {
  sim_emb(emb_fd, a, b)
}


#' @export
#' @rdname sim_emb
sim_gd <- function(a, b) {
  sim_emb(emb_gd, a, b)
}


#' @export
#' @rdname sim_emb
sim_gg <- function(a, b) {
  sim_emb(emb_gg, a, b)
}


#' @export
#' @rdname sim_emb
sim_rs <- function(a, b) {
  sim_emb(emb_rs, a, b)
}

