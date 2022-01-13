
embedding_simmilarity <- function(data, emb, agg, verbose = F) {
  nobjs <- nrow(data)
  #smat <- matrix(0, ncol = nobjs, nrow = nobjs)

  obj1 <- 1
  obj2 <- 2
  smat <-
  lapply(1:(nobjs-1), function(obj1) {
    m <- (sapply((obj1+1):nobjs, function(obj2) {
      emb_vector <- sapply(seq(1, ncol(data), 2), function(i) {
        emb(data[obj1, c(i,i+1)], data[obj2, c(i,i+1)])
      })
    }))
    t(m)
  })

  smat <- lapply(smat, function(x) {
    apply(x, 1, agg)
  })

  # Pad with zeros for the matrix
  smat <- t(sapply(smat, function(x, n) {
    c(rep(0, n-length(x)), x)
  }, nobjs))

  return(smat)
}
