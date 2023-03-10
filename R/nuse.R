n <- 5
m <- 50

i <- 0
eval <- 0
while(i < 10000) {
  i <- i + 1
  set.seed(i)
  r <- random_profile_of_rankings(ncandidates = n, nvoters = m)
  b <- as.numeric(borda(r))
  v <- votrix(r)
  if(length(unique(b)) == ncol(v)) {
    eval <- eval+1
    if (kemeny_distance(v, b) > (tr(n)*m)/2)
      stop(i)
  }
}
print(eval)

tr <- function(n) {
  return(n*(n-1)/2)
}

kemeny_distance <- function(om, ranking) {
  r <- match(1:ncol(om), ranking)
  om <- om[r,r]
  return(sum(om[lower.tri(om)]))
}


#Datos temperaturas maxima y minima de Asturias por mes en orden descendente (enero=1,febrero=2,etc)

dataAsturias <- tibble::tribble(~v1L,~v1R)
dataAsturias <- dplyr::bind_rows(dataAsturias,
                                 tibble::tibble(
                                   v1L = c(0,.125,.0669,.093,.1831,.3227,.4099,.3983,.3459,.25,.1279,.0669),
                                   v1R = c(.6067,.7064,.875,.689,.7733,.8576,.8895,.875,1,.8517,.5669,.686)
                                 ))
sim_emb_matrix(dataAsturias, sim_w, mean, verbose = T)
