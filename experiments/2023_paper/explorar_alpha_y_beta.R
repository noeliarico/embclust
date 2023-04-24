#' en la definicion 3.5 M es la media aritmetica y S es la fórmula de la definicion de 2.4
srm <- function(a, b, alpha, beta) {

  i <- intersection(a,b,interval = T)
  if(length(i) == 1 && is.na(i)) {
    return(0)
  }

  print(i)
  print(width(i))

  a_minus_b <- width(a)-width(i)
  b_minus_a <- width(b)-width(i)

  numerator <- width(i)
  denominator <- width(i) + alpha*a_minus_b + beta*b_minus_a

  return(numerator/denominator)

}

get_value <- function(a, b, alpha, beta) {
  v1 <- srm(a,b,alpha,beta)
  v2 <- srm(b,a,alpha,beta)
  return(mean(c(v1, v2)))
}


#' CASO 3: [0.2,0.6] y [0.5,0.9]
#' CASO 4: [0.2,0.9] y [0.5,0.6]

# srm(c(0.2,0.5), c(0.6, 0.9), alpha = 0.5, beta = 0.5) # siempre 0
srm(c(0.2,0.6), c(0.5, 0.9), alpha = 0.5, beta = 0.5)
srm(c(0.2,0.9), c(0.5, 0.6), alpha = 0.5, beta = 0.5)

values <- expand.grid(alpha = seq(0,1,0.1),
              beta = seq(0,1,0.1)) %>%
  as_tibble()

values <- values %>% mutate(
  srm = srm(c(0.2,0.6), c(0.5, 0.9), alpha, beta)
  #srm = srm(c(0.2,0.9), c(0.5, 0.6), alpha, beta)
)

ggplotly(
  ggplot(values, aes(alpha, beta, size = srm)) +
    geom_point() +
    theme_bw()
)
