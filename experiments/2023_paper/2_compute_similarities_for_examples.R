results <- tibble::tribble(~ejemplo, ~aL, ~aR, ~bL, ~bR, ~alpha,
                           ~sDice, ~sJaccard, ~sMin, ~sProd)

compute_similarities <- function(a, b, ej) {
  tibble(ejemplo = ej,
         aL = a[1],
         aR = a[2],
         bL = b[1],
         bR = b[2],
         # alpha = alpha,
         sDice = similarity(a, b, "dice"),
         sJaccard = similarity(a, b, "jaccard"),
         sProd = similarity(a, b, "product"),
         sMin = similarity(a, b, "min"),
         sMean = similarity(a, b, "mean"),
         sgMean = similarity(a, b, "gmean")
  )
}

# Ejemplo 1 --------------------------------------------------------------------
# para demostrar el aliasing

# con Alpha variando entre 0 y 0.6
ej1 <- lapply(seq(0, 0.6, 0.1), function(alpha) {
  a <- c(alpha, 0.8)
  b <- c(0, 0.2+alpha)
  compute_similarities(a, b, 1)
}) %>% bind_rows()

# Ejemplo 2 --------------------------------------------------------------------
# un intervalo está contenido en el otro (crecimiento de la intersección)

# con Alpha variando entre 0.1 y 0.8
ej2 <- lapply(seq(0.1, 0.8, 0.1), function(alpha) {
  a <- c(0, 0.9)
  b <- c(0, alpha)
  compute_similarities(a, b, 2)
}) %>% bind_rows()

# Ejemplo 3 --------------------------------------------------------------------
# un intervalo está contenido en el otro (decrecimiento de la intersección)

# con Alpha variando entre 0.2 y 1
ej3 <- lapply(seq(0.2, 1, 0.1), function(alpha) {
  a <- c(0, alpha)
  b <- c(0, 0.1)
  compute_similarities(a, b, 3)
}) %>% bind_rows()


# Ejemplo 4 --------------------------------------------------------------------
# intervalos de igual longitud

# con Alpha variando entre 0.1 y 0.4 (bueno, el último no lo consideraríamos
# para gráfica, es decir cuando Alpha es 0.4, acá podríamos tomar pasos de 0.05
# o depende cómo hagamos la gráfica)
ej4 <- lapply(seq(0.1, 0.4, 0.05), function(alpha) {
  a <- c(0.1, 0.5)
  b <- c(0.1+alpha, 0.5+alpha)
  compute_similarities(a, b, 4)
}) %>% bind_rows()


# Ejemplo 5 --------------------------------------------------------------------
# aumento del solapamiento, de la intersección

# con Alpha variando entre 0.1 y 0.4
ej5 <- lapply(seq(0.1, 0.4, 0.1), function(alpha) {
  a <- c(0.1+alpha, 0.5+alpha)
  b <- c(0.5, 0.9)
  compute_similarities(a, b, 5)
}) %>% bind_rows()

# Ejemplo 6 --------------------------------------------------------------------
# multiplicación de intervalo por escalar, la similitud aumneta igual (axioma 8)

# con alpha = 1,2,3,4,5
ej6 <- lapply(1:5, function(alpha) {
  a <- c(0.12*alpha, 0.2*alpha)
  b <- c(0.1*alpha, 0.15*alpha)
  compute_similarities(a, b, 6)
}) %>% bind_rows()

# Ejemplo 7 --------------------------------------------------------------------
# Nuevo para este artículo
ej7 <- lapply(seq(0, 0.2, 0.1), function(alpha) {
  a <- c(0.4+alpha, 0.8)
  b <- c(0.2, 0.6+alpha)
  compute_similarities(a, b, 7)
}) %>% bind_rows()
ej7

# Juntar ejemplos --------------------------------------------------------------

ej <- bind_rows(ej1, ej2, ej3, ej4, ej5, ej6, ej7)
print(ej)

