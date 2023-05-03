library(rgl)
# Create some dummy data
dat <- replicate(2, 1:3)

# Initialize the scene, no data plotted
plot3d(dat, type = 'n', xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-3, 3), xlab = '', ylab = '', zlab = '')

# Add planes
planes3d(1, 1, 1, 0, col = 'red', alpha = 0.6)
planes3d(1, -1, 1, 0, col = 'orange', alpha = 0.6)
planes3d(1, -1, -1, -0.8, col = 'blue', alpha = 0.6)


library(rgl)

# Definir los coeficientes de las inecuaciones
a_coef <- c(1, 0, -1, 0, 0, -1)
b_coef <- c(-1, 1, 0, 0, -1, 0)
c_coef <- c(0, 0, 1, 1, 0, 1)
m_coef <- c(0, 0, 0, -1, -1, 0)

# Definir los términos independientes de las inecuaciones
rhs <- c(0, 0, 0, 0, 0, 0)

# Crear una ventana de gráfico 3D
open3d()

plot3d(dat, type = 'n', xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-3, 3), xlab = '', ylab = '', zlab = '')


# Representar los planos
planes3d(a_coef[1], b_coef[1], c_coef[1], rhs[1], col = 'red', alpha = 0.6)
planes3d(a_coef[2], b_coef[2], c_coef[2], rhs[2], col = 'orange', alpha = 0.6)
planes3d(a_coef[3], b_coef[3], c_coef[3], rhs[3], col = 'blue', alpha = 0.6)
planes3d(a_coef[4], b_coef[4], c_coef[4], rhs[4], col = 'green', alpha = 0.6)
planes3d(a_coef[5], b_coef[5], c_coef[5], rhs[5], col = 'cyan', alpha = 0.6)
planes3d(a_coef[6], b_coef[6], c_coef[6], rhs[6], col = 'magenta', alpha = 0.6)

# naranja y cyan son equivalentes (2 y 5)
# azul y magenta son equivalentes (3 y 6)
close3d()
