
# Load packages
library(mathart) 
#devtools::install_github("marcusvolz/mathart")
library(tidyverse)

# Set parameters (see mathart::mollusc() documentation for details)
n_s <- 650L
n_t <- 2000L
n <- 1000
alpha <- 82.6
beta <- 1.515
phi <- 14.3
mu <- 0
Omega <- 0
s_min <- -193.8
s_max <- 69.4
A <- 7.031
a <- 2.377
b <- 6.42
P <- 0
W_1 <- 1
W_2 <- 1
N <- 0
L <- 0
D <- 1
theta_start <- 0
theta_end <- 10*pi

# Generate data
df <- mollusc(n_s = n_s, n_t = n_t,
              alpha = alpha, beta = beta, phi = phi, mu = mu, Omega = Omega, s_min = s_min, s_max = s_max,
              A = A, a = a, b = b, P = P, W_1 = W_1, W_2 = W_2, N = N, L = L, D = D,
              theta_start = theta_start, theta_end = theta_end)

# Create plot
p <- ggplot() +
  geom_point(aes(x, z), df, size = 0.03, alpha = 0.03) +
  geom_path(aes(x, z), df, size = 0.03, alpha = 0.03) +
  coord_equal() +
  theme_blankcanvas(margin_cm = 0)

p
