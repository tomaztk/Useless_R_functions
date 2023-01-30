library(ggplot2)
library(dplyr)
library(tidyr)
library(faux)


dat <- rnorm_multi(n = 100, 
                   mu = c(0, 20, 20),
                   sd = c(1, 5, 5),
                   r = c(0.5, 0.5, 0.25), 
                   varnames = c("A", "B", "C"),
                   empirical = FALSE)



bvn <- rnorm_multi(100, 5, 0, 1, .3, varnames = letters[1:5])


cmat <- cor(iris[,1:4])
bvn <- rnorm_multi(100, 4, 0, 1, cmat, 
                   varnames = colnames(cmat))


cmat <- c(1, .3, .5,
          .3, 1, 0,
          .5, 0, 1)
bvn <- rnorm_multi(100, 3, 0, 1, cmat, 
                   varnames = c("first", "second", "third"))


rho1_2 <- .3
rho1_3 <- .5
rho1_4 <- .5
rho2_3 <- .2
rho2_4 <- 0
rho3_4 <- -.3
cmat <- c(rho1_2, rho1_3, rho1_4, rho2_3, rho2_4, rho3_4)
bvn <- rnorm_multi(100, 4, 0, 1, cmat, 
                   varnames = letters[1:4])



# empirical true

bvn <- rnorm_multi(100, 5, 0, 1, .3, 
                   varnames = letters[1:5], 
                   empirical = T)


dat <- rnorm_multi(varnames = "A") %>%
  mutate(B = rnorm_pre(A, mu = 10, sd = 2, r = 0.5))


dat$C <- rnorm_pre(dat$A, mu = 10, sd = 2, r = 0.5, empirical = TRUE)

dat$D <- rnorm_pre(dat, r = c(.1, .2, .3), empirical = TRUE)

summary(dat)
cor(dat)




####
# generate X, Y Variables of sample size 200 with r coeff of 0.91
samples = 200
r_coef = 0.91

data <- rnorm_multi(n = samples, vars = 3, r=(0.91), varnames = c("X", "Y", "Z"), empirical = TRUE)

head(data)

cor(data)
