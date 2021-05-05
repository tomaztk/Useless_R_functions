#GARCH Time Series error estimation serially calculation

library(fGarch)
library(tseries)
library(fBasics)


google.fit <- garchFit(formula = ~ arma(1, 1) + garch(1, 1),     
                       data = google.d, 
                       include.mean=TRUE)


google<-data.frame(google)
google<-ts(google)
google.d<-diff(log(google)) 



epsilon <- rnorm(200, 0, 1)
alpha_0 = 2
alpha_1 = 2
y = c(1)
sigma = c()
for (i in 1:length(epsilon)){
  #Tính sigma_t
  sigma_t = sqrt(alpha_0 + alpha_1*y[length(y)]^2)
  sigma = c(sigma, sigma_t)
  #Tính y_t
  y_t = sigma_t*epsilon[i]
  y = c(y, y_t)
}

y <- y[2:length(y)]
plot(y, type = 'l')



## 200,1
epsilon <- rnorm(200, 0, 1)
alpha_0 = 2
alpha_1 = 0.5
y = c(1)
sigma = c()
for (i in 1:length(epsilon)){
  #Tính sigma_t
  sigma_t = sqrt(alpha_0 + alpha_1*y[length(y)]^2)
  sigma = c(sigma, sigma_t)
  #Tính y_t
  y_t = sigma_t*epsilon[i]
  y = c(y, y_t)
}

y <- y[2:length(y)]
y[1:10]