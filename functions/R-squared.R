
# R-squared useful over R-squared useless

set.seed(2908)                   

# some toy/random data
x <- 1:30                        
y <- 2 + 0.5*x + rnorm(30,0,4)    
mod <- lm(y~x)                    
summary(mod)$r.squared            



# R-squared (coefficient of determination) is
# also sum of squared residuals (fitted-value deviations) (mms)
# over total sum of squared (tts)

f <- mod$fitted.values       # extract fitted (or predicted) values from model
mss <- sum((f - mean(f))^2)  
tss <- sum((y - mean(y))^2)  
mss/tss                      

#check
all.equal(summary(mod)$r.squared  , mss/tss  )


# R-squared and demonstrate them with simulations in R.
# Assuming: 1. R-squared doesn't  necessarily mean  measure goodness of fit. 
# It can be arbitrarily low when the model is completely correct. By making sigma2 large 
# large, we drive R-squared towards 0, even when every assumption of the simple linear regression model is correct in every particular.

r2.0 <- function(sig){
  x <- seq(1,10,length.out = 100)        
  y <- 2 + 1.2*x + rnorm(100,0,sd = sig) #  adding  some random noise to function
  summary(lm(y ~ x))$r.squared            
}


# plot shows sinking sigmas (starting with 1) where R2=1 and the model is completely wrong, respectively.

sigmas <- seq(0.5,20,length.out = 20)
rout <- sapply(sigmas, r2.0)             # apply our function to a series of sigma values
plot(rout ~ sigmas, type="b")



set.seed(2908)
x <- rexp(80,rate=0.005)                     # our predictor is data from an exponential distribution
y <- (x-1)^2 * runif(80, min=0.8, max=1.2)   # non-linear data generation
plot(x,y)				   


# check R squared
summary(lm(y ~ x))$r.squared

# R-squared says nothing about prediction error, even with 
# exactly the same, and no change in the coefficients. 
# R-squared can be anywhere between 0 and 1 just by changing the range of X. 
# We're better off using Mean Square Error (MSE) as a measure of prediction error.

set.seed(2908)
x <- seq(1,10,length.out = 200)
y <- 2 + 1.2*x + rnorm(200,0,sd = 0.9) #sd = 0.9
mod1 <- lm(y ~ x)
summary(mod1)$r.squared
# [1] 0.9250846

# calculate MSE
sum((fitted(mod1) - y)^2)/200  
# [1] 0.7815316

# changing the range
set.seed(2908)

#getting different R2
x <- seq(1,3,length.out = 200)

y <- 2 + 1.2*x + rnorm(200,0,sd = 0.9)
mod1 <- lm(y ~ x)
summary(mod1)$r.squared
# [1] 0.3645032

#but MSE is the same
sum((fitted(mod1) - y)^2)/200  
# [1] 0.7815316


# The R2 falls from 0.92 to 0.36 but the MSE is practically unchanged!
