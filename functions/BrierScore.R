## Brier score

fit <- glm(am~hp+wt,data=mtcars,family='binomial')

library(ggplot2)
ggplot(mtcars, aes(x=hp, y=wt), label=Name) + geom_point(aes(color=am)) + geom_smooth() +  
  geom_text(aes(label=as.character(Name),hjust=0,vjust=0))

rownames(mtcars)

pred.prob <- predict(fit,type='response')

sort(pred.prob)

brierScore <- mean((pred.prob-mtcars$am)^2)
# 0.04659236q




ld <- rep(0:5, 2)
nun <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
s <- factor(rep(c("M", "F"), c(6, 6)))
sf <- cbind(nun, nuna = 20 - nun)

# Run a model
bu<- glm(sf  ~ s + ld - 1, family = "binomial")
# Brier score
mean(bu$residuals^2)
## 0.2848604

