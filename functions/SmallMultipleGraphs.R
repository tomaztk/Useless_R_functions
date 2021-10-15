##########################################
# 
#         Small multiple graphs
#
# Series:
# Little Useless-useful R functions #28
#
# Created: September 14, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com

# Changelog: 
###########################################

set.seed(2908)

# 1. Making data of 20 cases
myData <- data.frame(value=rnorm(2000, mean = 40, sd=20))
myControls <- data.frame(value=rnorm(2000, mean = 30, sd=20))
cases <- NULL
controls <- NULL
for (i in 1:20) {
  cases[[i]] <- sample(myData$value, size=20)
  controls[[i]] <- sample(myControls$value, size=20)
}

estimates <- data.frame(lower=NA, 
                        meanDiff=sapply(cases, mean)-sapply(controls,mean),
                        caseSSE= sapply(cases, function(x) sum((x-mean(x))^2)),
                        controlSSE = sapply(controls, function(x) sum((x-mean(x))^2)),
                        sd=NA,
                        upper=NA)


# Calculating statistics
estimates$sd <- sqrt((estimates$caseSSE+estimates$controlSSE)/64)
se <- estimates$sd/sqrt(32)  
tBound <- qt(0.975, df=31)
zBound <-qnorm(0.975)
estimates$lower <- estimates$meanDiff - se*tBound 
estimates$upper <- estimates$meanDiff + se*tBound
estimates$problem = estimates$lower >10 | estimates$upper < 10


tTest <- mapply(t.test, x=controls, y=cases)
tTest <- as.data.frame(t(tTest))
estimates$p <- unlist(tTest$p.value)
estimates$p <- round(estimates$p, 4)
estimates$significance <- ""
estimates$significance[estimates$p<.05] <- "*"
estimates$significance[estimates$p<.01] <- "**"
estimates$significance[estimates$p<.001] <- "***"

estimates$sampleNum <- as.numeric(row.names(estimates))
popDifferenceSE <- sqrt(20^2+20^2)/sqrt(32)
fakeData<-data.frame(value=rnorm(1000000, mean=10, sd=popDifferenceSE))



# 2. Get some libs for plotting

# get libraries we need for plotting and stacking the plots
library(ggplot2)


problemColors <- c("TRUE"="red", "FALSE"="blue")
colorScale <- scale_colour_manual(name="problem", values=problemColors)


# Plot Graph
ggplot(data=estimates, aes(x=meanDiff, y=sampleNum)) +
  
  geom_errorbarh(aes(xmin=lower,xmax=upper, color=problem)) +
  geom_point(aes(color=problem))  + 
  geom_vline(xintercept = 10, color="darkgreen") +
  scale_y_reverse() + 
  geom_text(aes(x=-24, y=sampleNum, 
                label=paste("Mean:", round(meanDiff,2))),
            size = 2.5, hjust="inward") +
  
  geom_text(aes(x=-17, y=sampleNum, 
                label=paste("P(x): ",
                            p, sep="")),
            size = 2.5, hjust="inward") +
  
  scale_x_continuous(limits=c(-26,30)) +
  colorScale +
  theme_void() +

  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.65),
        plot.margin = unit(c(10, 0, -2, 0), "pt")) +
  
  ggtitle("Mean with 95% CI") +
  geom_text(aes(x=-12, y=sampleNum, 
                label=estimates$significance),
            size = 2.5, hjust="inward") 





# 4. Clean
rm(popDifferenceSE,i,se,tBound, zBound, cases, controls, myData, myControls, tTest)
rm(colorScale, estimates, fakeData, smallMultiples, finalGraph, problemColors)

