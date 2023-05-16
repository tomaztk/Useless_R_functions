##########################################
#
#
# Drawing GISS surface temperature
# Climate Spiral
#
# Series:
# Little Useless-useful R functions #53
# Created: May 10, 2023
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0
#
# Changelog: 
#
##########################################

library(ggradar)
library(fmsb)
library(scales)
library(RColorBrewer)

#data url: https://data.giss.nasa.gov/gistemp/
# Global-mean monthly, seasonal, and annual means, 1880-present, updated through most recent month: TXT, CSV

#data txt and preparation
df <-read.csv("Documents/GLB.Ts+dSST.csv",header = TRUE, sep = ",", skip = 1, dec="." )[1:13]
df <- as.data.frame(sapply(df[1:143,], as.numeric))
df_months <- names(df)[2:13]
df_years <- df$Year
rownames(df) <- df_years
df <- df[,2:13]

# adding max min
max_min <- data.frame(
  Jan = c(1.4, -0.85), Feb = c(1.4, -0.85), Mar = c(1.4, -0.85),
  Apr = c(1.4, -0.85), May = c(1.4, -0.85), Jun = c(1.4, -0.85),
  Jul = c(1.4, -0.85), Aug = c(1.4, -0.85), Sep = c(1.4, -0.85),
  Oct = c(1.4, -0.85), Nov = c(1.4, -0.85), Dec = c(1.4, -0.85)
)
rownames(max_min) <- c("Max", "Min")
#merging
df <- rbind(max_min, df)

# Set graphic colors
nb.cols <- length(df_years)
mycolors <- colorRampPalette(brewer.pal(8, "Set2"))(nb.cols)
colors_border <- mycolors  
colors_in <- alpha(mycolors, 0.3) 


for (i in 1:length(df_years)){
  y <- df_years[1:i]
  df_tmp <- df[rownames(df)%in%y,1:12]
  df_tmp <- rbind(max_min, df_tmp)
  radarchart( df_tmp, maxmin=TRUE, axistype=1,seg=3,vlabels = df_months,
              plwd=0.5 , plty=1,centerzero=FALSE,caxislabels = c(-1, 0, 1, 1.4),
              cglcol="grey", cglty=2, axislabcol="black",  
              vlcex=1.2,
              title= paste0("GISS Surface temperature for years until ", tail(y,1)) )  
  legend(x=-0.35, y=0.15, legend = tail(y,1), bty = "n", pch=30 , col=colors_in , text.col ="black", cex=1.3, pt.cex=3)
}

