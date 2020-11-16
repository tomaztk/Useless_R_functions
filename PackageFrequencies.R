##########################################
# 
# Frequency of words in vignettes / libraries
# Series:
# Little Useless-useful R functions #8
# Created: November 11, 2020
# Author: Tomaz Kastrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: - 
###########################################

packs <- c("ggplot2", "Hmisc")

require("ggplot2")
require("Hmisc")


rr <<- help(package="MASS")
rr <<-browseVignettes(package="ggplot2")
rr <<-vignette(package="ggplot2") 
rr <<-demo(package="ggplot2")


# good
help.search("^glm")

# get all functions per package
fx <- ls("package:ggplot2")

# get content for each function per package
w2 <- help("geom_label",package="ggplot2") 
w2$text

#ls commadnd
ls(w2)
