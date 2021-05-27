
##########################################
# 
# Are two dataframes same?
#
# Series:
# Little Useless-useful R functions #24
# Created: May 27, 2021
# Author: Tomaž Kaštrun
# Blog: tomaztsql.wordpress.com
# V.1.0

# Changelog: 
###########################################
library("compareDF")
#install.packages("compareDF")

# two samples of Dataframe
ds1 <- data.frame(col1=c("t1","t2","t3","s4"),col2=c(1,2,5,7))
ds2 <- data.frame(col1=c("t1","t2","t3","s4"),col2=c(1,2,5,8))


#using equality with all.equal
all.equal(ds1,ds2,check.attributes = TRUE, use.names = TRUE)


# Using compareDF difference / equality

df_compare <- compare_df(ds1, ds2, "col2")
df_compare$comparison_df

