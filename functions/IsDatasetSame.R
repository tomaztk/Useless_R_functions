
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

# two samples of Dataframe
ds1 <- data.frame(col1=c("t1","t2","t3","s4"),col2=c(1,2,5,7))
ds2 <- data.frame(col1=c("t1","t2","t3","s4"),col2=c(1,2,5,7))

#using WriteBin
zzfil1 <- tempfile("testbin1")
zz1 <- file(zzfil1, "wb")

zzfil2 <- tempfile("testbin2")
zz2 <- file(zzfil2, "wb")

d1 <- writeBin(toString(ds1), zz1)
d2 <- writeBin(toString(ds2), zz2)

#check
identical(d1,d2)


#using equality with all.equal
all.equal(ds1,ds2,check.attributes = TRUE, use.names = TRUE)
