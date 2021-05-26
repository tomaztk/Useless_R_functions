# Is data-set the same?
# comparing datasets


ds1 <- data.frame(col1=c("t1","t2","t3","s4"),col2=c(1,2,5,7))

ds2 <- data.frame(col1=c("t1","t2","t3","s4"),col2=c(1,2,5,7))


zzfil1 <- tempfile("testbin1")
zz1 <- file(zzfil1, "wb")

zzfil2 <- tempfile("testbin2")
zz2 <- file(zzfil2, "wb")

d1 <- writeBin(toString(ds1), zz1)
d2 <- writeBin(toString(ds2), zz2)


identical(d1,d2)


#equality
all.equal(ds1,ds2,check.attributes = TRUE, use.names = TRUE)
