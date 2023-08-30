# dynamic R


dynamic <- "result <- 2 * 3"
eval(parse(text = dynamic))
print(result)


base_packages = getOption('defaultPackages')
names(base_packages) = base_packages

bsp <- lapply(base_packages, function (pkg) ls(paste0('package:', pkg)))
bs <- ls("package:base")

#get random dataset
sample(bsp$datasets,1)

dynamic <- "
base_packages <- getOption('defaultPackages');
bsp <- lapply(base_packages, function (pkg) ls(paste0('package:', pkg)));
ds<-sample(bsp$datasets,1);
"

eval(parse(text = dynamic))
head(ds,10)
