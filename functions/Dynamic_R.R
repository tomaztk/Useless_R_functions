# dynamic R


dynamic <- "result <- 2 * 3"
eval(parse(text = dynamic))
print(result)


base_packages = getOption('defaultPackages')
names(base_packages) = base_packages
use
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


# plotting
dyn_plt = "
xs <- seq(-2*pi,2*pi,pi/100)
w <- sin(3*xs)
plot(xs,w,type='l',ylim=c(-1,1))
abline(h=0,lty=3)
"

eval(parse(text = dyn_plt))


