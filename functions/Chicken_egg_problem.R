# what came first the chicken or the egg?
# 🥚 Egg
# 🐔 Chicken

whatcamefirst <- c("🥚","🐔" )
r <- sort(whatcamefirst, decreasing = FALSE)
r
#> [1] "🐔" "🥚"
is.unsorted(r)     
#> [1] FALSE