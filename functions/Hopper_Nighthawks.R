#  Homage to - Nighthawks Edward Hopper

getwd()
setwd("/Users/tomazkastrun/Desktop")

P <- list(
  night     = "#0c1113", brick    = "#6a4436", brick_dk = "#41291f",
  brick_lt  = "#7b5343", cornice  = "#281a14", shopglass= "#161110",
  shopframe = "#47362d", shade    = "#9d8b64", street   = "#39433c",
  walk_lit  = "#b0b795", walk_mid = "#8d9a7c", walk_dim = "#66735d",
  green     = "#2c4b3f", green_dk = "#1a2d26", fascia   = "#a89457",
  wall      = "#c8a545", wall_dk  = "#a68338", ceiling  = "#eed37a",
  glow      = "#f0d987", counter  = "#ecd992", counter_e= "#c9a952",
  urn       = "#cccbbc", urn_dk   = "#8a8a7d", white    = "#f1ecdf",
  white_sh  = "#c2bda9", skin     = "#dda87a", skin_dk  = "#b4824e",
  suitA     = "#2a2b39", suitB    = "#2f3442", hat      = "#1e1f28",
  dress     = "#a52220", dress_dk = "#791516", hair     = "#ab4b2a",
  hair_dk   = "#7c3520", sign     = "#8c7a41",
  hat_gry   = "#6e737a", shirt    = "#5d7794", jade     = "#579f7d",
  stool     = "#d3a455", stool_dk = "#a2762f"
)
W <- 1000; H <- 552

X   <- function(t) 145 + 700*t;   XR  <- function(s) 845 + 155*s
YB  <- function(t) 195 -  60*t;   YRB <- function(s) 135 +  44*s
YT  <- function(t) 355 +  40*t;   YRT <- function(s) 395 -  34*s
hh  <- function(t) YT(t)-YB(t)
Y   <- function(t,u) YB(t)  + (YT(t)-YB(t))*u
YR  <- function(s,u) YRB(s) + (YRT(s)-YRB(s))*u

band <- function(t1,t2,u1,u2,col)
  polygon(c(X(t1),X(t2),X(t2),X(t1)), c(Y(t1,u1),Y(t2,u1),Y(t2,u2),Y(t1,u2)),
          col=col, border=NA)
bandR <- function(s1,s2,u1,u2,col)
  polygon(c(XR(s1),XR(s2),XR(s2),XR(s1)), c(YR(s1,u1),YR(s2,u1),YR(s2,u2),YR(s1,u2)),
          col=col, border=NA)
ell <- function(cx,cy,rx,ry,col,n=80)
  polygon(cx+rx*cos(seq(0,2*pi,length.out=n)), cy+ry*sin(seq(0,2*pi,length.out=n)),
          col=col, border=NA)
fp <- function(t,dx,dy,col){h<-hh(t); polygon(X(t)+dx*h, YB(t)+dy*h, col=col, border=NA)}
fe <- function(t,dx,dy,rx,ry,col){h<-hh(t); ell(X(t)+dx*h, YB(t)+dy*h, rx*h, ry*h, col)}

png("nighthawks4.png", width=2000, height=1104, res=200)
par(mar=c(0,0,0,0), bg=P$night)
plot.new(); plot.window(c(0,W), c(0,H), asp=1, xaxs="i", yaxs="i")
rect(0,0,W,H, col=P$night, border=NA)

 
rect(430,168,900,420, col=P$brick_dk, border=NA)
rect(700,168,W,470, col="#1b1512", border=NA)
rect(  0,168,430,552, col=P$brick,    border=NA)
rect(355,168,430,552, col=P$brick_dk, border=NA)
rect(  0,500,435,518, col=P$cornice,  border=NA)
rect(  0,518,435,552, col=P$brick_dk, border=NA)
for (yy in c(312,402)) for (xx in seq(26,296,by=68)) {
  rect(xx,yy,xx+41,yy+60, col=P$night, border=NA)
  rect(xx,yy+35,xx+41,yy+60, col=P$shade, border=NA)
  rect(xx-5,yy+60,xx+46,yy+67, col=P$brick_lt, border=NA)
}
rect( 18,180,345,288, col=P$shopframe, border=NA)
rect( 31,192,172,277, col=P$shopglass, border=NA)
rect(189,192,332,277, col=P$shopglass, border=NA)
rect(258,199,288,229, col=P$walk_dim,  border=NA)

 polygon(c(0,W,W,0), c(0,0,128,174), col=P$walk_mid, border=NA)
polygon(c(0,145,845,W,W,0), c(108,160,76,116,46,38), col=P$walk_lit, border=NA)
polygon(c(0,150,150,0), c(180,174,158,164), col=P$walk_dim, border=NA)
polygon(c(0,152,152,0), c(168,163,152,157), col=P$street,   border=NA)
polygon(c(150,845,W,W,0,0), c(160,76,116,74,44,58), col="#bcc2a0", border=NA)
polygon(c(0,W,W,0), c(0,0,34,12), col=P$walk_dim, border=NA)
polygon(c(0,W,W,0), c(0,0,14,2), col=P$street, border=NA)

polygon(c(X(0),X(1),X(1),X(0)), c(YB(0),YB(1),YB(1)-46,YB(0)-40), col=P$green, border=NA)
polygon(c(XR(0),XR(1),XR(1),XR(0)), c(YRB(0),YRB(1),YRB(1)-40,YRB(0)-46),
        col=P$green, border=NA)

for (f in 1:2) {
  b <- if (f==1) band else bandR
  b(0,1,0.00,1.00, P$glow)
  b(0,1,0.36,0.87, P$wall)
  b(0,1,0.87,1.00, P$ceiling)
  b(0,1,0.82,0.87, P$wall_dk)
  b(0,1,0.355,0.375, P$counter_e)
  b(0,1, 0.13, 0.36, P$counter)                           
  b(0,1, 0.10, 0.14, P$counter_e)
}
band(0.79,0.97,0.36,0.80, P$wall_dk)                

for (tt in c(0.575, 0.648)) {                         
  fp(tt, c(-.062,.062,.062,-.062), c(.375,.375,.615,.615), P$urn)
  fp(tt, c( .026,.062,.062, .026), c(.375,.375,.615,.615), P$urn_dk)
  fe(tt, 0, .615, .062, .040, P$urn)
  fe(tt, 0, .668, .019, .030, P$urn_dk)
  fp(tt, c(-.016,.016,.016,-.016), c(.392,.392,.418,.418), P$urn_dk)
}

stool <- function(t) {
  h <- hh(t)
  fp(t, c(-.016,.016,.016,-.016), c(.015,.015,.105,.105), P$stool_dk)
  ell(X(t), YB(t)+.100*h, .088*h, .028*h, P$stool_dk)
  ell(X(t), YB(t)+.114*h, .088*h, .028*h, P$stool)
}
for (tt in c(0.075, 0.168, 0.355, 0.440, 0.610, 0.930)) stool(tt)


t <- 0.472
fp(t, c(-.150,.110,.128,.100,.048,-.048,-.108,-.140),
   c(.28,.28,.43,.492,.520,.520,.492,.43), P$white)
fp(t, c(-.150,-.075,-.055,-.140), c(.28,.28,.492,.43), P$white_sh)
fp(t, c(.048,.100,.128,.110,.052), c(.520,.492,.43,.36,.40), P$white_sh)
fp(t, c(-.030,.030,.030,-.030), c(.505,.505,.545,.545), P$skin_dk)
fe(t, .010, .585, .049, .057, P$skin)
fp(t, c(-.041,-.010,-.005,-.040), c(.585,.590,.625,.618), P$hair_dk)
fp(t, c(-.052,.070,.066,-.046), c(.618,.626,.660,.652), P$white)

t <- 0.260
fp(t, c(-.030,.030,.030,-.030), c(.44,.44,.52,.52), P$skin_dk)
fp(t, c(-.105,.105,.135,.100,.048,-.048,-.100,-.135),
   c(.03,.03,.42,.47,.495,.495,.47,.42), P$suitA)
fp(t, c(.100,.150,.140,.096), c(.060,.105,.34,.33), P$suitA)
fe(t, 0, .548, .054, .064, P$hat)
fe(t, 0, .588, .105, .023, P$hat)
fp(t, c(-.062,.062,.052,-.052), c(.582,.582,.652,.652), P$hat)

t <- 0.695
fp(t, c(-.115,.115,.142,.105,.048,-.052,-.108,-.142),
   c(.03,.03,.40,.455,.482,.482,.455,.395), P$suitB)
fp(t, c(-.140,-.062,-.050,-.128), c(.09,.062,.27,.30), P$suitB)
fp(t, c(-.052,.048,.042,-.046), c(.466,.466,.494,.494), P$shirt)
fp(t, c(-.040,.035,.035,-.040), c(.44,.44,.49,.49), P$skin_dk)
fe(t, -.012, .528, .048, .056, P$skin)
fp(t, c(-.058,-.047,-.056), c(.524,.536,.512), P$skin)
fp(t, c(.004,.045,.045,.009), c(.492,.492,.574,.576), P$hair_dk)
fe(t, -.006, .576, .105, .022, P$hat_gry)
fp(t, c(-.058,.054,.046,-.050), c(.570,.570,.638,.638), P$hat_gry)
fp(t, c(-.056,.052,.048,-.052), c(.572,.572,.596,.596), P$hat)
segments(X(t)-.138*hh(t), Y(t,.078), X(t)-.172*hh(t), Y(t,.086), col=P$white, lwd=2)

t <- 0.815
fp(t, c(-.100,.100,.124,.082,-.066,-.120), c(.03,.03,.38,.44,.44,.375), P$dress)
fp(t, c( .030,.100,.124,.055), c(.03,.03,.38,.40), P$dress_dk)
fp(t, c(-.114,-.040,-.026,-.100), c(.13,.092,.245,.28), P$dress)
fe(t, -.062, .120, .031, .026, P$skin)
fe(t, -.054, .141, .017, .014, P$white_sh)
fp(t, c(-.034,.034,.032,-.032), c(.405,.405,.462,.462), P$skin)
fe(t,  .014, .518, .057, .065, P$hair)
fe(t, -.014, .498, .046, .054, P$skin)
fp(t, c(-.056,-.046,-.054), c(.494,.506,.483), P$skin)
fp(t, c(-.034,.046,.058,-.012), c(.538,.548,.500,.516), P$hair)
fp(t, c(.010,.068,.062,.004), c(.44,.45,.548,.548), P$hair)

for (tt in c(0.335,0.650,0.775)) fe(tt,.055,.205,.019,.013,P$white)
fe(0.752,0,.196,.009,.020,P$white_sh); fe(0.772,0,.196,.009,.020,P$white_sh)

band(0,1,0.985,1.015,P$green_dk); bandR(0,1,0.985,1.015,P$green_dk)
band(0,1,-0.015,0.005,P$green_dk); bandR(0,1,-0.015,0.005,P$green_dk)
band(0,1,-0.004,0.034,P$jade); bandR(0,1,-0.004,0.034,P$jade)
for (f in 1:2) { b <- if (f==1) band else bandR
b(0,1,1.00,1.10,P$fascia); b(0,1,1.10,1.30,P$sign); b(0,1,1.30,1.35,P$green_dk) }
polygon(c(X(0)-20,X(0),X(0),X(0)-20), c(YB(0)-40,YB(0),Y(0,1.35),Y(0,1.35)),
        col=P$green, border=NA)
polygon(c(X(1)-9,X(1)+9,X(1)+9,X(1)-9), c(YB(1)-46,YB(1)-46,Y(1,1.02),Y(1,1.02)),
        col=P$green_dk, border=NA)
text(X(0.34), Y(0.34,1.20), "PHILLIES", col=P$night, cex=.66, family="serif", font=2)
text(X(0.86), Y(0.86,1.20), "5c CIGARS", col=P$night, cex=.42, family="serif")

set.seed(1942)
for (i in 1:2600) {
  xs <- runif(1, 0, W); ys <- runif(1, 0, H); ln <- runif(1, 8, 46)
  segments(xs, ys, xs+ln, ys+runif(1,-1.2,1.2), lwd = runif(1, .6, 1.6),
           col = rgb(1,1,1, .022) )
}
for (i in 1:1400) {
  xs <- runif(1, 0, W); ys <- runif(1, 0, H); ln <- runif(1, 8, 40)
  segments(xs, ys, xs+ln, ys+runif(1,-1.2,1.2), lwd = runif(1, .6, 1.4),
           col = rgb(0,0,0, .022))
}
for (i in seq(0, 26)) {
  a <- 0.014
  rect(-6+i*1.6, -6+i*1.0, W+6-i*1.6, H+6-i*1.0, border = rgb(0,0,0,a), lwd = 3)
}

dev.off()