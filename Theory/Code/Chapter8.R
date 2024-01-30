## Lidar data
library(SemiPar)
#library(car)
data(lidar)
plot(logratio~range,data=lidar)

# Scatterplot smoothing of LIDAR data with polynomial regression of different degrees

library(lattice)
cols. <- rainbow(5)
range.seq <- seq(min(lidar$range),max(lidar$range),l=200)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
for(i in 1:5){
  lines(range.seq,predict(lm(logratio~poly(range,i),data=lidar),data.frame(range=range.seq)),lwd=2,col=cols.[i])
}
legend("bottomleft",c("p = 1","p = 2","p = 3","p = 4","p = 5"),col=cols.,lty=1:5,lwd=4)

# loess smoothing with different spans and polynomial degree=2

attach(lidar)
span <- 1/c(1,2,3,4,5)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
cols <- rainbow(length(span))
for(i in 1:length(span)){
  lines(loess.smooth(range,logratio,span=span[i], degree=2),col=cols.[i],lwd=2.5,lty=i)
}
legend("bottomleft",paste("span = ",round(span,3)),col=cols.,lwd=2.5,lty=1:6)

# Kernel smoothing with different bandwiths and Gaussian kernel

attach(lidar)
b <- c(10,20,50,120,1000)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
cols. <- rainbow(length(b))
for(i in 1:length(b)){
  lines(ksmooth(range,logratio,x.points=range,"normal", bandwidth=b[i]),col=cols.[i],lwd=2.5,lty=i)
}
legend("bottomleft",paste("bandwidth = ",b),col=cols.,lwd=2.5,lty=1:5)

# Natural splines smoothing with different degrees of freedom

library(splines)
attach(lidar)
df <- c(1,3,5,8,15)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
cols. <- rainbow(length(df))
for(i in 1:length(df)){
  lines(range,predict(lm(logratio~ns(range,df=df[i]))),col=cols.[i],lwd=2.5,lty=i)
}
legend("bottomleft",paste("df = ",df),col=cols.,lwd=2.5,lty=1:5)

# Smoothing splines fit with different degrees of freedom

attach(lidar)
df <- c(3,5,10,15,30)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
cols. <- rainbow(length(df))
for(i in 1:length(df)){
  lines(smooth.spline(range,logratio,df=df[i]),col=cols.[i],lwd=2.5,lty=i)
}
legend("bottomleft",paste("df = ",df),col=cols.,lwd=2.5,lty=1:5)

# Smoothing splines fit with different values of `spar`

lam <- c(1e-1,.5,1,2)
par(mfrow=c(2,2))
for(i in 1:4){
  plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05)); title(bquote(lambda == .(lam[i])))
  lines(smooth.spline(range,logratio,spar=lam[i]),col=i,lwd=2.5,lty=1)
}

## #default mode chooses df by generalized cross-validation
## sm.gcv <- smooth.spline(range,logratio)
##  sm.cv <- smooth.spline(range,logratio,cv=TRUE)  # cv

# Simulated data example

set.seed(2015)
n <- 200
x <- seq(0,1,l=n)
# original function
f <- sin(3*pi*x)
y <- f + rnorm(n,.33)

plot(x,y,col="grey",pch=19)
lines(x,f,col=1,lwd=3)

#
# Function to create TPFs
tpoly <- function(x,t,p){
  B = NULL
  for (i in 1:length(t)){
    B <- cbind(B,(x-t[i])^p*(x>t[i]))
  }
  B
}

# Truncated Polynomial Functions with different degrees p
K <- 10
knots <- seq(0,1, length= (K+2))[-c(1,K+2)]
x <- seq(0,1,l=100)
B = list()
p <- c(0,1,2,3)
for(i in 1:4){
  B[[i]] <- tpoly(x,knots,p[i])
}

par(mfrow=c(2,2))
for(i in 1:4){
  #matplot(x,B[[1]],t='l',col=NA)
  matplot(x,B[[i]],col=i,t="l",lwd=2,xlab="x", ylab = " ", lty=2:5); title(bquote(p == .(p[i])))
  points(knots,0*knots,col=i,pch=15)
}

## ------------------------------------------------------------------------
library(splines)
bspline <- function(x,xl,xr,ndx,bdeg){
  dx <- (xr-xl)/ndx
  knots <- seq(xl-bdeg*dx,xr+bdeg*dx,by=dx)
  B <- spline.des(knots,x,bdeg+1,0*x,outer.ok=TRUE)$design
  output <- list(knots=knots,B=B)
  return(output)
}

# B-spline bases with different degrees q

ndx = 7
x <- seq(0,1,l=200)
B = list()
xl <- min(x)+0.01
xr <- max(x)+0.01
deg <- c(0,1,2,3)
knots <- bspline(x,xl,xr,ndx,bdeg=3)$knots
for(i in 1:4){
  B[[i]] <- bspline(x,xl,xr,ndx,bdeg=deg[i])$B
}

par(mfrow=c(2,2))
for(i in 1:4){
  #matplot(x,B[[1]],t='l',col=NA)
  matplot(x,B[[i]],col=i,t="l",lwd=2,xlab="x", ylab = " ", lty=1); title(bquote(q == .(deg[i])))
  points(knots,0*knots,col=i,pch=15)
}

# B-spline regression with different number of segments

ndx <- c(8,12,20)
plot(x,y,col="grey",pch=19,cex=0.55)
for(i in 1:3){
  B <- bspline(x,xl,xr,ndx=ndx[i],bdeg=3)$B
  fit <- B%*%solve(t(B)%*%B,t(B)%*%y)
  lines(x,fit,col=i,lwd=3,lty=1)
}
legend("topright",paste("ndx = ",ndx),col=1:3,lwd=4,lty=1)

# B-spline regression with different values of the smoothing parameter

ndx = 10
pord <- 2
lam <- c(1e-3,10,100)
plot(x,y,col="grey",pch=19,cex=.55); title(paste("ndx = ",ndx, "and d =", pord))
for(i in 1:3){
  B <- bspline(x,xl,xr,ndx=ndx,bdeg=3)$B
  D <- diff(diag(ncol(B)),differences=pord)
  P <- lam[i]*t(D)%*%D
  fit <- B%*%solve(t(B)%*%B + P,t(B)%*%y)
  lines(x,fit,col=i,lwd=3,lty=1)
}
legend("topright", lty=1,col=1:3,lwd=4, legend = as.expression(lapply(lam, function(m)bquote(lambda ==.(m)))))

# P-spline regression fit with basis and coefficients

ndx = 15
pord <- 2
lam <- c(1e-3,1,100,1000)
par(mfrow=c(2,2))
for(i in 1:4){
  plot(x,y,col="grey",pch=19,cex=.55); title(bquote(lambda == .(lam[i])))
  Basis <- bspline(x,0,1,ndx=ndx,bdeg=3)
  B <- Basis$B
  knots <- Basis$knots
  D <- diff(diag(ncol(B)),differences=pord)
  P <- lam[i]*t(D)%*%D
  theta <- solve(t(B)%*%B + P,t(B)%*%y)
  fit <- B%*%theta
  lines(x,fit,col=i,lwd=3,lty=1)
  matlines(x,B%*%diag(c(theta)),col=i,lty=2)
  points(knots,0*knots,col=i,cex=1.5,lwd=2.5,pch=15)
  points(knots[-c(1:2,length(knots)-1,length(knots))],theta,col=i,cex=1.5,lwd=2.5)
}

## ------------------------------------------------------------------------
# Function to fit a Gaussian P-spline for given lambda
psfit <- function(x,y,pord=2,ndx=10,lambda=1){
  xl = min(x)
  xr = max(x)
  n <- length(y)
  B <- bspline(x,xl,xr,ndx,bdeg=3)$B
  nb <- ncol(B)
  P <- diff(diag(nb),differences=pord)
  
  # Construct penalty stuff
  P <- sqrt(lambda) * diff(diag(nb), diff = pord)
  nix = rep(0, nb - pord)
  
  # Fit
  f = lsfit(rbind(B, P), c(y, nix), intercept = FALSE)
  h = hat(f$qr)[1:n]
  theta = f$coef
  f.hat = B %*% theta
  
  # Cross-validation and dispersion
  trH <- sum(h)
  rss <- sum((y-f.hat)^2)
  gcv <- n*rss/(n-trH)^2
  sigma = sqrt(rss / (n - trH))
  
  # Error bands ("Bayesian estimate")
  Covb = solve(t(B) %*% B + t(P) %*% P)
  Covz = sigma ^ 2 * B %*% Covb %*% t(B)
  seb = sqrt(diag(Covz))
  
  output <- list(gcv=gcv,sigma=sigma,
                 f=f,theta=theta,f.hat=f.hat,seb=seb,trH=trH)
  return(output)
}

# P-spline fit with confidence bands computed with `psfit`

set.seed(2015)
n <- 200
x <- seq(0,1,l=n)
# original function
f = 2*x + sin(2*pi *x) 
#f <- sin(3*pi*x)
y <- f + rnorm(n,0,1)

fit <- psfit(x,y,ndx=20,lambda=1)
plot(x,y,cex=.65,col="lightgrey"); title(main=bquote(lambda ==1))
lines(x,fit$f.hat,col=4)
lines(x,fit$f.hat+1.96*fit$seb,col=2,lty=2)
lines(x,fit$f.hat-1.96*fit$seb,col=2,lty=2)

## ------------------------------------------------------------------------
# plot of GCV criteria is more useful
lla = seq(-2, 2, by = 0.10)
cvs = 0 * lla
for (k in 1:length(lla)) {
  lambda = 10 ^ lla[k]
  pn =  psfit(x,y, lambda = lambda)
  cvs[k] = pn$gcv
}
# 
lam.cv <- 10^(lla[which.min(cvs)])
lam.cv # lambda chosen by generalized cv

fit.cv <- psfit(x,y,lambda=lam.cv)

fit.cv$sigma # estimated sigma error

# P-spline fit with confidence bands computed with `psfit` and smoothing parameter selected by minimizing gcv

par(mfrow=c(1,2))
plot(lla, cvs, xlab = 'log10(lambda)', ylab = 'CV')
lines(lla, cvs)
abline(v=lla[which.min(cvs)])

plot(x,y,cex=.65,col="lightgrey"); title(main=bquote(hat(sigma)^2==.(round(fit.cv$sigma,3))^2))
lines(x,fit.cv$f.hat,col=4)
lines(x,fit.cv$f.hat+1.96*fit$seb,col=2,lty=2)
lines(x,fit.cv$f.hat-1.96*fit$seb,col=2,lty=2)
legend("topright",legend=bquote(lambda==.(round(lam.cv,3))),border=NULL)


# Scatterplot of ratios of strontium isotopes (x 10,000) and age in million of years

library(SemiPar)
data(fossil)
attach(fossil)
y <- 10000*strontium.ratio
x <- age

## ------------------------------------------------------------------------
fit.spm <- spm(y ~ f(x, basis="trunc.poly", degree=3))
summary(fit.spm)

# P-splines fit using the mixed model reparameterization by @Ruppert03

plot(fit.spm, xlab="Age", ylab="Ratio of strotium isotopes (x10,000)") 
points(x,y,pch=15,col="red") # add points

## ------------------------------------------------------------------------
names(fit.spm)

## ------------------------------------------------------------------------
names(fit.spm$fit)

## ------------------------------------------------------------------------
names(fit.spm$info)

## ------------------------------------------------------------------------
names(fit.spm$aux)

## ------------------------------------------------------------------------
library(mgcv)
fit.gam <- gam(y ~ s(x))

# Smooth fit with `mgcv`'s `gam` function
# See ?plot.gam for plotting options
plot(fit.gam,shade=TRUE,seWithMean=TRUE,pch=19,1,cex=.55)

## -----------------------------------------------------------------------
## gam(formula,method="",select="",family=gaussian())

## ------------------------------------------------------------------------
summary(fit.gam)

##  Diagnostics for fitted gam model with `gam.check`, see `?gam.check` for details
gam.check(fit.gam)

# Residuals plots
plot(fitted(fit.gam),residuals(fit.gam))

# Different smooth fits for k=10,20,30,40
fit10.gam <- gam(y~s(x,k=10,bs="ps"), method="REML")
fit20.gam <- gam(y~s(x,k=20,bs="ps"))
fit30.gam <- gam(y~s(x,k=30,bs="ps"))
fit40.gam <- gam(y~s(x,k=40,bs="ps"))

x.seq <- seq(min(x),max(x),l=100)
plot(x,y,cex=.55,pch=15)
lines(x.seq,predict(fit10.gam,data.frame(x=x.seq)),col=2,lwd=3)
lines(x.seq,predict(fit20.gam,data.frame(x=x.seq)),col=3,lwd=3)
lines(x.seq,predict(fit30.gam,data.frame(x=x.seq)),col=4,lwd=3,lty=2)
lines(x.seq,predict(fit40.gam,data.frame(x=x.seq)),col=5,lwd=3,lty=3)
legend("bottomleft",paste(" k = ", c(10,20,30,40)),col=2:5,lwd=4)


## gamm(formula,method="",random=NULL,correlation=NULL,select="",family=gaussian())

## ------------------------------------------------------------------------
fit.gamm <- gamm(y ~s(x,bs="ps",k=20), method="REML") 

## ------------------------------------------------------------------------
summary(fit.gamm$gam)
summary(fit.gamm$lme)

library(mgcv)
# Poisson example
n = 100
x <- seq(0,1,l=n)
f <- sin(2.8*x*pi)
g <- exp(f)
z <- rpois(rep(1,n),g) # simulate a Poisson response vble with mean exp(f)

# gam fit 
fit.pois <- gam(z ~ s(x,bs="ps",m=2,k=40),family=poisson,
                method="REML")

# gamm fit (using PQL)
fit.pois2 <- gamm(z ~ s(x,bs="ps",m=2,k=40),family=poisson,
                  method="REML")

# Simulated example with Poisson data

plot(x,z,t="h")
lines(x,predict(fit.pois,type="response"),col=2,lwd=3)
lines(x,predict(fit.pois2$gam,type="response"),col=4,lwd=3,lty=2)
legend("topleft",c("gam","gamm"),col=c(2,4),lwd=3,lty=c(1,2))

## ------------------------------------------------------------------------

# Scatterplot matrix
data(airquality)
pairs(airquality)

## ------------------------------------------------------------------------
airq.lm <- lm(Ozone ~ Temp + Wind + Solar.R, data=airquality)
summary(airq.lm)

## Plots of `lm` fit*", fig.widht=8, fig.height=3------------
par(mfrow=c(1,3))
termplot(airq.lm,se=TRUE)

## Plots of `lm` residuals*", fig.widht=6,fig.height=4-------
par(mfrow=c(1,2))
plot(airq.lm,which=1:2)

## histograms of `Ozone` and `log(Ozone)`*", fig.width=6,fig.height=4----
par(mfrow=c(1,2))
hist(airquality$Ozone, main="Ozone")
hist(log(airquality$Ozone), main ="log(Ozone)")

lairq.lm <- lm(log(Ozone)~ Temp + Wind + Solar.R, data=airquality)
summary(lairq.lm)
plot(lairq.lm)

## ------------------------------------------------------------------------
library(mgcv)
airq.gam1 <- gam(log(Ozone) ~ s(Wind,bs="ps",m=2,k=10), 
                 method="REML", select=TRUE,data=airquality)
summary(airq.gam1)

##  Estimated smooth effect of `Wind` on `log(Ozone)`. Data points are the residuals
plot(airq.gam1,residuals=TRUE,scheme=1)

## Check plots by `gam.check`
gam.check(airq.gam1)

## ------------------------------------------------------------------------
resids <- residuals(airq.gam1)
resids.gam <- gam(resids~s(Wind,k=20,m=2),method="REML",
                  select=TRUE,data=airq.gam1$model)
summary(resids.gam)

##  Wind effect vs the residuals of `airq.gam1`
plot(resids.gam)

## Predicted curve and CI's
airq.pred <- data.frame(Wind=seq(min(airquality$Wind),max(airquality$Wind),
                                 length.out=200))
p <- predict(airq.gam1, newdata = airq.pred, type="response", se.fit = TRUE)

plot(airq.pred$Wind,p$fit, xlab="Wind", ylab="log(Ozone)", 
     type="l", ylim=c(0,6))
lines(airq.pred$Wind,p$fit + 1.96 * p$se.fit, lty=2)
lines(airq.pred$Wind,p$fit - 1.96 * p$se.fit, lty=2)
points(airquality$Wind,log(airquality$Ozone),cex=.55,col="grey", pch=15)


airq.gam2 <- gam(log(Ozone)~s(Wind,bs="ps",m=2,k=10)+s(Temp,bs="ps",m=2,k=10),
                 method="REML",select=TRUE,data=airquality)
summary(airq.gam2)

## Estimated smooth effect of `Wind` and `Temp` on `log(Ozone)`. Data points are the residuals

par(mfrow=c(1,2))
plot(airq.gam2,residuals=TRUE,scheme=1)

## ------------------------------------------------------------------------
anova(airq.gam1,airq.gam2)

## ------------------------------------------------------------------------
AIC(airq.gam1)
AIC(airq.gam2)

## ------------------------------------------------------------------------
sum(is.na(airquality$Solar.R))

## ------------------------------------------------------------------------
new.airquality <- na.omit(airquality)

airq.gam22=gam(log(Ozone)~s(Wind,bs="ps",m=2,k=10)+s(Temp,bs="ps",m=2,k=10),
               method="REML",select=TRUE,data=new.airquality)

airq.gam3=gam(log(Ozone)~s(Wind,bs="ps",m=2,k=10)+s(Temp,bs="ps",m=2,k=10)+s(Solar.R,bs="ps",m=2,k=20),
              method="REML",select=TRUE,data=new.airquality)

summary(airq.gam22)
summary(airq.gam3)

AIC(airq.gam22)
AIC(airq.gam3)

airq.gam4=gam(log(Ozone)~s(Wind,bs="ps",m=2,k=10)
              +s(Solar.R,bs="ps",m=2,k=20)+factor(Month)+s(Day),
              method="REML",select=TRUE,data=new.airquality)

par(mfrow=c(3,2))
plot(airq.gam4,residuals=TRUE)

##  Smooth effects of `Wind`, `Temp` and `Solar.R`
#pdf("plot1.pdf",width=12,height=8)
par(mfrow=c(2,2))
plot(airq.gam3,residuals=TRUE)
#dev.off()


##  Estimated smooth effects.
set.seed(666)
data=gamSim(eg=1,n=400,dist="normal")
fit=gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=data,method="REML",select=TRUE)
summary(fit)
par(mfrow=c(2,2))
plot(fit,residuals=TRUE)

##  Onions yield in two locations 
library(SemiPar)
data(onions)
attach(onions)
points.cols <- c("red","blue")
plot(dens,log(yield),col=points.cols[location+1],pch=16)
legend("topright",c("Purnong Landing","Virginia"),col=points.cols,pch=rep(16,2))

## ------------------------------------------------------------------------
# create a factor for location
L <- factor(location)

levels(L) <- c("Purnong Landing","Virginia")

# fit a gam with location factor

fit1 <- gam(log(yield) ~ L + s(dens,k=20,m=2,bs="ps"), 
            method="REML", select=TRUE)

summary(fit1)

# Fitted curves for each location

L.P <- rep(levels(L)[1],100)
L.V <- rep(levels(L)[2],100)
dens.g <- seq(min(dens),max(dens),l=100)
fit1.P <- predict(fit1,newdata=data.frame(L=L.P,dens=dens.g),se.fit=TRUE)
fit1.V <- predict(fit1,newdata=data.frame(L=L.V,dens=dens.g),se.fit=TRUE)

plot(dens,log(yield),col=points.cols[location+1],pch=16,cex=.55)
lines(dens.g,fit1.P$fit,col=2,lwd=2)
lines(dens.g,fit1.P$fit+1.96*fit1.P$se.fit,col=2,lwd=2,lty=2)
lines(dens.g,fit1.P$fit-1.96*fit1.P$se.fit,col=2,lwd=2,lty=2)
lines(dens.g,fit1.V$fit,col=4,lwd=2)
lines(dens.g,fit1.V$fit+1.96*fit1.V$se.fit,col=4,lwd=2,lty=2)
lines(dens.g,fit1.V$fit-1.96*fit1.V$se.fit,col=4,lwd=2,lty=2)

## ------------------------------------------------------------------------
fit2 <- gam(log(yield) ~ L + s(dens,k=20,m=2,bs="ps",by=L), 
            method="REML", select=TRUE)
summary(fit2)

## Fitted curves by location

AIC(fit1)
AIC(fit2)

fit1$sp
fit2$sp

## Fitted curves by location

# plot the smooth effects 
par(mfrow=c(2,2))
plot(fit2, se=TRUE)

# In the same plot
fit2.P <- predict(fit2,newdata=data.frame(L=L.P,dens=dens.g),se.fit=TRUE)
fit2.V <- predict(fit2,newdata=data.frame(L=L.V,dens=dens.g),se.fit=TRUE)

plot(dens,log(yield),col=points.cols[location+1],pch=16,cex=.55)
lines(dens.g,fit2.P$fit,col=2,lwd=2)
lines(dens.g,fit2.P$fit+1.96*fit1.P$se.fit,col=2,lwd=2,lty=2)
lines(dens.g,fit2.P$fit-1.96*fit1.P$se.fit,col=2,lwd=2,lty=2)
lines(dens.g,fit2.V$fit,col=4,lwd=2)
lines(dens.g,fit2.V$fit+1.96*fit2.V$se.fit,col=4,lwd=2,lty=2)
lines(dens.g,fit2.V$fit-1.96*fit2.V$se.fit,col=4,lwd=2,lty=2)

## ------------------------------------------------------------------------
SCRI <-read.table("Datasets/SCRI.txt", header=TRUE) 
names(SCRI)

## ------------------------------------------------------------------------
library(mgcv)
#Fit additive model

fit.SCRI=gam(Production~s(Row,k=10,bs="ps",m=2)+s(Column,k=7,bs="ps",m=2),data=SCRI)
summary(fit.SCRI)

#plot fitted model

plot(fit.SCRI, se=2,shade=TRUE,resid=TRUE,pages=1)
vis.gam(fit.SCRI, cex=1.2, d=2, theta = -35, phi = 30)


#Plot residuals
res=fit.SCRI$resid
library(lattice)
xyplot(res~SCRI$Row|SCRI$Column, prepanel= function(x, y) prepanel.loess(x, y), 
       xlab = "Row", ylab = "Column",
       panel = function(x, y) { 
         panel.grid(h=-1, v= 2) 
         panel.xyplot(x, y) 
         panel.loess(x,y)
       }) 
title("Residuals")

fit.SCRI2=gam(Production~te(Row,Column,k=c(10,7),bs=c("ps","ps"),m=2),method="REML",data=SCRI)
vis.gam(fit.SCRI2, cex=1.2, d=2, theta = -35, phi = 30)

## ------------------------------------------------------------------------
mortality <- read.table("Datasets/mortalidad.txt", header=TRUE)
names(mortality)

## ------------------------------------------------------------------------

fit=gam(deaths~s(age,k=20,bs="ps",m=2)+s(year,k=20,bs="ps",m=2)+offset(log(exposure)),method="REML",family="poisson",data=mortality)
plot(fit, se=2,shade=TRUE,resid=TRUE,pages=1)
vis.gam(fit,view=c("age","year"),theta = -35, phi = 30,cex.lab=0.8)

fit2=gam(deaths~te(age,year,k=c(10,10),bs=c("ps","ps"),m=2)+offset(log(exposure)),method="REML" ,family="poisson",data=mortality)
vis.gam(fit2,view=c("age","year"),theta = -35, phi = 30,cex.lab=0.8)

par(mfrow=c(1,2))
plot(fit2$fitted,deaths)
abline(0,1,col=3)
plot(sqrt(fit$fitted), residuals(fit2, type="pearson"))


fit3=gam(deaths~s(age,k=20,bs="ps",m=2)+s(year,k=20,bs="ps",m=2)+
           te(age,year,k=c(10,10),bs=c("ps","ps"),m=2)+offset(off),
         method="REML" ,family="poisson")
vis.gam(fit3,view=c("age","year"), phi=15)

## ------------------------------------------------------------------------
calif <- read.table("Datasets/cadata.dat", header=TRUE)
names(calif)

## ------------------------------------------------------------------------
linfit <- lm(log(MedianHouseValue)~.,data=calif)
print(summary(linfit))

## Actual median house values (horizontal axis) versus those predicted by the linear model (black dots), plus or minus two standard errors (grey bars). The dashed line shows where actual and predicted prices would be equal.*",fig.width=12, fig.height=8----
predictions = predict(linfit,se.fit=TRUE)
plot(calif$MedianHouseValue,exp(predictions$fit),cex=0.1,
     xlab="Actual price",ylab="Predicted")
segments(calif$MedianHouseValue,exp(predictions$fit-2*predictions$se.fit),
         calif$MedianHouseValue,exp(predictions$fit+2*predictions$se.fit),
         col="red")
abline(a=0,b=1,lty=2,col=4,lwd=2)

## ---------------------------------------------------------------------------
library(mgcv)
addfit <- gam(log(MedianHouseValue) ~ s(MedianIncome)
              + s(MedianHouseAge) + s(TotalRooms)
              + s(TotalBedrooms) + s(Population) + s(Households)
              + s(Latitude) + s(Longitude), data=calif)

## Actual versus predicted prices for the additive model

plot(addfit,scale=0,se=2,shade=TRUE,resid=TRUE,pages=1)

predictions = predict(addfit,se.fit=TRUE)
plot(calif$MedianHouseValue,exp(predictions$fit),cex=0.1,
     xlab="Actual price",ylab="Predicted")
segments(calif$MedianHouseValue,exp(predictions$fit-2*predictions$se.fit),
         calif$MedianHouseValue,exp(predictions$fit+2*predictions$se.fit),
         col="grey")
abline(a=0,b=1,lty=2)

## --------------------------------------------------------------------------
addfit2 <- gam(log(MedianHouseValue) ~ s(MedianIncome) + s(MedianHouseAge)
               + s(TotalRooms) +s(TotalBedrooms) + s(Population) + s(Households)
               + te(Longitude,Latitude,bs="ps"), data=calif)

## Partial response functions and partial residuals for `addfit2`
plot(addfit2,scale=0,se=2,shade=TRUE,resid=TRUE,pages=1)

## Perspective and surface plot of the spatial component of `addfit2`
par(mfrow=c(1,2))
plot(addfit2,select=7,phi=60,pers=TRUE)
plot(addfit2,select=7,scheme=2)

## Surface plot with `mba.surf` 
pred2 <- predict(addfit2,type="terms") 

library(MBA)
library(fields)
image.plot(mba.surf(cbind(calif$Longitude,calif$Latitude,pred2[,7]),100,100,
                    extend=FALSE)$xyz.est, main="smooth spatial effect")
contour(mba.surf(cbind(calif$Longitude,calif$Latitude,pred2[,7]),100,100,
                 extend=FALSE)$xyz.est,add=TRUE)
points(calif$Longitude,calif$Latitude,cex=.1,col=1)

## Mackerel eggs abundance
library(gamair)
library(sm)
library(mgcv)
library(fields)
library(maps)

data(mackerel)
data(mackp)
attach(mackerel)
Latitude=mack.lat
Longitude=-mack.long

# plot the egg densities against location
plot(Longitude,Latitude,cex=Density/150,col=2,asp=.85)
map("world",add=TRUE,fill=TRUE,col="lightgrey")

## ------------------------------------------------------------------------

m0<-gam(log(Density)~s(Longitude,Latitude,k=30))
# vis.gam(m0,plot.type="contour",color="terrain")
# map("world",add=TRUE,fill=TRUE,col="grey")
summary(m0)

## Smooth spatial trend
# plot
zz = array(NA,57*57)
zz[mackp$area.index]<- predict(m0,data.frame(Longitude=mackp$lon,Latitude=mackp$lat))

lon<-seq(-15,-1,1/4)
lat<-seq(44,58,1/4)
image.plot(lon,lat,matrix(zz,57,57),main="smooth bivariaty log(Density)",col=terrain.colors(256))
contour(lon,lat,matrix(zz,57,57),add=TRUE)
map("world",add=TRUE,fill=TRUE,col="grey")

## Additive model `gam` fit
ldepth <- log(mack.depth) # logarithm scale
m1<-gam(log(Density)~s(Longitude,Latitude)+s(Temperature)+s(Salinity)+s(ldepth))
par(mfrow=c(2,2))
plot(m1,scheme=2)
summary(m1)

## `gam.check` results
gam.check(m1)

## Smooth term of `m2` model
m2<-gam(log(Density)~s(Longitude,Latitude)+s(Temperature)+s(ldepth))
par(mfrow=c(2,2))
plot(m2,scheme=2,1)

## ------------------------------------------------------------------------
library(sm)
data(smacker)
attach(smacker)
Presence <- Density
Presence[Presence>0]<-1
Longitude <- -smack.long
Latitude <- smack.lat
Position <- cbind(Longitude,Latitude)
ldepth <- log(smack.depth)

##  Sampling positions with presence and absence of eggs
plot(Position,col=NULL,xlim=c(-10,-1),ylim=c(43,48))
map("world",add=TRUE,fill=TRUE,col="grey")
points(Position[Presence==1,],pch=1,cex=.5,col=4)
points(Position[Presence==0,],pch=16,cex=.5,col=2)
legend("topleft",c("Presence ", "Absence"), col=c(4,2),pch=c(1,16),cex=.85)

## logistic regression estimate of the relationship between presence and log of depth
library(mgcv)
logit.gam <- gam(Presence ~ s(ldepth),family=binomial)
plot(logit.gam)

## ------------------------------------------------------------------------
logit1 <- gam(Presence~s(Longitude,Latitude)+
                s(ldepth)+s(Temperature),family=binomial)
summary(logit1)
logit2 <- gam(Presence~s(Longitude,Latitude)+
                s(ldepth),family=binomial)
summary(logit2)
logit3 <- gam(Presence~s(Longitude,Latitude)+
                s(Temperature),family=binomial)
summary(logit3)

