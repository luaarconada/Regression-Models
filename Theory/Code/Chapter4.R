
####################################
simu=read.table(file = "./Datasets/Transform.txt", header = TRUE)

#Model with x1
m1=lm(y~x1,data=simu)

#plot data and residuals
par(mfrow=c(1,3))
plot(simu$x1, simu$y, xlab="x1", ylab="y")
plot(m1,which=c(1,2))
####################################



####################################
#Model with transformed y
m2=lm(y~I(x1^0.5),data=simu)

#plot data and residuals
par(mfrow=c(1,3))
plot((simu$x1)^0.5, simu$y, xlab="x1", ylab="y")
plot(m2,which=c(1,2))
####################################



####################################
#Model y1 ~x1
simu2=read.table(file = "./Datasets/Transform2.txt", header = TRUE)
m3=lm(y1~x1,data=simu2)

#plot data and residuals
par(mfrow=c(1,3))
plot(simu2$x1, simu2$y, xlab="x1", ylab="y")
plot(m3,which=c(1,2))
####################################



####################################
library(MASS)
boxcox(m3)
####################################



####################################
boxcox(m3, lambda=seq(-.3,.3,0.01))
boxcox(m3, lambda=seq(-.3,.3,0.01),plotit=FALSE)
####################################



####################################
#Model y1 ~x1 with ln transformation
m4=lm(log(y1)~x1,data=simu2)

#plot data and residuals
par(mfrow=c(1,3))
plot(simu2$x1, log(simu2$y1), xlab="x1", ylab="y")
plot(m4,which=c(1,2))
####################################



####################################
apartments=read.table("./Datasets/apartments.txt")
#Check that the variable is define as numeric
summary(apartments$elevator)

#Define the variable as factor and check it
apartments$elevator=factor(apartments$elevator)
summary(apartments$elevator)
####################################



####################################
#Fit a model with the variable `elevator`
flats1=lm(totalprice~elevator, data=apartments)
summary(flats1)
####################################



####################################
#Check that the variable is defined as a factor
summary(apartments$heating)
####################################



####################################
#Fit a model with `heating` as predictor
flats2=lm(totalprice~heating,data=apartments)
summary(flats2)
####################################



####################################
flats2.2=lm(totalprice~heating-1,data=apartments)
summary(flats2.2)
####################################


####################################
#Calculate means for each group
library(dplyr)
summarise(group_by(apartments, heating), mn = mean(totalprice))
####################################



####################################
library(DescTools)
#reorder the coefficients and fit again the model
heating2=  reorder(apartments$heating, new.order=c(4,1,2,3))
summary(lm(totalprice ~ heating2, data = apartments))$coef
####################################



####################################
#Check how the dummy variables are created
contrasts(apartments$heating)
contrasts(heating2)
####################################



####################################
#Fit the model with both variables and their interaction
modTotal=lm(totalprice~area+elevator+area:elevator,data=apartments)
summary(modTotal)
####################################



####################################
#Fit the model without the variable `elevator`
modarea=lm(totalprice~area,data=apartments)
#Use anova table to compare models
anova(modarea,modTotal)
####################################



####################################
#In this case since the anova table, when the argument is a single model, it adds
#each term sequentially, we only need to check the last line of the table
anova(modTotal)
####################################



####################################
#fit a model without `elevator`variable, but with the interaction
modInter=lm(totalprice~area+area:elevator,data=apartments)
anova(modInter,modTotal)
####################################



####################################
#Final model
summary(modInter)
####################################



####################################
bodyfat <- read.table(file = "./Datasets/bodyfat.txt", header=TRUE)
####################################



####################################
library(glmnet)
#Calculate the regression matrix without the intercept
X=model.matrix(hwfat ~.-1,data=bodyfat)
#the function `glmnet` will chose a range of values of the ridge parameter
#We need to specify alpha=0
fit.ridge=glmnet(X,bodyfat$hwfat,alpha=0)
#Plot estimated coefficients for different values of the ridge parameter
plot(fit.ridge,xvar="lambda",label=TRUE)
legend(6,-0.2, c("age", "ht", "wt", "abs","triceps","subscap"),lty=1:6,col=1:6)
####################################



####################################
#Calculate optimal ridge parameter via cross-validation
#Plot mean squared error versus ridge parameter (in the log scale)
cv.out = cv.glmnet(X, bodyfat$hwfat, alpha = 0)  #alpha=0 means Ridge Regression
plot(cv.out)
####################################



####################################
## ----glmnet1, collapse = TRUE, cache = TRUE,fig.height=3-----------------
#Getting optimal value for lambda
opt_lambda <- cv.out$lambda.min
opt_lambda

#Fitting the model for that value of the ridge parameter
predict(fit.ridge, type = "coefficients", s = opt_lambda)
####################################


