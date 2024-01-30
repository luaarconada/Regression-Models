####################################
bodyfat <- read.table(file = "./Datasets/bodyfat.txt", header=TRUE)
####################################



####################################
# Model with all predictors
modall <- lm(hwfat ~., data = bodyfat)
summary(modall)$coefficients
####################################



####################################
# Model without ht
mod1=update(modall,.~.-ht)
summary(mod1)$coefficients
####################################



####################################
# Model without subscap
mod2=update(mod1,.~.-subscap)
summary(mod2)$coefficients
####################################



####################################
# Model without wt
mod3=update(mod2,.~.-wt)
summary(mod3)$coefficients
####################################



####################################
# Model without ht
drop1(modall,test="F")
####################################



####################################
#Start with a model with only the intercept
mod0=lm(hwfat~1, data=bodyfat)
#The add functions includes each predictor 
add1(mod0,scope=(~.+age+ht+wt+abs+triceps+subscap),test="F",data=bodyfat)
####################################



####################################
#Include `abs` in the model
mod1=lm(hwfat~abs, data=bodyfat)
#Use the add() function with the rest of the predcitors
add1(mod1,scope=(~.+age+ht+wt+triceps+subscap),test="F",data=bodyfat)
####################################



####################################
#Include `triceps`
mod2=lm(hwfat~abs+triceps, data=bodyfat)

add1(mod2,scope=(~.+age+ht+wt+subscap),test="F",data=bodyfat)
####################################


####################################
#Include `age`
mod3=lm(hwfat~abs+triceps+age, data=bodyfat)

add1(mod3,scope=(~.+ht+wt+subscap),test="F",data=bodyfat)
####################################



####################################
#load package `leaps`
library(leaps)
#Use the function `regsubset`
#The input is the matrix of predictors
r2a=regsubsets(bodyfat[,1:6], bodyfat[,7])
summary(r2a)
summary(r2a)$adjr2
####################################



####################################
#Calculate influence measures
inf=influence.measures(mod3)
#Show Cook's distance and hii for the 10 first observations
inf$infmat[1:10,7:8]
#Check which observations could be influencial
inf$is.inf[1:10,7:8]
####################################



####################################
## par(mfrow=c(2,2))
## #Diagnostics plots
## plot(mod3)
####################################

