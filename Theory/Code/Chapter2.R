####################################
index=read.table("./Datasets/index.txt",header=TRUE)
plot(index$PovPct,index$Brth15to17,pch=16,col=4,xlab="Poverty Index",ylab="Birth Rate")
####################################



####################################
plot(index$PovPct,index$Brth15to17,pch=16,col=4,xlab="Poverty Index",ylab="Birth Rate")
ols=lm(index$Brth15to17~index$PovPct)
abline(ols)
segments(index$PovPct,index$Brth15to17,index$PovPct,predict(ols))
####################################



####################################
index=read.table("./Datasets/index.txt",header=TRUE)
plot(index$PovPct,index$Brth15to17,pch=16,col=4,xlab="Poverty Index",ylab="Birth Rate")
abline(3.5,1.5)
abline(4.5,1.1)
abline(4,2)
####################################



#################################### 
## # Read data
## bodyfat <- read.table(file = "./Datasets/bodyfat.txt", header = TRUE)

## ---- wintab-2, warning=FALSE, collapse = TRUE, cache = TRUE, fig.asp = 1, out.width = '90%', fig.pos = 'h!'----
# Numerical 
summary(bodyfat)

# Graphical - pairwise relations

pairs(bodyfat)
####################################



####################################
# hwfat ~ wt
modwt <- lm(hwfat ~ wt, data = bodyfat)

# Summary of the model
summary(modwt)
####################################



####################################
# hwfat ~ wt
modall <- lm(hwfat ~., data = bodyfat)

# Summary of the model
summary(modall)
####################################



####################################
# Model coefficients
modall
####################################



####################################
age2=bodyfat$age+1
#create the data.frame for prediction
new=data.frame(age=age2,ht=bodyfat$ht,wt=bodyfat$wt, abs=bodyfat$abs,
               triceps=bodyfat$triceps, subscap=bodyfat$subscap)
#get new fitted values
new.fit=predict(modall,newdata=new)

summary(new.fit-modall$fitted)
####################################



####################################
confint(modall,level=0.95)
####################################



####################################
summary(modall)
####################################



####################################
summary(modwt)
####################################



####################################
round(cor(model.matrix(modall)[,-1]),2)
####################################



####################################
summary(modall)
####################################



####################################
#ANOVA Table for model `modall`
anova(modall)
####################################



####################################
#Model with all variables in different order
modall2=lm(hwfat~triceps+abs+subscap+wt+ht+age, data=bodyfat)
#results from the both methods
summary(modall2)
anova(modall2)
####################################



####################################
#Anova table to test whether  `age` should be in the model
#The input is the model without and with `age`
anova(update(modall,.~.-age), modall2)
####################################



####################################
# R^2 for model with all variables except subscap
summary(update(modall, .~.-subscap))$r.squared

# R^2 for model with all variables 
summary(modall)$r.squared

# Adjusted R^2 for model with all variables except subscap
summary(update(modall, .~.-subscap))$adj.r.squared

# Adjusted R^2 for model with all variables 
summary(modall)$adj.r.squared
####################################



####################################
#Generate data with large variabilty
x <- 1:20                        
set.seed(2)                      
y <- 2 + 0.5*x + rnorm(20,0,4) 
#Fit the model
mod <- lm(y~x)                   
summary(mod)$adj.r.squared 
plot(x,y)
abline(mod)
####################################



####################################
set.seed(1)
 # non-linear data generation
x <- rexp(50,rate=0.005)                     
y <- (x-1)^2 * runif(50, min=0.8, max=1.2)  
mod=lm(y ~ x)
#still a good $r2$
summary(mod)$adj.r.squared
plot(x,y)
abline(mod)
####################################

