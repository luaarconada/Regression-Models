############################################
## ---- healthtable, out.width = '80%', fig.pos = 'h!', cache = TRUE-------
health <- read.table(file = "./Datasets/health.txt", header = TRUE)
knitr::kable(
  head(health, 10),
  booktabs = TRUE,
  longtable = TRUE,
  caption = 'First 10 rows of the `heath` dataset.'
)
############################################


############################################
#Start by defining the categorical variables as factors
health$drink=factor(health$drink)
health$sex=factor(health$sex)
#Summary of the data
library(faraway)
summary(health[,c("g02","sex","age","drink")])
############################################


############################################
## #plot response versus age
## par(mfrow=c(1,2))
## plot(health$age, health$g02, xlab="Age", ylab="Health")
## plot(jitter(g02,0.1) ~ jitter(age), health, xlab="Age", ylab="Health", pch=".")
############################################

############################################
# Logistic Regression using sex and drink
lmod <- glm(g02 ~ sex + drink, family = binomial, health)
summary(lmod)
############################################

############################################
# Logistic Regression using sex
lmod.sex=glm(g02 ~ sex, family = binomial, health)
#Calculate the odds ratio
exp(coef(lmod.sex))
############################################


############################################
#Calculate the probability of self-preceived good health for females.
p1=predict(lmod.sex,newdata=data.frame(sex="2"),type="response")
#Calculate the probability of self-preceived good health for males.  
p2=predict(lmod.sex,newdata=data.frame(sex="1"),type="response")

##Relative Risk
p1/p2
# Odds ratio
(p1/(1-p1))/(p2/(1-p2))
############################################


############################################
# Logistic Regression using drink
lmod.drink=glm(g02 ~ drink, family = binomial, health)
#Calculate the odds ratio
exp(coef(lmod.drink))
############################################


############################################
# Logistic Regression using imc
lmod.imc=glm(g02 ~ imc, family = binomial, health)
summary(lmod.imc)
#Calculate the odds ratio
exp(coef(lmod.imc))
############################################


############################################
# Calculate logits
logit34 <- predict(lmod.imc,data.frame(imc=34))
logit35 <- predict(lmod.imc,data.frame(imc=35))
# Take the difference
logit35-logit34
############################################


############################################
# Change in odds with an increse of 1 unit of imc
exp(logit35-logit34)
############################################


############################################
# Change in odds with an increse of 10 units of imc
c=10
exp(logit35-logit34)^c 
# is equivalent to exp(c*logit35-c*logit34)
############################################


############################################
# Obtain fitted probabilities
fitted.imc <- predict(lmod.imc,type="response") 
# Plot fitted probabilities
plot(health$imc[order(health$imc)],fitted.imc[order(health$imc)],t='b',
     xlab="imc", ylab="Prob",ylim=c(0,1), main="response") # order by imc
abline(h=c(0,1),col="grey",lty=2)
############################################


############################################
# Obtain fitted probabilities
lmod.sexage=glm(g02 ~ sex+age, family = binomial, health)
summary(lmod.sexage)
############################################


############################################
par(mfrow=c(1,2))
# Obtain fitted linear predictor
fittedlp=predict(lmod.sexage)
#Plot linear predictor for males and females
plot(health$age,fittedlp,type="n",main="No interaction (logit)",xlab="Age",ylab="Linear predictor")
#Get ages and linear predictor for males
age1=health$age[health$sex==1]
lp1=fittedlp[health$sex==1]
#Order according to age
o=order(age1)
lines(age1[o],lp1[o],col=2,t='l')
#Get ages and linear predcitor for females
age2=health$age[health$sex==2]
lp2=fittedlp[health$sex==2]
#Order according to age
o=order(age2)
lines(age2[o],lp2[o],col=4,t='l')
legend(15,1, col=c(2,4),c("male","female"),lty=1,bty="n",cex=0.8)

#Repeat the plot for fitted probabilities
# Obtain fitted linear probabilities
fittedp=predict(lmod.sexage, type="response")
#Plot probailities for males and females
plot(health$age,fittedp,type="n",main="No interaction (prob.)",xlab="Age",ylab="Probability")
#Get ages and probabilities for males
age2=health$age[health$sex==1]
p1=fittedp[health$sex==1]
#Order according to age
o=order(age1)
lines(age1[o],p1[o],col=2,t='l')
#Get ages and probabilities for females
age2=health$age[health$sex==2]
p2=fittedp[health$sex==2]
#Order according to age
o=order(age2)
lines(age2[o],p2[o],col=4,t='l')
legend(15,0.65, col=c(2,4),c("male","female"), lty=1, bty="n",cex=0.8)
############################################


############################################
# Obtain fitted probabilities
lmod.sexage2=glm(g02 ~ sex+age+sex:age, family = binomial, health)
summary(lmod.sexage2)
############################################

############################################
# Obtain coefficients
coef(lmod.sexage2)
############################################

############################################
# Wald test
summary(lmod.sexage2)
# Confidence intervals for parameters
confint(lmod.sexage2)
#Confidence interla for the odds ratio
exp(confint(lmod.sexage2))
############################################


############################################
#Get fitted values of logit and their standard errord
fitted=predict(lmod.sexage, se.fit=TRUE)
names(fitted)
#Calculate lower limit of C.I. for probability by transforming lower limit of C.I. of logit
L.inf=with(fitted,exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)))
L.sup=with(fitted,exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)))
with(fitted,plot(health$age[health$sex==1],(exp(fit)/(1+exp(fit)))[health$sex==1],
                 ylim=c(0.1,1),ylab="Probability"),xlab="Age")
points(health$age[health$sex==1],L.inf[health$sex==1],col=2)
points(health$age[health$sex==1],L.sup[health$sex==1],col=2)
legend(20,0.6, c("Probability","C.I."),col=c(1,2),pch=c(1,1),bty="n")
############################################

############################################
# Logistic Regression using sex and age and the interaction
summary(lmod.sexage2)
############################################


############################################
# Calculate the change in deviance
7177.9-6684.4
#Calculate the change in degrees of freedom
7356-7353
# Calculate the p-value
1-pchisq(493.5,3)
############################################

############################################
#use function anova, now we need to specify the distribution of the test
anova(lmod.sexage2,lmod.sexage, test="Chisq")
############################################


############################################
#plot of effects in the presence of interaction
library(sjPlot)
library(ggplot2)
plot_model(lmod.sexage2, type = "pred", terms = c("age", "sex"))
############################################

############################################
#We can get AIC directly form the glm model
summary(lmod.sexage)$aic
summary(lmod.sexage2)$aic
#We can calculate BIC using the deviance
2*summary(lmod.sexage)$deviance+log(nrow(health))*length(coef(lmod.sexage))
2*summary(lmod.sexage2)$deviance+log(nrow(health))*length(coef(lmod.sexage2))
############################################

############################################
#Notice that I have included the link, in this case it is not necessary since it is the default link
lmod2 <- glm(formula = g02 ~ educa+age+sex*imc, family = binomial(link = logit),data=health)
library(ResourceSelection)
hoslem.test(health$g02, predict(lmod2,type="response"))
############################################

############################################
library(Epi)
#The ROC function
ROC(form=g02~educa+age+sex*imc, data=health,plot="ROC",lwd=3,cex=1.5)
############################################

############################################
library(boot)
par(mfrow=c(2,2)) 
glm.diag.plots(lmod2)
############################################


############################################
orings=read.table(file = "./Datasets/challenger.txt", header = TRUE)
names(orings)
plot(orings$Temp,orings$Damaged, xlab="temperature",ylab="number of incidents", pch=16)
############################################


############################################
orings.mod=glm(Damaged/O.rings~Temp, family=binomial, weights=O.rings,data=orings)
summary(orings.mod)
############################################
