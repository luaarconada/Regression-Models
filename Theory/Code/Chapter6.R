## Linear regression methods (constant variance, normal errors) are not

## **Task 9**

## ---- biotable, out.width = '80%', fig.pos = 'h!', cache = TRUE----------
species <- read.table(file = "./Datasets/species.txt", header = TRUE)
knitr::kable(
  head(species, 10),
  booktabs = TRUE,
  longtable = TRUE,
  caption = 'First 10 rows of the `Species` dataset.')

## ---- pois1, cache = TRUE,fig.height=3,fig.align="center"----------------
#define pH as factor
species$pH=factor(species$pH)
#plot the result of fitting a linear model
library(ggplot2)
ggplot(species, aes(x = Biomass, y = Species, colour = pH)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## ---- pois2, cache = TRUE------------------------------------------------
#Fit and test Biomass
m0=glm(Species~Biomass,family=poisson,data=species)
summary(m0)
anova(m0,test="Chisq")
#Fit and test Ph
m1=glm(Species~pH,family=poisson,data=species)
summary(m1)

## ---- pois3, cache = TRUE------------------------------------------------
#Fit and test Biomass
m2 <- glm(Species~Biomass+pH,family=poisson,data=species)
m3 <- glm(Species~Biomass*pH,family=poisson,data=species)
anova(m2,m3,test="Chisq")

## ---- poi4, cache = TRUE,fig.height=4,fig.align="center"-----------------
yv2<-predict(m2,type="response") 
yvs2<-split(yv2,species$pH)
yv3<-predict(m3,type="response") 
yvs3<-split(yv3,species$pH)
bvs<-split(species$Biomass,species$pH) 
svs<-split(species$Species,species$pH)
plot(species$Biomass,species$Species, type="n",xlab="Biomass",ylab="Species")
points(bvs[[1]],svs[[1]],col=2, pch=16)
points(bvs[[2]],svs[[2]],col=3, pch=16)
points(bvs[[3]],svs[[3]],col=4, pch=16)
for(i in 1:3){
  o=order(bvs[[i]])
  lines((bvs[[i]])[o],(yvs2[[i]])[o],lty=2,col=i+1)
  lines((bvs[[i]])[o],(yvs3[[i]])[o],col=i+1)
}
legend(7,45, legend=c("without interaction", "with interaction"),lty=c(2,1),bty="n",cex=0.8)

## ---- pois4, cache = TRUE,fig.height=3,fig.align="center"----------------
#Fit and test Biomass
par(mfrow=c(1,2))
#Plot residuals versus fitted values
plot(predict(m3),residuals(m3,type="pearson"))
#Qauntile plot
qqnorm(residuals(m3,type="pearson"))

## ---- pois5, cache = TRUE------------------------------------------------
#Pearson's chi-squared statistic
pchisq(m3$deviance,m3$df.residual,lower.tail=FALSE)

## **Task 10**

## ---- biotable2, out.width = '80%', fig.pos = 'h!', cache = TRUE---------
crime <- read.table(file = "./Datasets/Campus_Crime.txt", header = TRUE)
knitr::kable(
  head(species, 10),
  booktabs = TRUE,
  longtable = TRUE,
  caption = 'First 10 rows of the `Campus Crime` dataset.')

## ---- nviolent, fig.align="center",out.width="60%", fig.cap='Histogram of number of violent crimes by institution',echo=FALSE, warning=FALSE, message=FALSE----

#Histogram of counts
library(ggplot2)
ggplot(crime, aes(x=Violent)) + 
  geom_histogram(bins = 15) +
  xlab("Number of violent crimes")

## ---- boxtyperegion, fig.align="center",out.width="60%", fig.cap='.', warning=FALSE, message=FALSE----
#boxplot per type and region
#This needs a caption
ggplot(crime, aes(x = Region, y = 1000*Violent/Enrollment, fill = Type)) +
  geom_boxplot() +
  ylab("Violent crimes per 1000 students")

## ---- crime1, comment=NA-------------------------------------------------
## Modeling
modeltr = glm(Violent~Type+Region,family=poisson, offset=log(Enrollment),data=crime)
summary(modeltr)

## What would be the meaning of the coefficient for Type?

## ---- 4verb12, comment=NA------------------------------------------------
library(multcomp)
summary(glht(modeltr, mcp(Region="Tukey")))

## ---- 4verb13, comment=NA------------------------------------------------
#Model with interaction
modeli = glm(Violent~Type+Region+Type:Region,family=poisson, offset=log(Enrollment),data=crime)
summary(modeli)

## ---- 4verb14, comment=NA------------------------------------------------
#Chisq test
anova(modeltr,modeli,test="Chisq") 

## **Task 11**

## ---- 4verb15, comment=NA------------------------------------------------
#Pearson's chi-squared statistic
pchisq(modeli$deviance,modeli$df.residual,lower.tail=FALSE)

## 
## ---- 4verb16, comment=NA------------------------------------------------
#Quasipoisson Model
modeliq = glm(Violent~Type+Region+Type:Region,family=quasipoisson, offset=log(Enrollment),data=crime)
summary(modeliq)

## ---- 4verb17, comment=NA------------------------------------------------
#Negative Binomial Model
#Now the offset is passed as weights
modelinb = glm.nb(Violent~Type+Region+Type:Region, 
                  weights=offset(log(Enrollment)),link=log,data=crime)
summary(modelinb)

## **Task 13**

## **Task 14**

## ---- biotable3, out.width = '80%', fig.pos = 'h!', cache = TRUE---------
library(pscl)
knitr::kable(
  head(bioChemists, 10),
  booktabs = TRUE,
  longtable = TRUE,
  caption = 'First 10 rows of the `bioChemists` dataset.')

## ---- hist2, collapse = TRUE, cache = TRUE, fig.height=3, fig.align="center"----
#Histogram of counts
hist(bioChemists$art,breaks=0:max(bioChemists$art))

## ---- 4verb18, comment=NA,cache=TRUE-------------------------------------
# Poisson Model with Predictors
pois.m1=glm(art~.,family=poisson, data=bioChemists)
summary(pois.m1)

## ---- 4verb19, comment=NA------------------------------------------------
# Goodness of fit test
gof.ts = pois.m1$deviance
gof.pvalue = 1 - pchisq(gof.ts, pois.m1$df.residual)
gof.pvalue

## ---- 4verb20, comment=NA------------------------------------------------
# ZIP Model with Predictors
library(pscl)
pois.m2=zeroinfl(art~., data=bioChemists)
summary(pois.m2)

## ---- 4verb21, comment=NA------------------------------------------------
# ZIP Model with selected Predictors
pois.m3=zeroinfl(art~fem+kid5+ment |ment, data=bioChemists)
summary(pois.m3)

## ---- 4verb22, comment=NA------------------------------------------------
# likelihood ratio test
lrt <- 2*(pois.m2$loglik-pois.m3$loglik)
df=pois.m3$df.residual-pois.m2$df.residual
1-pchisq(lrt,df)

## ---- 4verb23, comment=NA------------------------------------------------
# parameter interpretation
exp(coef(pois.m3))

## ---- 4verb24, comment=NA------------------------------------------------
# prediction of new observations
newman <- data.frame(fem="Men",mar="Single",kid5=0,ment=6)
predict(pois.m3, newdata=newman, type="prob")

## ---- 4verb25, comment=NA------------------------------------------------
# prediction from the zero part of the model
predict(pois.m3, newdata=newman, type="zero")

## **Task 15**
