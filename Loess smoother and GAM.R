oz <- read.delim("clipboard",as.is = TRUE)
library(lattice)
splom(~oz)
with(oz, scatter.smooth(temperature, ozone, span = 2/3, degree=2))
par(mfrow=c(2,2))
with(oz, scatter.smooth(temperature, ozone, span = 1, degree=2))
with(oz, scatter.smooth(temperature, ozone, span = 1/2, degree=2))
with(oz, scatter.smooth(temperature, ozone, span = 1, degree=1))
with(oz, scatter.smooth(temperature, ozone, span = 1/2, degree=1))

#2 parameters to vhange span to fit the regressions. choice of choosing degree 1 or 2 based on looking on data with scatterplot and 
#having expectations how comlex fitting curve should be
#Here we can compare what plot smooth the best the function.

library(locfit)
install.packages("locfit")

LF.1 <- locfit (ozone~lp(radiation, nn=2/3, deg=1), data=oz)

#non paramteric model so no regression coefficiens
summary(LF.1)
par(mfrow=c(1,1))
plot(LF.1)
plot(LF.1, get.data=TRUE, band="global")

LF.ap <- aicplot(ozone ~ radiation, data=oz, deg=1, alpha=seq(0.1, 2, by=0.1))

plot(LF.ap, type="l")
names(LF.ap)

LF.2 <- locfit(ozone ~ lp(radiation, nn=0.8, deg=1), data=oz)
plot(LF.2 ,get.data = TRUE, band="global")
#predictors no longer combine like in glm so it would be best to more advanced form of non parametric model

#predictors are not additive in loess model so if we want to get info about them we need to run GAM


#GAM
loyn2 <- read.delim("clipboard")
summary(loyn2)

LM.1 <- lm(ABUND ~ AREA + GRAZE, data=loyn2)
library(effects)
plot(allEffects(LM.1, resid=T))

#orange plots translate the values I observed into contribution

#additive predictors instead of linear predictors
#Y = b0 + b1A+b2G
#becomes  g(EY)=b0+S1A+S2G

library(mgcv)
GAM.1 <- gam(ABUND ~ s(AREA)+s(GRAZE), data=loyn2)
unique(loyn2$GRAZE)

GAM.1 <- gam(ABUND ~ s(AREA)+s(GRAZE, k=4), data=loyn2)
summary(GAM.1)

par(mfrow=c(1,2))
plot(GAM.1)

plot(GAM.1, seWithMean= TRUE, residuals=T, pch=16, shade=TRUE)

GAM.2 <- update(GAM.1, .~. AREA + s(GRAZE, k=4))
sumary(GAM.2)


GAM.3 <- gam(ABUND~s(AREA)+s(GRAZE, k=4)+s(DIST)+s(LDIST)+s(ALT)+s(AGE), data=loyn2, select=TRUE)
summary(GAM.3)


#it excluded 3 variables from the model
#How to tets for interaction between graze intenisty and Area 

GAM.4 <- gam(ABUND ~ ti(AREA)+ti(GRAZE), data=loyn2)
GAM.5 <- update(GAM.4, .~. + ti(AREA, GRAZE))

#which gam is better

anova(GAM.4, GAM.5, test="F")
#GAM.4 is better 

vis.gam(GAM.4, type="response", plot.type = "contour")
#2 preidctors one is Area one is grazing.
vis.gam(GAM.5, type="response", plot.type = "contour")


pol <- read.delim("clipboard")
GAM.Fag <- gam(cbind(Fagus, Total-Fagus)~s(DegDays)+s(AnnualP), data=pol, family=binomial)
anova(GAM.Fag)

names(GAM.Fag)

with(GAM.Fag, (deviance/df.residual))
#massive overdispersion

GAM.Fag2 <- update(GAM.Fag, family=quasibinomial)
anova(GAM.Fag2)

plot(GAM.Fag2, shade=T)

GAM.Fag3 <- update(GAM.Fag2, .~poly(DegDays, 2)+AnnualP)
anova(GAM.Fag2, GAM.Fag3, test="F")
plot(GAM.Fag3)
par(mfrow=c(1,2))
plot(GAM.Fag3, shade=T , all.terms=TRUE)
