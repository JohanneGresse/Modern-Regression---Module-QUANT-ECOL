##correction homework mixed effect model
LME.2(update (LME.1, ramdom= ~Soil|Pot)
anova(LME.1, LME.2
fixef(LME.1


#EXAM TEEEST
#Poisson count data spiders
#Import data
spider <- read.delim("clipboard", as.is=F)
summary(spider)



hist(spider$Auloalbi)
plot(log(Auloalbi)~WaterCon, data=spider)
plot(Auloalbi~ShrubCov, data=spider) 



GLM.0 <- glm(Auloalbi ~ WaterCon + ShrubCov, data=spider, family=quasipoisson)
summary(GLM.0)

GLM.0 <- glm(Auloalbi ~ ShrubCov + WaterCon, data=spider, family=poisson)
summary(GLM.0)


add1<- (GLM.0, ~.+ShrubCov:WaterCon, test="Chisq")

GLM.1 <- glm(Auloalbi ~ WaterCon + ShrubCov + WaterCon:ShrubCov, data=spider, family=quasipoisson)
summary(GLM.1)
anova(GLM.1, test="Chisq")


#survival patients heart transplant

heart <- read.delim("clipboard", as.is=F)
summary(heart)


COX.0 <- coxph(Surv(days, status)~+1, data=heart)

installed.packages("survival")
library(survival)
library(MASS)

step.1 <- stepAIC(COX.0, ~age+T5)

step.1$anova

summary(step.1)

#drop1(cox.1m test="chisq) Test significant effect of both perdictors without order


SF.1 <- survfit(Surv(days,status)~age, data=heart)
plot(SF.1)



COX <- survfit(step.1, data=heart)

SF.1<-survfit(step.1)
plot(SF.1)


plot(step.1, col=c("blue","red"), conf.int=TRUE)

###CORRECTION PETR SMILAUER###

#in the word doc show import and summary of the data

#TASK 1
#coef(GLM.2) to get estimate and direction of the effect
#check for overdispersion (no need for model diagonstics)
#try to als oremove the effect of ShrubCov in the model
#try also with interaction WaterCon:ShrubCov


#TASK2
#summary(cox.1) you take exp(coef) and log it to have 3 -> patient hazard rate increase by 3% with every year of age
#since only two perdictors do not need for stepaic, can use drop(GLM.2, test="F") or Chisq
#coef(GLM.2)



