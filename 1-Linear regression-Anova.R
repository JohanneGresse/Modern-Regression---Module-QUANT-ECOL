#1-AREA-----------------------------------------------------

loyn <- read.delim("clipboard", as.is= FALSE)
View(loyn)

summary(loyn)

library(lattice)
splom(loyn)

summary(lm(ABUND ~ ., data=loyn)) #DO NOT DO THIS

loyn2 <- transform(loyn, AREA=log(AREA), DIST=log(DIST), LDIST=log(LDIST)) #log of predictors
loyn2 <- transform(loyn2, AGE=log(1984-YR.ISOL)) #we care about how many years of isolation not the number of the year itself. Biologically meaningful

names(loyn2)
loyn2 <- loyn2[,-3]  #take everything inside loyn2 except 3 parameter which is YR.ISOL when we use names(loyn2)

splom(loyn2)

LM.1 <- lm(ABUND~AREA, data=loyn2)
summary(LM.1)

plot(loyn2$ABUND~loyn2$AREA)

predict(LM.1, newdata=list(AREA=log(1)))  #from an increase of 1 hectare what abundance is predicted

predict(LM.1, newdata=list(AREA=log(1)), interval="conf") 

log(2)*4.2467  #increase of area double
log(1.1)*4.2467  #check impact of abundance by a change of 10percent in area

par(mfrow=c(2,2))
plot(LM.1)

loyn2 <- loyn2[c(-55,-56),] #remove two last rows of dataset because they impact the most (leverage plot)


LM.1 <- update(LM.1, data=loyn2)  #use present state of the data
summary(LM.1)
plot(LM.1)

par(mfrow=c(1,1))
plot(ABUND~AREA, data=loyn2)
abline(LM.1, lwd=2, col="blue")
abline(h=mean(loyn$ABUND, col="red"))

anova(LM.1) #reject null hypothesis of no effect of area on abundance

names(loyn2)
add1(LM.1, .~.+DIST+LDIST+GRAZE+ALT+AGE, test= "F")

LM.2 <- update(LM.1, .~.+GRAZE)

add1(LM.2, .~.+DIST+LDIST+GRAZE+ALT+AGE, test= "F")
anova(LM.2)

LM.2alt <-lm(ABUND~GRAZE+AREA, data=loyn2)  #change the order of predictors
anova(LM.2alt) #sequential decomposition of the variance

summary(LM.2)

drop1(LM.2 ,test="F")

#AIC
LM.0 <- lm (ABUND ~+1, data=loyn2) #null model
LM.new <- step(LM.0 ,scope=~AREA+DIST+LDIST+GRAZE+ALT+AGE)


#Compare Area and Graze because coeff corelation = 0.51. They are correlated
loyn2.std <- data.frame(scale(loyn2)) #scale all values so as to have the min same unit comparable 
View(loyn2.std)
summary(loyn2.std)

#to standardize: substract by average and then divide by standard deviation
LM.2std <- lm(ABUND ~ AREA + GRAZE, data = loyn2.std) 
summary(LM.2std)

with(loyn2, cor(AREA, GRAZE))  #pearson correlation coefficient

#Can this model with 2 predictors can be further extended

add1(LM.2, .~.+DIST+LDIST+ALT+AGE, test="F") #extension of possible variables. try a mother.each row is one model with adding each variable. Partial F test, partial effect of predictor considered
#--> Based on F test, out of the 4 remaining predictors only alt can be added. BUT IN WHAT WAY IT CHANGES THE MODEL?

#Regression residuals = what I failed to explain by using area and graze. additional effect of altitude.
LM2.res <- resid(LM.2)
summary(LM2.res)

#Does altitude have additional effect
plot(LM2.res ~ ALT, data= loyn2)  #look at effect of altitude on a model where there is not altitude. doe snot match which what alt oculd add in the model
LM.alt <- lm(LM2.res ~ ALT, data=loyn2) 
abline(LM.alt, col="blue")
anova(LM.alt)

#Need to use partial residuals
LM.3 <- update(LM.2, .~. + ALT)

LM.pr <- resid(LM.3, type="partial")
dim(LM.pr) #3 columns : Area, graze, alt
summary(LM.pr) #variance explained altitude was ommited

coef(lm(LM.pr[,3]~ALT, data=loyn2))
coef(lm(LM.3)) #the same coef, so Altitude explains the same by partial residuals

#Scatterplot with area on horizontal axis and on Y partial residual
par(mfrow=c(1,3))
scatter.smooth(loyn2$AREA, LM.pr[,1]) #partial residuals for Area
scatter.smooth(loyn2$GRAZE, LM.pr[,2]) #partial residuals for Area  #not linear effect of grazing on area. here overestimation and underestimation
scatter.smooth(loyn2$ALT, LM.pr[,3]) #partial residuals for Area

library(effects)
install.packages("carData")

plot(allEffects(LM.3))
plot(allEffects(LM.3, resid=T))  #linear effect are reasonable. For Graze we see horizontal pattern to left and steep on right (overestimating on left and underestimating in middle)
plot(LM.3) # here looking at residuals of the model 

par(mfrow=c(2,3))
plot(LM.3, which = 1:6)

#PLOT1
#residuals vs fitted= normal residuals (diff between observed and predicted values of response variable at scale response variable). 
#residuals against predicted values.check if model could be a little bit more complex.
#the fitted parametric line starts with decline and then decrease. Graze shape. Linear effect of grazing not appropriate. discrepancy appears.
#without grazing maybe discrepancy would disapear

#PLOT 2
#fit of the model

#PLOT 3
#absolute values. homogeneity of variances.

#PLOT4
#high of the bar measure cook distance. essentially summarizing how much affected if i remove observations -> high pic if I remove this observation = the ones that are most impactful
#deviation cause by particular observations because: values of explanatory variables are maybe unusual. OR. cross check values correspond to what found in field

#PLOT 6
#isolines

influence.measures(LM.3)   #suggest values that are influencial, maybe could remove them


#2-URSINS-----------------------------------------------

andrew <- read.delim("clipboard", as.is= FALSE)
andrew <- transform(andrew, PATCH=as.factor(PATCH))
summary(andrew)
plot(ALGAE~TREAT, data=andrew)
par(mfrow=c(1,1))
plot(ALGAE~TREAT, data=andrew)
with(andrew, tapply(ALGAE, TREAT, mean))


#ONE WAY ANOVA
AOV.1 <- aov(ALGAE~TREAT, data=andrew)
summary(AOV.1)
anova(AOV.1)

summary.lm(AOV.1) #here get regression coefficients

#contrasts-categorical values 
coef(aov(ALGAE~TREAT-1, data=andrew)) #here test without intercept here so without referring to control. THS APPROACH TO DEMONSTRATE AVERAGES CALCULATED.
#NEVER DO IT THIS WAY. only if you want to force the intercept to be zero for example


plot(AOV.1, which=3)
bartlett.test(ALGAE ~ TREAT, data=andrew)
AOV.2 <- update(AOV.1, log(ALGAE) ~ TREAT) #find inf values because of zeros so no log possible

AOV.2 <- update(AOV.1, log(ALGAE+1) ~ TREAT)
bartlett.test(log(ALGAE+1) ~ TREAT, data=andrew)  # test of homogeneity of variances.

#we can try power transformation --> values become barplots would not show signf diff but it is just formal

plot(log(ALGAE+1)~TREAT, data=andrew) #here transformation of response variable
summary(AOV.2)

TukeyHSD(AOV.2, ordered=T) #largest to smallest to have positive differences --> easier to compare accross sites. Test of differenecs between groups means

#PB!!! repeated measure in quadrates that are in same block. If evalute effect of treatment: 4 replicates for each treatment. not 20.
#average accross patches - nb of observations inside block. work at the quadrat level. Average per block.

ALGAE.av <- tapply(log(andrew$ALGAE+1), andrew$PATCH, mean)
length(ALGAE.av) #4 tratments x 4 Pacthes
TREAT.av <- as.factor(rep("ctrl", "d2/3", "d1/3", "rem", rep(4,4)))
TREAT.av <- as.factor(rep(c("ctrl", "d2/3", "d1/3", "rem"), rep(4,4)))
length(TREAT.av)
summary(TREAT.av)
summary(aov(ALGAE.av ~ TREAT.av))

AOV.3 <- aov(log(ALGAE+1)~TREAT+Error(PATCH), data=andrew)
summary(AOV.3)  #oh no it is not significant when we account for the fact that there are repeated measures inside patch and account for it

andrew$URCHINS <- rep(c(1, 0.67, 0.33, 0.0), rep(20,4))
plot(URCHINS~TREAT, data=andrew)


AOV.4 <- update(AOV.2, .~URCHINS + Error(PATCH)) #the larger extent of removal the more algea there are 
summary(AOV.4)

#Maybe can make more replicates in each patch. Pilot study could help to know how much needed to have significancies