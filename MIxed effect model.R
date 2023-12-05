spruce <- read.delim("clipboard", as.is=F)
summary(spruce)

#Data exploration
plot(size~time, data=spruce)
plot(size~treat, data=spruce)

spruce <- transform(spruce, tree=as.factor(tree))
summary(spruce)

#Model of analysis of covariance - Linear model - size of tree change with numerical

LM.1 <- lm(size~time + treat, data=spruce)
anova(LM.1)
summary(LM.1)

plot(size~time, data=spruce)

#attention but no independent observations because of the tree

#measurements at individual are split plot

#-->better visualize data
library(nlme)
spruce2<- groupedData(size~time|tree,data=spruce, FUN=mean, outer=~treat, labels=list(x="time", y="tree size")) #FUn allows in plot to order info about individual tree, by size #outer all plot factors. outside indiv obsv it changes on the tree level
summary(spruce2)
plot(spruce2)

#compare average positions of the trees (combine 3 lines for each treatment in same subplot)
plot(spruce2, outer=T)
plot(spruce2, outer=T, key=F, aspect=1)

#within same treatment, each subplot. differences between trees should be random effect. to account for variation in size between the trees.
#dominant pattern in data is growth of tree in time, some trees are growing moer quickly growing (yellow line compared to other)

#compare estimates (slope) accross trees. evaluate how trees differ in these parameters. if they do we must include random effect for intercept

LML.1 <- lmList(size ~ time | tree, data= spruce2) #models separately for individual trees. set of regressions lines for each observations belonging to each tree
summary(LML.1)

plot(intervals(LML.1)) #look at spread in confidence interval - for time look a little bit better (smaller IC)
pairs(LML.1)


#intercept and estimate slope calculating at same time= big contirbution of interecept when small of slope
#here issue intercept evolve with time


summary(spruce2$time)
spruce2$time <- spruce2$time - 540
#now values of predictors range between -44 and 39. average very close to zero. fix issue of correlation in regression coeff estimates

LML.2 <- lmList(size~time|tree,data=spruce2)
pairs(LML.2)
#scatterplot but not declining relationship as before

plot(intervals(LML.2))
#confidence interval much narrows better estimates. there is a random effect of intercept

#centered scaled by calculating arithmetic mean and substract

#CCL = beyond the fixed effect, estimate of intercept and slope, we also need random effect for both parameters. estimate accross the trees.

#First linear mixed effect model

LME.1 <- lme(size ~ time, random=~time|tree, data=spruce2) # if 1 instead of time, intercept random effect. and also for slope. usually only intercept
summary(LME.1)


#correlations between random estimates (additional along random effect)

#is model try better than model with only interecept random

LME.2 <- update(LME.1, random=~1|tree) #here random effect just defined like this
anova(LME.2,LME.1)

#two models differ in their df. original one has 6.

#larger likelihood better is the model. Test L.Ratio (likelihood ratio)
#our extended model with random effect of slope in addition to random effect of intercept is better! we cannot simplify model to singl emodel effect. LME1 is netter than LME2

#check effect of time and also treatment ozone

LME.3 <- update(LME.1, .~.+treat)
anova(LME.1, LME.3)
#they have different fixed effects

#random and fixed effects in summary

#max likelihood used to estimate corr. True likelihood method. REML (restriced max likelihood). in max likelhiood underestimate size of
#Max likelihood calculate variance with dividing by n instead of n-1 = underestimate true size of variance, usually n-1. REML

LME.1ML <- update(LME.1, method="ML") #rather than REML
LME.3ML <- update(LME.3, method="ML")
anova(LME.1ML, LME.3ML)
#single additional fixed effect. testing actual effect of tree. LME 3 bettter

anova(LME.3ML)
#independent effect of time, in addition with variaiton explained by time

drop1(LME.3ML, test="Chisq")
#compares with Chisq distribution

#plot fitted model
plot(augPred(LME.3, level=0:1)) #only if you use rando meffect correction for slope
plot(augPred(LME.3, level=0:1), aspect="xy")

#slopes up compared to blue line fitted for ozone tree. orange ones are with random effect --> if the two lines do not match, random effect important

AOV.1 <- aov(size~time + treat + Error(tree), data=spruce2)
summary(AOV.1)
#here getting reg coeff would be more difficult

#be careful with time auto correlation if 4 observations (1 and 3 correlated)


##NLME package
data(Theoph)
summary(Theoph)
Theoph[1:14,]
class(Theoph)
plot(Theoph)
?SSfol

#wich of the model parameters are affected by individuality of the patient
#fit non linear equation for each patient separately and compare how three parameters differ among patients

NLS.1 <- nlsList(conc ~ SSfol(Dose, Time, lKe, lKa, lCl)|Subject, data=Theoph)
pairs(NLS.1)
plot(intervals(NLS.1))
NLME.1 <- nlme(NLS.1, random=list(lKa~1, lCl ~1))
#plane efect, random effect not affected by its size
NLME.1

intervals(NLME.1)
#confidence intervals
#correlation between two random effects not significantly different from zero

anova(NLME.1)
plot(augPred(NLME.1, level=0:1)) #both without and with random effects. how inclusion of random effect improves fit data
#wether the fit also affected by weight of the patient. high ratio rate the faster get alcohol rid from your body

#now plot the atcual values of variation of patients against their weight to see if there is a trend

NLME.1RE <- ranef(NLME.1, augFrame=T)
plot(NLME.1RE, form=lCl~Wt) #against patient weight. unimodal variation according to weight
plot(NLME.1RE, form=lKa~Wt) #now test with another random factor

class(NLME.1RE)
?plot.ranef.lme
#parameter span.loess 

plot(NLME.1RE, form=lCl~Wt, control=list(span.loess=1))

#now not unimodal, but lCl does not change to much
#lCl is not constant accross all patients and depend on weight, but we have seen stronger effect on Ka paramter

plot(NLME.1RE, form=lKa~Wt, control=list(span.loess=1))
#here indeed almost linear increase

#change description of fixed parameter in a new model 
NLME.2 <- update(NLME.1, fixed=list(lKe~1,lKa~Wt, lCl~1)) #here only lKa depends on weight. lka ~ bo + b2 Wt. (lKe ~ 1)b1

#initial estimate of these fixed parameters
fixef(NLME.1)

NLME.2 <- update(NLME.1, fixed=list(lKe~1,lKa~Wt, lCl~1), start=c(-2.45,0.47,0.0,-3.23)) #here correct estimates otherwise NLME.2 does not run
fixef(NLME.2)

anova(NLME.1, NLME.2)
#worth doing NLME2? new model signficantly better but signf not so good

#they do not provide full info, used a lot of repeated measurementds at individual subjects with random effects


###log transform count data of explanatory variables but not if work on probability

#REML GLMM

LME.1 <- lme(size ~ time, random=~time|tree, data=spruce2)
detach()
library(lme4)

LMER.1 <- lmer(size~time + (time|tree), data=spruce2)
fixef(LMER.1)

anova(LMER.1)

LMER.2 <- update(LMER.1, .~.-time) #ignore error message convergence of model failure
anova(LMER.1, LMER.2)                 
