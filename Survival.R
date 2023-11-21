###################SURVIVAL - Seedlings dataset from MRM
seedl <- read.delim("clipboard")
summary(seedl)

library(survival)  #package survival installed by default

SF.1 <- survfit(Surv(day,died)~+1, data=seedl) #no effect of observed circumstances on survivance of seedlings
with(seedl, Surv(day,died))

SF.1

par(mfrow=c(1,1))
plot(SF.1)

summary(SF.1)  #describe temporal sequences of year. proportion of seedlings survived at day t

#expected prop surviving from the start, we have to take into account the ones died earlier 

(30-1)/30  #96 survived (vertical position of survival curve)

((30-1)/30)*((29-1)/29)

((30-1)/30)*((29-1)/29)*(28-3)/28


#CURVE - expected proportion of seedlings that will be alive at particular time - proportion of seedlings which have time span larger that particular time

#Hazard rate


SF.2 <- survfit(Surv(day,died)~species, data=seedl) #this time explaining variable, species identity
summary(SF.2)

plot(SF.2)

plot(SF.2, col=c("blue", "red", 'green'))

SF.2$strata

legend(1,0.7, c("AM", "HL", "PL"), col=c("blue", "red", "green"), lty=1)

survdiff(Surv(day,died)~species, data=seedl) #contingency table #no diffrences between these 3 species (p=0.7)

names(seedl)
#We can check if there are differences between two places

survdiff(Surv(day,died)~place, data=seedl)
#There is significant difference between the two places--> survival different between two places. near forest it is driest area they did not survive so well (5<12)
#survdiff vcan give parameter rho

?survdiff

#2 types of tets wiht rho 0 (default) or 1
#if you want to accent the survival of young seedlings (beginning of survival curve) use rho =1
survdiff(Surv(day,died)~place, data=seedl, rho=1)

SF.3 <- survfit(Surv(day,died)~place, data=seedl)

plot(SF.3, col=c("blue","red"))
plot(SF.3, col=c("blue","red"), conf.int=TRUE)
plot(SF.3, col=c("blue","red"), conf.int=TRUE, log=T) #log transformation
#only if factors into groups

#if numerical factor, then we cannot use this kind of testing --> Kox proportional hazard 


################Cox proportional hazard model
#OTHER DATASAT - RATS (available from R) - cancer of rats
data(rats)
search()
summary(rats)
rats[1:7,]

SF.1 <- survfit(Surv(time ,status)~+1, data=rats)
SF.1

survdiff(Surv(time,status)~rx,data=rats)

#observed rats for 100 days, not entire lifespan

CPH.1 <- coxph(Surv(time,status)~rx, data=rats) #miss random effect of litter
summary(CPH.1)
#if rx applies, you increase hazard rate, higher rx has higher chance of cancer developping (increase two times)
#if rx applies, increase of hazard rate can increase from 1.115 and 3.739


1/2.042

#0.0208* cannot be for granted, based on aproximation that the distirbution of estimate regression coeff around true value is well described by gaussian distribution
#which of this is best depends on type if data
#if we look p values they are identical.  chisquare test usually used in regression. (also likelihood but does not translate to its robustness)

#Take into account litter (random effect)
CPH.2 <- update(CPH.1, .~. +cluster(litter))
summary(CPH.2)
#power of the test increased (z calculated using robustness), lower variation of estimate so not sure if sign diff from zeros
#present Robust in results and the p-value it is this test to present

#display statist results of testing for proportional hazard

cox.zph(CPH.2)
#reject the idea of proportional hazard, p-value is 0.049 so not significant

plot(cox.zph(CPH.2))
#effect of r characterizes by 2.sthing... here original for beta. solid line. if flat no effect of r.
#0.05 near p value represented here by touching of lines 


###DATASET PL-Leafs
PL <- read.delim("clipboard", as.is = FALSE)  #year is a factor
summary(PL)

#estimate median life span of plantago lanceolata leaves
#survival curve

#method 1 = null model
survfit(Surv(span, status)~+1, data=PL)  #null model
 #average life span is ten and half week

#method 2 = cox method
COX.1 <- coxph(Surv(span, status)~+1, data=PL)
survfit(COX.1)

#plot 
abline(h=0.5, lwd=2)
abline(v=10.5)  #median life span
plot(survfit(COX.1))
abline(h=0.5, lwd=2)
abline(v=10.5) #position of the median

COX.2 <- update(COX.1, .~ mown+cluster(plant))
summary(COX.2)
#focus on significance test - ROBUST = 1.2, p =0.3 > not good, cannot reject of null hypothesis about null effect of mowing on life span
#no interest to look at effect of mowning

#Look on the meterological information
COX.3 <- update(COX.2, .~cluster(plant)+AvTemp+SumPrec,subset=(crosses=="n"))  #plant as random factor ?
summary(COX.3)
unique(PL$crosses)

#both predictors significant 
#when avtemp increase hazard rate of time span increase
#when sumprec increase hazard rate decrease

#what real effect these 2 predictors
#how precipitation amount changes survival curve
plot(survfit(COX.3))

#decide the appropriate values for two predictors
summary(PL$AvTemp) #decide median (or could use arithmetic average, but distribution is not too much symetrical so median better)
summary(PL$SumPrec)

lines(survfit(COX.3, newdata=list(AvTemp=c(13.35,13.35),  #median
                                 SumPrec=c(9.6,14.9))),     #lower and upper limit
                                 col=c("red", "blue"))  #blue for wetter conditions
par(mfrow=c(1,2))
plot(cox.zph(COX.3))
cox.zph(COX.3)
#cannot reject of hypothesis about effect of these two predictors changing lifespan of leaf. so previous model was
#sufficiently fulfilled
#if temp increases, the shorter time the leaf lives. the more moisture the more longer they live

#check for both variables partial residuals, check effect without getting rid for other one
COX.3R <- resid(COX.3, type="partial")
dim(COX.3R)
#second dim is partrial resid

not.cross <- (PL$crosses=="n")
sum(not.cross)

scatter.smooth(PL$AvTemp[not.cross], COX.3R[,1])
scatter.smooth(PL$SumPrec[not.cross], COX.3R[,2])

COX.4 <- update(COX.3, .~AvTemp + poly(SumPrec,2)+cluster(plant))

anova(COX.3, COX.4)
#alternative way to compare models parsimony = AIC (because here anova doe snot work)

AIC(COX.3)
AIC(COX.4)
