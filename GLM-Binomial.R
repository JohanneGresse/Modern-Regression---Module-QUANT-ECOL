#BINOMIAL FAMILY

#ANOLIS DATASET
anolis <- read.delim("clipboard", as.is = F)
View(anolis)

GLM.0 <- glm(cbind(G,O)~+1, family=binomial, data=anolis) #attention, here order G O matters, here predict probability of species G. p(G)+p(O) = 100%
                                                          #here if G is present we look at his probabilities. G+O = total nb of events
summary(GLM.0)

lapply(anolis[,1:2], sum)

#nb of times each species appear : G appeared 431 times

431/(431+133)  #probability of any picked observation to have G species 

#Odds and probabilities. Odd = ratio of the probabilities between the 2 species

#for example if we have p(G)=0.75 and p(O)=0.25, the probabilities of G is three times higher, the odd to have G is 3 times higher than to have O.

#Log transform ratio log(p/(1-p)). cover pool range from -inf and +inf, can cover negative values and positive if p inf or sup to 1
#exp(n)/(1+exp(n))
#link logit 
#But we will use model estimate to estimate how many times odd ratio increase when increase of 1 unit

#Odd ratio
0.764/(1-0.764)

log(0.764/(1-0.764))  #same value as estimate in summary = log(odd ratio)


add1(GLM.0, ~Light + Perch + Position + Time, test="Chisq")
#comparing quality better with chisq 
#position of perch most significant

GLM.1 <- update(GLM.0, .~.+Position)   #position already in model because significant
add1(GLM.1, ~Light + Perch + Position + Time, test="Chisq")  #here even if there is Perch it does not appear in summary, alread included. what happens if add other predictors

GLM.2 <- update(GLM.1, .~.+Perch)
add1(GLM.2, ~ Light+Perch+Position+Time, test="Chisq")

GLM.3 <- update(GLM.2, .~.+Time)

GLM.4 <- update(GLM.3, .~.+Light)


#All possible predictors but added as if theer were added independently. but need to consider possible interactions 


add1(GLM.4, ~(Light+Perch+Position+Time)^2, test="Chisq") #permutation test decided appropriate from summary of model


coef(GLM.4)  #if position low, perch small it does not like afternoon timelate and preferes timemid compared with afternoon and the sun

#would need to sum up coef and use inverse function exp(n)/(1-exp(n))

names(GLM.4)

names(family(GLM.4))

family(GLM.4)$linkinv   #function with parameter eta and tells probability. we can use it

LINV <- family(GLM.4)$linkinv

coef(GLM.4)

#Best for Anolis grahami:
LINV(1.465+0+0.763+0.227+0.84)  #use coef, we chose mid day rather than later to describe best conditions for this species. 96% of this species occur in these conditions

LINV(1.465+ -1.130+0 + -0.73 + 0)  #last zeros means "no sun". These conditions are better for species O



#BROOMRAPE DATASET
broomrape <- read.delim("clipboard", as.is = F)
summary(broomrape)

GLM.B0 <- glm(cbind(count, total-count)~genotype + host, data=broomrape, family=binomial)  #cbind(p, 1-p)

GLM.B1 <- update(GLM.B0, .~.+genotype:host)
anova(GLM.B0, GLM.B1, test="Chisq")   #deviance two times larger than df, worrying because observed count of germinated seeds are overdispersed. so conclusion about significance based on chisq are not good because do not consider overdispersion. p-value estimated is too law

#Update the model with quasi binomial
GLM.B0 <- update(GLM.B0, family=quasibinomial)
GLM.B1 <- update(GLM.B1, family=quasibinomial)

anova(GLM.B0, GLM.B1, test="F") #all other cases than poisson family gama, etc... test F

summary(GLM.4)  #here not overdispersion 

#Careful, response variable of binomial can be even easier than the ones used so far (Bernoulli d)



ReprEff <- read.delim("clipboard")
summary(ReprEff)
ReprEff <- transform(ReprEff,Flowers=log(Flowers), Rhizome=log(Rhizome))

GLM.0 <- glm(Survived ~+1, family=binomial, data = ReprEff)
add1(GLM.0, ~ Flowers + Rhizome, test="Chisq") #Rhizome has small effect so lets add it to the model

GLM.1 <- update(GLM.0, ~.+Rhizome)
add1(GLM.1, ~ Flowers + Rhizome, test="Chisq")  #we should look at partial effect of 


#first extend model, flowers alread present there
GLM.2 <- update (GLM.1, .~.+Flowers)
  #evaluate quality of both predictors
drop1(GLM.2, test="Chisq")

#Both predictors have partial effect, effect of flowers in addition to rhizome length explain

summary(GLM.2)

#when predicting flowers counts, 

exp(log(2)*3.775) # when x double y increase from 13.7

#PLOTS = contour plot, look at partial effect of these 2 predictors

library(effects)
library(carData)

plot(allEffects(GLM.2))  #plot with independent effect of each variable of the model without considering the other variables.predictors on log transformed scale
plot(allEffects(GLM.2, resid=TRUE))


#GAMMA DISTRIBUTION
barley <- read.delim("clipboard")

#exclude pots where only one of the species was sown (because not comparable then)

comp <- with(barley, (s.Hor > 0) & (s.Sin >0))
sum(comp)  #25 values in dataset, 25 pots were both species sown

#subet original dataset
bar <- barley[comp,]
summary(bar)  

GLM.comp <- glm(w.Hor/w.Sin ~ log(s.Hor/s.Sin)+log(s.Hor+s.Sin), data=bar, family=Gamma(link=log)) #here we use ling log 

#Gamma assumption, appropiate test is F test
anova(GLM.comp, test="F") #here test is wrong 
summary(GLM.comp) #regre coeff b1 is not significantly diff from zero (positive estimate) then value of predictor proportion gets larger - Barley coping better with higher density thta Sin

#Toal variation in our data represented by null variance

100*(53.7512-4.72)/53.7512
#91.2% of explained variance 

R2.nag
R2.nag(GLM.comp)  

plot(log(w.Hor/w.Sin)~log(s.Hor/s.Sin), data=bar) #linearly correlated

#NOt Bernoulli this time but Counts - Rotifers dataset
rotif <- read.delim("clipboard", as.is=F)


is.KC <- rotif$species=="KeraCoch"
is.PM <- rotif$species=="PolyMajo"
plot(y[is.PM]/n[is.PM] ~ density[is.PM] ,data=rotif, pch="P", ylim=c(0,1), wlab="density", ylab="fraction", col="blue")
with(rotif, points(density[is.KC], (y/n)[is.KC], pch="K" ,col="red"))

#logit link log transformation make sure on left line approach zero and on right extreme approach plateau

GLM.R1 <- glm(cbind(y,n-y)~density+species, family=binomial, data=rotif)

summary(GLM.R1)

#the larger the density, the higher proportion will be 

#overdispersion = look at residual deviance
GLM.R2 <- update(GLM.R1, family=quasibinomial)
summary(GLM.R2)

drop1(GLM.R2, test="F")



