#BINOMIAL FAMILY
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





