pollen <- read.delim("clipboard")
summary(pollen)
View(pollen)

GLM.1 <- glm(cbind(Picea,Total-Picea)~DegDays, family=binomial, data=pollen)
summary(GLM.1)

# Creating the second-order polynomial for DegDays
DegDays_poly <- (pollen$DegDays)^2

# Adding the new variable to the existing dataset
pollen <- cbind(pollen, DegDays_poly = DegDays_poly)
summary(pollen)

GLM.2 <- glm(cbind(Picea,Total-Picea)~ DegDays_poly, family=binomial, data=pollen)
summary(GLM.2)

#second order degdays seem to be better than first order. for both models, the deviance is more than two times larger than df, worrying because observed count of Picea are overdispersed.
#SO use of quasi binomial

GLM.3 <- glm(cbind(Picea,Total-Picea)~DegDays, family=quasibinomial, data=pollen)
summary(GLM.3)

#try quasi binomial if overdispersion
GLM.4 <- glm(cbind(Picea,Total-Picea)~DegDays_poly, family=quasibinomial, data=pollen)
summary(GLM.4)


#Comparison of the two models
anova(GLM.3, GLM.4, test="F") #F test for quasi binomial

#GRAPHS
library(effects)
library(carData)
plot(allEffects(GLM.4)) 

plot(GLM.4, which=1)

plot(Picea~DegDays, data=pollen)
plot(Picea~DegDays_poly, data=pollen)



#Calculation of probability of Picea and then use this model
GLM.0 <- glm(Picea~+1, family=binomial, data = pollen)
add1(GLM.0, ~ Flowers + Rhizome, test="Chisq") #Rhizome has small effect so lets add it to the model

GLM.1 <- update(GLM.0, ~.+Rhizome)
add1(GLM.1, ~ Flowers + Rhizome, test="Chisq")  #we should look at partial effect of 


