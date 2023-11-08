pollen <- read.delim("clipboard")
summary(pollen)
View(pollen)

GLM.1 <- glm(cbind(Picea,Total-Picea)~DegDays, family=binomial, data=pollen)
summary(GLM.1)

# Creating the second-order polynomial for DegDays
DegDays_poly <- (pollen$DegDays)^2

# Adding the new variable to the existing dataset
pollen <- cbind(pollen, DegDays_poly = DegDays_poly)

View(pollen)

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

