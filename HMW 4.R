pollen <- read.delim("clipboard")
summary(pollen)

GLM.1 <- glm(cbind(Picea,Total-Picea)~DegDays, family=binomial, data=pollen)
summary(GLM.1)


GLM.2 <- glm(cbind(Picea,Total-Picea)~DegDays^2, family=binomial, data=pollen)
summary(GLM.2)


#try quasi binomial if overdispersion


#Comparison of the two models
anova(GLM.1, GLM.2, test="Chisq") #all other cases than poisson family gama, etc... test F

#F test for quasi binomial