nest <- read.delim("clipboard", as.is= FALSE)
View(nest)
summary(nest)

hist(nest$Counts)

#PLots to overview
plot(Counts~Cereal, data=nest)
plot(Counts~Shrub, data=nest)
plot(Counts~Nest, data=nest)

#Assumption Poisson distribution
var <- var(nest$Counts)
mean <- mean(nest$Counts)

Dispersion <- var/mean

print(Dispersion)


#NUll model with no interaction
GLM.0 <- glm (Counts ~ Nest + Cereal * Shrub, data = nest, family = poisson)

#Fitted function
summary(fitted(GLM.0))


#First order interaction
add1(GLM.0, ~.+Nest:(Cereal+Shrub), test="Chisq")


#Model with interaction between Cereal and Shrub
GLM.1 <- update(GLM.0, .~.+Nest:Cereal, test="Chisq")
summary(fitted(GLM.1))
anova(GLM.1, test="Chisq")

#Add other predictors
add1(GLM.1, .~. +Nest:(Cereal+Shrub), test="Chisq")
anova(GLM.1, test="Chisq")

