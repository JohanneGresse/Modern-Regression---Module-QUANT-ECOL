stream <- read.delim("clipboard", as.is= FALSE)
View(stream)
summary(stream)

#Log transformation of the predictors
stream$EC <- log(stream$EC)
stream$Velocity <- log(stream$Velocity)
summary(stream)


library(lattice)
splom(stream)


#Linear model
install.packages("carData")
library("car")
LM.1 <- lm(EC~Velocity + Stream, data = stream)
summary(LM.1)
Anova(LM.1, type=3)


#CHeck interaction
LM.2 <- lm(EC~Velocity * Stream, data = stream)
summary(LM.2)
Anova(LM.2)

#Check if adding something to add Velocity
LM.0 <- lm(EC~Stream, data = stream)

add1(LM.0, .~.+Velocity, test= "F")

#Linear final model
LM.final <- lm(EC~Stream, data = stream)
summary(LM.final)
Anova(LM.final)

#PLot the graphical results
plot(EC~Stream, data=stream)

#look at the estimate in the model
6.25/5.25
#estimate 1.19 means increase of 20_ more (0.19, adding 0.19 to 1)


#Model validity
par(mfrow=c(2,2))
plot(LM.final)


