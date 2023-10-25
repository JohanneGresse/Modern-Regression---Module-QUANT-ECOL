stream <- read.delim("clipboard", as.is= FALSE)

summary(stream)
head(stream)

library(lattice)
splom(stream)

#Log transformed scale
stream$EC<- log(stream$EC)
stream$Velocity <- log(stream$Velocity)
head(stream)
splom(stream)

#Linear model
LM.1 <- lm(EC~Velocity+Stream, data=stream)
summary(LM.1)
anova(LM.1)

LM.2 <- lm(EC~Velocity*Stream, data=stream)
summary(LM.2)
anova(LM.2)


###SIGNIFICANT BOTH!!!!!!!!!!!!!!!!!!!!!!!


#Test a model with only Stream as predictor only and try to see if we add a predictor if it would add something to the model
LM.3 <- lm(EC~Stream, data=stream)
summary(LM.3)

#Can this model be further extended ? check here
add1(LM.3, .~.+Velocity, test="F")

#Here we can see that adding Velocity would not make the model significant


