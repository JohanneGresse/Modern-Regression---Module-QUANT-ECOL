birds <- read.delim("clipboard", as.is= FALSE)
View(birds)
summary(birds)

birds$Richness <- log(birds$Richness)

hist(birds$Altitude)
hist(birds$ForCover)
hist(birds$ForDens)
hist(birds$HerbCover)
hist(birds$Slope)

library(lattice)
splom(birds)

# Need to log transform some predictors > ForCover, ForDens
#birds$ForCover <- log(birds$ForCover)
#birds$ForDens <- log(birds$ForDens)
#birds$HerbCover <- log(birds$HerbCover)


#No even weirder when log transofming predictors


#5 potential predictors but we can choose the ones with stronger relation (from splom plot) = Richness - Altitude, Richness - Slope 
#Environment first driver over biotic conditions

#AIC with selected 2 variables
#LM.0 <- lm (Richness ~+1, data=birds) #null model
#LM.final <- step(LM.0 ,scope=~Altitude+Slope)


#AIC with all variables
LM.0 <- lm (Richness ~+1, data=birds) #null model
LM.new <- step(LM.0 ,scope=~Altitude+ForCover+ForDens+HerbCover+Slope)

#Altitude and Slope are the variables that matter. Nearly same AIC but less predictors so it is more parsiminous.

