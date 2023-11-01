#FLOWERS ReprEff dataset

flow <- read.delim("clipboard") #2last columns, omited the first one
summary(flow)


#Need to log transform the predictors: Linearize relationship and predicted values have same variation around predicted values
plot(Flowers ~ Rhiwome, data = flow)
plot(log(Flowers)~log(Rhizome), data=flow)
plot(Flowers ~ Rhizome, data = flow, log="xy")
plot(Flowers ~ Rhizome, data = flow)

GLM.1 <- glm (Flowers ~ Rhizome, data = flow, family = poisson)

GLM.1 <- glm (Flowers ~ log(Rhizome), data = flow, family = poisson)
summary(GLM.1)

anova(GLM.1)  #here we do not have p value
605.61/1416.71  

1-605.61/1416.71 #evaluating importance of predictors, p value calculated manually by deviance


anova(GLM.1, test="Chisq")


#Calculating the overdispersion parameter of the model
GLM.2 <- update (GLM.1, family = quasipoisson)
summary(GLM.2)

anova(GLM.2, test="F")

sqrt(12.2) #Bota

exp(log(2)*0.563)  #log (2) because interested in how much Y increases when we double predictor. here answer by 47percent
#different from homework because there predictor was logged and here not so we use the log(2)
#if it was 2.35 then it means it increases from 135percent

#the number of flowers increase by 47percent


#Sometime slog or quasi poisson not enough
plot(GLM.2, which = 1)

GLM.3 <- update (GLM.2, .~poly(log(Rhizome), 2))  #model polynomial order 2. X + X2. quadratic dependency
anova(GLM.2, GLM.3, test="F") #compare two models

plot(Flowers~Rhizome, data=flow)
summary(flow$Rhizome)

rhiz.pred <- data.frame(Rhizome=seq(0.01,2.4, length=30))
rhiz.pred[1:10,] #what is the predicted values of the response
flow.pred <- predict(GLM.2, rhiz.pred, type="response")

flow.pred[1:10]
lines(rhiz.pred$Rhizome, flow.pred, col="blue", lwd=2)



###BARDLEY DATASET - research focus monoculture so focus on intraspecific competition. only if Sin not present so exclude other observations
bar <- read.delim("clipboard")
just.barley <- (bar$s.Sin < 1) 
bar2 <- bar[just.barley,]
summary(bar2)
just.barley

bar2
with(bar2, scatter.smooth(s.Hor, (w.Hor/n.Hor)))
with(bar2, scatter.smooth(s.Hor, log(w.Hor/n.Hor)))
with(bar2, scatter.smooth(s.Hor, (n.Hor/w.Hor)))
GLM.B1 <- glm((w.Hor/n.Hor)~s.Hor, data=bar2, family=Gamma)
GLM.B1X <- glm((w.Hor/n.Hor)~s.Hor, data=bar2, family=Gamma(link=log))
summary(GLM.B1) #In Gama family we dont care about Dispersion parameter, does not mean anything here


#Calculation coeff of determination
1-0.927/4.384

#D0, D
#(1-exp(D-D0)/n)/1-exp(-D0/n)  #n number of obs

sort(names(GLM.B1))


GLM.B1$dev

DM <- GLM.B1$dev
D0 <- GLM.B1$null
n <- length(GLM.B1$resid)
n
(1-exp((DM-D0)/n))/(1-exp(-D0/n))

#variance explained VS unexplained one

R2.nag <- function(GLM) {}


#Coeff of determination of the model
R2.nag(GLM.B1)
R2.nag(GLM.2)
plot(GLM.B1, which=1)
GLM.B2 <- update(GLM.B1, .~poly(s.Hor,2))
anova(GLM.B1, GLM.B2, test = "F")

#Creation of dataframe
pheas <- data.frame()
fix(pheas)
pheas
GLM.P0 <- glm(N ~ Germinated+Eaten, data=pheas, family=poisson)
GLM.P1 <- glm(N ~ Germinated * Eaten, data=pheas, family=poisson)
anova(GLM.P1, test="Chisq")
#germinaiton rate higher when fruits were eaten
#Final model with interaction fully saterated. df = 0
#Eaten zero explained variance, balanced experiment there is 50 percent chance to be eaten or not

squir <- read.delim("clipboard", as.is=FALSE)
summary(squir)

sqiur <- transform(squir, Year=as.factor(Year))
GLM.S0 <- glm(Count ~ Treatment * Year + Age, data=squir, family=poisson)

#second order interaction
add1(GLM.S0, .~.+(Treatment+Year)*Age, test="Chisq")  #contingency table 


GLM.S1 <- update(GLM.S0, .~. +Treatment:Age)
GLM.S1 <- update(GLM.S0, .~. +Year:Age)

add1(GLM.S1, .~.+(Treatment+Year)*Age, test="Chisq")  #contingency table 

GLM.S2 <- update(GLM.S1, .~.+Treatment:Year:Age)
anova(GLM.S1, GLM.S2, test="Chisq")

#ccl: pop structure not affected in any way by the treatment