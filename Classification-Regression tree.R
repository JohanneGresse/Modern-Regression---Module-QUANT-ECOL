
###EXAM -- MATERIAL ALLOWED
#5th, 12th= trial exam and 19th = if you succeed this it is one. otherwise other exam in January (several dates)
#describe method and results as in research paper #16min for 2 examples


#Classification regression trees

#Predict numerical variables
TX <- read.delim("clipboard", as.is = F, row.names=1)
summary(TX[,1:6])

library(rpart)
RP.1 <- rpart(RepPref ~., data=TX)  #use all of them #determine to what extent the tree should be branched
plot(RP.1)

text(RP.1,cex=2/3)  #adding the names to the graph. cex = diminishing size of text

plot(RP.1, margin=0.05)  #shrink and center the tree
text(RP.1,cex=1/2)  #smaller text (too small)
text(RP.1,cex=2/3)  #if proportion of people < 90.3 then go left.  if yes go to the left if no go to the right. from left to right proportion increasing. can swap the branches < if becomes > then lef tand right paths exhanged

#for each predictoir we can estimate how good the separation (treshold) is
#long branch means good rule good separation 

#try for all the variables 
#if branches are too small no need of splitting them again into groups (small groups) --> too much branches,  too complex regression models

#check how this model perform with a new dataset

dim(TX)

#null model  = no terminal links, no branches
#Cross-validation, regression model 

plotcp(RP.1, col= "blue") # because 10 groups (K=10), it has also confidence intervals (for each dot 10 samples). first dot is start of tree last dot is last part of the tree
#only second dot is significant and is correct to be grouped

#RP.1 has also the insignificant predictors so now RP.2 show only the predictors that are significantly grouped 
RP.2 <- prune(RP.1, cp=0.06) #cp = complexity parameter (from our graph plot we would prefer 0.04 where blue line cross curve)
plot(RP.2, margim=0.05)
text(RP.2)

RP.2
#divide all dataset into 2 groups. * at the end shows it is terminal branch (3 terminal branches here 2, 6, 7)

summary(RP.2)
#how frequently variables were considered, primary splits first ones to be considered (most frequent) + surrogates
#surrogates can be good if we have too much missing values


###OTHER DATASET PLANTATT
att <- read.delim("clipboard", as.is=F)
summary(att)

#exclude species with no information about their distribution change. Chg has 242 NA so to remove
Chg.mis <- is.na(att$Chg)
sum(Chg.mis)
att2 <- att[!Chg.mis,] #new dataset without NAs

RP.A1 <- rpart(Chg~., data=att2) #nothing left behind in the dataset
plot(RP.A1, margin=0.05)  #add whit margin around the tree
text(RP.A1)
plot(RP.A1, margin=0.05)
text(RP.A1, pretty=0)  #instead of indexes a b c d e i written for LF1 it is replaced by their actual variable name

#what is max size to predict new values here

#Cross validation results
plotcp(RP.A1, col="blue") #none of the predictors are affected. compared to null model the first group explains only 1% of the variation

plot(RP.A1, margin=0.05, uniform=T) #all the branches have same weight now (provide less information but can look clearer)
plot(RP.A1, margin=0.05, uniform=T, branch=0) #for the phylogenetic trees style
plot(RP.A1, margin=0.05, uniform=T, branch=0.5)
text(RP.A1, pretty=0)

####MELAMP DATASET
mel <- read.delim("clipboard", as.is=F)
summary (mel)

RPC.1 <- rpart(Taxon ~ ., data=mel, minsplit=9) #default of minsplit = 20. define how large a group have to be. here allows to split into groups if it has at least 9 observations (9 plants in the group)

RPC.1$control
#how many surrogate variables and how they are used in the predictions : not used, used if the selected predictors have missing values, or if all surrogates have NA
#ATTENTION: if NA in the tree we dont know how to divide into group 

print(RPC.1, dig=2) #print all the results with all digits infinite so if you use dig then you will decrease nb of digits 0.0003333333
##interpretation : for example for 13) it means there are 5 individuals of T3. how pure predictions in terms of being close to 1 for single category or 0
#45 individuals for T1 for roots

plotcp(RPC.1) #to use before prune function. how many groups.
#printcp(RPC.1) # here be careful not same numbers of CP different from previous graph. from here split = 3 IGNORE THIS AND USE GRAPH

RPC.2 <- prune(RPC.1, cp=0.094)
plotcp(RPC.2)

plot(RPC.2, margin=0.05, branch=0.5) #all the branches have same weight now (provide less information but can look clearer)
text(RPC.2, use.n = TRUE)

#now look at the surrogates
summary(RPC.2)

#look at fist table (correspond to first split)

#ClxTthw reproduce info given by LLipcor height about 88% 

#look what happens if linear relationship with single predictor and then if there is interaction between two predictors


#creation of explanatory and explained variable related by linear relation
x <-  rep(1:10, rep(2,10))
y <- 2+3*x + rnorm(20,0,2)  #sd=2
plot(y~x)  #looks too good ot be true

y <- 2+3*x + rnorm(20,0,6)  
plot(y~x)
abline(lm(y~x))
summary(lm(y~x)) #relationship is signicant

#now regression tree

RP.5 <- rpart(y~x, minsplit=6)
plot(RP.5, margin=0.05)
text(RP.5)

#alternative package
install.packages("tree")
library(tree)

tree.1 <- tree(y~x, minsize=6, mincut=2)
partition.tree(tree.1)
points(x,y)


##DATASET TreeTest
treetest <- read.delim("clipboard", as.is = F)  #as.is used when categorical variables
summary(treetest)
plot(X2 ~ X1, data=treetest, type="n")
with(treetest, text(X1,X2,ifelse(success=="no", "-", "+"))) #if success variable has value no then we use minus sign in the plot is yes sign +
#plot at chosen coordinates what happens

tree.2 <- tree(success~X1+X2, data=treetest, minsize=3, mincut=1)
partition.tree(tree.2, add=TRUE, ordvars=c("X1", "X2"), col="blue") #not create a new graph but add info provided into existing tree. add the yes and no
tree.2
