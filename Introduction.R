mean(x)
#unknown because there is NA value 
mean(x, na.rm = TRUE)
# with na.rm you can specified that tyou did not delet the missing values 
is.na(x)
germ<- c(1,3,5,2,0,7,8,2,4,6)
light <- rep(c("light","dark"),c(5,5))
?rep
class(light)
light <- as.factor (light)
class(light)
light
summary(light)
plot(germ~light)
germ
germ.two<- cbind(germ, 30-germ)
germ.two
germ.two[9,]
germ.two[9,1]
germ.two[-9,1]
apply(germ.two, 2,mean)
germ.two[2,]
germ.two[,2]
apply(germ.two,2,var)
tapply(germ.two[,1], light, mean)
germ.two[,1]
mean(germ.two[2,])
mean(germ.two[,2])
dim(germ.two)
germ.two[,2]
tree.1<- list(diam=c(30,40,20,15,8), height=c(8,7,9,8,6), spc=c("L","L","B","B","B"))
tree.1
dim(tree.1)
length(tree.1)
names(tree.1)
tree.1$diam
tree.1[[1]]
tree.1[1,,]
tree.1[1]
trees.df <- data.frame(tree.1)
trees.df$spc <- as.factor(trees.df$spc)
trees.df
trees.df<- transform(trees.df, spc=as.factor(spc))
summary(trees.df)
first.2<- read.delim("clipboard", as.is=FALSE, row.names = 1)
first.2
summary(first.2)
first.2[1:5,]
1:5
names(first)
par(mfrow=c(1,2))
plot(abg~blg, data=first)
plot(log(abg)~log(blg),data=first)
plot(abg~blg, data=first)
plot(abg~blg, data=first, log="xy", xlab="Belowground biomass[mg]",
     ylab = "Aboveground biomass [mg]")
par(mfrow=c(1,1))
col.1 <- with(first, ifelse((plant=="lotco"), "darkgreen", "blue"))
col.1
plot(abg~blg, data=first, log="xy", xlab="Belowground biomass[mg]",
     ylab = "Aboveground biomass [mg]", col=col.1, pch=16)
plot((abg+blg)~plant, data=first)
Lm.1 <- lm(log(abg)~log(blg), data=first)
anova(LM.1)
#F-Statistik 77.966, values of freedom 1, 56
# significance 3.40 