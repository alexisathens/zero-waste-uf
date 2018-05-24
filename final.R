rm(list=ls(all=TRUE))

library(plyr); library(dplyr)
library(ggplot2)
library(car)

#library(tidyr)

dat <- read.csv("project/CCDataApr10.csv",
                col.names=c("gender","year","plastic.b","paper.b","willing","plastic.a","paper.a","credit"))
#View(dat)

#add additional columns
dat <- mutate(dat, waste.b=plastic.b+paper.b) #total weekly waste before
dat <- mutate(dat, waste.a=plastic.a+paper.a) #total weekly waste after
dat <- mutate(dat, diff=waste.b-waste.a) #total weekly waste after

#clean data
#remove after values larger than before values
dat <- dat[-which(dat$diff<0),]

nt=dim(dat)[1]


#rephrase credit variable
dat$credit <- mapvalues(dat$credit, from = c("Redeem additional reusable items for personal use", 
                      "Fund an on-campus organization of your choice"), 
          to = c("Personal", "Club"))


hist(dat$waste.b,xlab="# consumables thrown away weekly before program")
hist(dat$waste.b) #number of consumables thrown away weekly before the program
#hist(dat$waste.a); hist(dat$diff) #other responses not considered

attach(dat)

#Order factors
gender <- factor(gender, levels=c("Male","Female"))
year <- factor(year, levels=c("Freshman","Sophomore","Junior","Senior","Graduate Student"))
willing <- factor(willing, levels=c("Yes","Sometimes","No"))
credit <- factor(credit,levels=c("Personal","Club"))

a <- length(levels(gender))
b <- length(levels(year))

#view data histogram
ggplot(data=dat,aes(x=waste.b,color=waste.b)) + geom_bar(fill="steelblue") + 
  labs(x="Weekly number of consumables")



###model and assumptions

#Run type I anova
before.mod1 <- aov(waste.b ~ gender * year)
#anova(before.mod1) #large p-value for interaction -> continue with type II anova

#Check model assumptions
#normal probability plot of residuals
qqnorm(waste.b)
qqline(waste.b)
#plot indicates that the data is (right-tailed) skewed, zero-inflated with outliers
summary(waste.b)
hist(waste.b)
abline(v=median(waste.b),col="red")

#transform the response
sqrt.waste.b <- sqrt(waste.b)
qqnorm(sqrt.waste.b) #helps but zero-inflation still apparent
qqline(sqrt.waste.b)

#transformation 2! cubed root
cubed.waste.b <- waste.b^(1/3)
qqnorm(cubed.waste.b) #helps but zero-inflation still apparent
qqline(cubed.waste.b)

#histograms of data
hist(waste.b)
hist(sqrt.waste.b)
hist(cubed.waste.b)

qqnorm(e.mod2)
qqline(e.mod2)

#improve model by using zero-inflated poisson regression (count data), continued with transforming the
#response for the sake of this class 
#normal assumption not best because data is non-negative and discrete

options(contrasts=c("contr.sum","contr.poly"))
before.mod2 <- aov(sqrt.waste.b ~ gender * year) #with interaction
anova(before.mod2) #interaction not significant, but continue

before.mod3 <- aov(sqrt.waste.b ~ gender + year) #without interaction
anova(before.mod3,before.mod2) #interaction not significant

#try cubed root
before.mod4 <- aov(cubed.waste.b ~ gender * year) #with interaction
Anova(before.mod4,type="III")

Anova(before.mod2,type="III") #all main effects significant
Anova(before.mod2,before.mod3,type="III") #interaction not significant

##conclude that both gender and year in school are significent predictors for amount wasted
#sig diff between means

#Anova(before.mod2,type="II") #gender significant..
Anova(before.mod3,type="II") #no intercept
sum.mod3 <- Anova(before.mod3,type="III")

#residuals vs fits plot
#before transformation
e.mod1 <- resid(before.mod1)
yhat.mod1 <- predict(before.mod1)
plot(yhat.mod1,e.mod1,main="residuals vs. fitted values",xlab="fitted values",ylab="residuals")
abline(h=0)
#plot indicates non-linearity since a higher proportion of points fall beneath line, could also indicates
#outliers in the plot and perhaps non-constant variance (test for this -> levene's)

hist(resid(before.mod1))
hist(resid(before.mod2))

#after transformation
e.mod2 <- resid(before.mod2)
yhat.mod2 <- predict(before.mod2)
plot(yhat.mod2,e.mod2,main="residuals vs. fitted values",xlab="fitted values",ylab="residuals")
abline(h=0)
#plot indicates a bit of variation in variances, test using levene

leveneTest(before.mod1)
leveneTest(before.mod2)
leveneTest(before.mod4)
#H0: group variances all equal. high p-value -> can conclude variances equal

#Shapiro-Wilk normality of error test
shapiro.test(sqrt.waste.b) #p-value of 0... error terms not normally distributed
shapiro.test(cubed.waste.b)

#Assumptions have been met:
#linearity (see qqplot), independence (no trend in resids vs fitted), equal variance, and errors are
#normally distributed #all these assumptions are reasonable


#-----Method of analysis
#Group means waste.b
ybar.groups.b <- round(tapply(waste.b,list(gender, year), mean),2)
ybar.groups.b

ybar.dot.j <- round(tapply(waste.b,list(year), mean),2) #sum over all a levels
ybar.i.dot <- round(tapply(waste.b,list(gender), mean),2) #sum over all b levels
overall.mean <- mean(waste.b)

round(tapply(sqrt.waste.b,list(gender), median),2) #median of genders
round(tapply(sqrt.waste.b,list(year), median),2) #median of year

#Group sample sizes
groups.size <- tapply(waste.b,list(gender, year), length)
groups.size #smallest group sample size of 11

#Pairwise mean comparisons
TukeyHSD(before.mod3, c("year")) #significant difference btwn jr-fr and sr-fr and so-fr
TukeyHSD(before.mod3, c("gender"))
TukeyHSD(before.mod3, c("gender","year")) #why not significant between genders??

#--------

#Get ANOVAS for factor effects on waste before
before.mod1 <- aov(waste.b ~ gender + year)
anova(before.mod1)
before.mod2 <- aov(waste.b ~ gender * year) #include interaction
anova(before.mod2) #interaction not significant
anova(before.mod1,before.mod2) #no significant difference
before.mod3 <- aov(waste.b ~ year) #remove gender effect
anova(before.mod3)
anova(before.mod1,before.mod3) #no significant difference


#Get ANOVAS for factor effects on waste after
after.mod1 <- aov(waste.a ~ gender + year)
anova(after.mod1)
after.mod2 <- aov(waste.a ~ gender * year) #interaction not significant
anova(after.mod2)
anova(after.mod1,after.mod2) #no significant difference


#Look at group means!! #before
ybar.groups.b <- round(tapply(waste.b,list(gender, year), mean),2)
ybar.groups.b
round(tapply(waste.b,list(year), mean),2)
round(tapply(waste.b,list(gender), mean),2)
mean(waste.b)

#after
ybar.groups.a <- round(tapply(waste.a,list(gender, year), mean),2)
ybar.groups.a
round(tapply(waste.a,list(year), mean),2)
round(tapply(waste.a,list(gender), mean),2)
mean(waste.a)

qqnorm(y=waste.b)
qqnorm(y=sqrt(waste.b))
qqnorm(y=log(waste.b))

#test for equal variances... if not, use power transformation (see wiki)

#ggplot(data=dat,aes(x=dat$year,y=dat$waste.b)) + geom_boxplot()
library(wesanderson)
ggplot(data=dat,aes(x=year,y=sqrt(dat$waste.b),fill=year)) + geom_boxplot() + 
  scale_fill_manual(values=wes_palette(n=5, name="FantasticFox"))

ggplot(data=dat,aes(x=dat$gender,y=sqrt(dat$waste.b))) + geom_boxplot()

ybar.groups.b.sd <- round(tapply(waste.b,list(gender, year), sd),2)
ybar.groups.b.sd

#Group sample sizes
n.groups.b <- tapply(waste.b,list(gender, year), length)
n.groups.b

#Group standard deviation
sd.groups.b <- tapply(waste.b,list(gender, year), sd)
sd.groups.b


#Pairwise means
TukeyHSD(before.mod1, c("year")) #significant difference btwn jr-fr and sr-fr


#Plot the residuals versus fitted values, and obtain a normal probability plot of residuals
e.mod1 <- resid(before.mod1)
yhat.mod1 <- predict(before.mod1)
plot(yhat.mod1,e.mod1,main="residuals vs. fitted values",xlab="fitted values",ylab="residuals")
abline(h=0)
#points are randomly scattered around the line -> linear assumption is reasonable
#residuals form a band around the line -> constant variance assumption reasonable
#no one residual stands out -> suggests no outliers
?write.csv
write.csv(dat,file="project/CCdata_final.csv")


library(wesanderson)
ggplot(data=dat,aes(x=year,y=sqrt.waste.b,fill=year)) + geom_boxplot() + 
  scale_fill_manual(values=wes_palette(n=5, name="FantasticFox")) + 
  labs(x = "Year") + labs(y="Sqrt weekly number of consumables") + 
  labs(title="Sqrt number of consumables by year in school")

ggplot(data=dat,aes(x=gender,y=sqrt.waste.b,fill=gender)) + geom_boxplot() + 
  scale_fill_manual(values=wes_palette(n=2, name="Darjeeling")) + 
  labs(x = "Gender") + labs(y="Sqrt weekly number of consumables") + 
  labs(title="Sqrt number of consumables by gender")

ggplot(data=dat,aes(x=waste.b,color=waste.b)) + geom_bar(fill="steelblue") + 
  labs(x="Weekly number of consumables")



par(mfrow=c(2,3))
qqnorm(waste.b, main="Normal Q-Q Plot, original")
qqline(waste.b)

#histogram of residuals
hist(resid(before.mod1),main="histogram of residuals")

#residuals vs fits plot
e.mod1 <- resid(before.mod1)
yhat.mod1 <- predict(before.mod1)
plot(yhat.mod1,e.mod1,main="residuals vs. fitted values",xlab="fitted values",ylab="residuals")
abline(h=0)



#after response transformation...
sqrt.waste.b <- sqrt(waste.b) #square root transformation to reduce right skew
before.mod2 <- aov(sqrt.waste.b ~ gender * year) #with interaction and transformed y

#normal probability plot of residuals
qqnorm(sqrt.waste.b, main="Normal Q-Q Plot, transformed") #helps but zero-inflation still apparent
qqline(sqrt.waste.b)

#histogram of residuals
hist(resid(before.mod2),main="histogram of residuals")

#residuals vs fits plot
e.mod2 <- resid(before.mod2)
yhat.mod2 <- predict(before.mod2)
plot(yhat.mod2,e.mod2,main="residuals vs. fitted values",xlab="fitted values",ylab="residuals")
abline(h=0)