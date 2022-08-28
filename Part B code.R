.libPaths("D:/soft/r/3.6")#this row only for labs
library(rlang)
library(MASS)
library(fitdistrplus)
library(magrittr)
library(dplyr)
library(lazyeval)
library(parallel)
library(e1071)
library(plotly)
library("Hmisc")
library(corrplot)
library(tseries)
#install.packages("strucchange", repos = "http://cran.us.r-project.org")
library(strucchange)
#install.packages("Rlof", repos = "http://cran.us.r-project.org")
library(Rlof)
##-----------------------------UPLOADE DATA------------------------------------------------
dataset<-read.csv(file.choose(),header = TRUE)
summary(dataset)
##-------------------------------------תרשימי פיזור---------------------------------------
fit <- lm(Y~X1, data=dataset)   ##1תרשים פיזור
plot(x=dataset$X1,y=dataset$Y,xlab="Outdoor air pollution (%)",ylab="Life expectancy")  
lines(dataset$X1, fitted(fit), col="blue")

fit <- lm(Y~X2, data=dataset)   ##2תרשים פיזור
plot(x=dataset$X2,y=dataset$Y,xlab="HIV - Estimated number of people that have been infected",ylab="Life expectancy")  
lines(dataset$X2, fitted(fit), col="blue")

fit <- lm(Y~X3, data=dataset)   ##3תרשים פיזור
plot(x=dataset$X3,y=dataset$Y,xlab="malaria - Estimated number of people that have been infected",ylab="Life expectancy")  
lines(dataset$X3, fitted(fit), col="blue")

fit <- lm(Y~X4, data=dataset)   ##4תרשים פיזור
plot(x=dataset$X4,y=dataset$Y,xlab="Avarage income per person ($)",ylab="Life expectancy")  
lines(dataset$X4, fitted(fit), col="blue")

fit <- lm(Y~X5, data=dataset)   ##5תרשים פיזור
plot(x=dataset$X5,y=dataset$Y,xlab="Alcohol consumption per person (liters, year)",ylab="Life expectancy")  
lines(dataset$X5, fitted(fit), col="blue")

fit <- lm(Y~X6, data=dataset)   ##6תרשים פיזור
plot(x=dataset$X6,y=dataset$Y,xlab="density per squre (km)",ylab="Life expectancy")  
lines(dataset$X6, fitted(fit), col="blue")

fit <- lm(Y~X7, data=dataset)   ##7תרשים פיזור
plot(x=dataset$X7,y=dataset$Y,xlab="Ciggaret consumption (%)",ylab="Life expectancy")  
lines(dataset$X7, fitted(fit), col="blue")

fit <- lm(Y~X8, data=dataset)   ##8תרשים פיזור
plot(x=dataset$X8,y=dataset$Y,xlab="Continent",ylab="Life expectancy")  

fit <- lm(Y~X9, data=dataset)   ##9תרשים פיזור
plot(x=dataset$X9,y=dataset$Y,xlab="Member of OECD",ylab="Life expectancy")  

##---------------------------------------בדיקת מובהקות------------------------------------------
fit1 <- lm(Y ~ X1,data=dataset)
fit2 <- lm(Y ~ X2,data=dataset)
fit3 <- lm(Y ~ X3,data=dataset)
fit4 <- lm(Y ~ X4,data=dataset)
fit5 <- lm(Y ~ X5,data=dataset)
fit6 <- lm(Y ~ X6,data=dataset)
fit7 <- lm(Y ~ X7,data=dataset)
fit8 <- lm(Y ~ X8,data=dataset)
fit9 <- lm(Y ~ X9,data=dataset)
summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)
summary(fit5)
summary(fit6)
summary(fit7)
summary(fit8)
summary(fit9)

##----------------------------------------מטריצת המקדמים של פירסון------------------------------
x<- cor(dataset[sapply(dataset, function(x) !is.factor(x))])
View(x)
##------------------------------------איחוד קטגורית יבשות---------------------------------------
dataset1<-read.csv(file.choose(),header = TRUE)
fit <- lm(Y~X8, data=dataset1)   ##7תרשים פיזור
plot(x=dataset1$X8,y=dataset1$Y,xlab="Continent",ylab="Life expectancy")  

#-----------------------------------משתני אינטרקציה---------------------------------------
  #life expectancy:income-Continent
color<-ifelse(dataset1$X8==1,"green",ifelse(dataset1$X8==2,"purple","red"))
plot(x=dataset1$X4,y=dataset1$Y, main='Avarage income per person VS. Life expectancy',xlab="Avarage income per person",ylab="Life expectancy",col=color)
abline(lm(dataset1$Y~dataset1$X4,data=dataset1, subset
          =color=="green"),col="green")
abline(lm(dataset1$Y~dataset1$X4,data=dataset1, subset
          =color=="purple"),col="purple")
abline(lm(dataset1$Y~dataset1$X4,data=dataset1, subset
          =color=="red"),col="red")
legend("bottomright", inset=0, title="Continent", c("1","2","6"), pch=c(1,1), col =
         c("green","purple","red"), horiz=F, cex=1)

#life expectancy:Malaria-Continent
color<-ifelse(dataset1$X8==1,"green",ifelse(dataset1$X8==2,"purple","red"))
plot(x=dataset1$X3,y=dataset1$Y, main='malaria - Estimated number of people that have been infected VS. Life expectancy',xlab="malaria - Estimated number of people that have been infected",ylab="Life expectancy",col=color)
abline(lm(dataset1$Y~dataset1$X3,data=dataset1, subset
          =color=="green"),col="green")
abline(lm(dataset1$Y~dataset1$X3,data=dataset1, subset
          =color=="purple"),col="purple")
abline(lm(dataset1$Y~dataset1$X3,data=dataset1, subset
          =color=="red"),col="red")
legend("bottomright", inset=0, title="Continent", c("1","2","6"), pch=c(1,1), col =
         c("green","purple","red"), horiz=F, cex=1)

#life expectancy-Air Pollution-Member of the OECD
color<-ifelse(dataset1$X9==1,"green","red")
plot(x=dataset1$X1,y=dataset1$Y, main='Air Pollution VS. life expectancy',xlab="Air Pollution",ylab="life expectancy",col=color)
abline(lm(dataset1$Y~dataset1$X1,data=dataset1, subset =color=="green"),col="green")
abline(lm(dataset1$Y~dataset1$X1,data=dataset1, subset =color=="red"),col="red")
legend("topright", inset=0, title="OECD", c("1","0"), pch=c(1,1), col = c("green","red"), horiz=F, cex=1)

##----------------------------------FULLMODEL-------------------------------------
fullModel <- lm(formula = dataset1$Y ~ dataset1$X1 + dataset1$X2 + dataset1$X3 + dataset1$X4
                + dataset1$X5 + dataset1$X6 + dataset1$X7 +factor(dataset1$X8)*dataset1$X3+ factor(dataset1$X8)*dataset1$X4 +
                  factor(dataset1$X9)*dataset1$X1 , data =dataset1)
summary(fullModel)

#------------------------------------רגרסיה לפנים------------------------------------------
forwardModelAIC <- step(lm(dataset1$Y~1,data=dataset1),scope=(~(dataset1$X1 + dataset1$X2 ##FOR AIC
          + dataset1$X3 + dataset1$X4 +dataset1$X5 + dataset1$X6 + dataset1$X7 +
        +factor(dataset1$X8)*dataset1$X3+ factor(dataset1$X8)*dataset1$X4 +
       factor(dataset1$X9)*dataset1$X1)),direction="forward",trace = TRUE, k = 2)
summary (forwardModelAIC)
AIC(forwardModelAIC)

forwardModelBIC <- step(lm(dataset1$Y~1,data=dataset1),scope=(~(dataset1$X1 + dataset1$X2 ##FOR BIC
                        + dataset1$X3 + dataset1$X4 +dataset1$X5 + dataset1$X6 + dataset1$X7 +
                      +factor(dataset1$X8)*dataset1$X3+ factor(dataset1$X8)*dataset1$X4 +
                 factor(dataset1$X9)*dataset1$X1)),direction="forward",trace = TRUE, k = log(101))
summary (forwardModelBIC)
#------------------------------------רגרסיה לאחור------------------------------------------
BackModelAIC <- step(fullModel,direction="backward", K=2) ##FOR AIC
summary (BackModelAIC)

BackModelBIC <- step(fullModel,direction="backward", k = log(101)) ##FOR BIC
summary (BackModelBIC)
#-----------------------------------רגרסיה בצעדים------------------------------------------
StepsModelAIC <- step(fullModel,direction="both", K=2) ##FOR AIC
summary (StepsModelAIC)

StepsModelBIC <- step(fullModel,direction="both", k = log(101)) ##FOR BIC
summary (StepsModelBIC)


##--------------------------MODEL AFTER CHECKS---------------------------------

NewModel <-lm(dataset1$Y~dataset1$X4 + factor(dataset1$X8) + dataset1$X6)
summary (NewModel)

#----------------------שיוויון שונויות + ליניאריות המודל בדיקה --------------

fitted<-fitted(NewModel) # predicted values
residuals<-residuals(NewModel) # residuals
s.e_res <- sqrt(var(residuals))
stan_residuals<-(residuals(NewModel)/s.e_res)
plot(stan_residuals~fitted,xlab="Predicted value",ylab = "Normalized error")
abline(lm(stan_residuals~fitted))

#-----------------------הסקה על נורמליות-----------------------------------------------------

#------------------------------QQPLOT---------------------------------
qqnorm(dataset1$stan_residuals)
abline(a=0,b=1)
hist(stan_residuals, xlab="Normalized Error",main="Histogram of normalized error")
#-------------------------------KS + SW TEST--------------------------------
ks.test(x= stan_residuals,y="pnorm",alternative = "two.sided", exact = NULL)  ##kstest
shapiro.test(stan_residuals) ##shapiro test
#---------------------------------CHOW TEST---------------------------

sctest(dataset1$Y~dataset1$X4+factor(dataset1$X8) + dataset1$X6, data = dataset1, type = "Chow")

##---------------------------------Hypothesis Test----------------------------------------------
qf(0.95,df1=4,df2=101)
anova(NewModel)
##----------------------------------------Shipor-------------------------------------------------
  shiporModel <- lm(formula = dataset1$Y~log(dataset1$X4) + factor(dataset1$X8) + dataset1$X6)
summary(shiporModel)

#------------------------------------רגרסיה לפנים------------------------------------------
forwardshipoModel <- step(lm(dataset1$Y~1,data=dataset1),scope=(~(log(dataset1$X4)+ factor(dataset1$X8) + dataset1$X6)),direction="forward",k=2)
summary (forwardshipoModel)

forwardshipoModel1 <- step(lm(dataset1$Y~1,data=dataset1),scope=(~(log(dataset1$X4) + factor(dataset1$X8) + dataset1$X6)),direction="forward",k=log(101))
summary (forwardshipoModel1)
  #------------------------------------רגרסיה לאחור------------------------------------------
BackshipoModel <- step(forwardshipoModel,direction="backward",k=2)
summary (BackshipoModel)

BackshipoModel1 <- step(forwardshipoModel,direction="backward", k= log(100))
summary (BackshipoModel1)

#-----------------------------------רגרסיה בצעדים------------------------------------------

StepsshipoModel <- step(forwardshipoModel,direction="backward",k=2)
summary (BackshipoModel)

StepsshipoModel1 <- step(forwardshipoModel,direction="backward", k= log(100))
summary (StepsshipoModel1)
#---------------------------------------מתאם פירסון לX4 לאחר טרנספורמציה---------------------------
cor(x=log(dataset1$X4),y=dataset1$Y, method = c("pearson"))

