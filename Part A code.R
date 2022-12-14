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


dataset<-read.csv(file.choose(),header = T)
summary(dataset)
cov(dataset)

plot(dataset)
hist(dataset$X1)

hist(dataset$Y,prob=TRUE, main='Life-expectancy',xlab = 'Y',col="grey")#?????????1
lines(density(dataset$Y),col="blue",lwd=2)

hist(dataset$X2,prob=TRUE, main='HIV - Estimated number of people that have been infected',xlab = 'X2',col="grey")#????????? 2
lines(density(dataset$X2),col="blue",lwd=2)

hist(dataset$X4,prob=TRUE, main='Avarage income per person ($)',xlab = 'X4',col="grey")#?????????3
lines(density(dataset$X4),col="blue",lwd=2)

hist(dataset$X7,prob=TRUE, main='Ciggaret consumption (%)',xlab = 'X7',col="grey")#?????????4
lines(density(dataset$X7),col="blue",lwd=2)

plot(ecdf(dataset$Y),main='Life-expectancy',verticals = TRUE, col = "darkgreen")#??????? ??????1
plot(ecdf(dataset$X2),main='HIV - Estimated number of people that have been infected',verticals = TRUE, col = "darkgreen")#??????? ??????2
plot(ecdf(dataset$X4),main='Avarage income per person ($)',verticals = TRUE, col = "darkgreen")#??????? ??????3
plot(ecdf(dataset$X7),main='Ciggaret consumption (%)',verticals = TRUE, col = "darkgreen")#??????? ??????4


plot(dataset)

fit <- lm(Y~X4, data=dataset)   ##1????? ?????
plot(x=dataset$X4,y=dataset$Y,xlab="Income per person",ylab="Life expectancy")  
lines(dataset$X4, fitted(fit), col="blue")

fit <- lm(X6~X3, data=dataset)   ##2????? ?????
plot(x=dataset$X3,y=dataset$X6,xlab="Malaria",ylab="Density")  
lines(dataset$X3, fitted(fit), col="blue")

fit <- lm(X6~X4, data=dataset)   ##3????? ?????
plot(x=dataset$X4,y=dataset$X6,xlab="Income per person",ylab="Density")  
lines(dataset$X4, fitted(fit), col="blue")

fit <- lm(X5~X7, data=dataset)   ##4????? ?????
plot(x=dataset$X7,y=dataset$X5,xlab="Ciggaret",ylab="Alcohol consumption")  
lines(dataset$X7, fitted(fit), col="blue")

datasetfit <- lm(Y~X1, data=dataset)  ##5????? ?????
plot(x=dataset$X1,y=dataset$Y,xlab="Air pollution",ylab="Life expectancy") 
lines(dataset$X1, fitted(fit),col="blue")



cbind(Freq=table(cut(dataset$X4,breaks=seq(0,70000,5000))),relative= prop.table(table(cut(dataset$X4,breaks = seq(0,70000,5000))))) # ?????-???? ?????? ?? ?????1
cbind(Freq=table(cut(dataset$X3,breaks=seq(-5000,85000,5000))),relative= prop.table(table(cut(dataset$X3,breaks = seq(-5000,85000,5000))))) #?????- ???? ?????? ?? ????? 2

cbind(Freq=table(cut(dataset$X5,breaks=seq(0,35,7)),  #1??????? ????????- ???? ?????? ?? ?????
cut(dataset$X7,breaks=seq(0,42,7))),
relative=prop.table(table(cut(dataset$X5,breaks=seq(0,35,7)), 
cut(dataset$X7,breaks=seq(0,42,7)))))

cbind(Freq=table(cut(dataset$X6,breaks=seq(0,1300,100)),#?????? ???????? ?????? ?????- ???? ?????? ?? ????? 2
cut(dataset$X1,breaks=seq(0,100,20))),
relative=prop.table(table(cut(dataset$X6,breaks=seq(0,1300,100)), 
cut(dataset$X1,breaks=seq(0,100,20)))))


