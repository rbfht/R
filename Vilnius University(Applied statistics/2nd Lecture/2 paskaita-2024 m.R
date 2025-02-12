1. Pad?ties ir sklaidos charakteristikos
Vidurkis
install.packages("carData")
library(carData)
data(Davis)
Davis
attach(Davis)
mean(height)

mean(repht[sex=="M"])
mean(repht[sex=="F"])

mean(repht[sex=="F"],na.rm=TRUE)
mean(repht[sex=="M"],na.rm=TRUE)

mean(height[sex=="M"],trim=0.1)
mean(height[sex=="F"],trim=0.1)
Mediana

median(height[sex=="M"])
mean(height[sex=="M"])

Moda

install.packages("modeest")
library(modeest)
mfv(height[sex=="F"])


Kvantiliai
?sort
?quantile
sort(height[sex=="M"])
quantile(height[sex=="M"],0.95)
quantile(height[sex=="M"],0.05)

Standartinio normaliojo skirstinio kvantiliai

empiriniai
n=rnorm(100)
quantile(n,0.05)
quantile(n,0.95)
plot(density(n))

teoriniai kvantiliai

qnorm(0.95)
qnorm(0.05)
qqnorm(n)
qqline(n)
atvirk?tin? komanda

pnorm(1.644854)
?pnorm

summary(height[sex=="M"])

Dispersija ir standartinis nuokrypis

var(height[sex=="M"])
sd(height[sex=="M"])

library(Hmisc)
smean.sd(height[sex=="M"])

2. Skirstinio formos charakteristikos

install.packages("moments")
library(moments)

Asimetrijos koeficientas

at<-rnorm(100000)
skewness(at)
base::plot(density(at))

install.packages("e1071")
library(e1071)

skewness(at,type= 1)
skewness(at,type= 2)
skewness(at,type=3)

?par
par(mfrow=c(2,1))
hist(c(10,11,12,13,20))
hist(c(1,2,30,31,32,33))

skewness(c(10,11,12,13,20))
skewness(c(1,2,30,31,32,33))

install.packages("lillies")
library(lillies)
data(simu_data)
simu_data[1:10,]
attach(simu_data)
base::plot(density(age_death))
e1071::skewness(age_death,type=3)

eksesas


library(moments)
kurtosis(at<-rnorm(100000))
kurtosis(at)

library(e1071)
kurtosis(at)

3. Grafin?s proced?ros

D??inis grafikas

library(carData)
data(Davis)
Davis
attach(Davis)

boxplot(height[sex=="F"])
horizontal=TRUE
boxplot(height[sex=="F"],horizontal=TRUE)
boxplot(Davis, col=rainbow(5))
boxplot(height~sex)
boxplot(height~sex, data=Davis, main="Vyr? ir moter? ugiai")
points(c(mean(height[sex=="F"]),mean(height[sex=="M"])),col="red")

Kvantili? grafikas

quantile(height[sex=="M"],1/88)
quantile(height[sex=="M"],3/88)
quantile(height[sex=="M"],4/88)
quantile(height[sex=="M"],87/88)

qqnorm(height[sex=="M"]) 
qqline(height[sex=="M"]) 

qqnorm(weight[sex=="M"]) 
qqline(weight[sex=="M"])







