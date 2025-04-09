
Pavyzdys 1.

ts(c(1,2,3,4), frequency = 12, start = c(1914, 2))
ts(c(1,2,3,4), frequency = 4, start = c(1914, 1))
ts(c(1,2,3,4), frequency = 4, start = c(1914, 2))


Pavyzdys 2.
install.packages("fpp2")
library(fpp2)
library(tidyverse)
view(ausbeer)
mean(ausbeer)
meanf(ausbeer,level = 95,h=1)
meanf(ausbeer,h=1)
meanf(ausbeer,h=5)
meanf(ausbeer)
Pavyzdys 3.

naive(ausbeer,h=5)
snaive(ausbeer, h=16)

Pavyzdys 4.

rwf(ausbeer, h=5,drift="FALSE")
rwf(ausbeer, h=5,drift="TRUE")


Pavyzdys 5.

autoplot(naive(ausbeer,h=16))
autoplot(snaive(ausbeer,h=16))
autoplot(rwf(ausbeer,h=16,drif=TRUE))

Pavyzdys 6.

view(gold)
ts(gold,frequency = 365, start = c(1985, 1))



gold[1:70]
mean(gold[1:70],na.rm = TRUE)
install.packages("imputeTS")
library(imputeTS)
na_mean(gold[1:70])

Pavyzdys 7.

na_locf(gold[1:70])
na_locf(gold)

sum(is.na(gold))


is.na(gold)
which(is.na(gold))

na_locf(gold[1:70])
na_locf(gold[1:70],option = "nocb")


na_replace(gold[1:70],fill=5)

na_replace(gold[1:70])


na_seadec(gold)

na_kalman(gold)

statsNA(gold)

Pavyzdys 8.

y=print(na_kalman(gold[1:70]))
ggplot_na_imputations(gold[1:70], y)


Pavyzdys 9.

tsAirgap
tsAirgapComplete

trukst.r=na_seadec(tsAirgap)

ggplot_na_imputations(tsAirgap, trukst.r, tsAirgapComplete)


ggplot_na_distribution(tsAirgap)


Pavyzdys 10.
#Trend if we ignore seasonal changes
gold
laikas=print(as.numeric(time(gold)))
g=lm(gold~laikas)
summary(g)

Grafikai

plot(gold,type="l",main="Aukso kainos tiesinis trendas")
lines(predict(g),col="2")


Pavyzdys 11.

g2=lm(gold~time(gold)+I(laikas^2))
summary(g2)
plot(gold,type="l")
lines(predict(g2),col="2")


Pavyzdys 12.

g3=lm(gold~time(gold)+I(laikas^2)+I(laikas^3))
summary(g3)
plot(gold,type="l")
lines(predict(g3),col="2")


Pavyzdys 13. 

ausbeer
metai=print(factor(c(rep(1956:2009,rep(4,54)),2010,2010)))
ketvirciai=print(factor(c(rep(1:4,54),1,2)))
ausbeer.mk=lm(ausbeer~metai+ketvirciai)
summary(ausbeer.mk)
#trend seasonal white noise
Pavyzdys 14.
library(gam)
gold.splain20=gam(gold~s(laikas,20)) 
plot(gold,type="l")
lines(predict(gold.splain20),col="2")

Pavyzdys 15. 


na_kalman(gold)
filter(na_kalman(gold),rep(1,50)/50,sides =2)
plot(gold)
lines(filter(na_kalman(gold),rep(1,50)/50,sides =1),col=2)


Pavyzdys 16. 

library(fpp2)
ausbeer
#Paememe metini vidurki ir atemam(adityvus modelis)
aus_papildymas=print(c(ausbeer,419,488))
eilute=print(ts(aus_papildymas,start=c(1956,1), frequency=4))
ausmat_papildymas=print(matrix(aus_papildymas, nrow = 55, ncol =4,byrow = TRUE))
metinis_trend=print(apply(ausmat_papildymas,1,mean))
norm=print(sweep(ausmat_papildymas,1,metinis_trend))#atemam metini trenda is duomen
s_k=print(apply(norm,MARGIN=2,mean))
sum(s_k)


au_beseason=print(sweep(ausmat_papildymas,2,s_k))

laik=print(as.numeric(time(eilute)))
plot(laik,eilute,type="l")
lines(laik,au_beseason,,col="2")
legend("topleft", legend=c("pradinis", "be sezoni?kumo"),lty = 1,
col=c("1","2"))

Pavyzdys 17.

decompose(ausbeer)
plot(decompose(ausbeer))











