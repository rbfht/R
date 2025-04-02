
Pavyzdys 1.

library(AER)
data(CPS1985)
CPS1985
CPS1985[1:20,]
attach(CPS1985)

(we1=lm(wage~experience))
summary(we1)


Pavyzdys 2.

(we2<-lm(wage~experience+I(experience^2)))
summary(we2)


Pavyzdys 3.

(we3<-lm(wage~experience+I(experience^2)+I(experience^3)))
summary(we3)


Tiesinis modelis nuo dviejø kintamøjø

Pavyzdys 4.

(wage.ed.exp<-lm(wage~education+experience))
summary(wage.ed.exp)


Pavyzdys 5.

library(QuantPsyc)
lm.beta(wage.ed.exp)


library(car)
avPlots(wage.ed.exp)



Pavyzdys 6.

Modelio prielaidu tikrinimas

1. Ar liekamosios paklaidos normalios ir ar prognozuojamas kintamasis normalusis

plot(density(wage.ed.exp$residuals))
shapiro.test(wage.ed.exp$residuals)

2. Regresoriu koreliacija ir multikolinearumas


(duom=CPS1985[,c("wage","experience","education")])[1:20,]

cor(duom)
library(car)
vif(wage.ed.exp)

3. Iskirtys



(cooksd=cooks.distance(wage.ed.exp))

plot(cooksd, pch="*", cex=2,
main="Modelio koeficientø suminis pokytis
(Influential Obs by Cooks distance)")


(riba=4/nrow(CPS1985)) 

abline(h = riba, col="red",lwd=3,lty=2)

influencePlot(wage.ed.exp)


(dfbeta=as.data.frame(dfbetas(wage.ed.exp)))

(riba=2/sqrt(nrow(CPS1985)))
(itak=which(abs(dfbeta)>riba))




4. Homoskedastiskumas

predict(wage.ed.exp)
wage.ed.exp$fitted
wage.ed.exp$residuals
plot(wage.ed.exp$fitted,wage.ed.exp$residuals)

abline(0,0)

as.numeric(names(which(wage.ed.exp$residuals>30)))

wage.ed.exp$residuals[-171]
wage.ed.exp$fitted[-171]
plot(wage.ed.exp$fitted[-171],wage.ed.exp$residuals[-171])
abline(0,0)

library(lmtest)
bptest(wage.ed.exp)

Pavyzdys 7.

wage.ed.gender=lm(wage~education+factor(gender))
summary(wage.ed.gender)

vyrai=relevel(gender,ref="female")
wage.ed.male=lm(wage~education+vyrai)
summary(wage.ed.male)


moterys=relevel(gender,ref="male")
wage.ed.female=lm(wage~education+moterys)
summary(wage.ed.female)





