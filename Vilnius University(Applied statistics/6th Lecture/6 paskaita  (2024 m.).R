


pnorm(490.67,mean=450,sd=20,lower.tail = FALSE)
qnorm(0.95,mean=450,sd=20)

Pavyzdys 1. 

library(carData)
Davis[1:20,]
attach(Davis)

t.test(weight[sex=="M"],mu=80)

t.test(weight[sex=="M"],mu=80,conf.level=0.95)
t.test(weight[sex=="M"],mu=80,conf.level=0.99)

t.test(weight[sex=="M"],mu=80,conf.level=1-0.001712)

t.test(weight[sex=="M"],mu=80,alternative="less")

t.test(weight[sex=="M"],mu=80,alternative="greater")

Pavyzdys 2. 

library(Ecdat)
Wages1[1:10,]
attach(Wages1)

t.test(exper[sex=="female"],exper[sex=="male"],
var.equal=FALSE,alternative="less")

t.test(exper[sex=="female"],exper[sex=="male"],
var.equal=TRUE,alternative="less")

Pavyzdys 3.

var.test(exper[sex=="female"],exper[sex=="male"])

Pavyzdys 4.

wilcox.test(exper[sex=="female"],exper[sex=="male"])

Pavyzdys 5.

library(datasets)
airquality 
attach(airquality)
boxplot(Ozone~Month)
kruskal.test(Ozone~Month)


boxplot(Ozone[Month==7],Ozone[Month==8])

median(Ozone[Month==7],na.rm = TRUE)
median(Ozone[Month==8],na.rm = TRUE)
wilcox.test(Ozone[Month==7],Ozone[Month==8])

Pavyzdys 6.

binom.test(a,b,p = c/d,alternative = "greater")
binom.test(136,1000,p = 0.15)
binom.test(136,1000,p = 0.15, alternative = "less")
binom.test(136,1000,p = 0.15, alternative = "greater")
binom.test(1360,10000,p = 0.15, alternative = "less")



