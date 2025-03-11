
Pavyzdys 1. 

x <- 1:10   
mat=matrix(data=0, nrow=10, ncol=100) 
for(i in 1:10) {
mat[i,1:100]=rnorm(100)
}
mat
v<-apply(mat,1, mean)
s<-apply(mat,1, sd)
plot(x,v,ylim=c(-1, 1), pch=16, cex=1)
arrows(x, v+qnorm(0.05)*s/sqrt(100),  x, v+qnorm(1-0.05)*s/sqrt(100), code=3,
angle=90, length=0.1,col='blue')
abline(h=0)

Pavyzdys 2. 

library(carData)
data(Davis)
attach(Davis)
Davis[1:20,]

meanst<-function(x,conf.level=0.95){
n <- length(x)
kvant<-(1-conf.level)/2
mean(x)+c(qt(kvant,df=n-1)*sd(x)/sqrt(n),-qt(kvant,df=n-1)*sd(x)/sqrt(n)) }
meanst(weight[sex=="F"],conf.level=0.95)
meanst(weight[sex=="F"])
meanst(weight[sex=="F"],conf.level=0.85)
mean(weight[sex=="F"])
Pavyzdys 3. 

library(Hmisc)
smean.cl.normal(weight[sex=="F"])
meanst(weight[sex=="F"],conf.level=0.95)

smean.cl.normal(weight[sex=="F"],conf.int=0.99)
meanst(weight[sex=="F"],conf.level=0.99)

library(Rmisc)
CI(weight[sex=="F"],ci=0.95)

Pavyzdys 4. 

smean.cl.boot(weight[sex=="F"])

Pavyzdys 5. 

median(weight[sex=="F"])
smedian.hilow(weight[sex=="F"], conf.int=0.95)

Pavyzdys 6. 

library(Hmisc)
binconf(687, 987,alpha = 0.05)
binconf(687, 987,alpha = 0.05, method="wilson")
binconf(687, 987,alpha = 0.05, method="asymptotic")
binconf(687, 987,alpha = 0.05, method="all")

Pavyzdys 7. 

library(Rmisc)
data.frame(sex,weight)
group.CI(weight~sex,data=data.frame(sex,weight),ci = 0.95)



