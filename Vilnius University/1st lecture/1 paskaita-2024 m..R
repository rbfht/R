
Ivadas 

1. Reiksmes priskirimas kintamajam

x=2
x
y<-3
y

(y=4)
z=print(3)

2. Sekos

(K=1:10)

(E=c(2.1,3,4,5.3))

(F=c(1,2,rep(3,5)))

(G=seq(5, 10, 2))

3. sarasas, matricos

(H=list(c(1,2,3),c(2.1,3,4,5.3)))


matrix(c(2,3,4,5), nrow = 2, ncol = 2)

matrix(c(2,3,4,5), nrow = 2, ncol = 2, byrow = FALSE)

matrix(c(2,3,4,5), nrow = 2, ncol = 2, byrow = TRUE)
?matrix
matrix(data=0,nrow = 2, ncol = 5)

4. ciklai

(b=as.numeric(10))
for (i in 1:5) {
b[i+1]=b[i]+1
}
b

5. Paketai

install.packages("UsingR")
library(UsingR)

Gaunamas paketo duomenu sarasas

library(help=UsingR)
library(UsingR)
?homedata
homedata
 
homedata[1:10,]

homedata[,1]

homedata[1:10,1,]

homedata[1:10,]$y1970

yellowfin
yellowfin$count
attach(yellowfin)
count
sum(count<=3)
sum(count<=5&count>=2)

6. Grafikai ir spalvinimas.
(gelt.tunas=yellowfin$count)
plot(gelt.tunas, type="l")
plot(year,gelt.tunas,type="l")





(x=seq(-4,4,0.1))

y=function(x){
(r=x^2)
}

y(x)

plot(x,y(x),type="l")
points(c(-2,2),c(y(-2),y(2)))
(a=seq(2,4,length=100))
(funkc_arg=c(a,sort(a, decreasing = TRUE)))

(funkc_reik=c(rep(0,100),y(sort(a, decreasing = TRUE))))

polygon(funkc_arg,funkc_reik,col="lightblue")

7. Normalioji kreivë ir jos grafikas

x<-seq(-3,7,by=0.1) 
plot(x,1/(0.5*sqrt(2*pi))*exp(-(x-2)^2/(2*0.5^2)),ylab="f(x)",type="l",col="blue1")
lines(x,1/(sqrt(2*pi))*exp(-(x-2)^2/2),col="red",type="l")
lines(x,1/(2*sqrt(2*pi))*exp(-(x-2)^2/(2*2^2)),col="green")
legend("topleft",c("sigma=0.5","sigma=1","sigma=2"),lty=1, col=c("blue1","red","green"))

8. Histograma

(b_l=rnorm(1000,mean=450,sd=50))
min(b_l)
max(b_l)
(max(b_l)-min(b_l))/5

sum(b_l>=400&b_l<=450)

int.galai=as.numeric(5)
for (i in 0:5) {
int.galai[i+1]=min(b_l)+i*(max(b_l)-min(b_l))/5
}
int.galai

pj=cut(b_l, int.galai,right=TRUE)
table(pj)

hist(b_l,main="Prie bankomato praleisto laiko histograma",breaks=int.galai,
border="blue",col="darkseagreen1",labels=TRUE,freq=FALSE)


hist(b_l,main="Prie bankomato praleisto laiko histograma",breaks=int.galai,
border="blue",col="darkseagreen1",labels=TRUE,freq = FALSE,prob = TRUE,
lines(density(b_l)))

Automatinis stulpeliø parinkimas (pagal taisyklæ)

par(mfrow=c(2,1))
hist(b_l,breaks="FD",border="blue",col="darkseagreen1")
hist(b_l,breaks="Sturges",border="blue",col="darkseagreen1")



9. Diagramos

#Skritulinë diagrama
library(Ecdat)
data(Wages1)
attach(Wages1) 
Wages1[1:20,] 
range(wage)
int.galai=c(0,3,6,10,20,50)
(int.wage=cut(wage,int.galai))
table(int.wage)

(sds=proportions(table(int.wage)))
(procent=round(sds*100))


pie(procent)

opcijos

main
labels
col
legend
fill
cex

pie(procent,main="Atlyginimø pasiskirstymas doleriais per valandà 1980m.")

(z=c("iki 3", "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20",
"virð 20"))

paste(procent)
(z=paste(procent,"%"))
pie(procent,main="Atlyginimø pasiskirstymas doleriais per valandà 1980m.",labels=z)


pie(procent,main="Atlyginimø pasiskirstymas doleriais per valandà 1980m.",labels=z,col=rainbow(5))
legend("topright",c("iki 3" , "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20", "virð 20"))
legend("topright",c("iki 3" , "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20", "virð 20"),fill=rainbow(5))
legend("topright",c("iki 3" , "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20", "virð 20"),
cex=0.8,fill=rainbow(5))

library(plotrix)
pie3D(procent,main="Atlyginimø pasiskirstymas doleriais per valandà 1980 m.",labels = z,explode = 0.1)
legend("topright", c("iki 3 ", "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20", "virð 20"),
cex = 0.8, fill = rainbow(5))



#Stulpelinë diagrama
library(carData)
data(Davis)
Davis
attach(Davis)
wage
int.galai<-c(0,3,6,10,20,50)
int.wage<-cut(wage,int.galai)
table(int.wage)
(s=barplot(table(int.wage),main="Atlyginimø pasiskirstymas doleriais per valandà 1980m.",col=rainbow(5)))

text(s, 0, round(table(int.wage)),pos=3)
legend("topright", c("iki 3 ", "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20", "virð 20"),
cex = 0.8, fill = rainbow(5))

cbind(table(int.wage))
proportions(cbind(table(int.wage)))*100
(h=round(proportions(cbind(table(int.wage)))*100))


barplot(table(int.wage))
barplot(h)
barplot(h,beside=TRUE)
barplot(h, beside=TRUE,main="Atlyginimø pasiskirstymas doleriais per valandà 1980m.",col=rainbow(5))
text(s, 0,round(h),pos=4) 
legend("topright", c("iki 3 ", "nuo 3 iki 6", "nuo 6 iki10", "nuo 10 iki 20", "virð 20"), cex = 0.8, fill = rainbow(5))


Wages1[1:20,] 
attach(Wages1)
table(sex,exper)
table(exper,sex)
barplot(table(sex,exper),beside=TRUE, main="Moterø -vyrø darbo staþo diagrama",col=c("blue1","green"), legend.text = TRUE) 

proportions(table(sex,exper))
barplot(proportions(table(sex,exper)),beside=TRUE,
main="Moterø -vyrø darbo staþo  santykiniø daþniø diagrama ",col=c("blue1","green"), legend.text = TRUE)


library(MASS)
survey[1:4,]
attach(survey)
Height
sort(Height)
(int.galai=c(150,160,170,180,190,200))     

(Height.int=cut(Height, int.galai))

table(Smoke,Height.int)

barplot(table(Smoke,Height.int),beside=TRUE,
main="Rûkanèiøjø pasiskirstymas  ávairaus ûgio grupëse",
col=c("blue1","green","plum1","lightblue"), legend.text = TRUE)








