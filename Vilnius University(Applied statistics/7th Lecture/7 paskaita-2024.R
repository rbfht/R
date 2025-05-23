Pavyzdys 1. 

library(UsingR)
data(five.yr.temperature)
five.yr.temperature
attach(five.yr.temperature)

v<-print(mean(temps))
s<-print(sd(temps))

range(temps)

int<-print(c(10,30,50,70,90,110))  

dala<- cut(temps, int, right=TRUE) 
table(dala)
l<-as.matrix(table(dala))
l



a <- numeric(5)
b <- numeric(5)
p <- numeric(5)
for(i in 1:5) {
p[i] <- pnorm(30+20*(i-1), mean =v, sd = s)-pnorm(10+20*(i-1), mean =v, sd =s)
a[i] <- length(temps)*p[i]
b[i] <- (l[i,]-a[i])^2/a[i]
}
h<-cbind(l[1:5],p[1:5],a[1:5], b[1:5])
h

e<-c("(10,30]","(30,50]","(50,70]","(70,90]","(90,110]")

st<-c("O","P","E","(O-E)^2/E")

matrix(h, nrow = 5, ncol = 4,dimnames=list(e,st))

sum(h[,4])

Kriterijaus reik�m�
qchisq(0.05,df=4,lower.tail=FALSE)

I�vada:

Gauta kriterijaus reik�m�, priklauso kritinei sri�iai, tod�l hipotez�,
kad duomenys normal�s turime rimt� pagrind� atmesti.  

Pavyzdys 2.

sum(h[,2])
sumuodami santykinius da�nius lygiai 1 negauname.

Tod�l naudodami chisq.test renkam�s opcija rescale.p=TRUE

chisq.test(h[,1],p=h[,2],rescale.p=TRUE)

Pavyzdys 3.

shapiro.test(temps)

Pavyzdys 4.

library(MASS)
survey
attach(survey)
lent<-print(table(Smoke))

Stebimi da�niai

O<-print(cbind(table(Smoke)))
sum(O)

Tik�tinos tikimyb�s 

Heavy<-print(0.03)
Never<-print(0.92)
Occas<-print(0.03)
Regul<-print(0.02)

p<-print(rbind(Heavy,Never,Occas, Regul))
colnames(p)<-"pop_prop"
p
sum(p)

Tik�tini da�niai

E<-print(p*sum(O))
Skai�iuojame chi kvadrat�

s<-print((O-E)^2/E)

Galutin� lentel�

g<-print(cbind(O,p,E,s))
colnames(g)<-c("O","p","E","s")
g

sum(s)

Randame kritin� reik�m�

qchisq(0.05,df=3,lower.tail=FALSE)

chisq.test(g[,1],p=g[,2])

plot(g[,1],type="l")
lines(g[,3],col='2')
legend("topright",c("Stebimi da�niai O","Tik�tini da�niai E"),
lt=1, col=c("black","red"))

I�vada:
Nulin� hipotez� turime pagrind� atmesti. 

Tik�tinos reik�m�s su kuriomis nulin�s hipotez�s neatmestume

chisq.test(g[,1],p=g[,2])$expected


Pavyzdys 5.

library(MASS)
survey
attach(survey)


lent<-print(table(Sex,Smoke))
chisq.test(lent)


I�vada: Didel� p-value = 0.3139 reik�m� indikuoja, kad nulin�s hipotez�s,  
r�kymo ypatumai ir lytis yra nesusij� kintamieji atmesti n�ra pagrindo.

Randame tik�tinas reik�mes, jei tai nesusij� kintamieji 
chisq.test(lent)$expected

Jei atliktume test� su �iomis reik�m�mis gautume p-value = 1
chisq.test(chisq.test(lent)$expected)

lent[1,]
chisq.test(lent)$expected[1,]

Grafinis stebim� ir tik�tin� da�ni� palyginimas

plot(lent[1,],type="l")
lines(chisq.test(lent)$expected[1,],col='2')
legend("topright",c("Stebimi da�niai O","Tik�tini da�niai E"),
lt=1, col=c("black","red"))

Pavyzdys 6.

fisher.test(lent)

Gautos p-value reik�m�s skiriasi nedaug 
nulin�s hipotez�s atmesti neturime pagrindo. 














