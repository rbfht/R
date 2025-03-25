
1 u?duotis

Atraskite duomenis five.yr.temperature (biblioteka UsingR), tai 1995-2001 met?
temperat?riniai duomenys. Atrinkite kiekvien? met? sausio m?nesio duomenis.
1. Patikrinkite ?i? duomen? normalum? naudodami chi kvadrato test?;
2. Patikrinkite ?i? duomen? normalum? naudodami Shapiro-Vilko test?;
3. Nubr??kite normalaus skirstinio ir sausio m?nesio temperat?ros tankio
grafikus viename paveiksle.
4. Koki? dar grafin? proced?r? i? ankstesni? paskait? galite pritaikyti,
tikrinant sausio m?nesio temperat?rini? duomen? normalum?. Pritaikykite j?. 


1.

library(UsingR)
data(five.yr.temperature)
five.yr.temperature
attach(five.yr.temperature)
five.yr.temperature[1:35,]

(temp_sausio=temps[days<=31])
length(temp_sausio)
range(temp_sausio)



(temp_sausio=temps[days<=31])



v<-print(mean(temp_sausio))

s<-print(sd(temp_sausio))


int<-print(c(10,20,30,40,50,60))  

dala<- cut(temp_sausio, int, right=TRUE) 
table(dala)
l<-as.matrix(table(dala))
l


a <- numeric(5)
b <- numeric(5)
p <- numeric(5)
for(i in 1:5) {
p[i] <- pnorm(20+10*(i-1), mean =v, sd = s)-pnorm(10+10*(i-1), mean =v, sd =s)
a[i] <- length(temp_sausio)*p[i]
b[i] <- (l[i,]-a[i])^2/a[i]
}

h<-cbind(l[1:5],p[1:5],a[1:5], b[1:5])
h

e<-c("(10,20]","(20,30]","(30,40]","(40,50]","(50,60]")
h
st<-c("O","P","E","(O-E)^2/E")

matrix(h, nrow = 5, ncol = 4,dimnames=list(e,st))
sum(h[,4])
chisq.test(h[,1],p=h[,2],rescale.p=TRUE)

I?vada:

p-value = 0.4954 reik?m? rodo, kad atmesti nulin? hipotez?, 
kad duomenys normal?s n?ra pagrindo.


2.

Patikrinsime hipotez? apie duomen? normalum? naudodami ?apiro-Vilko test?. 

shapiro.test(temp_sausio)

I?vada:

p-value = 0.4824 reik?m? rodo, kad atmesti nulin? hipotez?, 
kad duomenys normal?s v?l n?ra pagrindo.

3.

(x=seq(10,60,length=210))
y=function(x){
dnorm(x,mean=v,sd=s)}
y(x)
density(temp_sausio)
plot(density(temp_sausio),main='Sausio m?nesio temperat?ros tankio palyginimas 
su normaliuoju')
lines(x,y(x),type="l",col="2")
legend("topright",c("sausio m?n. tankis","normalusis tankis"),lty=1, col=c("1","2"))


4.

Sausio m?nesio temperat?ros skirstinio palyginimui su normaliuoju galime pritaikyti 
kvantili? grafik?. 

qqnorm(temp_sausio,main="Sausio m?nesio temperat?ros kvantili? grafikas")
qqline(temp_sausio)

I?vada:

Pagal kvantili? grafik? galime spr?sti, kad sausio m?nesio temperat?ra
turi normal?j? skirstin?

2 u?duotis

Duomenys prussian (biblioteka pscl) yra apie kareivi? ?uvusi? Pr?s? armijoje
1875-1894 metais nuo arklio spyrio. Raskite  ?uvusi? kareivi? skai?iaus 
visuose korpusuose 1875-1894 metais empirin? skirstin?. 
Patikrinkite hipotez?, kad bendras ?uvusi?j? skai?ius nurodytais metais turi 
Puasono pasiskirstym?. 

I?valome atmint?

rm(list=ls())


library(pscl)
data(prussian)
prussian
attach(prussian)

(t=table(y))

mean(y)
proportions(table(y))
length(y)

p=as.numeric(5)
E <- numeric(5)
chi <- numeric(5)

for (i in 0:4) {
p[i+1]=dpois(i,lambda=mean(y))
E[i+1]=length(y)*p[i+1]
chi[i+1]=(t[i+1]-E[i+1])^2/E[i+1]
}

(lent=cbind(t,p,E, chi))
sum(chi)

chisq.test(lent[,1],p=lent[,2],rescale.p=TRUE)

I?vada:
H_0 hipotez?s atmesti neturime pagrindo. Taigi, hipotez?, kad duomenys turi Puasono
skirstin? priimame.(p-value = 0.7082)


3 u?duotis

Bibliotekoje MASS suraskite duomenis  survey. I? ?ia paimkite informacij? apie
vyr? ir moter? r?kymo ypatumus, kurie suskirstyti ? 4 kategorijas
(Heavy,Never,Occas, Regul). Patikrinkite hipotez?, kad duomenys paimti i?
populiacijos, kurioje vyr? ir moter? r?kymo ypatumai, pagal i?vardytas
kategorijas atitinka proporcijas, nurodytas lentel?je:
       Heavy Never  Occas  Regul
Female 0.012 0.468 0.0130 0.0085
Male   0.017 0.464 0.0085 0.0090



library(MASS)
survey
attach(survey)


O<-print(table(Sex,Smoke))

Padarysime lentel?, kurios 2 stulpelyje  i?d?styme moter? duomenis, o po to vyr? 
atitinkamai pa?ym?dami 0 arba 1. 

0-female
1-male


lent<-print(cbind(c(O[1,],O[2,]),c(rep(0,4),rep(1,4))))

Sura?ome teorines proporcija ? lentel?

Heavy<-print(c(0.012,0.017))
Never<-print(c(0.468,0.464))
Occas<-print(c(0.013,0.0085))
Regul<-print(c(0.0085,0.009))

p<-cbind(Heavy,Never,Occas, Regul)
p
rownames(p)<-c("Female","Male")
p
sum(p)

Lentel? papildome teorin?mis proporcijomis

lent<-print(cbind(lent,c(p[1,],p[2,])))

Skai?iuojame tik?tinus da?nius ir u?dedame lentel?s pavadinimus

E<-print(sum(O)*p)
lent<-print(cbind(lent,c(E[1,],E[2,])))
colnames(lent)<-c("O","Sex","p","E")
lent

Atliekame test?

chisq.test(lent[,1],p=lent[,3])

I?vada:
H_0 hipotez? atmetame: n?ra pagrindo teigti, kad stebimi duomenys parinkti
i? populiacijos su pateiktomis proporcijomis. 



Rankiniu b?du paskai?iuojame chi testo kritin? reik?m? 

sum((lent[,1]-lent[,4])^2/lent[,4])

Gauta kritin? reik?m? 101.813 prakti?kai nesiskiria gautos su testu. 


chisq.test(lent[,1],p=lent[,3])$expected




4 u?duotis

Bibliotekoje MASS suraskite duomenis  caith. Tai duomenys 
apie plauk? ir aki? spalv?. Patikrinkite hipotez? ar aki? ir plauk?  
spalva susij? kintamieji. 

library(MASS)
caith
chisq.test(caith)

I?vada: H_0 hipotez? atmetame:
teigti, kad po?ymiai yra nepriklausomi n?ra pagrindo. 


5 u?duotis

Atraskite duomenis earthquakes (biblioteka geostats). Tai 20 000 ?ra?? apie 
?em?s dreb?jimus 2017-2020 m. Stulpelis mag rodo ?em?s dreb?jimo stiprum?. 
1. Suraskite kiek atskirai ?em?s dreb?jim? ?vyko
2017,2018,2019 ir 2020 metais;
2. Raskite kiekvien? met? po?ymio, kad ?em?s dreb?jimo stiprumas didesnis arba
lygus 5 balams empirin? proporcij?. Ir patikrinkite ar ?i proporcija
kiekvienais metais skiriasi statisti?kai reik?mingai;
3. Patikrinkite hipotez?, kad ?em?s dreb?jim? stiprumo intervalini? kategorij?
[4.5,5.5) [5.5,6.50) [6.5,7.5) ir [7.5,8.5)  proporcijos 2017 metais yra tokios
pa?ios, kaip ir bendrai 2017-2020 met? proporcijos tose pa?iose kategorijose. 



1.

library(geostats)
attach(earthquakes)
earthquakes[1:20,]


?em?s dreb?jim? skai?ius 217-2020 metais

s=as.numeric(0)
for (i in 1:4) {
s[i]=length(mag[which(year==2016+i)])
}
s


2.

?em?s dreb?jim? skai?ius 2017-2020 metais, kuri? stiprumas 5 ir daugiau bal?. 

c(sum(mag[which(year==2017)]>=5),
sum(mag[which(year==2018)]>=5),
sum(mag[which(year==2019)]>=5),
sum(mag[which(year==2020)]>=5))

?em?s dreb?jim?, kuri? skai?ius vir?ijo 5 balus empirin? proporcija

s5=as.numeric(0)
for (i in 1:4) {
s5[i]=sum(mag[which(year==2016+i)]>=5)/length(mag[which(year==2016+i)])
}
s5

2017-2020 met? proporcij? paskliautinieji intervalai

library(Hmisc)
binconf(sum(mag[which(year==2017)]>=5),length(mag[which(year==2017)]))
binconf(sum(mag[which(year==2018)]>=5),length(mag[which(year==2018)]))
binconf(sum(mag[which(year==2019)]>=5),length(mag[which(year==2019)]))
binconf(sum(mag[which(year==2020)]>=5),length(mag[which(year==2020)]))


I?vada: Kadangi, proporcijos pasikliautinieji intervalai kertasi proporcijos
nesiskiria statisti?kai reik?mingai.

3.

range(mag)

2017 met? ?em?s dreb?jimo stiprumo proporcijos 4 kategorijose

int<-print(c(4.5,5.5,6.5,7.5,8.5))  
dala_2017<- cut(mag[which(year==2017)], int, right=FALSE) 
table(dala_2017)
proportions(table(dala_2017))

Bendroji 2017-2020 met?  ?em?s dreb?jimo stiprumo proporcijos 4 kategorijose

dala<- cut(mag, int, right=FALSE) 
table(dala)
proportions(table(dala))

Tikriname hipotez?, kad proporcija 2017 metais atitinka 2017-2020 met?
bendr?j? proporcij?

chisq.test(proportions(table(dala_2017)),p=proportions(table(dala)))
I?vada: 
Hipotez?, kad visose kategorijose, ?em?s dreb?jimo stiprumo proprcijos
2017 metais ir bendros 2017-2020 met? proporcijos nesiskira priimame. (p-value = 1)











