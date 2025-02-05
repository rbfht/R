U�duotys.

#1. 
#Pakete UsingR suraskite duomenis homedata apie nekilnojamojo turto kainas.
#Naudodami duomenis is 1970 metu  stulpelio, duomenis suskirstykite
#i 3 intervalus, uzdarus is kaires ir atvirus is desines,
#skaidymo taskais imdami 0,50000,100000,300000. 
#Padarykite lenteles skrituline diagrama. 
#My sltn:
library(UsingR)
homedata[1:10,]
int.galai <- c(0,50000,100000,300000)
skirstymai <- cut(homedata$y1970, int.galai, right = FALSE)
pie(table(skirstymai))
#2.
#Duomenyse homedata skirtingu metu duomenis padalinkite i 3 intervalus,
#uzdarus is kaires ir atvirus is desines,skaidymo taskais 
#imdami 0,100000,500000,1200000. Padarykite palyginamasias staciakampes diagramas,
#pirmoje palygindami 1970 ir 2000 metu duomenis skirtingu intervalu kategorijose,
#antroje diagramoje palyginkite skirtingu intervalu kategoriju pasiskirstyma 1970 ir 
#2000 metais. 
#My sltn:
int.galai <- c(0,100000,500000,1200000)
padalijimas70 <- cut(homedata$y1970, int.galai, right = FALSE)
padalijimas00 <- cut(homedata$y2000, int.galai, right = FALSE)
barplot(table(padalijimas70, padalijimas00), beside = TRUE, col=c("blue1","green"), legend.text=c("1970","2000"))
prop70 <- proportions(table(padalijimas70))
prop00 <- proportions(table(padalijimas00))
install.packages("dplyr")
library(dplyr)
#Converting the tables to dataframes to perform full join
proc70 <- as.data.frame(round(prop70*100))
proc00 <- as.data.frame(round(prop00*100))
colnames(proc70) <- c("Intervalas", "Freq")
colnames(proc00) <- c("Intervalas", "Freq")
table_proc_both = full_join(proc70, proc00, by = "Intervalas")
table_proc_both
barplot(c(table_proc_both$Freq.x, table_proc_both$Freq.y),
	names.arg = rep(table_proc_both$Intervalas, 2),
	col = c("blue", "green"),
	legend.text= c("1970", "2000"),
	beside = TRUE,
	xlab="Intervalai",
	ylab="Procentai",
	ylim=c(0,100))
#3.
#Duomenu  homedata 2000 metu stulpeli padalykite i 5 vienodo ilgio intervalus 
#atvirus is kaires, bet uzdarus is desines, atitinkamai parinkdami skaidymo taskus.
#Nubrezkite atitinkama histograma, kurioje matytusi NT vienetu pasiskirstymas
#pasirinktose kainu kategorijose.
#My sltn:
library(UsingR)
max(homedata$y2000)
int.galai <- c(0,100000,300000,400000,500000,max(homedata$y2000))
hist(homedata$y2000,
	breaks = int.galai, 
	col = "green",
	right = TRUE,
	probability=TRUE)
#4.
#Pakete UsingR suraskite duomenis five.yr.temperature apie 1995-2000 metu temperatura.
#Temperaturos duomenis padalykite i intervalus:uzdarus is kaires ir atvirus
#is desines,skaidymo taskais imdami 0,40,80,100. 
#Padarykite palyginamasias staciakampes intervalines temperaturos diagramas, pagal 
#skirtingu metu kategorijas. 
df <- five.yr.temperature
int.galai <- c(0,40,80,100)
library(dplyr)
df_new <- mutate(df, intervalai = cut(df$temps, int.galai, right = FALSE))
perc95 <- round(proportions(table(filter(df_new, years == 1995)$intervalai))*100)
perc96 <- round(proportions(table(filter(df_new, years == 1996)$intervalai))*100)
perc97 <- round(proportions(table(filter(df_new, years == 1997)$intervalai))*100)
perc98 <- round(proportions(table(filter(df_new, years == 1998)$intervalai))*100)
perc99 <- round(proportions(table(filter(df_new, years == 1999)$intervalai))*100)
perc00 <- round(proportions(table(filter(df_new, years == 2000)$intervalai))*100)
table(df_new%intervalai)
?table
unique(df_new$intervalai)
barplot(c(perc95, perc96, perc97, perc98, perc99,perc00),
	names.arg = rep(unique(df_new$intervalai), 6),
	beside = TRUE,
	col = c("green", "blue", "red", "purple", "orange", "yellow"))
5.
Duomenyse five.yr.temperature imdami stulpel� years, juos suskaidykite � dvi
kategorijas: imtinai 1995-1998 ir imtinai 1998-2001
Temperat�ros duomenis padalykite � intervalus:u�darus i� kair�s ir atvirus
i� de�in�s,skaidymo ta�kais imdami 0,40,80,100. (kaip ir 4-oje u�d.)
Padarykite palyginam�sias sta�iakampes intervalines temperat�ros diagramas, pagal 
�ias dvi sukurtas  met� kategorijas. 

Pakartokite �iuos skai�iavimus imdami met� kategorijas: 
 1995-1998 (imtinai)  ir  1998-2001 (imtinai) 

Padarykite abi diagramas viename lape.

par(mfrow=c(2,1))

6.
Nubr��kite dvi normali�sias kreives su vidurkiu 2  ir standartiniu nuokrypiu
0.5 ir 1 atitinkamai. Plot� nuo vidurkio iki artimiausi� dviej�
�i� kreivi� susikirtimo ta�k� nuspalvinkite geltonai. Susikirtimo ta�kus galite
nustatyti empiri�kai. 


