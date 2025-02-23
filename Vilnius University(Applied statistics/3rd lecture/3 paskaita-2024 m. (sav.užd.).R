#1.

#Pakete datasets raskite duomenis airquality.
#a). Sudarykite tiesini modeli,rodanti ozono atmosferoje priklausomybe nuo 
#saules radiacijos. Padarykite modelio sklaidos diagrama skirtingu menesiu
#duomenis pazymedami skaiciais ir nuspalvindami skirtingomis spalvomis; 
library(datasets)
data(airquality)
attach(airquality)
airquality
plot(Ozone, Solar.R, pch=as.character(Month), col=as.numeric(Month)
)
#Data cleaning, dropping NA values
df_clean <- na.omit(airquality)
plot(df_clean$Ozone, df_clean$Solar.R, pch=as.character(Month), col=as.numeric(Month)
)
LM <- lm(df_clean$Ozone ~ df_clean$Solar.R)
abline(LM)
#b). Raskite tiesines koreliacijos stipruma tarp ozono kiekio ir saules
#radiacijos;
cor(df_clean$Ozone, df_clean$Solar.R, method="pearson")
#c).Sudarykite koreliaciju matrica, parodanti tiesinio rysio stipruma tarp
#ozono kiekio, saules radiacijos, vejo ir temperaturos;
k_matrica <- cor(df_clean[, c(1,2,3,4)], method ="pearson")
#d). Padarykite sudarytos koreliaciju matricos diagrama, su skaiciais. 
library(corrplot)
corrplot(k_matrica)
2.
#a) Nubrezkite chi kvadrato skirstinio tankio funkcijas su skirtingais laisves 
#laispsniu skaiciais ir gaukite paveikslely pateikta per paskaita. 
library(ggplot2)
ggplot()+
  stat_function(fun = dchisq, args =list(df=5))+#df cia laisves laipsniu skc
  stat_function(fun = dchisq, args =list(df=6), col='yellow')+
  xlim(0,50)
#b) Nubrezkite chi kvadrato skirstini su 90 laisves laipsniu ir normaluji
#skirstini viename grafike. Gaukite paveiksleli pateikta per paskaitas. 
ggplot()+
  stat_function(fun = dchisq, args =list(df=90))+#df cia laisves laipsniu skc
  stat_function(fun = dnorm, args =list(mean=90, sd=13.4), col='yellow')+
  xlim(0,200)
3.

Duomenyse Animals (paketas MASS) suskirstykite gyv?nus ? 5 kategorijas.
?ias kategorijas atitinka ?ra?ai:
primatai(10,13,14,17,24), ?ol?d?iai(2,4,8,9,12,22), pl??r?nai(3,11,23),
dinozaurai(6,16,26), grau?ikai(1,5,19,20,21,25,27).
a) Apskai?iuokite proc.santyk?, kuri? dal? ?i? gyv?n? galvos smegen? mas?s
sudaro k?no mas?. I?veskite kiekvienos kategorijos santykio vidurk?. 
b)Padarykite ?io santykio stulpelin? kategorij? diagram?. 
Diagramoje u?d?kite ?ymas kiekvienos kategorijos santyk? suapvalindami iki 
2 skai?i? po kablelio. 

4.
Duomenyse simu_data (paketas "lillies") pa?alinkite visas eilutes,
kuriuose mirties data didesn? u? 80 met?. Nubrai?ykite gaut? nauj? 
duomen? tankio funkcijos grafik?.


5.
Duomenys VonBort paketas(vcd) yra apie kari? skai?i?
Pr?s? armijoje 1875-1894 m.?uvusi? nuo arklio spyrio ? galv?. 
?uvusi? skai?ius pateiktas atskirai kiekviename armijos korpuse.

a) Raskite bendr? ?uvusi?j? skai?i? visuose korpusuose kas ketverius metus:
1875-1878 m
1879-1882 m.
1883-1886 m.
1887-1890 m.
1891-1894 m.
Padarykite 3D skritulin? diagram? vaizduojan?i? ?uvusi?j? skai?i? nurodytais
metais;


b) 
I? 4a) padarytos diagramos matyti, kad bendras ma?iausias ?uvusi?
skai?ius-24 buvo 1875-1878 metais, o dvigubai didesnis 1879-1882 metais.
I?siai?kinsime, kaip nurodytais laikotarpiais skyr?si ?uvusi? skai?ius
kiekvienais metais, tai yra palyginsime bendr? ?uvusi? skai?i?:
1875 ir 1879 m, 1876 ir 1880 m, 1877 ir 1881 m, 1878  ir 1882 metais.
Padarykite palyginam?j? sta?iakamp? diagram?, kuri lygint? bendr? visuose
korpusuose ?uvusi? skai?i? i?vardytais metais. 


c) 
Paskai?iuokite bendr? mir?i? vidurk?. Raskite kiekvien? met? vidurk?. Nustatykite,
kuriais metais mir?i? vidurkis did?iausias. 
Padalykite metin? vidurk? ? 5 intervalus ir pagal dalybos ta?kus
nubr??kite metinio vidurkio histogram?. 






