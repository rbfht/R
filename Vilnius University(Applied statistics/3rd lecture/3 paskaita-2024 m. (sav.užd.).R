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
#3.
#Duomenyse Animals (paketas MASS) suskirstykite gyvunus i 5 kategorijas.
#sias kategorijas atitinka irasai:
#primatai(10,13,14,17,24), zoledziai(2,4,8,9,12,22), plesrunai(3,11,23),
#dinozaurai(6,16,26), grauzikai(1,5,19,20,21,25,27).
#a) Apskaiciuokite proc.santyki, kuria dali siu gyvunu galvos smegenu mases
#sudaro kuno mase. Isveskite kiekvienos kategorijos santykio vidurki.
library(MASS)
library(dplyr)
df <- Animals
df <- df |>
  mutate(santykis = round(brain*0.1/body, 2))#nes body yra kilogramais, o brain gramais
nr=1:length(df$body)
cbind(nr, df)
primatai = df[c(10,13,14,17,24), ]
zole <- df[c(2,4,8,9,12,22), ]
ples <- df[c(3,11,23),]
dino <- df[c(6,16,26),]
grz <- df[c(1,5,19,20,21,25,27),]
primatai_vid <- mean(primatai$santykis)
zole_vid <- mean(zole$santykis)
#analogiskai kiti
#b)Padarykite Sio santykio stulpeline kategorijU diagrama. 
#Diagramoje uzdekite zymas kiekvienos kategorijos santyki suapvalindami iki 
#2 skaiciu po kablelio. 
barplot(c(primatai_vid, zole_vid), col = c('yellow', 'green'), legend.text = c('primatai', 'zole'), name = 'santykiai')
#4.
#Duomenyse simu_data (paketas "lillies") pasalinkite visas eilutes,
#kuriuose mirties data didesne uz 80 metu. Nubraizykite gauta nauja 
#duomenu tankio funkcijos grafika.
library(lillies)
df<-simu_data
df<-df |>
  filter(as.numeric(age_death) <= 80)
plot(density(df$age_death))
#5.
#Duomenys VonBort paketas(vcd) yra apie kariu skaiciu
#Prusu armijoje 1875-1894 m.zuvusiu nuo arklio spyrio i galva. 
#zuvusiu skaicius pateiktas atskirai kiekviename armijos korpuse.

#a) Raskite bendra zuvusiuju skaiciu visuose korpusuose kas ketverius metus:
#1875-1878 m
#1879-1882 m.
#1883-1886 m.
#1887-1890 m.
#1891-1894 m.
#Padarykite 3D skrituline diagrama vaizduojancia zuvusiuju skaiciu nurodytais
#metais;
library(vcd)
library(plotrix)
df<-VonBort
int <- cut(df$year, breaks =c(1875, 1878,1882,1886,1890,1894), right = TRUE)
pie3D(table(int), labels = names(table(int)))
#b) 
#Is 4a) padarytos diagramos matyti, kad bendras maziausias zuvusiu
#skaicius-24 buvo 1875-1878 metais, o dvigubai didesnis 1879-1882 metais.
#Issiaiskinsime, kaip nurodytais laikotarpiais skyresi zuvusiu skaicius
#kiekvienais metais, tai yra palyginsime bendra zuvusiu skaiciu:
#1875 ir 1879 m, 1876 ir 1880 m, 1877 ir 1881 m, 1878  ir 1882 metais.
#Padarykite palyginamaja staciakampe diagrama, kuri lygintu bendra visuose
#korpusuose zuvusiu skaiciu isvardytais metais. 


c) 
Paskai?iuokite bendr? mir?i? vidurk?. Raskite kiekvien? met? vidurk?. Nustatykite,
kuriais metais mir?i? vidurkis did?iausias. 
Padalykite metin? vidurk? ? 5 intervalus ir pagal dalybos ta?kus
nubr??kite metinio vidurkio histogram?. 






