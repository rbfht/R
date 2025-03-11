#1 uzduotis.

#Duomenyse Wages1 (biblioteka Ecdat) paimkite darbo patirties duomenis 
#(stulpelis exper). 
#1) Raskite moteru darbo patirties medianos 95 procentu 
#pasikliautiniji intervala. 
library("Ecdat")
df <- Wages1
qqnorm(df$exper)
qqline(df$exper)
shapiro.test(df$exper)
#Matome, kad imtis nera normali, todel naudosime smedian.hilow
library(Hmisc)
smedian.hilow(df$exper, conf.int = 0.95)
#2) Nustatykite ar vyru ir moteru vidutinis atlyginimas skiriasi statistiskai 
#reiksmingai. 
library(dplyr)
df_male <- df |>
  filter(sex=='male')
qqnorm(df_male$wage)
qqline(df_male$wage)
shapiro.test(df_male$wage)
df_female <- df|>
  filter(sex=='female')
qqnorm(df_female$wage)
qqline(df_female$wage)
shapiro.test(df_female$wage)
#Kaip matome pagal grafikus ir shapiro testa, duomenys nera normalus del "uodegu"
library(Hmisc)
smean.cl.boot(df_male$wage, conf.int = 0.95)
smean.cl.boot(df_female$wage, conf.int = 0.95)
wilcox.test(df_male$wage, df_female$wage)
#Matome, kad vidurkiai skiriasi statistiskai reiksmingai

#2 uzduotis.

#Duomenyse Wages1 (biblioteka Ecdat) raskite kiek darbo patirties 
#imtyje yra moteru su ilgesniu, nei 10 metu darbo patirtimi.
library("Ecdat")
df <- Wages1
df_female <- df|>
  filter(sex=='female')
n <- df_female |>
  filter(exper > 10) |>
  count()
#Koks siu moteru skaiciaus procentinis santykis nuo visu moteru skaiciaus. 
N <- df_female |>
  count()
n/N
#Raskite populiacijos procentinio santykio rezius is kurios sudaryta si imtis.
#(santykio rezius imkite 95 procentu lygmens) 
#Pirma, n ir N yra tipo tibble, tai reikia istraukti pati skaicius, kad veiktu funkcija
n_val <- n$n[1]
N_val <- N$n[1]
library(Hmisc)
binconf(n_val, N_val, alpha=0.05)

#3 uzduotis.

#Duomenyse Wages1 (biblioteka Ecdat) raskite atskirai 
#vyru ir moteru  skaiciaus su ilgesne, nei 10 metu darbo patirtimi procentini santyki.

#2 Uzduotyje jau radome moteru, tai panaudosime ji
n_mot <- n_val
N_mot <- N_val

tib <- df_male |>
  filter(exper > 10) |>
  count()
n_vyr <- tib$n[1]

tib1 <- df_male |>
  count()
N_vyr <- tib1$n[1]

n_vyr/N_vyr
#Ar yra pagrindo teigti, kad visoje populiacijoje sis santykis reiksmingai skiriasi. 
binconf(n_vyr, N_vyr, alpha=0.05)
binconf(n_mot, N_mot, alpha=0.05)
#Taip, intervalai nesikerta.

#4 uzduotis.

Duomenyse prussian (biblioteka pscl) pateikti duomenys apie Pr?s? armijoje
?uvusi? kari? skai?i? 1875-1894 metais nuo arklio spyrio.  ?uvusi? skai?ius 
pateiktas atskirais korpusais. 
1) ?uvusi? kari? skai?i? visuose korpusuose grafi?kai palyginkite su Puasono 
atsitiktiniu dyd?iu
2)Sudarykite speciali? funkcij? pagal per paskait? pateikt? formul?
ir raskite Puasono skirstinio parametro lambda 95 proc. pasikliautin?j? interval?. 

5 u?duotis.

Pasinaudodami duomenimis Davis (biblioteka carData), apskai?iuokite vyr?  ir moter? k?no
mas?s indeks? (KMI). KMI=k?no mas? (kg)/(?gio(cm))^2. 

1) Padarykite  vyr? ir moter? KMI d??inius grafikus ir atraskite did?iausi?  bendr? abiems
lytims KMI. J? pa?alinkite i? turim? duomen? KMI s?ra?o ir v?l padarykite abiej? ly?i? 
d??inius grafikus su pa?alint?ja KMI i?skirtimi. 

2) Apskai?iuokite abiej? ly?i? KMI vidurkius pa?alinus min?t? i?skirt?. Ar galime teigti, 
kad vyr? ir moter? populiacijos KMI vidurkis skiriasi. 



