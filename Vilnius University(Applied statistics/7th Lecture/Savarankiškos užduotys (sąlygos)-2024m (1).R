#1 uzduotis

#Atraskite duomenis five.yr.temperature (biblioteka UsingR), tai 1995-2001 metu
#temperaturiniai duomenys. Atrinkite kiekvienu metu sausio menesio duomenis.
library(UsingR)
library(dplyr)
df <- five.yr.temperature |>
  filter(days >= 1 & days <= 31)
#1. Patikrinkite siu duomenu normaluma naudodami chi kvadrato testa;
n <- nrow(df)
r <- round(1.72*n^(1/3))
ilgis <- max(df$temps) - min(df$temps)
zingsn <- ilgis/(r-1)
a <- numeric(r)
p_t <- numeric(r - 1)
m <- numeric(r - 1)
a[1] = min(df$temps)
a[r] = max(df$temps)
for (i in 0:(r-2)){
  a[i+1] = min(df$temps) + i*zingsn
  p_t[i+1] = pnorm(a[i+2], mean = mean(df$temps), sd = sd(df$temps)) - pnorm(a[i+1], mean = mean(df$temps), sd = sd(df$temps))
  m[i+1] = nrow(df[df$temps >= a[i+1] & df$temps <= a[i+2], ])
}
chisq.test(m, p_t, rescale.p = TRUE)
#Nera pagrindo atmesti nuline hipoteze, kad duomenys yra is normaliojo skirstinio

#2. Patikrinkite siu duomenu normaluma naudodami Shapiro-Vilko testa;

shapiro.test(df$temps)
#Nera pagrindo atmesti nuline hipoteze, kad duomenys yra is normaliojo skirstinio

#3. Nubrezkite normalaus skirstinio ir sausio menesio temperaturos tankio
#grafikus viename paveiksle.

library(ggplot2)
df |>
  ggplot(aes(x=temps))+
  stat_function(fun=dnorm, args = list(mean = mean(df$temps), sd = sd(df$temps)))+
  geom_histogram(aes(y = after_stat(density)))

#4. Kokia dar grafine procedura is ankstesniu paskaitu galite pritaikyti,
#tikrinant sausio menesio temperaturiniu duomenu normaluma. Pritaikykite ji. 

#Kvantiliu grafikas
qqnorm(df$temps)
qqline(df$temps)

#2 uzduotis

#Duomenys prussian (biblioteka pscl) yra apie kareiviu zuvusiu Prusu armijoje
#1875-1894 metais nuo arklio spyrio. Raskite  zuvusiu kareiviu skaiciaus 
#visuose korpusuose 1875-1894 metais empirini skirstini. 
library(pscl)
df <- prussian
m <- numeric(5)
p <- numeric(5)
for (i in min(df$y):max(df$y)){
  m[i+1] = nrow(df[df$y == i, ])
  p[i+1] = dpois(i ,mean(df$y))
}

#Patikrinkite hipoteze, kad bendras zuvusiuju skaicius nurodytais metais turi 
#Puasono pasiskirstyma. 

chisq.test(m, p, rescale.p = TRUE)
#Nera pagrindo atmesti nulines hipotezes

#3 uzduotis

#Bibliotekoje MASS suraskite duomenis  survey. Is cia paimkite informacija apie
#vyru ir moteru rukymo ypatumus, kurie suskirstyti i 4 kategorijas
#(Heavy,Never,Occas, Regul). Patikrinkite hipoteze, kad duomenys paimti is
#populiacijos, kurioje vyru ir moteru rukymo ypatumai, pagal isvardytas
#kategorijas atitinka proporcijas, nurodytas lenteleje:
       #Heavy Never  Occas  Regul
#Female 0.012 0.468 0.0130 0.0085
#Male   0.017 0.464 0.0085 0.0090

#Viskas sprendziasi labai panasiai kaip ankstesniuose uzdaviniuose, cia expected probability duoti salygoje. Reiketu suskirstyti vyrus ir moterys atskirai ir vel apskaiciuoti empirinius ju skirstinius ir patikrinti su chisq.test

#4 uzduotis

#Bibliotekoje MASS suraskite duomenis  caith. Tai duomenys 
#apie plauku ir akiu spalva. Patikrinkite hipoteze ar akiu ir plauku  
#spalva susije kintamieji. 

library(MASS)
df <- caith
chisq.test(df)
#Nuline hipoteze, kad jie yra susije turime pagrindo atmesti

#5 uzduotis

#Atraskite duomenis earthquakes (biblioteka geostats). Tai 20 000 irasu apie 
#zemes drebejimus 2017-2020 m. Stulpelis mag rodo zemes drebejimo stipruma.

library(geostats)
df <- earthquakes

#1. Suraskite kiek atskirai zemes drebejimu ivyko
#2017,2018,2019 ir 2020 metais;

#2. Raskite kiekvienu metu pozymio, kad zemes drebejimo stiprumas didesnis arba
#lygus 5 balams empirine proporcija. Ir patikrinkite ar si proporcija
#kiekvienais metais skiriasi statistiskai reiksmingai;

library(Hmisc)
n <- numeric(max(df$year)-min(df$year)+1)
m <- numeric(max(df$year)-min(df$year)+1)
for (i in min(df$year):max(df$year)){
  n[i-min(df$year)+1] = nrow(df[df$year == toString(i), ])
  m[i-min(df$year)+1] = nrow(df[df$mag >= 5 & df$year == toString(i), ])
  print(binconf(m[i-min(df$year)+1], n[i-min(df$year)+1]))
}
#Intervalai kertasi, tai proporcijos skiriasi statistiskai nereiksmingai

#3. Patikrinkite hipoteze, kad zemes drebejimu stiprumo intervaliniu kategoriju
#[4.5,5.5) [5.5,6.50) [6.5,7.5) ir [7.5,8.5)  proporcijos 2017 metais yra tokios
#pacios, kaip ir bendrai 2017-2020 metu proporcijos tose paciose kategorijose. 

library(dplyr)
df_2017 <- df|>
  filter(year == 2017)
m_2017 <- numeric(4)
m <- numeric(4)
p <- numeric(4)
for (i in 1:4){
  m[i] = nrow(df[df$mag>= 4.5 + i-1 & df$mag < 5.5 + i-1, ])
  m_2017[i] = nrow(df_2017[df_2017$mag>= 4.5 + i-1 & df_2017$mag < 5.5 + i-1, ])
  p[i] = m[i]/nrow(df)
}

chisq.test(m_2017, p = p, rescale.p = TRUE)
#Neturime pagrindo atmesti nulines hipotezes