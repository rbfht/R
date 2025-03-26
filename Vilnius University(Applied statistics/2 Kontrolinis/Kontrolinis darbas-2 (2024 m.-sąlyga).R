1. uzduotis 

Pakete  five.yr.temperature (biblioteka UsingR)paimkite duomenis apie 1995 ir
2001 metu temperatura. Pradzioje patikrine hipoteze, kad 1995 ir 2001 metu temperaturu dispersijos
yra lygios, patikrinkite hipoteze, kad 1995 ir 2001 metu vidutine temperatura tokia pati. 
(1 taskas) 

library(UsingR)
df <- five.yr.temperature
var.test(df[df$years == 1995, ]$temps, df[df$years == 2001, ]$temps)
#Nera pagrindo atmesti 0 hipotezes, kad var lygios
t.test(df[df$years == 1995, ]$temps, df[df$years == 2001, ]$temps, var.equal = TRUE)
#Taip pat nera pagrindo atmesti 0 hipot.
2. uzduotis

Pakete Wages1 (biblioteka Ecdat) pateikti  duomenis apie vyru ir moteru darbo patirti.
Raskite siu duomenu, kad vyru darbo patirtis ilgesne,  nei 5 metai populiacijos proporcija 
ir sios proporcijos 95 proc. lygmens pasikliautinaji intervala. 
 (1 taskas)

library(Ecdat)
library(dplyr)
library(Hmisc)
df_m <- Wages1 |>
  filter(sex == "male")
m <- nrow(df_m[df_m$exper > 5, ])
n <- nrow(df_m)
prop <- m/n
binconf(m, n, alpha=0.05)



3. uzduotis

Patikrinkite, hipoteze,  kad zuvusiuju kariu skaicius nuo arklio spyrio
kavalerijos I-mame korpuse (duomenys prussian biblioteka pscl),
atitinka Puasono pasiskirstyma.
(2 taskai) 

library(pscl)
library(dplyr)
df <- prussian |>
  filter(corp == "I")
max <- max(df$y)
min <- min(df$y)
m <- numeric(max - min + 1)
p <- numeric(max - min + 1)
for (i in min:max){
  m[i-min+1]=nrow(df[df$y == i, ])
  p[i-min+1]=dpois(i, mean(df$y))
}
chisq.test(m, p, rescale.p = TRUE)
#Neturime pagrindo atmesti nuline hipoteze, kad atitinka Puasono pasiskirstyma

4. uzduotis

Zemiau esancioje lenteleje:

I II III IV
Melyna 95 40  80 25
Ruda   65 50  40  5

pateikti 400 zmoni? duomenys apie  akiu spalva ir kraujo grupe.
Sudarykite sia lentele ir patikrinkite hipoteze, apie zmogaus akiu 
spalvos ir kraujo grupes priklausomuma. (2 taskai)

df <- data.frame(
  Kat = c("Melyna", "Ruda"),
  I = c(95, 65),
  II = c(40, 50),
  III = c(80, 40),
  IV = c(25, 5)
)
library(dplyr)
chisq.test(df|>
             select(-Kat))
#Turime pagrindo atmesti 0 hipoteze, kad sie dydziai yra nepriklausomi

5. uzduotis.

#Paimkite duomenis prussian(biblioteka pscl). 

#1. Atlikite zuvusi? kariu skaiciaus 
#kiekviename korpuse medianu lygybes testa ir patikrinkite
#hipoteze, kad visos medianos kiekviename korpuse yra lygios
#pries alternatyva, kad bent viename korpuse mediana skiriasi nuo 
#medianu kituose korpusuose. Raskite testo  p-value reiksme.  (1 taskas) 

library(pscl)
df <- prussian
print(kruskal.test(df$y ~ df$corp)$p.value)
#Neturime pagrindo atmesti 0 hipotezes, kad medianos lygios

#2. Atlikite medianu lygybes testa V, IX ir X korpusuose ir patikrinkite, kad 
#visos trys medianos lygios pries alternatyva, kad ne. 
#Patarimas: Tam, kad galetumete atlikti keliu medianu lygybes testa, isskirkite 
#V,IX ir X korpuso duomenis, ivesdami papildoma stulpeli  group. 
#Stulpelyje group  V,IX ir X korpusu duomenis pazym?kite tais pa?iais skaiciais, o kitur
#ira?ykite trukstama reiksme NA. Tokios lemteles fragmentas parodytas zemiau. 

#Raskite testo p-value reiksme. (2 taskai)

library(dplyr)
df <- df |>
  filter(corp == "V" | corp == "IX" | corp == "X")
print(kruskal.test(df$y ~ df$corp)$p.value)
#Neturime pagrindo atmesti 0 hipotezes, kad medianos nelygios
3. 
#Paimkite duomenis prussian(biblioteka pscl). 

#Patikrinkite ar visuose korpusuose vidutinis zuvusiu kariu skaicius
#skiriasi statistiskai reiksmingai, naudodami 0.95 lygmens 
#pasikliautinuosius intervalus. 
#Patarimas: Kad tikrinti butu lengviau galite nusibrezti
# pasikliautinuju intervalu grafikus. 
#(1 taskas)
library(pscl)
df <- prussian
library(dplyr)
library(Hmisc)
library(ggplot2)
corps <- levels(c(unique(df$corp)))
shapiro.test(df$y)
apat <- numeric(length(corps))
virs <- numeric(length(corps))
z <- 1
#Imtis nera normali
for (i in corps){
  print(i)
  rez <- smean.cl.boot(df[df$corp == toString(i), ]$y)
  apat[z] = rez[2]
  virs[z] = rez[3]
  z <- z + 1
}

df_intervalai <- data.frame(
  Korp = corps,
  Apat = apat,
  Virsut = virs
)
df_intervalai |>
  ggplot(aes(x=Korp, y=Virsut, ymin=Apat, ymax=Virsut))+
  geom_linerange(size =8)
#Skiriasi statistiskai reiksmingai, VIII su (XI ir XIV), XV su (XIV, XI) ir IV su (XI ir XIV)