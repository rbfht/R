
#Uzduotis 1. 

#Paimkite duomenis five.yr.temperature (biblioteka UsingR). Paskaiciuokite 
#stulpelio temps vidurki ir taip pat 1995 metu temperaturos vidurki.
#Visais atvejais suformuluokite atitinkamas isvadas. 
library(UsingR)
library(dplyr)
df <- five.yr.temperature
mean(df$temps)
df_1995 <- df |>
  filter(years == '1995')
mean(df_1995$temps)
#1. Sudarykite hipotezes su reiksmingumo lygmeniu 0.05 dvipuse kritine sriti, 
#kad 1995 metu temperaturos  vidurkis lygus temps stulpelio vidurkiui, kai 
#1995 temperaturos dispersija lygi temps stulpelio dispersijai.

qnorm(0.975, mean(df$temps), sd=sd(df$temps))
qnorm(0.025, mean(df$temps), sd=sd(df$temps))
mean(df_1995$temps)
#Kritine sritis (-inf,23) U (88.7,inf)
#Vidukis nepatenka i kritine sriti, tai nera pagrindo atmesti hipotezes, kad vidurkiai lygus
#Skirtuma tarp vidurkiu galime paaiskinti imties atsitiktinumu
#qqnorm(df$temps)
#qqline(df$temps)
#smean.cl.boot(df$temps, conf.int = 0.95)

#CL yra turimos imties skirstinio, o kritine sritis is teorinio skirstinio

#2. Sudarykite su tuo paciu reiksmingumo lygmeniu vienpuses  hipotezes, kad 
#1995 metu temperaturos vidurkis yra mazesnis uz temps vidurki kairine
#kritine sriti.

qnorm(0.05, mean(df$temps), sd=sd(df$temps))
#Kairine kritine sritis (0, 28.26)
#Vidurkis nepatenka i kritine sriti todel galime atmesti hipoteze, kad vidurkis 1995 metais yra mazesnis

#3. Patikrinkite vienpuse hipoteze su t.testu, kad 1995 metu temperaturos
#vidurkis mazesnis uz temps vidurki.

var.test(df_1995$temps, df$temps)
t.test(df_1995$temps, mu = mean(df$temps), alternative ='less', var.equal = TRUE)
#Isvada: Nuline hipoteze, kad vidurkiai lygus nera pagrindo atmesti



#Uzduotis 2. 
#1000 kartu generuokite du standartinius normaliuosius atsitiktinius dydzius
#po 100 reiksmiu.
#Kiekviena karta atlikite jiems vidurkio ir dispersijos lygybes testus. 
p_v=as.numeric(0)
p_m=as.numeric(0)
for (i in 1:1000){
  X <- rnorm(100)
  Y <- rnorm(100)
  p_m[i]=t.test(X,Y)$p.value
  p_v[i]=var.test(X,Y)$p.value
}

#Empiriskai raskite:

#1. Tikimybe, kad abieju imciu vidurkiai ir dispersijos lygios;
lygu <- sum(p_v>=0.05&p_m>=0.05)
lygu/1000

#2. Salygine tikimybe:  vidurkiai lygus su salyga, kad lygios dispersijos;
n <- sum(p_v >= 0.05)
j <- sum(p_v >= 0.05 & p_m >= 0.05)
j / n
#3. Pirmos rusies klaidos tikimybe, kad abieju imciu dispersijos lygios.
j <- sum(p_v < 0.05)
j/1000
 
#Uzduotis 3. 
#1000 kartu generuokite  100 standartinio normaliojo atsitiktinio
#dydzio reiksmiu.  Keliais atvejais galima teigti, kad imtis sudaryta is 
#standartines normaliosios populiacijos.

library(EnvStats)
p_v=as.numeric(0)
p_m=as.numeric(0)
for (i in 1:1000){
  X <- rnorm(100)
  p_m[i]=t.test(X, mu = 0)$p.value
  p_v[i]=varTest(X,sigma.squared =1)$p.value
}
sum(p_v>=0.05 & p_m>=0.05)

#Uzduotis 4. 

#Suraskite duomenis Wages1 (biblioteka Ecdat). Nagrinekite valandini atlyginima
#(stulpelis wage), pagal 3 darbo patirties kategorijas (stulpelis exper).
#Tuo tikslu padalykite valandini atlyginima i 3 kategorijas:

#I kategorija Valandinis atlyginimas iki 6 metu imtinai;
#II kategorija Valandinis atlyginimas nuo 7 iki 12 m. imtinai;
#III katrgorija Valandinis atlyginimas nuo 13 metu imtinai iki 18 metu. 

#Kaip suskirstyti atlyginimus i sias 3 kategorijas informacija
#suraskite internete. Galite pasinaudoti nuoroda:
#https://www.geeksforgeeks.org/how-to-create-categorical-variables-in-r/

#My personal attempt to make the 3 categories

library(Ecdat)
library(dplyr)
df <- Wages1
#Creating new column for categories
df |>
  mutate(kategorija = NULL)
#Loop which assigns the categories
for (i in seq_len(nrow(df))) {
  if(df$exper[i] <= 6){
    kat <- 1
  }
  else if(7 <= df$exper[i] & df$exper[i] <= 12){
    kat <- 2
  }
  else if(13 <= df$exper[i] & df$exper[i] <= 18){
    kat <- 3
  }
  else {
    break
  }
  df[i, "kategorija"] <- kat
}
#Delete rows whose category is null
df <- df |>
  filter(!(is.null(kategorija)))

#The solution from the provided website
library(Ecdat)
df <- Wages1
df$kategorija <- as.factor(ifelse(df$exper<=6, 'I',
                          ifelse(7 <= df$exper & df$exper <= 12, "II",
                          ifelse(13 <= df$exper & df$exper <= 18, "III", "-"       
                                  ))))

#1. 
#Paskaiciuokite kiekvienos kategorijos valandinio atlyginimo mediana ir
#padarykite visu triju kategoriju atlyginimu dezinius grafikus, taip 
#grafiskai palygindami visas 3 medianas.
#Patikrinkite hipoteze, kad valandinio atlyginimo medianos visose 3 
#kategorijose yra lygios. 

median(df$wage[df$kategorija == "I"])
median(df$wage[df$kategorija == "II"])
median(df$wage[df$kategorija == "III"])
boxplot(df$wage[df$kategorija == "I"], df$wage[df$kategorija == "II"], df$wage[df$kategorija == "III"])
kruskal.test(wage ~ kategorija, df)
#Maza p reiksme, tai turime pagrindo atmesti nuline hipoteze, kad visu kategoriju medianos yra lygios

#2. 
#Padarykite I-os ir III-ios kategoriju valandinio atlyginimo dezinius grafikus,
#lygindami I-os ir III-ios kategoriju medianas.
#Patikrinkite hipoteze, kad valandinio atlyginimo medianos vosose I-oje ir 
#III-ioje   kategorijose yra lygios. 

boxplot(df$wage[df$kategorija == "I"], df$wage[df$kategorija == "III"])
wilcox.test(df$wage[df$kategorija == "I"], df$wage[df$kategorija == "III"])
#Nera pagrindo atmesti nulines hipotezes

#3.
#Patikrinkite hipoteze apie  triju sudarytu kategoriju vidurkiu lygybe. 

aov(wage ~ kategorija, data = df) |>
  summary()
#Turime pagrindo atmesti nuline hipoteze


U?duotis 5. 

Duomenys HairEyeColor (biblioteka UsingR) yra apie vyr? ir moter? aki? ir 
plauk? spalv?. Suraskite vyr? ir moter? juod? plauk? imties proporcijas, bei 
patikrinkite  hipotez?, kad vyr?  populiacijos juod? plauk? proporcija
yra didesn? u? moter? juod? plauk? proporcij?. 



