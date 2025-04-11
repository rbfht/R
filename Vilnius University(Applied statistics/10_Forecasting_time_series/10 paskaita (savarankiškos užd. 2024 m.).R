
#1 uzduotis 

#Raskite duomenis tsNH4 (biblioteka imputeTS)
#apie nitratU kieki vandenyje tam tikru laikotarpiu. Siuose duomenyse yra daug
#praleistu reiksmiu NA. Nagrinedami pirmus 90 laiko eilutes nariu raskite:
library(imputeTS)
df <- tsNH4

#1. praleistu duomenu numerius;
which(is.na(df[1:90]))

#2. Atstatykite tuos duomenis, naudodami dreifo metodu;
na_kalman(df[1:90])

#3. padarykite laiko eilutes su atstatytais duoimenimis grafika;
y=print(na_kalman(df[1:90]))
ggplot_na_imputations(df[1:90], y)

#4. Toje pa?coje bibliotekoje atraskite duomenis tsNH4Complete, kurie yra yra 
#tsNH4 duomenys be praleitu reiksmiu. Vel imdami 90 laiko eilutes nariu
#nubrezkite duomenu grafika su atstatytomis ir tikromis reiksmemis. 
ggplot_na_imputations(df[1:90],y,tsNH4Complete[1:90])


#2 uzduotis 

#Duomenys AirPassengers yra  apie keliones lektuvais  nuo 1946 metu
#sausio menesio iki 1960 metu gruodzio menesio. Naudodami siuos duomenis:
view(AirPassengers)
df <- AirPassengers
#1. Padarykite tiesines regresijos modeli priklausanti nuo dvieju regresoriu:
#metu ir menesiu. Koks modelio determinacijos koeficientas;
sum(is.na(df))
laikas = as.numeric(time(df))
metai = floor(laikas)
men = factor(cycle(df))
z = lm(df ~ men + metai)
summary(z)
#0.95
layout(matrix(1:4, nrow = 2))
plot(z)
confint(z)
#2. Nubrezkite AirPassengers ir jo modelio grafikus viename paveiksle.
plot(AirPassengers, type="l")
plot(predict(z),col="2",type="l")

#Vienam grafike
library(ggplot2)
df_data <- data.frame(
  index = 1:length(df),
  actual = as.numeric(df),
  fitted = z$fitted.values
)

ggplot(df_data, aes(x=index)) +
  geom_line(aes(y=actual), color = 'green')+
  geom_line(aes(y=fitted), color = 'red')+
  scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red"))+
  labs(x="Obs index",
       y="Passengers",
       title="Actual vs predicted")

#3 uzduotis 
#Pakete stats suraskite duomenis EuStockMarkets. Is siu duomenu
#isskirkite stulpeli DAX. Tai duomenys apie 40 didziausiu
#Vokietijos imoniu kainu indeksa.
library(stats)
df <- EuStockMarkets
?EuStockMarkets
library(tidyverse)
head(df)
df <- df[,1]

#1) Nubrezkite DAX indekso  grafika;
plot(df)
#2) Isskirkite DAX indekso trenda, su penktos eiles splainu;
library(gam)
laikas = print(as.numeric(time(df)))
splain <- gam(df ~ s(laikas,5))
#3) Padarykite modelio liku?i? grafika, kuris parodytu trendo pasalinima;
plot(df-splain$fitted.values)
#arba
plot(splain$res, type = 'l')
#4) Isskirkite trenda, naudodami vienpusi glodinima, imdami 100 tasku. 
#Nubrezkite 2) ir 4) uzduoties trendus viename grafike. 
par(mfrow=c(2,1))
plot(df)
plot(laikas, stats::filter(df, rep(1,100)/100,sides=1), col = '2', type='l', color ="red")
lines(laikas, predict(splain), col = '3')
#ARBA
trend <- stats::filter(df, rep(1, 100)/100, sides = 1)
df_data <- data.frame(
  time = as.numeric(time(df)),
  original = as.numeric(df),
  trend = as.numeric(trend)
)
ggplot(df_data, aes(x=time))+
  geom_line(aes(y=trend), color = 'red')+
  geom_line(aes(y=original), color = 'black')+
  geom_line(aes(y=splain$fitted.values), color='green' )
#Naudojome vienpusi glodinima t.y. kiekvienas x_j pakeiciamas mean(x_j,x_j-1,...,x_j-99), todel pirmos 99 reiksmiu yra NA, nes nera pakankamai duomeny, tai galime matyti raudonoje kreiveje
#4 uzduotis
#Is duomenu AirPassengers pasalinkite metini trenda, o po to sezoniskuma. Padarykite 
#laiko eilutes dalies pasalinus sezoniskuma ir sezonines dalies grafikus. 

#Faster approach:
df <- as_tsibble(AirPassengers)
df <- df |> 
  mutate(value = log(value))
dcmp <- df |> model(stl = STL(value))
components(dcmp) |>
  autoplot()


#
?AirPassengers
df <- AirPassengers
head(df)
sum(is.na(df))
?AirPassengers#Monthly data

trend <- stats::filter(df, rep(1, 12)/12, sides =2)#Naudojame centrini vidurki,x_j <- mean(x_j-5,...,x_j-1,x_j,x_j+1,..,x_j+6)

df_data <- data.frame(
  time = as.numeric(time(df)),
  original = as.numeric(df),
  trend = as.numeric(trend),
  sezon_noise = as.numeric(df)-as.numeric(trend)
)
#Dabar paemame kiekvieno menesio vidurki ir atemate
library(tidyverse)
df_data <- df_data |>
  mutate(
    month = rep(1:12, length.out = n()),#Sukuriam menesio stulp
    monthly_mean = ave(sezon_noise, month, FUN = function(x) mean(x, na.rm = TRUE)),#Suskaiciuojam kiekvieno men vidurki
    noise = sezon_noise - monthly_mean#Apskaiciuojam noise
  )
sum(df_data$noise, na.rm=TRUE)

ggplot(df_data, aes(x=time))+
  geom_line(aes(y=noise), color = 'black')+#Pasalinus trenda ir sezoniskuma
  geom_line(aes(y=sezon_noise), color = 'red')#Pasalinus tik trenda

ggplot(df_data, aes(x=time))+
  geom_line(aes(y=noise), color = 'black')
#Vistiek matome periodiskuma.

#Pabandysime multiplikatyvu modeli:
df <- AirPassengers

trend <- stats::filter(df, rep(1, 12)/12, sides =2)#Naudojame centrini vidurki,x_j <- mean(x_j-5,...,x_j-1,x_j,x_j+1,..,x_j+6)

df_data <- data.frame(
  time = as.numeric(time(df)),
  original = as.numeric(df),
  trend = as.numeric(trend),
  sezon_noise = as.numeric(df)/as.numeric(trend)
)
library(tidyverse)
df_data <- df_data |>
  mutate(
    month = rep(1:12, length.out = n()),#Sukuriam menesio stulp
    monthly_mean = ave(sezon_noise, month, FUN = function(x) mean(x, na.rm = TRUE)),#Suskaiciuojam kiekvieno men vidurki
    noise = sezon_noise / monthly_mean#Apskaiciuojam noise
  )
prod(df_data$noise, na.rm=TRUE)#0.93 tai netoli vieneto
ggplot(df_data, aes(x=time))+
  geom_line(aes(y=noise), color = 'black')
#Atrodo maziau periodiskai grafiskai nei adityvus modelis