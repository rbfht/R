
#1 uzduotis

#Suraskite duomenis gala (biblioteka faraway).
library(faraway)
df <- gala

#1. Istirkite kintamojo Species tiesine priklausomybe nuo regresoriu:
#Area,Elevation,Nearest,Scruz,Adjacent sudarydami visus imanomus
#vieno kintamojo tiesinius modelius su kiekvienu regresoriumi. 
#Isrinkite geriausia vieno kintamojo tiesini modeli;

attach(df)
lm <- lm(Species~Area+Elevation+Nearest+Scruz+Adjacent)
summary(lm)#Isrenku kintamaji su maziausia p-reiksme, tai tikriausiai ir bus geriausias, nes nera kitu stipriu kandidatu
lm1 <- lm(Species~Elevation)
summary(lm1)

#2. Patikrinkite ar geriausio modelio paklaidos normalios;

plot(density(lm1$residuals))
shapiro.test(lm1$residuals)
#Paklaidos nera normalios, tai ir Species nera normalus

#3. Naudodami Kuko mata patikrinkite ar duomenys turi
# isskirciu, suraskite sias isskirtis;

cooksd = cooks.distance(lm1)
plot(cooksd)
lim = 4/nrow(df)
abline(h = lim)
#12 ir 25
library(car)
influencePlot(lm1)
#16, 12 ir 25

#4. Pasalinkite isskirtis ir sudarykite nauja geriausia
#vieno kintamojo modeli. Stebekite, kaip kinta determinacijos koeficientas. 
 lm1_g_c <- lm(df[c(1:11,13:24,26:30),]$Species~df[c(1:11,13:24,26:30),]$Elevation)
df1 <- df[c(1:11,13:15,17:24,26:30),]
lm1_g <- lm(df1$Species ~ df1$Elevation)
summary(lm1_g)
summary(lm1)
summary(lm1_g_c)#Sitas geriausias

#2 uzduotis

#Naudodami duomenis gala sudarykite kintamojo Species modeli nuo regresoriaus Elevation.
#Koks regresoriaus laipsnis duoda geriausia modeli. 


lm2 <- lm(Species~Elevation+I(Elevation^2))
lm3 <- lm(Species~Elevation+I(Elevation^2)+I(Elevation^3))
lm4 <- lm(Species~Elevation+I(Elevation^2)+I(Elevation^3)+I(Elevation^4))
lm5 <- lm(Species~Elevation+I(Elevation^2)+I(Elevation^3)+I(Elevation^4)+I(Elevation^5))
summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
#Nesamoninga uzduotis

#3 uzduotis

#1.
#Naudodami duomenis gala sudarykite kintamojo Species 
#tiesines  priklausomybes modeli nuo 4  regresoriu:
#Area,Elevation,Scruz,Adjacent;

library(faraway)
df <- gala
attach(df)
lm <- lm(Species~Area+Elevation+Scruz+Adjacent)

#2. Nustatykite, kuris regresorius yra itakingiausias;

summary(lm)
#Elevation

#3. Istirkite multikolinearuma ir nustatykite, kuriuos regresorius reiketu 
#pasalinti is modelio;

library(car)
vif(lm)
#Galima ziureti koreliacijos matricos koeficientus tarp regresoriu arba:
library(car)
vif(lm)
#Visi dispersijos mazejimo rodikliai mazesni uz 4, todel multikolinearumo nera

#4. Sudarykite nauj? model? su pa?alintais regresoriais ir v?l patikrinkite
#multikolinearum?;

#Nera

#5. Padarykite stebimu reiksmiu ir abieju modeliu grafikus viename lape;

#Nebuvo, bet
#predict(lm, data=df) (modelio lm reiksmes) tada galime nubrezti tiese ir taskus duomenu
#Taciau cia butu regresija ne nuo vieno regresoriaus, tai grafiskai pavaizduoti sunku. Reiktu izoliuoti viena? ir kitus vidurki?

#6. I? modelio su pa?alintais regresoriais pa?alinkite i?skirtis ir sudarykite nauj? 
#model?. Steb?kite determinacijos koeficiento kitim?. 

#Labai panasiai i 1 uzd

#4 uzduotis

#Suraskite duomenis airquality (biblioteka datasets). Padarykite Ozono priklausomybes 
#nuo menesio faktoriaus tiesini modeli.
#Ka galite pasakyti apie Ozono kiekio padidejima birzelio-rugsejo menesiais. Ar sie 
#padidejimai statistiskai reiksmingi? 

library(datasets)
df <-airquality
#Menesiai yra kategorijos, tai naudosime factor funkcija(One hot encoding)(Sukuria naujus stulpeliu su kategorijom ir priskiria 0 arba 1)
lm <- lm(df$Ozone~factor(df$Month))
summary(lm)
#Liepos ir rugpjucio tik yra stat. reiksmingi(mazos p-reiksmes)