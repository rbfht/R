#Suraskite duomenis marketing (paketas datarium). Tai duomenis apie pardavimu
#priklausomybe nuo reklamos yotube, facebook ir laikrasciuose. 

#1.
install.packages("datarium")
library(datarium)
df <- marketing
#Sudarykite  visus imanomus tiesinius  pardavimu priklausomybes nuo siu
#3 regresoriu modelius ir is ju isrinkite geriausia;
#(2 taskai)
library(fpp3)
head(df)
attach(df)
lm(sales ~ youtube) |> summary()
lm(sales ~ facebook) |> summary()
lm(sales ~ newspaper) |> summary()
lm(sales ~ newspaper + youtube) |> summary()
lm(sales ~ newspaper + facebook) |> summary()
lm(sales ~ youtube + facebook) |> summary()#0.8962 GERIAUSIAS
lm(sales ~ youtube + facebook + newspaper) |> summary()#0.8956 

#2. Sudarytame geriausiame modelyje patikrinkite homoskedasti�kumo
#prielaid�;  (2 ta�kai)

linear<-lm(sales ~ youtube + facebook) 
library(lmtest)
bptest(linear)
#p reiksme .09>0.05 todel negalime atmesti prielaidos, kad duomenys yra homoskedastiski

#3. 
#Sudarykite tiesini modeli aprasanti pardavimu kitima nuo regresoriu  
#facebook ir youtube; Nustaykite, kuris regresorius yra itakingesnis
#paradavimams; (2 ta�kai)
attach(marketing)
lm(sales ~ youtube + facebook) |> summary()
mean(marketing$youtube)
mean(marketing$facebook)
median(marketing$youtube)
median(marketing$facebook)
#Matome, kad duomenyse youtube reiksmes yra gan didesnes, tai paaiskina kodel koeficientas yra mazesnis.
#Youtube, nes didesne t reiksme(parodo kiek sd yra nuo nulio koeficientas t=est.coeff/std.error)

#4.
#Suraskite duomenis goog(paketas fpp2). Tai duomenis apie akcij� kainas
#kiekvien� dien� 1000 dien�. Padarykite �ios laiko eilut�s  prognoz� 
#drifto metodu 20 dien� � priek�. Nubr��kite prognoz�s grafik�; (2 ta�kai)
library(fpp2)
df <- goog

library(imputeTS)
rwf(df, h=20, drift="TRUE") |> autoplot()
#5. 
#Duomenyse goog isskirkite trenda, panaudodami 5-os eiles splaina. (2 ta�kai)
library(fpp3)
library(gam)
df<-goog
laikas = print(as.numeric(time(df)))
splain <- gam(df ~ s(laikas,5))
base::plot(splain$fitted.values)


data_f <- data.frame(
  laikas = as.numeric(time(df)),
  fitted = splain$fitted.values
)

df |> autoplot() +
  geom_line(data=data_f, aes(y=fitted, x=laikas), color = 'blue')

#####################################################################

tsibble <- as_tsibble(df)
tsibble |>
  features(value, features = guerrero)#lembda = 0.347

library(fpp3)
tsibble |> autoplot(box_cox(value, 0.347))

df |> autoplot()#Beveik nieko nepakito, nes amplitude islieka gan pastovi(neatsizvlegus i trenda)



md <- tsibble |> model(stl = STL(value))


tsibble |> autoplot(value)+
  autolayer(components(md), trend, color = 'red') + 
  geom_line(data=data_f, aes(y=fitted, x=laikas), color = 'blue')


md1 <- tsibble |> model(classical_decomposition(value, type="additive")
tsibble_avg <- tsibble |>
  mutate(
    `5` = slider::slide_dbl(value, .before=2,  .after=2, .complete=TRUE)
  )
?slider::slide_dbl
tsibble |> autoplot(value)+
  autolayer(components(md), trend, color = 'red') + 
  geom_line(data=data_f, aes(y=fitted, x=laikas), color = 'blue')+
  autolayer(components(md1), trend, color='green')
