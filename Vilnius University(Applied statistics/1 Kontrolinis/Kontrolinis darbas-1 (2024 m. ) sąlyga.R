#1.Uzduotis

#Duomenyse Animals (paketas MASS) paimkite 4 skirtingu bezdzioniu ir zmogaus
#galvos smegeu ir kuno mases duomenis (10,13,14,17,24) duomenu eilutes.
#Apskaiciuokite  siu gyvunu galvos smegenu ir  kuno mases procentini
#santyki.
#Padarykite isvardytu  individu galvos smegenu mases ir kuno mases procentinio 
#santykio staciakampe palyginamaja diagrama. (2 taskai)
library(MASS)
library(dplyr)
df <- Animals
gyv <- df[c(10,13,14,17,24), ]
gyv <- gyv |>
  mutate(santykis = round(brain*0.1/body,2))
barplot(gyv$santykis, names.arg = rownames(gyv), col = "blue",
        main = "Smegenu-Kuno santykis", ylab = "Santykis")
gyv
#2. uzduotis
#Raskite duomenis Davis (paketas carData). Siu duomenu 12-oje eiluteje
#ivestas moters ugis (stulpelis height) skiriasi nuo kitu duomenu ir 
#yra ivedimo klaida. Pasalinkite sia eilute is Davis duomenu ir
#pavadinkite naujus duomenis Davis1. Paskaiciuokite nauju duomenu Davis1
#moteru ugio (2-ojo stulpelio) vidurk?. (1 taskas)
library(carData)
library(dplyr)
Davis
Davis1 <- Davis |>
  filter(nr != 12)
Davis1
ugio_vid <- mean(Davis1$height)
ugio_vid
#3. uzduotis

#Paimkite duomenis CPS1985 (paketas AER):
#a)Sudarykite valandinio atlyginimo priklausomybes nuo amziaus tiesini modeli
#ir padarykite sio modelio sklaidos diagrama, joje nubrezdami tiesini trenda. 
#Suraskite tiesinio rysio stipruma; (1 taskas)
library(AER)
df <- CPS1985
LM <- lm(df$wage~df$age)
plot(df$age, df$wage)
abline(LM)
cor(df$wage, df$age)
#b) Sudarykite atlyginimu, issilavinimo, patirties ir amziaus koreliaciju
#matrica ir paskaiciuokite koreliacijas tarp isvardytu faktoriu, Pirsono,
#Spearmano ir Kendalo metodu, padarykite koreliaciju matricu diagramas
#viename lape, aiskiai uzrasykite diagramu pavadinimus.(1 taskas)
k_p_matrica <- cor(df[, c(1, 2, 3, 4)], method = 'pearson')
k_s_matrica <- cor(df[, c(1, 2, 3, 4)], method = 'spearman')
k_k_matrica <- cor(df[, c(1, 2, 3, 4)], method = 'kendal')
library(corrplot)
par(mfrow = c(1, 3))
corrplot(k_p_matrica, title = "Pearson", method = 'number')
corrplot(k_s_matrica, title="Spearman", method = 'number')
corrplot(k_k_matrica, title="Kendal", method = 'number')
#4. uzduotis

#Nubrezkite du chi kvadrato skirstinio su 8 ir 12 laisves laipsniu tankio
#grafikus. Tuomet, plota figuros, kuri susidaro tarp grafiku
#nuo 0 iki grafiku susikirtimo nuspalvinkite zaliai, o plota figuros, kuri
#susidaro tarp grafiku  nuo ju susikirtimo iki 17 nuspalvinkite geltonai. 
#(2 taskai)
library(ggplot2)
x_int <- 9
x_vals <- seq(0, 17, length.out = 300)
df_shade <- data.frame(
  x = x_vals,
  y8 = dchisq(x_vals, df = 8),
  y12 = dchisq(x_vals, df = 12)
)
df_region1 <- subset(df_shade, x <= x_int)
df_region2 <- subset(df_shade, x >= x_int)
ggplot()+
  geom_ribbon(data = df_region2, aes(x = x, ymin = y8, ymax = y12), fill = "yellow", alpha = 0.5)+
  geom_ribbon(data = df_region1, aes(x = x, ymin = y12, ymax = y8), fill = "green", alpha = 0.5) +
  stat_function(fun = dchisq, args =list(df=8))+
  stat_function(fun = dchisq, args =list(df=12))+
  xlim(0,50)
#5. uzduotis

#Duomenys VonBort paketas(vcd) yra apie kariu skaiciu Prusu armijoje 1875-1894 m.
#zuvusiu nuo arklio spyrio. 

#a) Sudarykite lentele (matrica), kurioje butu pateikti duomenys,
#apie bendra 1875-1884 metais ir 1885-1894 metais zuvusiu kariu skaiciu
#atskirai 5 korpusuose: G,I,II, III,IV. Nubrezkite siu dvieju metiniu
#kategoriju ir zuvusiu kariu skaiciaus isvardytuose korpusuose staciakampe
#palyginamaja diagrama.Diagramoje turi buti matyti isvardyti korpusai ir dvi 
#pateiktos metines kategorijos.
#Diagramoje uzdekite atitinkamas zymas ir pavadinima. (2 taskai)
library(vcd)
library(dplyr)
library(tidyr)
df<-VonBort
df_n <- df |>
    mutate(period = case_when(#case_when if else sakiniai
      year >= 1875 & year <= 1884 ~ "1875-1884",
      year >= 1885 & year <= 1894 ~ "1885-1894"),)|>
    group_by(period, corps) |>  
    summarise(total = sum(deaths, na.rm = TRUE))
ggplot(df_n, aes(x = corps, y = total, fill = period))+
  geom_col(position = "dodge") +#Sustato stulpelius salia vienas kito
  labs(title = "Viso mirciu pagal korpusa ir perioda",
       x = "Korpusas",
       y = "Is viso mirciu",
       fill = "Periodas") +
      theme_minimal() 
#b) 
#Raskite mirusiu skaiciaus bendrai visuose korpusuose 1875-1894 metais
#tipine (dazniausiai pasikartojancia reiksme) (1 taskas) 
df <- VonBort |>
  filter(year >= 1875 & year <= 1894)
library(modeest)
mfv(df$deaths)











