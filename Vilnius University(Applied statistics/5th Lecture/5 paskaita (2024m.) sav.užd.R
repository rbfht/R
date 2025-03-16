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

#Duomenyse prussian (biblioteka pscl) pateikti duomenys apie Prusu armijoje
#zuvusiu kariu skaiciu 1875-1894 metais nuo arklio spyrio. Zuvusiu skaicius 
#pateiktas atskirais korpusais. 

#1) Zuvusiu kariu skaiciu visuose korpusuose grafiskai palyginkite su Puasono 
#atsitiktiniu dydziu
library(pscl)
library(ggplot2)
library(dplyr)
df <- prussian
df_n <- df |>
  group_by(year) |>
  summarise(total_d = sum(y))
k_val <- 0:max(df_n$total_d)
p <- dpois(k_val, (mean(df_n$total_d)))
pois_df <- data.frame(k_val, p)
ggplot(df_n, aes(x=total_d)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1)+
  geom_line(data = pois_df, aes(x= k_val, y=p), color = 'red')

#2)Sudarykite specialia funkcija pagal per paskaitu pateikta formule
#ir raskite Puasono skirstinio parametro lambda 95 proc. pasikliautiniji intervala. 
puas <- function(x, conf.level=0.95){
  n<-length(x)
  return(c(mean(x) - qnorm(0.975)*(sqrt(mean(x)/n)), mean(x) + qnorm(0.975)*(sqrt(mean(x)/n))))
}
puas(df_n$total_d, conf.level = 0.95)
library(Hmisc)
smean.cl.boot(df_n$total_d)

#5 uzduotis.

#Pasinaudodami duomenimis Davis (biblioteka carData), apskaiciuokite vyru  ir moteru kuno
#mases indeksa (KMI). KMI=kuno mase (kg)/(ugio(cm))^2. 
library(carData)
library(dplyr)
df_v <- Davis |>
  filter(sex == "M") |>
  mutate(kmi = weight/height)
df_m <- Davis |>
  filter(sex == "F") |>
  mutate(kmi = weight/height)

#1) Padarykite  vyru ir moteru KMI dezinius grafikus ir atraskite didziausia  bendra abiems
#lytims KMI. Ji pasalinkite is turimu duomen? KMI saraso ir vel padarykite abieju lyciu 
#dezinius grafikus su pasalintaja KMI isskirtimi. 
boxplot(df_m$kmi, df_v$kmi)
b <- c()
for(kmi_m in df_m$kmi){
  
  for(kmi_v in df_v$kmi){
    
    if(kmi_m == kmi_v){b <- c(b, kmi_m)}
  }
}
max(b)
2) Apskai?iuokite abiej? ly?i? KMI vidurkius pa?alinus min?t? i?skirt?. Ar galime teigti, 
kad vyr? ir moter? populiacijos KMI vidurkis skiriasi. 



