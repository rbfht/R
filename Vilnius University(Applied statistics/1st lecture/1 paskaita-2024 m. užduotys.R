#Uzduotys.

#1. 
#Pakete UsingR suraskite duomenis homedata apie nekilnojamojo turto kainas.
#Naudodami duomenis is 1970 metu  stulpelio, duomenis suskirstykite
#i 3 intervalus, uzdarus is kaires ir atvirus is desines,
#skaidymo taskais imdami 0,50000,100000,300000. 
#Padarykite lenteles skrituline diagrama. 
#My sltn:
library(UsingR)
homedata[1:10,]
int.galai <- c(0,50000,100000,300000)
skirstymai <- cut(homedata$y1970, int.galai, right = FALSE)
pie(table(skirstymai))
#2.
#Duomenyse homedata skirtingu metu duomenis padalinkite i 3 intervalus,
#uzdarus is kaires ir atvirus is desines,skaidymo taskais 
#imdami 0,100000,500000,1200000. Padarykite palyginamasias staciakampes diagramas,
#pirmoje palygindami 1970 ir 2000 metu duomenis skirtingu intervalu kategorijose,
#antroje diagramoje palyginkite skirtingu intervalu kategoriju pasiskirstyma 1970 ir 
#2000 metais. 
#My sltn:
int.galai <- c(0,100000,500000,1200000)
padalijimas70 <- cut(homedata$y1970, int.galai, right = FALSE)
padalijimas00 <- cut(homedata$y2000, int.galai, right = FALSE)
barplot(table(padalijimas70, padalijimas00), beside = TRUE, col=c("blue1","green"), legend.text=c("1970","2000"))
prop70 <- proportions(table(padalijimas70))
prop00 <- proportions(table(padalijimas00))
install.packages("dplyr")
library(dplyr)
#Converting the tables to dataframes to perform full join
proc70 <- as.data.frame(round(prop70*100))
proc00 <- as.data.frame(round(prop00*100))
colnames(proc70) <- c("Intervalas", "Freq")
colnames(proc00) <- c("Intervalas", "Freq")
table_proc_both = full_join(proc70, proc00, by = "Intervalas")
table_proc_both
barplot(c(table_proc_both$Freq.x, table_proc_both$Freq.y),
	names.arg = rep(table_proc_both$Intervalas, 2),
	col = c("blue", "green"),
	legend.text= c("1970", "2000"),
	beside = TRUE,
	xlab="Intervalai",
	ylab="Procentai",
	ylim=c(0,100))
#3.
#Duomenu  homedata 2000 metu stulpeli padalykite i 5 vienodo ilgio intervalus 
#atvirus is kaires, bet uzdarus is desines, atitinkamai parinkdami skaidymo taskus.
#Nubrezkite atitinkama histograma, kurioje matytusi NT vienetu pasiskirstymas
#pasirinktose kainu kategorijose.
#My sltn:
library(UsingR)
max(homedata$y2000)
int.galai <- seq(min(homedata$y2000), max(homedata$y2000), length.out=6)
hist(homedata$y2000,
	breaks = int.galai, 
	col = "green",
	right = TRUE,
	probability=TRUE)
#4.
#Pakete UsingR suraskite duomenis five.yr.temperature apie 1995-2000 metu temperatura.
#Temperaturos duomenis padalykite i intervalus:uzdarus is kaires ir atvirus
#is desines,skaidymo taskais imdami 0,40,80,100. 
#Padarykite palyginamasias staciakampes intervalines temperaturos diagramas, pagal 
#skirtingu metu kategorijas. 
#Sltn nr.1
library(UsingR)
df <- five.yr.temperature
int.galai <- c(0,40,80,100)
library(dplyr)
library(tidyr)
df_new <- df |>
  mutate(intervalai = cut(temps, int.galai, right = FALSE))|>
  count(years, intervalai)|>
  group_by(years) |>
  mutate(Procentai = round((n/sum(n))*100, 2)) #Nereikia apib n, nes count priskiria by default
  #install.packages("ggplot2")
library(ggplot2)
df_new$years <- factor(df_new$years)#Pakeiciam years stulpeli i kategorijas, o ne skaicius, kad veiktu fill funkcija ggplot, nes ji kitaip laiko kaip tolydu kintamaji
df_new |> 
  ggplot(mapping = aes(x = intervalai, #Grupes
                       y = Procentai,#aukstis
                       fill = years))+#fill pasako spalvas, t.y. kiek elementu bus kiekvienoj grupej
  geom_bar(stat = "identity", position = "dodge")+#stat naudojame, kad imtu the ne el.skc kiekviename intervale, o turimas vertes, position, kad butu vienas salia kito, o ne ant virsaus
  scale_fill_manual(values = c("green", "blue", "red", "purple", "orange", "yellow", "black")) +
  labs(x = "Intervalai", y = "%", fill = "Metai") +
  theme_minimal()
#Sltn nr.2
#df_new <- mutate(df, intervalai = cut(df$temps, int.galai, right = FALSE))
#perc95 <- round(proportions(table(filter(df_new, years == 1995)$intervalai))*100)
#perc96 <- round(proportions(table(filter(df_new, years == 1996)$intervalai))*100)
#perc97 <- round(proportions(table(filter(df_new, years == 1997)$intervalai))*100)
#perc98 <- round(proportions(table(filter(df_new, years == 1998)$intervalai))*100)
#perc99 <- round(proportions(table(filter(df_new, years == 1999)$intervalai))*100)
#perc00 <- round(proportions(table(filter(df_new, years == 2000)$intervalai))*100)
#barplot(matrix(c(perc95, perc96, perc97, perc98, perc99, perc00), nrow = 6, byrow = TRUE),
#	names.arg = unique(df_new$intervalai),
#beside = TRUE,
#	col = c("green", "blue", "red", "purple", "orange", "yellow"),
#	legend.text = c("1995", "1996", "1997", "1998", "1999", "2000"),
#	ylim = c(0,100),
#	xlab = "Temp",
# ylab = "%")
5.
#Duomenyse five.yr.temperature imdami stulpeli years, juos suskaidykite i dvi
#kategorijas: imtinai 1995-1998 ir imtinai 1998-2001
#Temperaturos duomenis padalykite i intervalus:uzdarus is kaires ir atvirus
#is deeines,skaidymo taskais imdami 0,40,80,100. (kaip ir 4-oje uzd.)
#Padarykite palyginamasias staciakampes intervalines temperaturos diagramas, pagal 
#sias dvi sukurtas  metu kategorijas. 
#Pakartokite siuos skaiciavimus imdami metu kategorijas: 
#  1995-1998 (imtinai)  ir  1998-2001 (imtinai) 

#Padarykite abi diagramas viename lape.
#?par
#par(mfrow=c(1,2))
#My sltn
library(UsingR)
df <- five.yr.temperature
int.galai_temp <- c(0,40,80,100)
int.galai_met <- c(1995,1998,2001)
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#Funkcija keisti intervalus
df_new <- function(df, breaks_t, breaks_m, right){return(df |>
  mutate(temp_int = cut(temps, breaks_t, right = right)) |>
  mutate(met_int = cut(years, breaks_m, right=right)) |>
  drop_na(met_int) |>
  group_by(met_int) |>
  count(temp_int) |>
  mutate(procentai = round(100*n/sum(n), 2)) |>
  dplyr::select(-n))}
#Funkcija grafikui
plot <- function(df){return(df |>
  ggplot(mapping = aes(x=temp_int,
                       y=procentai,
                       fill=met_int))+
  geom_bar(stat = "identity", position = 'dodge')+
  labs(x="Temp. Intervalai", y="%", fill="Metu Intervalai")+
  theme_minimal())}
#ggplot2 funkcija vietoj par(), kuri veikia tik su bazine R grafika
grid.arrange(plot(df_new(df, int.galai_temp, int.galai_met, T)), plot(df_new(df, int.galai_temp, int.galai_met, F)), ncol = 2)
#6.
#Nubrezite dvi normaliasias kreives su vidurkiu 2  ir standartiniu nuokrypiu
#0.5 ir 1 atitinkamai. Plota nuo vidurkio iki artimiausiu dvieju
#siu kreiviu susikirtimo tasku nuspalvinkite geltonai. Susikirtimo taskus galite
#nustatyti empiriskai.
#My sltn 1:
#d1 <- dnorm(seq(0,4, 0.01) , mean =2 , sd=0.5)
#d2 <- dnorm(seq(0, 4, 0.01), mean =2 , sd=1)
#library(ggplot2)
#df <- data.frame(x = seq(0,4,0.01),  y1=d1, y2=d2)
#intersection <- df %>% 
#  filter(abs(y1-y2) <= 0.001 )#Neveikia su |>, nes filter yra is dplyr 
#df |>
#  ggplot(aes(x=x)) +
#  stat_function(fun = dnorm, args = list(mean=2, sd=1), color = 'blue') +
#  stat_function(fun=dnorm, args=list(mean=2, sd=0.5), color = 'red') +
#  geom_ribbon(
#    data = df |> filter(x >= min(intersection$x) & x <= max(intersection$x)),
#    aes(ymin = 0, 
#        ymax = pmin(y1, y2)),
 #       fill = 'yellow'
#  ) +
#  theme_minimal()
#Sltn nr.2
library(ggplot2)
library(dplyr)
d1 <- dnorm(seq(0,4, 0.01) , mean =2 , sd=0.5)
d2 <- dnorm(seq(0, 4, 0.01), mean =2 , sd=1)
df <- data.frame(x = seq(0,4,0.01),  y1=d1, y2=d2)
diff <- function(x){return(dnorm(x, mean=2, sd=0.5)-dnorm(x, mean=2, sd=1))}
root1 <- uniroot(diff, interval = c(0,2))$root
root2 <- uniroot(diff, interval = c(2,4))$root#Extracting actual values with $root
df |>
  ggplot(aes(x=x)) +
  stat_function(fun = dnorm, args = list(mean=2, sd=1), color = 'blue') +
  stat_function(fun=dnorm, args=list(mean=2, sd=0.5), color = 'red') +
  geom_ribbon(
    data = df |> filter(between(x, root1, root2)),
    aes(ymin = 0, 
        ymax = pmin(y1, y2)),
    fill = 'yellow'
  ) +
  theme_minimal()