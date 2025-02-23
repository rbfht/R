#Uzduotys.
li
1. 
#Pakete lillies paimkite duomenis simu_data. Raskite:
#a) Mirties amziaus riba, kurios nevirsija ne maziau 95 procentai visu 
#variacines eilutes reiksmiu  ir ne maziau 5 procentai variacines eilutes 
#reiksmiu yra uz ja nemazesni;
#b)tikimybe, kad zmogus mirs imtinai nuo 75 iki 85
#c)tikimybe, kad zmogus gyvens daugiau, kaip 75 metus imtinai;
#d) mirties amziaus riba, kurios nevirsija ne maziau puse visu 
#variacines eilutes reiksmiu  ir ne maziau puse  ju yra uz ja nemazesni.
#e) Padarykite kvantiliu grafika ir is jo padarykite isvadas apie mirties
#amziaus skirstinio normaluma. 
#Sltn:
#a)
library(lillies)
library(dplyr)
attach(simu_data)
quantile(age_death, 0.95)
#b)
sum(age_death>=75 & age_death <= 85)/sum(age_death>-1)
#c)
virs <- simu_data |> filter(age_death>=75) |> count()
total <- simu_data |>
  count(age_death) |> 
  summarise(sum(n))#You can use length or tally here aswell
proc <- virs/total
#d)
median(age_death)
#e)
qqnorm(age_death)
qqline(age_death)
#Skirstinys yra nenormalus
#2.
#Bibliotekoje UsingR suraskite duomenis five.yr.temperature. 
#a) Atskirai apskaiciuokite kiekvienu metu temperaturos vidurkius. 
#Tuomet, apskaiciuokite visu metu temperaturos vidurki ir kiekvienu metu 
#temperaturu vidurkiu vidurki. Ka galite pasakyti apie gautus rezultatus. 
library(UsingR)
library(tidyr)
#pivot_wider wasn't working as it said there are duplicates, so there's a mistake in dataset
#Finding duplicates
duplicates <- five.yr.temperature |>  
  group_by(days, years) |>
  tally() |>
  filter(n > 1)
print(duplicates)
#Creating a new dataframe where we drop the duplicating rows and add a new row which is a mean of the two previous
df <- five.yr.temperature |>
  filter(!(days == '60' & years == '1996'))
  bind_rows(tibble(days = 60, years=1996, temps = mean(c(33.1,29.2))))
#Reshaping our dataframe(wide format)
df_wide_n <- df |>
  pivot_wider(names_from = years, values_from = temps)
#Deleting days which don't have a recording in at least one year
df_wide <- na.omit(df_wide_n)
#Creating a function to calculate a mean for a given year
vid_m <- function(df, year){
  return(mean(df[[as.character(year)]]))
}
#Creating a loop for each year
vid_s <- list()
for (year in unique(df$years)){
  value <- vid_m(df_wide, year)
  vid_s[[length(vid_s)+1]] <- value
  print(year)
}
vid_b <- mean(df$temps)
vid_vid <- mean(unlist(vid_s))
print(vid_s)
print(vid_b)
print(vid_vid)
#Matome, kad vidurkiu vidurkis yra gan didesnis uz bendra vidurki, tai reiskia, kad tam tikri metai buvo siltesni

#b) Padarykite kiekvienu metu temperaturu dezinius  grafikus ir raudonu tasku
#pazymekite juose vidurki. 
boxplot(df_wide$`1995`,df_wide$`1996`,df_wide$`1997`,df_wide$`1998`,df_wide$`1999`,df_wide$`2000`,df_wide$`2001`)
points(unlist(vid_s), col='red')
#3.
#Triju sigma taisykle
#Padarykite brezini, kuris vizualizuotu 3 sigma taisykle. 
#Pradzioje nubrezkite standartinio normaliojo skirstinio grafika. 
#Geltonai nuspalvinkite grafiko uzimama plota, kuris nutoles nuo vidurkio per 1
#standartini nuokrypi, zaliai per du standartinius nuokrypius ir melynai
#per tris standartinius nuokrypius. Uzdekite atitinkama legenda.
#Patikslinkite gauta taisykle surasdami atitinkamus normaliojo skirstinio 
#kvantilius. ]
fn <- function(x) dnorm(x,)
library(ggplot2)
ggplot()+
  stat_function(fun=dnorm, args=list(mean=0,sd=1))+
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "area",
    xlim = c(-1, 1),
    fill = "yellow",
    alpha = 0.5)+
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "area",
    xlim = c(-2, -1),
    fill = "green",
    alpha = 0.5)+
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "area",
    xlim = c(1, 2),
    fill = "green",
    alpha = 0.5)+
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "area",
    xlim = c(-3, -2),
    fill = "blue",
    alpha = 0.5)+
  stat_function(
    fun = dnorm,
    args = list(mean = 0, sd = 1),
    geom = "area",
    xlim = c(2, 3),
    fill = "blue",
    alpha = 0.5)+
  xlim(-5,5)+
  theme_minimal()
4. Papildoma u?duotis.
?vykd? komandas
par(mfrow=c(2,1))
x=seq(-3,3,length=100) 
plot(x,dnorm(x),type="l") 
plot(density(rnorm(x))) 
gausite du normaliojo skirstinio tankio funkcij? grafikus. I?nagrin?j?
grafik?  skirtumus i?siai?kinkite k? atlieka kiekviena i? ?i? komand?. 

5. Papildoma u?duotis
Apie komand? which
?i komanda leid?ia nustatyti imties eil?s numer?.
Surinkite ir i?siai?kinkite k? padaro ?i komanda. 
which(c("A","C","B","C","C")=="C")
Duomenyse simu_data (paketas "lillies") pa?alinkite visas eilutes,
kuriuose mirties data ma?esn? u? 80 met?. Nubrai?ykite gaut? nauj? 
duomen? tankio funkcijos grafik?.
 



