library(fpp3)
#THEORY, GRAPHICS
#2.1  TSIBBLE OBJECTS 

global_economy
# A tsibble: 15,150 x 9 [1Y] : rows x columns [frequency]
# Key:       Country [263]; Shows how many different time series we have

tourism
# A tsibble: 24,320 x 5 [1Q] frequency is now in quarters(seq. of obs quarter apart)
# Key:       Region, State, Purpose [304]

#tsibble allows storage and maniulation of multiple time series
#It contains index, measured_variables(what we want to forecast) and key variables
#Works with tidyverse functions

#Creating tsibble
data <- tsibble(
  year = 2015:2019,
  y=c(123,39,78,52,110),
  index = year
)

#Converting tibble to tsibble
data <- tibble(
  year = 2015:2019,
  y=c(123,39,78,52,110)
) |>
  as_tsibble(index = year)

#Common helper function to convert to time
# yearquarter() - Quarterly
#yearmonth()
#yearweek()
#as_date(), (ymd() or myd() and other possible variations)  - Daily
#as_datetime() - Sub-Daily
?PBS
#Example 
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC/1e6) -> a10



#2.2 TIME PLOTS
a10 |>
  autoplot(Cost) +
  labs(y = "$ (millions)", title ="Australian antidiabetic drug sales")

a10 |>
  autoplot(Cost) + geom_point()#Adding actual obs points

#We can see periodical fluctations - a seasonal trend. An upward trend which is perhaps not linear.
#We can also see that difference between spikes and trough increases whih suggest a multiplicative effect
#Seasonality increases as the level of data increases

ansett

ansett |> autoplot(Passengers)

ansett |> distinct(Classes)
ansett |> distinct(Airports)

ansett |> 
  filter(Class == "Economy") |>
  autoplot()

mel_syd <- ansett |>
  filter(Airports == "MEL-SYD") |>
  select(-Airports)

mel_syd |> autoplot()

mel_syd |>
  filter(Class == "Economy") |>
  mutate(Passengers = Passengers/1000) |>
  autoplot(Passengers)


#2.3. Time series patterns

#Trend pattern exists when there is a LONG-TERM increase or decrease in the data
#Seasonal pattern exists when series is influenced by seasonal factors(over a year, like quarter, months ...)
#Cyclic pattern exists when data exhibit rises and falls THAT ARE NOT OF FIXED PERIOD(duration usually of at least  2 years)
#We could say that pattern is seasonal if its periods T<= 1 year while cyclic T >= 2 years
#But cyclic doesn't have to have the same length between cycles and magnitude is usually more variable
#The period also can differ, like recession we dont know when it's gonna happen
us_employment |>
  filter(Title == "Retail Trade", year(Month) >= 1980) |>
  autoplot(Employed/1e3) 
 
#We can see upward trend up to 1990. Also a seasonal pattern. In addition, here we can see cyclic trend. Periods of decrease followed by increase


#2.4 Seasonal plots

a10 |>
  autoplot(Cost)

#Seasonal plot is a plot of year at each season that data is observed. This could be monthly obs or quarterly.

a10 |> gg_season(Cost, labels = 'both')

#From this seasonal point we can confirm that the drug sales spike at January.
#We can see a small increase from february to march but this is probably due to that february has 28 days
#We can see 2008 March drop which is irregular with the pattern and 2008 June. So we would need to delve deeper into this and understand why that has happened

beer <- aus_production |>
  select(Quarter, Beer) |> filter(year(Quarter) >= 1992)
beer |> autoplot(Beer) + geom_point()
beer |> gg_season(Beer, labels ='right')
#From this seasonal plot we can clearly see that 2004 Q4 is irregular which would be harder to see with time plot


#Multiple seasonal patterns:

vic_elec |> autoplot()
vic_elec |> gg_season(Demand)#If not specified it will plot yearly

vic_elec |> gg_season(Demand, period='week')#Weekly period plot
#In this plot we can see that weekend demand is lower than weekday
#Also we can see that spikes in weekdays are equal of magnitude but in weekends it's lower in morning than evenings
#So when modelling we need to account for which day of the week we are capturing.

vic_elec |> gg_season(Demand, period='day')

#2.5 Seasonal subseries plots

a10 |> gg_subseries(Cost)

beer |> gg_subseries(Beer)
#From this subseries plot we can see that downward trend that we saw from time plot is mostly influenced by Q4 production going down over time
#This info was hard to notice without this plot.

holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
holidays |> autoplot()
holidays |> gg_season(Trips)+
  facet_wrap(vars(State), nrow = 2, scales = "free_y")
holidays |> gg_subseries(Trips)

#2.6. Scatterplots 

vic_elec_day_type <- vic_elec |>
  filter(year(Time) == 2014) |>
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"))
vic_elec_day_type

vic_elec_day_type |> autoplot(Demand)
vic_elec_day_type |> autoplot(Temperature)

vic_elec_day_type |>
  ggplot(aes(x=Temperature, y=Demand, color = Day_Type))+
  geom_point()
#In this case we see non linear relationship. So corr coef. doesn't tell us anything useful


library(GGally)
us_change |> ggpairs(columns=2:6)

#2.7. Lag plots 

new_production <- aus_production |>
  filter(year(Quarter) >= 1992)
new_production |> gg_lag(Beer, geom='point')
#Each graph shows y_t plotted against y_t-k for different values of k. 
#For example, lag1 is k=1. So if we look at Q4 points its being plotted against Q3 of the same year.
#If we look at lag4, k=4. We have Q4 plotted against previous year Q4

new_production |> gg_lag(Beer)

#2.8. Autocorrelation

new_production |> gg_lag(Beer, geom='point')
#Autocorrelations are correlations associated with these scatterplots.
#r1=correlation(y_t,y_t-1) and so on 

#autocovariance is c_k = cov(y_t, y_t-k), autocorrelation r_k = c_k/c_0

new_production |> ACF(Beer, lag_max = 9)#This computes r_k

new_production |> ACF(Beer, lag_max = 9) |> autoplot()
#This plot visualizes the r_k's as black vertical lines. It's called correlogram

retail <- us_employment |>
  filter(Title == "Retail Trade", year(Month)>=1980)
retail |> autoplot(Employed)
retail |> ACF(Employed) |> autoplot()


google_2015 <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date)==2015)  |>
  select(Date, Close)
google_2015 |> ACF(Close, lag_max=100) |> autoplot()

#2.9. White noise

set.seed(30)
wn <- tsibble(t=1:50, y=rnorm(50), index = t)
wn |> ACF() |> autoplot()
#Blue lines show the (95% CI) magnitude of corr coef if data would be just white noise 

pigs <- aus_livestock |>
  filter(State == "Victoria", Animal == "Pigs", year(Month)>=2014)
pigs |> autoplot(Count/1e3)
pigs |> ACF(Count, lag_max=20) |> autoplot()
#This plot has two bars where it goes outside the CI for white noise. We see the highest one at 12. This suggests that there's a bit of seasonality in this series
#We could also compute probability for 2 spikes outside the range if we assume that the series is white noise:
#P(2 spikes out of CI | DATA IS WHITE NOISE AND 20 OBS) =
(0.05)^2 * (0.95)^18 * 10*19
#We get 19% chance that two spikes occur out of range if we assume its white noise and ignore the dependence of each lag.



