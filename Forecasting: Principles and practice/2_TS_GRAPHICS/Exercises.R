#https://otexts.com/fpp3/graphics-exercises.html
#Exercises
#Chapter 2

#2.Use filter() to find what days corresponded to the peak closing price 
  #for each of the four stocks in gafa_stock
gafa_stock |> 
  group_by(Symbol) |>
  filter(Close == max(Close))

#4.The USgas package contains data on the demand for natural gas in the US.
library(USgas)
  #b)Create a tsibble from us_total with year as the index and state as the key.
us <- us_total |>
  mutate(year = as.integer(year)) |>
  as_tsibble(index = year, key = state)

  #c)Plot the annual natural gas consumption by state for the New England area 
  #(comprising the states of Maine, Vermont, New Hampshire, Massachusetts, Connecticut and Rhode Island)
unique(us$state)
us_NE <- us |>
  filter(state %in% c("Maine", "Vermont","New Hampshire", 
                      "Massachusetts" ,"Connecticut",
                      "Rhode Island"))
us_NE |> autoplot(y/1e3) + labs(title="Gas consp. for New England",y ="Billion Cubic Feet")

#6. The aus_arrivals data set comprises quarterly international arrivals to Australia from 
#Japan, New Zealand, UK and the US.
  #Use autoplot(), gg_season() and gg_subseries() 
  #to compare the differences between the arrivals from these four countries.
  #Can you identify any unusual observations?

aus_arrivals |> autoplot()#We can seasonal patterns from all of them and upward trend except Japan which has downward trend from 1999 approx.
aus_arrivals |> gg_season()#We see that seasonal patterns differ for each country. 
aus_arrivals |> gg_subseries()#This shows Japan downward trend isn't influenced by quarters but is consistent
#For Japan we see that from around 1995 the trend is downward
#For NZ the trend is upward 
#For UK we start seeing a downward trend from 2005
#For US we see stagnation from 2005

#7. Monthly Australian retail data is provided in aus_retail. 
#Select one of the time series as follows (but choose your own seed value):
set.seed(1234578)
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`,1))
#Can you spot any seasonality, cyclicity and trend? What do you learn about the series?

myseries |> autoplot(Turnover) #We can see an upward trend and seasonality
#Also we see that the difference between spikes and troughs increase.
#We can see a dip in turnover from 2010 this is probably due to recession

myseries |> gg_season(Turnover)
#We see that turnover peaks at December

myseries |> ACF(Turnover, max_lag=24) |> autoplot()

#9. The following time plots and ACF plots correspond to four different time series. 
#Your task is to match each time plot in the first row with one of the ACF plots in the second row.
#1A,2B,3C,4D

#10.The aus_livestock data contains the monthly total number of pigs slaughtered in Victoria, Australia, from Jul 1972 to Dec 2018. 
#Use filter() to extract pig slaughters in Victoria between 1990 and 1995. 
#Use autoplot() and ACF() for this data. 
live_stock <- aus_livestock |>
  filter(year(Month) >= 1990 & year(Month) <= 1995 & State =="Victoria" & Animal == "Pigs")
live_stock |> autoplot(Count) + geom_point()
live_stock |> ACF(Count) |> autoplot()
#How do they differ from white noise? 
#Strongly, all none of the coefficients are within CI of white noise

#If a longer period of data is used, what difference does it make to the ACF?

#It makes ACF more accurate with more observations, especially at higher lags.

#11. a)Use the following code to compute the daily changes in Google closing stock prices.
dgoog <- gafa_stock |>
  filter(Symbol == "GOOG", year(Date) >= 2018) |>
  mutate(trading_day = row_number()) |>
  update_tsibble(index = trading_day, regular = TRUE) |>
  mutate(diff = difference(Close))

#b)Why was it necessary to re-index the tsibble?
#Because index was made on date and it would be seen as missing observations.

#c)Plot these differences and their ACF.
dgoog |> autoplot(diff)
dgoog |> ACF(diff) |> autoplot()

#d)Do the changes in the stock prices look like white noise?
dgoog |> ACF(diff, lag_max = 50) |> autoplot()
#Yes
