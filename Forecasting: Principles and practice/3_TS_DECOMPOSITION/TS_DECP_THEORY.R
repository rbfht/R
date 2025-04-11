#THEORY
#3. TIME SERIES DECOMPOSITION
library(fpp3)
#3.1 Transformations and adjustments
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP/Population)
#Inflation adjustments when dealing with money over years

#Mathematical transformations
food <- aus_retail |>
  filter(Industry == "Food retailing") |>
  summarise(Turnover = sum(Turnover))
food |> autoplot()#The variations between peaks keep getting larger as years increase.
food |> autoplot(sqrt(Turnover))
food |> autoplot(log(Turnover))

#Box-Cox transformations
#w_t =log(y_t) lembda = 0
#w_t = (sign(y_t)*|y_t|^(lembda) - 1)/(lembda)

#We choose lembda so that var is even. 

#If the model is additive:
#trend has constant var because we model it as some function and seasonal
#component also has constant var because it's periodic so if all three components
#have const var then that implies that the sum of those have const. var.

#If the model is multiplicativie:
#Same argument doesn't hold because suppose we have growing trend then the variation of the product will increase
#We could apply ln transformation and make the same argument

#Guerrero transformation is an alogrithm to choose a good lembda estimate:
#Always check results, small lambda may give extremely large pred. intervals
food |>
  features(Turnover, features = guerrero)
food |> autoplot((sign(Turnover)*abs(Turnover)^(0.0895)- 1)/0.0895)
# OR MORE EFFECTIVE SYNTAX:
food |> autoplot(box_cox(Turnover, 0.0895))

#3.2. Time series components
 
#Y_t = F(S_t,T_t,R_t), where S_t - seasonal, T_t - Trend and cyclic, R_t - remainder

#The additive decomposition is the most appropriate if the magnitude of the seasonal fluctuations,
#or the variation around the trend-cycle, does not vary with the level of the time series.

#When the variation in the seasonal pattern, or the variation around the trend-cycle,
#appears to be proportional to the level of the time series, 
#then a multiplicative decomposition is more appropriate. 
#Multiplicative decompositions are common with economic time series.
#OR USE LOG TRANSFORMATION AND USE ADDITIVE

us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp) |>
  autoplot()
