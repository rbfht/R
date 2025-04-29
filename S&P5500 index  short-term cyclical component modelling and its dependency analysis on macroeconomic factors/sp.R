#SP
ind <- read.csv('sp500_index.csv')
funds_rate <- read.csv("FEDFUNDS.csv")
CPI <- read.csv("CPIAUCSL.csv")
unemployment <- read.csv("UNRATE.csv")
treasury_yield <- read.csv("GS10.csv")

library(fpp3)

#Converting to monthly, starting from 2014-11:
ind <- ind[-c(1,2,3,4,5,6,7,8,9,10),]
#Converting date into date format and renaming the price column
ind <- ind |>
  mutate(Date = ymd(Date), SP500=ind$S.P500) |>
  select(-S.P500)
#Creating column month and adding day = 01 so that it matches other data sets(preparing for merge)
ind$Month <- format(ind$Date, "%Y-%m-01")
#Calculating mean for every month
ind_m <- aggregate(SP500 ~ Month,
                  data = ind,
                  FUN = mean)
#Changing column name(preparing for merge)
colnames(ind_m)[colnames(ind_m) == "Month"] <- "observation_date"

#Dropping 2014-10
funds_rate <- funds_rate[-1,]
CPI <- CPI[-1,]
unemployment <- unemployment[-1,]
treasury_yield <- treasury_yield[-1,]

#Merging data
data <- ind_m |>
  full_join(funds_rate, by="observation_date") |>
  full_join(CPI, by="observation_date") |>
  full_join(unemployment, by="observation_date") |>
  full_join(treasury_yield, by="observation_date")

#Changing format to date for indexing
data$observation_date <- as.Date(data$observation_date)
data$observation_date <- format(data$observation_date, "%Y-%m")
data_f <- data |>
  mutate(observation_date = yearmonth(observation_date))

#Converting data frame to tsibble
data <- as_tsibble(data_f, index = observation_date)

#Taking ln for all values to fix variances
data <- data |>
  mutate(across(c(SP500, FEDFUNDS, CPIAUCSL, UNRATE, GS10),
                ~ box_cox(.x, 0)))


data |> gg_lag()
data |> ACF() |> autoplot()


#Decomposition

#Moving average
#data_ma <- data |>
#  mutate(
#    `6-MA` = slider::slide_dbl(SP500, mean,
#                                .before = 2, .after=3, .complete=TRUE),
#    `3x6-MA` = slider::slide_dbl(`6-MA`, mean,
#                                  .before = 2, .after= 0, .complte=TRUE))

#data_ma |>
#  autoplot()+
#  autolayer(data_ma, vars(`3x6-MA`))




#STL
dcmp_index <- data |> model(stl = STL(SP500))
dcmp_funds <- data |> model(stl = STL(FEDFUNDS))
dcmp_cpi <- data |> model(stl = STL(CPIAUCSL))
dcmp_unrate <- data |> model(stl = STL(UNRATE))
dcmp_gs10 <- data |> model(stl = STL(GS10))

rem_ind <- components(dcmp_index)$remainder
rem_funds <- components(dcmp_funds)$remainder
rem_cpi <- components(dcmp_cpi)$remainder
rem_unrate <- components(dcmp_unrate)$remainder
rem_gs10 <- components(dcmp_gs10)$remainder

#Checking stationarity
library(tseries)
adf.test(rem_ind)#Stat
adf.test(rem_funds)#Stat
adf.test(rem_cpi)#Stat
adf.test(rem_unrate)#Stat
adf.test(rem_gs10)#Stat

#Testing normality
vars <- list(
  ind    = rem_ind,
  funds  = rem_funds,
  cpi    = rem_cpi,
  unrate = rem_unrate,
  gs10   = rem_gs10
)

sapply(vars, function(x) {
  if (length(na.omit(x)) <= 5000) {
    pval <- shapiro.test(x)$p.value
    if (pval > 0.05) {
      "Normal"
    } else {
      "Not normal"
    }
  } else {
    "Sample too big for Shapiro-Wilk"
  }
})
#All of the data isn't normal

#Correlations: 
df_res <- as.data.frame(vars)
korel_matrica <- cor(df_res, 
                     method = "spearman", 
                     use = "pairwise.complete.obs")
library(corrplot)
corrplot(korel_matrica)

#Visualizations
df_res |> 
  ggplot(aes(x=funds, y=ind)) +
  geom_point()

df_res |> 
  ggplot(aes(x=unrate, y=ind)) +
  geom_point()

df_res |> 
  ggplot(aes(x=cpi, y=ind)) +
  geom_point()

df_res |> 
  ggplot(aes(x=gs10, y=ind)) +
  geom_point()

boxplot(df_res)

components(dcmp_index) |>
  autoplot()
components(dcmp_unrate) |>
  autoplot()
components(dcmp_funds) |>
  autoplot()



#Multi-linear regression:
linear_model_remainder <- lm(df_res$ind ~ df_res$funds + df_res$unrate + df_res$cpi + df_res$gs10)
linear_model_trend <- lm(components(dcmp_index)$trend ~ components(dcmp_funds)$trend + components(dcmp_cpi)$trend + components(dcmp_unrate)$trend + components(dcmp_gs10)$trend)
summary(linear_model_trend)
#Checking normality residual normality:
shapiro.test(linear_model_remainder$residuals)
shapiro.test(linear_model_trend$residuals)

#Checking multicolinearity:
library(car)
vif(linear_model_remainder)#Nera multikolinearumo
vif(linear_model_trend)#Yra multikolinearumo 

#Checking outliers
library(car)
influencePlot(linear_model_remainder)
influencePlot(linear_model_trend)

#Checking homoscedacity 
library(lmtest)
bptest(linear_model_remainder)#heteroscedacity
bptest(linear_model_trend)#heteroscedacity


#HC3
library(sandwich)
library(lmtest)
coeftest(linear_model_remainder, vcov = vcovHC(linear_model_remainder, type = "HC3"))
coeftest(linear_model_trend, vcov = vcovHC(linear_model_trend, type = "HC3"))

#Creating simpler model
simpler <- lm(df_res$ind ~ df_res$funds + df_res$unrate)

summary(simpler)#ADJ-R: 0.2938,

plot(x=simpler$fitted.values, y=simpler$residuals)

library(car)
influencePlot(simpler)

data[c(50,65,66,88),]


bptest(simpler)
plot(simpler$fitted,simpler$residuals)
abline(0,0)

vif(simpler)

shapiro.test(simpler$residuals)#Non-normal
plot(density(simpler$residuals))

library(forecast)
resid_vec <- residuals(simpler)
ggAcf(resid_vec) +
  ggtitle("Autokoreliacijos koeficientai liekamosioms paklaidoms")

dwtest(simpler)#Are autocorrelated


diag_df <- data.frame(
  fitted = fitted(simpler),
  resid  = residuals(simpler),
  stdres = rstandard(simpler),
  actual = df_res$ind
)

# Q–Q plot
ggplot(diag_df, aes(sample = stdres)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  labs(
    title = "Standartizuotų liekanų Q–Q grafikas ",
    x     = "Teoretiniai kvantiliai",
    y     = "Standartizuotos liekanos"
  )

# Residuals vs. Fitted
ggplot(diag_df, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "blue") +
  labs(
    title = "Liekamųjų paklaidų ir prognozuojamų reikšmių grafikas",
    x     = "prognozuojamos reikšmės",
    y     = "liekamosios paklaidos"
  )

ggplot(diag_df, aes(x = fitted, y = actual))+
  geom_point(alpha = 0.8)+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(
    x = "Prognozuojama kaina",
    y = "Tikroji kaina",
    title = "Prognozuojamos vs tikrosios"
  ) +
  theme_minimal

