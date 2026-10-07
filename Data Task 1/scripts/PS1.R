library(tidyverse)
library(tseries)
library(urca)
library(forecast)
library(lmtest)
library(ts575unc)
library(sarima)

ercot_resource_output <- read.csv("data/ercot_resource_output.csv")
ercot_resource_types <- read.csv("data/ercot_resource_types.csv")

#1
ercot_resource_output |> 
  distinct(qse)
#194 unique resource names

#2
# A QSE stands for "Qualified Scheduling Entity". A QSE operates as a qualified market participaent able to commuicate and settle payments/charges with ERCOT on the behalf of energy producers and sellers.

#3a
ercot_resource_output |> 
  distinct(qse, resource_name) |> 
  group_by(qse) |> 
  summarise(n = n_distinct(resource_name)) |> 
  arrange(desc(n)) |> 
  head(10)

# Yes there are many such cases where one QSE is paired with more than one resource name. 
#This could indicate that QSEs operate the Resource Names. 
#QTENSK, QLUMN, QNRGTX, QCALP, QECNR, QAEN, QLCRA, QCPSE, QTEN23, QSHEL2 are the top 10

#3b
ercot_resource_output |>
  distinct(qse,resource_name, .keep_all = TRUE) |> 
  group_by(resource_name) |> 
  mutate(n = n_distinct(qse)) |> 
  filter(n > 1) |> 
  View()
#This is true for six resource names. I am unable to find any reason given the data.

#4a
ercot_resource_types |> 
  distinct(resource_type)
 
# 15 unique non-missing values. WIND is wind power. SCGT90 is a single cycle gas turbie producing greater than 90 megawatts. NUC is nuclear power. HYDRO is hydroelectric power.

#4b
ercot_resource_types |> 
  filter(is.na(resource_type))
# GALLOWAY_SOLAR1, ROSELAND_SOLAR3, SSPURTWO_WIND_1, and SWEETWN2_WND24 are missing a resource type. The first two are probably solar and the last two a probably wind. 

types_clean <- ercot_resource_types |> 
  mutate(
    resource_type = case_when(
    str_detect(resource_name, "GALLOWAY_SOLAR1") ~ "PVGR", 
    str_detect(resource_name, "ROSELAND_SOLAR3") ~ "PVGR", 
    str_detect(resource_name, "SSPURTWO_WIND_1") ~ "WIND",
    str_detect(resource_name, "SWEETWN2_WND24")  ~ "WIND", 
    .default = resource_type
    )
  )



types_clean |> 
  filter(str_detect(resource_name, "GALLOWAY_SOLAR1"))
  
  

#5
fuel <- types_clean |> 
  mutate(fuel_types = case_when(
    str_detect(resource_type, "DSL") ~ "Other", 
    str_detect(resource_type, "SCGT90") ~ "Natural Gas",
    str_detect(resource_type, "WIND") ~ "Wind",
    str_detect(resource_type, "PWRSTR") ~ "Other",
    str_detect(resource_type, "HYDRO") ~ "Other",
    str_detect(resource_type, "CCGT90") ~ "Natural Gas",
    str_detect(resource_type, "PVGR") ~ "Solar",
    str_detect(resource_type, "SCLE90") ~ "Natural Gas",
    str_detect(resource_type, "GSREH") ~ "Natural Gas",
    str_detect(resource_type, "CCLE90") ~ "Natural Gas",
    str_detect(resource_type, "CLLIG") ~ "Coal",
    str_detect(resource_type, "GSSUP") ~ "Natural Gas",
    str_detect(resource_type, "NUC") ~ "Nuclear",
    str_detect(resource_type, "GSNONR") ~ "Natural Gas",
    str_detect(resource_type, "RENEW") ~ "Other"
    )
  )

fuel |> 
  filter(is.na(fuel_types))

#6
output_data <- left_join(ercot_resource_output, fuel, by = "resource_name") |> 
  mutate(
    sced_time_stamp = mdy_hm(sced_time_stamp),
    date = date(sced_time_stamp), 
    hour = hour(sced_time_stamp), 
    week = week(sced_time_stamp),
    week = as.factor(week),
    day = weekdays(sced_time_stamp), 
    day = fct_relevel(day, "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    ) 
  

#6a with day date
output_data |> 
  group_by(date) |> 
  summarise(net_day_output = sum(telemetered_net_output)) |> 
  ggplot(aes(date, net_day_output)) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  geom_line()

# This shows a very cyclic energy consumption pattern in this time series with energy peaking in the middle of the week and dipping during the weekend. This probably due to energy providers trying to match energy demand, as it would make sense that energy consumption would drop off during the weekend as many business close.

#6a aggregating day of the week
output_data |> 
  group_by(day) |> 
  summarise(net_day_output = sum(telemetered_net_output)) |> 
  ggplot(aes(day, net_day_output)) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  geom_col()

#Better confirms the observations that energy output peaks in the middle of the weeksn and dips around the weekend. This plot seems to show the dip during the weekend occurs close to the end of the week into Saturday, but a rise on Sunday which appears to be greater than Thursday or Friday which is interesting. 

#6b
output_data |> 
  group_by(hour) |> 
  summarise(net_hour_output = sum(telemetered_net_output)) |> 
  ggplot(aes(hour, net_hour_output)) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  geom_line()

#Indicates power output rises sharply in the morning, dips to a local minimum around 15:00 and rises again to a local maximum around 19:00. This would track well with energy consumption where energy consumption rises in the morning as consumers wake-up, dips as the head to work, and rises again when they return home. 

#6c as columns
output_data |> 
  group_by(hour, fuel_types) |> 
  summarise(net_hour_output = sum(telemetered_net_output)) |> 
  ggplot(aes(x = hour, y = net_hour_output, fill = fuel_types)) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  geom_col()

#Shows wind and natural gas dominating as fuel sources. Shwow a rise in solar production starting at 8:00 and decline at 18:00 which roughly aligns with sunrise and sunset in North America during the winter months.

#6c as lines
output_data |> 
  group_by(hour, fuel_types) |> 
  summarise(net_hour_output = sum(telemetered_net_output)) |> 
  ggplot(aes(x = hour, y = net_hour_output, color = fuel_types)) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  geom_line() +
  facet_wrap(~fuel_types)

#Better confirms the solar generation observation. Natural gas, coal, and other fueld sources resemble the cyclic power consumption of the aggregated by-hour chart as these would seem sources have the ability to start and stop at will, while wind and solar are variable to natural conditions and not market/consumption demand. Nuclear seams to remain fixed throughout the day indicating the ability to start and stop nuclear generation is likely not easy, thus a constant output. 

#7
#The data does look stationary.

data <- output_data |> 
  group_by(date) |> 
  summarise(net_day_output = sum(telemetered_net_output))

daily_output_ts<- as.ts(data$net_day_output, start = c(2023, 1), frequency = 365)

urca_output <- ur.df(daily_output_ts, type = "none", selectlags = "AIC")
summary(urca_output)

#Results confirm the data is non-stationary with a test-statistic of -0.4838 and -1.61 critical level at the 10% level using the ADF test.
#This means there is likely a unit root.

diff_output <- diff(daily_output_ts)
urca_diff_output <- ur.df(diff_output, type = "none", selectlags = "AIC")
summary(urca_diff_output)

plot(diff_output)

#This looks far more stationary, though the spike at the period 10 seems a bit anamolous. 

#8

day_hour_ts<- output_data |> 
  mutate(day_hour = floor_date(sced_time_stamp)) |> 
  group_by(day_hour) |> 
  summarise(net_hour_output = sum(telemetered_net_output)) |>
  pull(net_hour_output) |> 
  as.ts(start(2023, 1), frequency = 8760)

AR3_model<- Arima(day_hour_ts, order = c(3, 0, 0))
summary(AR3_model)
plot(AR3_model$residuals)

Box.test(AR3_model$residuals, lag = 10, type = "Ljung-Box")

# An AR(3) model fits but is not well specified. While the model fit is good, but the residuals do not test as white-noise indicating some serial autocorrelation in the residuals. This means that an AR(3) is likely underspecified. I tested for higher order ARs and found an AR(10) was the best specification.
# I do not think I would choose to fit an AR model onto this dataset because the autoregressive explaination is lacking. How would energy output influence future energy output?
# Furthermore, the cyclic nature oberserved is best explained as seasonality. 
# Energy output is likely highly correleated with energy consumption. 
# Energy consumption likely exhibits a high degree of seasonality in both within the day, the week, and year since patterns in daily activity (e.g. waking up in the morning, returning home in the evening, etc.), patterns in weekly activiety (e.g. Moreeconomic activeity occures during the work week), and yearly activity (seaonal temperature impacts energy needs through intensity of heating and cooling systems)
# Thus energy output likely follows a similar highly seasonl cyclic variation. 

#9a
fuel_model_i <- lm(data = output_data, telemetered_net_output ~ fuel_types)
summary(fuel_model_i)


#9b
weekday_model <- lm(data = output_data, telemetered_net_output ~ day)
summary(weekday_model)

#9c

week_model <- lm(data = output_data, telemetered_net_output ~ week)
summary(week_model)  

output_data |> 
  mutate(day_hour = floor_date(sced_time_stamp)) |> 
  group_by(day_hour, fuel_types) |> 
  summarise(net_hour_output = sum(telemetered_net_output)) |>
  ggplot(aes(x = day_hour, y = net_hour_output, color = fuel_types)) +
  scale_y_continuous(
    labels = function(y) paste0(y / 1000, "k")
  ) +
  geom_line() +
  facet_wrap(~fuel_types)

# The fuel types regression sets coal as the intercept. The coefficients indicate that most fuel types produce lower net output at any given minute compared to coal. This tracks for Solar and other types, but not for wind and natural gas. My guess is that the variability in these two types lead to large outliers in output production that skews this (though shouldn't this disappear in aggregate?) 
# The weekday model fits really well the idea that energy output, closely meeting consumption, peaks during the work week and drops on the weekend as many business close or reduce hours.
# The last plot could be explained by temperatures, as the biggest variability from week to week is likely related to fluctutation in heating/cooling needs. With longer time horizens this would likely be more obvious. 