library(tidyverse)
library(xts)
library(tidyquant)
library(fixest)

ozone_df <- readRDS("data/data-set-2025-ozone-R.Rdata")
PM_df <- readRDS("data/data-task-2025-PM-R.Rdata")
#Section 1
#1
ozone_clean <- ozone_df |> 
  mutate(date  = mdy(date), 
         date = ymd(date))

PM_clean <- PM_df |> 
  mutate(county_code = as.numeric(county_code), 
         date = ymd(date))

ozone_pm_data<- left_join(ozone_clean, PM_clean, by = join_by("county_code", "date", "siteid"))

#2
ozone_pm_data |>
  pivot_longer(
    cols = c(pm25_value, ozone_value, aqi),
    names_to = "pollutant",
    values_to = "value"
  ) |>
  group_by(pollutant) |>
  summarise(
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    min = min(value, na.rm = TRUE),
    max = max(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE)
  )

#3
ozone_pm_data |>
  group_by(ozone_source) |>
  summarise(
    mean = mean(ozone_value, na.rm = TRUE),
    median = median(ozone_value, na.rm = TRUE),
    min = min(ozone_value, na.rm = TRUE),
    max = max(ozone_value, na.rm = TRUE),
    sd = sd(ozone_value, na.rm = TRUE), 
    count = n()
  )

#4
# The table could suggest some systemtic differences. 
# The AirNow data shows a lower reported ozone pollution level in both the mean and median valaues compared to AQS. 
# It also shows a signifcantly smaller maximum value. 
# To further verify I would want to only look at the summarys stats where they two sources share the same site location. 
# If the difference holds then it would clearly show a systemic under reporting compared to AQS.


  
#5
ozone_pm_data |>
  group_by(county_code, date) |> 
  filter(n_distinct(ozone_source)>1) |> 
  group_by(ozone_source) |>
  summarise(
    mean = mean(ozone_value, na.rm = TRUE),
    median = median(ozone_value, na.rm = TRUE),
    min = min(ozone_value, na.rm = TRUE),
    max = max(ozone_value, na.rm = TRUE),
    sd = sd(ozone_value, na.rm = TRUE), 
    count = n()
  )
  
county_day_data <- ozone_pm_data |> 
  group_by(county_code, county_name, date, cbsacode, cbsaname) |> 
  summarise(
    aqi = mean(aqi, na.rm = TRUE),
    ozone_value = mean(ozone_value, na.rm = TRUE), 
    pm25_value = mean(pm25_value, na.rm = TRUE), 
    mortality = mean(mortality, na.rm = TRUE) 
  ) |> 
  ungroup()
# I am choosing to average out the across multiple sites for a single county. 

#6
county_day_data |> 
  group_by(county_name) |> 
  distinct(county_name)

county_day_data |> 
  group_by(county_code, county_name, cbsacode, cbsaname) |> 
  complete(date = seq.Date(min(date), max(date), by = "day")) |> 
  filter(is.na(aqi), is.na(ozone_value), is.na(pm25_value))|>
  group_by(county_name) |> 
  distinct(county_name)

# 31 counties are missing dates

# Section 2
#1
county_day_data |> 
  filter(!is.na(pm25_value), !is.na(ozone_value)) |>
  mutate(
    pm25_value  = as.numeric(pm25_value), 
    ozone_value = as.numeric(ozone_value),
    pm25_std    = as.numeric(scale(pm25_value)), 
    ozone_std   = as.numeric(scale(ozone_value)) 
    )|> 
  pivot_longer(
    cols = c(pm25_std, ozone_std),
    names_to = "pollutant",
    values_to = "value"
  ) |> 
  ggplot(aes(x = value, color = pollutant, linetype = pollutant)) +
  geom_density( alpha = 0.4, position = "identity") 

#2
la_feb24_oz_ts <- county_day_data |> 
  filter(county_code == 37, month(date) == 2) |> 
  with(xts(ozone_value, order.by = date))

colnames(la_feb24_oz_ts) <- "Value"

la_feb24_oz_ts |> 
  ggplot(aes(x = Index, y = Value)) +
  geom_line() +
  theme_minimal()

#It appears fairly cyclic which could point to some autocorrelation. You could test for autocorrelation by ploting the ACF and PACFs for the time series an analyzing. You could also try a Q-test using a few lags.

acf(la_feb24_oz_ts)
pacf(la_feb24_oz_ts)

#Sections 3

#1. 

pollution_mortality_model <- feols(data = county_day_data, mortality ~ aqi | county_name + date)
summary(pollution_mortality_model)

# beta1 is 0.528, meaning that every 1 unit change in the air quality index is associated with a 0.528 point increase in mortality
# The fixed effects are there to capture the time-invariant country level characteristics (e.g. fixed policies, income, culture etc.) and broader state, country, or world level changes over time (changes in state policy, changes in national policy, country level economic cycles, etc.)

#2
lag_pollution_mortality_model <- feols(
  mortality ~ aqi + l(aqi, 1)| county_name + date,
  data = county_day_data,
  panel.id = ~ county_name + date
  )
summary(lag_pollution_mortality_model)

# Beta 1 is 0.305 meaning a one unit increase in the today's air quality index leads to a 0.305 point increase in today's mortality.
# Beta 2 is a 0.369 maning a one unit increase in yesterday's air quality index leads to a 0.369 point increase in today's mortality.

#3
reg_data <- county_day_data |> 
  mutate(cbsa = if_else(!is.na(cbsacode), 1, 0)
         )

interact_model <- feols(data = reg_data, mortality ~ aqi * cbsa | county_name + date)
summary(interact_model)

# Beta 1 incicates there is slight positive association between today's air quality index and today's mortality.
# The interaction coefficient shows almost no impact of CBSA

#4
# The CBSA is colinear with the county fixed effects, since a county is only ever assigned one CBSA (if it is ever assigned one). 
# This means that the CBSA is a fixed characteristic of the county which is captured by the county fixed effects. 

#Section 4
#1
# It is not clear how many you would need to add. More information is needed. 
# Even if we know for certain that pollution is an AR(3), it only points to how the pollutions lags impact today's pollution level. 
# If we believe today's pollution is the true mechanism for mortality, then the lags only impact mortality through the AR process. 
# This would look like zero/near zero coeffcients on any lagged term we include or the AR coefficients scaled by the regression coefficient if we swap today's pollution with its AR process.
# It could also be the case that lags have even greater persistent effect on mortality as compared to is autoregressive persistence.
# This would mean that we might need more than three lags to capture long-run persistance of pollution.

#2
# We cannot make causual interpretations yet. 
# We have accounted for the time-invarient county fixed effects and the broader time fixed effects.
# Lags are not having any impact, but we are still accounting for county effects that change over time.
# For example, If we have some policy change at the county level during our obersvation.
# In this case the county fixed effect can't capture changes, while the time fixed effects can't capture localized changes. 
# The lags themselves cannot necessary capture these changes either as its possible that there is no correlation between the time-varient localized impacts and pollution changes. 
# For example, if LA county alone sees a suddent rise in homicide rates, then mortality would increase and is likely not associated with changes in pollution.

#3
# Assuming zero bias and a harvesting, the regression of today's pollution on tommorow's mortality would be negative, while today's pollution on today's mortality would be the same magnitude but positive. 
# This is because harvesting implies that increased pollution today would increase the number of people who would have otherwised died tommorow. This means mortality rises today but fall tommorow based on today's pollution increase. 
# This means that coefficient should be zero or near zero if we summed the mortality between today and tommorow. 
