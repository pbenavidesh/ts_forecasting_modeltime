# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: ADVANCED TIME SERIES FORECASTING FOR BUSINESS
# MODULE: VISUALIZATION TOOLS ----

# OBJECTIVES ----
# Introduction to:
# - Time Plot
# - Autocorrelation
# - Seasonality 
# - Anomalies
# - Seasonal Decomposition (STL)
# - Time Series Regression (TSLM)

# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)

# DATA ----

google_analytics_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_tbl 

mailchimp_users_tbl <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl

# DATA PREPARATION ---- 

google_analytics_long_hour_tbl <- google_analytics_tbl %>% 
  mutate(date = ymd_h(dateHour)) %>% 
  select(-dateHour) %>% 
  pivot_longer(cols = pageViews:sessions)

google_analytics_long_hour_tbl


subscribers_day_tbl <- mailchimp_users_tbl %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by       = "day", 
    optins    = n()) %>% 
  pad_by_time(.by = "day", .pad_value = 0)

subscribers_day_tbl

# 1.0 TIME SERIES PLOT ----
# - Primary Visualization Tool
# - Spot issues and understand one or more time series

?plot_time_series

# * Basics ----

subscribers_day_tbl %>% 
  plot_time_series(
    .date_var = optin_time,
    .value    = optins
  )

google_analytics_long_hour_tbl %>% 
  plot_time_series(
    .date_var = date,
    .value    = value
  )

# * Facets/Groups ----

google_analytics_long_hour_tbl %>% 
  plot_time_series(
    .date_var  = date,
    .value     = value,
    .color_var = name
  )

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_time_series(
    .date_var  = date,
    .value     = value,
  )

# same as adding the .facet_var argument

google_analytics_long_hour_tbl %>% 
  plot_time_series(
    .date_var  = date,
    .value     = value,
    .facet_var = name
  )

# * Mutations/Transformations ----

subscribers_day_tbl %>% 
  plot_time_series(optin_time, log(optins + 1))

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_time_series(
    .date_var = date,
    .value    = log(value + 1)
  )

# * Smoother Adjustment

subscribers_day_tbl %>% 
  plot_time_series(
    .date_var = optin_time,
    .value    = log(optins + 1),
    .smooth   = FALSE
  )

subscribers_day_tbl %>% 
  plot_time_series(
    .date_var      = optin_time,
    .value         = log(optins + 1),
    .smooth_period = "30 days"
  ) # is very variable

subscribers_day_tbl %>% 
  plot_time_series(
    .date_var       = optin_time,
    .value          = log(optins + 1),
    .smooth_period  = "90 days",
    .smooth_message = TRUE
  ) # looks more like a moving average

# Setting the degree of the polynomial to fit:

subscribers_day_tbl %>% 
  plot_time_series(
    .date_var       = optin_time,
    .value          = log(optins + 1),
    .smooth_period  = "90 days",
    .smooth_message = TRUE,
    .smooth_degree  = 1
  ) # looks more like a moving average

# .smooth_span takes a % of the data to smooth it
# (greater values yield smoother lines)

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_time_series(
    date, 
    log(value + 1),
    .smooth_period = "7 days"
  )


# * Static ggplot ----

subscribers_day_tbl %>% 
  plot_time_series(optin_time, optins, 
                   .plotly_slider = TRUE)

subscribers_day_tbl %>% 
  plot_time_series(optin_time, optins, 
                   .interactive = FALSE)

# 2.0 ACF Diagnostics ----
# - Detecting Lagged Features
?plot_acf_diagnostics

# * ACF / PACF -----
# - Date Features & Fourier Series 



# * CCF ----
# - Lagged External Regressors






# 3.0 SEASONALITY ----
# - Detecting Time-Based Features

?plot_seasonal_diagnostics





# 4.0 ANOMALIES ----
# - Detecting Events & Possible Data Issues

?plot_anomaly_diagnostics





# 5.0 SEASONAL DECOMPOSITION ----
# - Detecting Trend and Seasonal Cycles

?plot_stl_diagnostics





# 6.0 TIME SERIES REGRESSION PLOT ----
# - Finding features

?plot_time_series_regression



