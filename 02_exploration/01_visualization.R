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
    .date_var      = optin_time,
    .value         = optins,
    .plotly_slider = TRUE
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

subscribers_day_tbl %>% 
  plot_acf_diagnostics(optin_time, optins)

# Doesn't look too interesting, let's try with 
# a log transformation

subscribers_day_tbl %>% 
  plot_acf_diagnostics(optin_time, log(optins + 1))

# Modifying the lags to be used

subscribers_day_tbl %>% 
  plot_acf_diagnostics(optin_time, log(optins + 1),
                       # .lags = 100
                       # .lags = 25:100
                       .lags = "1 year")

# * CCF ----
# - Lagged External Regressors

google_analytics_day_tbl <- google_analytics_long_hour_tbl %>% 
  pivot_wider(
    names_from  = name,
    values_from = value 
  ) %>% 
  summarise_by_time(
    .date_var = date,
    .by       = "day",
    across(pageViews:sessions, .fns = sum)
  )

subscribers_ga_day_tbl <-  subscribers_day_tbl %>% 
  left_join(
    google_analytics_day_tbl, 
    by = c("optin_time" = "date")
  )

subscribers_ga_day_tbl %>% 
  drop_na() %>% 
  plot_acf_diagnostics(
    .date_var           = optin_time,
    .value              = optins,
    .ccf_vars           = pageViews:sessions,
    .show_ccf_vars_only = TRUE,
    .facet_ncol         = 2
  )


# 3.0 SEASONALITY ----
# - Detecting Time-Based Features

?plot_seasonal_diagnostics

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_seasonal_diagnostics(
    .date_var    = date,
    .value       = log(value + 1)
  )

# selecting a subset of time features

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_seasonal_diagnostics(
    .date_var    = date,
    .value       = log(value + 1),
    .feature_set = c("hour", "wday.lbl")
  )


# convert boxplots to violins

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_seasonal_diagnostics(
    .date_var    = date,
    .value       = log(value + 1),
    .feature_set = c("hour", "wday.lbl"),
    .geom        = "violin"
  )


# 4.0 ANOMALIES ----
# - Detecting Events & Possible Data Issues

?plot_anomaly_diagnostics

subscribers_day_tbl %>% 
  plot_anomaly_diagnostics(
    .date_var = optin_time,
    .value    = optins
  )

# adjusting the plot
# .alpha controls the bands defining the "normal"
# data. Lower values increase the width.
subscribers_day_tbl %>% 
  plot_anomaly_diagnostics(
    .date_var = optin_time,
    .value    = optins,
    .alpha    = 0.01
  )

# .max_anomalies controls the maximum percentage 
# of data allowed to be anomalous

subscribers_day_tbl %>% 
  plot_anomaly_diagnostics(
    .date_var      = optin_time,
    .value         = optins,
    .alpha         = 0.01,
    .max_anomalies = 0.01
  )

subscribers_day_tbl %>% 
  tk_anomaly_diagnostics(
    .date_var      = optin_time,
    .value         = optins,
    .alpha         = 0.01,
    .max_anomalies = 0.01
  )

# Grouped Anomalies

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_anomaly_diagnostics(date, value)

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  tk_anomaly_diagnostics(date, value)

# 5.0 SEASONAL DECOMPOSITION ----
# - Detecting Trend and Seasonal Cycles

?plot_stl_diagnostics

subscribers_day_tbl %>% 
  plot_stl_diagnostics(optin_time, optins)

# modifying the trend and seasonality
subscribers_day_tbl %>% 
  plot_stl_diagnostics(
    .date_var  = optin_time, 
    .value     = optins,
    .frequency = "1 month",
    .trend     = "1 year"
  )

# we can see more clearly using logs

subscribers_day_tbl %>% 
  plot_stl_diagnostics(
    .date_var  = optin_time, 
    .value     = log(optins + 1),
    .frequency = "1 month",
    .trend     = "1 year"
  )

# Grouped TS

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_stl_diagnostics(
    .date_var  = date, 
    .value     = log(value + 1),
    .frequency = "1 month",
    .trend     = "1 year"
  )

# retrieving the data
google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  tk_stl_diagnostics(
    .date_var  = date, 
    .value     = log(value + 1),
    .frequency = "1 month",
    .trend     = "1 year"
  )

# 6.0 TIME SERIES REGRESSION PLOT ----
# - Finding features

?plot_time_series_regression

# A basic linear regression that just captures the 
# trend
subscribers_day_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula  = optins ~ as.numeric(optin_time)
  )

# Adding weekday features
subscribers_day_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula  = optins ~ as.numeric(optin_time) + 
      wday(optin_time, label = TRUE)
  )

# Adding month features too

subscribers_day_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula  = optins ~ as.numeric(optin_time) + 
      wday(optin_time, label = TRUE) +
      month(optin_time, label = TRUE),
    .show_summary = TRUE
  )


# This last model's R2 is very low, mainly because of 
# the high outliers. We will try taking the log(+ 1)
# transformation

subscribers_day_tbl %>% 
  plot_time_series_regression(
    .date_var = optin_time,
    .formula  = log(optins + 1) ~ as.numeric(optin_time) + 
      wday(optin_time, label = TRUE) +
      month(optin_time, label = TRUE),
    .show_summary = TRUE
  )

# With grouped data

google_analytics_long_hour_tbl %>% 
  group_by(name) %>% 
  plot_time_series_regression(
    .date_var = date, 
    .formula  = log(value + 1) ~ as.numeric(date) + 
      as_factor(hour(date)) +
      wday(date, label = TRUE) +
      month(date, label = TRUE)
  )
