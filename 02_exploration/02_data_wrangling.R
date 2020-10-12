# BUSINESS SCIENCE UNIVERSITY
# DS4B 203-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: TIME SERIES DATA WRANGLING ----


# GOAL ----
# - Gain exposure to timetk data wrangling functionality

# OBJECTIVES ----
# - Summarize/Pad - Manipulate Data to different periodicities (scales, intervals)
# - Filter - Zoom in & Slice Time Series
# - Mutate - Apply mutations by time groups
# - Joining Time Series
# - Index Operations
# - Future Frame - Forecasting Exposure



# LIBRARIES ----

library(tidyverse)
library(timetk)
library(lubridate)
library(DataExplorer)

# DATA ----

google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")
google_analytics_summary_tbl 

mailchimp_users_tbl  <- read_rds("00_data/mailchimp_users.rds")
mailchimp_users_tbl 

transactions_tbl  <- read_rds("00_data/transactions_weekly.rds")
transactions_tbl 


# 1.0 SUMMARIZE BY TIME  ----
# - APPLY COMMON AGGREGATIONS
# - HIGH TO LOW FREQ

# * To Daily - Subscribers ----

# Ungrouped
subscribers_daily_tbl <- mailchimp_users_tbl %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by       = "day",
    optins    = n()
  )

# Grouped

mailchimp_users_tbl %>% 
  group_by(member_rating) %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by       = "day",
    optins    = n()
  )

mailchimp_users_tbl %>% 
  group_by(member_rating) %>% 
  summarise_by_time(
    .date_var = optin_time,
    .by       = "day",
    optins    = n()
  ) %>% 
  plot_time_series(optin_time, log1p(optins))

# * To Daily - GA Summary ----

google_analytics_summary_daily_tbl <-  
  google_analytics_summary_tbl %>% 
  mutate(dateHour = ymd_h(dateHour)) %>% 
  summarise_by_time(
    .date_var = dateHour,
    .by       = "day",
    across(.cols = pageViews:sessions, .fns = sum)
  )

google_analytics_summary_daily_tbl %>% 
  pivot_longer(pageViews:sessions) %>% 
  plot_time_series(dateHour, value, .facet_vars = name)

# * To Weekly - Subscribers ----

subscribers_weekly_tbl <- mailchimp_users_tbl %>% 
  summarise_by_time(
    optin_time, 
    .by = "1 week",
    optins = n()
  )


# * To Monthly - Transactions ----

transactions_monthly_tbl <- transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by       = "1 month", #try "2 quarters", "1 year 3 months" ...  
    revenue   = sum(revenue)
  )

# Floor, ceiling, round
# "floor" is done by default, which aggregates all time
# stamps to the first date on the curerent period.
# 
# "ceiling" aggregates all time stamps in a time period
# to the first date in the next period.
transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by       = "1 month",
    revenue   = sum(revenue),
    .type     = "ceiling"
  )

# "round" aggregates anything that happens within the
# halfway point between the period into that date.

transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by       = "1 month",
    revenue   = sum(revenue),
    .type     = "round"
  )

transactions_tbl %>% 
  summarise_by_time(
    .date_var = purchased_at,
    .by       = "1 month",
    revenue   = sum(revenue),
    .type     = "ceiling"
  ) %>% 
  mutate(purchased_at = purchased_at %-time% "1 day")
# %-time% is used to subtract a period (duration) from
# a time stamp

    
# 2.0 PAD BY TIME ----
# - Filling in Gaps
# - Going from Low to High Frequency (un-aggregating)

# * Fill Daily Gaps ----

subscribers_daily_tbl %>% 
  pad_by_time(
    .date_var   = optin_time,
    .by         = "day",
    .pad_value  = 0,
    .start_date = "2018-06-01"
  )

# You can filter data too by specifying a posterior start date

subscribers_daily_tbl %>% 
  pad_by_time(
    .date_var   = optin_time,
    .by         = "day",
    .pad_value  = 0,
    .start_date = "2018-07-01"
  )

# * Weekly to Daily ----

transactions_tbl %>% 
  pad_by_time(
    .date_var   = purchased_at,
    .by         = "day",
    .start_date = "2018-06"
  ) %>% 
  mutate_by_time(
    .by            = "week", 
    revenue_spread = sum(revenue, na.rm = TRUE) / 7)



# 3.0 FILTER BY TIME ----
# - Pare data down before modeling

# * Slicing - Everything after the BIG anomaly ----

subscribers_daily_tbl %>% 
  filter_by_time(
    .start_date = "2018-11-20"
  ) %>% 
  plot_time_series(optin_time, optins)


# * Zooming In - Just December 2018 ----

# There are many ways in which we can filter by time

# by month(s)
subscribers_daily_tbl %>% 
  filter_by_time(.start_date = "2019-12", .end_date = "2019-12")

# by year(s)

subscribers_daily_tbl %>% 
  filter_by_time(.start_date = "2019", .end_date = "2019")

# jan 2019

subscribers_daily_tbl %>% 
  filter_by_time(.start_date = "2019", .end_date = "2019-01")


# * Offsetting - Using plus-time and minus-time offsets to get things just right ----

subscribers_daily_tbl %>% 
  filter_by_time(
    .start_date = "2019-12", 
    .end_date   = "2019-12-01" %+time% "6 weeks"
  ) %>% 
  plot_time_series(optin_time, optins)


# 4.0 MUTATING BY TIME -----
# - Get change from beginning/end of period

# * First, Last, Mean, Median by Period ----

transactions_tbl %>% 
  plot_time_series(purchased_at, revenue)

transactions_tbl %>% 
  mutate_by_time(
    .by = "month",
    revenue_mean   = mean(revenue),
    revenue_median = median(revenue),
    revenue_max    = max(revenue),
    revenue_min    = min(revenue)
  ) %>% 
  pivot_longer(contains("revenue")) %>% 
  plot_time_series(purchased_at, value, name, .smooth = FALSE)


# 5.0 JOINING BY TIME ----
# - Investigating Relationships
# - Identify External Regressors

# * Subscribers + GA Summary Web Traffic ----

subscribers_daily_padded_tbl <- subscribers_daily_tbl %>% 
  pad_by_time(.pad_value = 0, .start_date = "2018-06")

subscribers_google_joined_tbl <- subscribers_daily_padded_tbl %>% 
  left_join(google_analytics_summary_daily_tbl, 
            by = c("optin_time" = "dateHour"))


# * Inspect Join -----

subscribers_google_joined_tbl %>% 
  plot_missing()

# 53% of the data from google is missing.

google_analytics_summary_daily_tbl %>%  tk_summary_diagnostics()
# google starts at 2019-05-08 -> 300 obs

subscribers_google_joined_tbl %>% tk_summary_diagnostics()
# starts in 2018-06 -> 641 obs

subscribers_google_joined_tbl %>% 
  pivot_longer(-optin_time) %>% 
  plot_time_series(optin_time, value, .color_var = name)


# * Visualization Techniques (Relationships) ----

log_standardized_subscribers_joined_tbl <- subscribers_google_joined_tbl %>% 
  drop_na() %>% 
  mutate(across(optins:sessions, .fns = log1p),
         across(optins:sessions, .fns = standardize_vec))

log_standardized_subscribers_joined_tbl %>% 
  pivot_longer(optins:sessions) %>% 
  plot_time_series(optin_time, value, name, .smooth = FALSE)

log_standardized_subscribers_joined_tbl %>% 
  plot_acf_diagnostics(
    .date_var           = optin_time, 
    .value              = optins, 
    .ccf_vars           = pageViews:sessions,
    .show_ccf_vars_only = TRUE
  )

# 6.0 WORKING WITH THE INDEX ----
# - Index Manipulations

# * Making an index ----


# * Holiday Sequence ----


# * Offsetting time ----


# * Extending an index ----




# 7.0 FUTURE FRAME ----
# - Forecasting helper



# * Future Frame ----



# * Modeling ----



# * Visualizing ----



