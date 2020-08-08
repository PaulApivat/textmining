# Sessioninfo
R version 3.6.3 (2020-02-29)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.5

# load libraries ----
library(tidyverse)

# read data ----

# Saku Page
saku_page <- read_csv("./data/Facebook Insights Data Export - Saku - 2020-08-03.csv")
glimpse(saku_page)



# Saku Post
saku_post <- read_csv("./data/Facebook Insights Data Export (Post Level) - Saku - 2020-08-03.csv")
glimpse(saku_post)

# exploring saku_page data ----

# NOTE: 

# saku_page: 90 obs, 1995 variables
# saku_post: 16 obs, 52 variables

# grabbing first 100 columns in saku_page
saku_page %>% 
    names() %>%
    head(n = 50)

# filter by column names
# list as vector

# Columns that contain 'Daily' n = 533
saku_page %>%
    select(contains("Daily")) %>% 
    colnames() %>% view()

# Columns that contain 'Weekly' n = 906
saku_page %>%
    select(contains("Weekly")) %>%
    colnames() %>% view()
    

## Lifetime, Daily, Weekly, "28 Days", 
## Total, Viral, Logged-in, Organic, Check-Ins
## impressions, Feedback, Video, Paid, Clicked, Repeats
## Daily/Weekly/28 Days...Total Frequency Distribution
## Daily/Weekly/28 Days...Page Posts Frequency Distribution
## Daily/Weekly/28 Days...Talking About This By Story Type
## Daily/Weekly/28 Days...Page Stories By Story Type
## Daily/Weekly/28 Days...Page Consumptions By Type
## Lifetime Likes by Gender and Age
## Lifetime Likes by Country
## Lifetime Likes by City
## Lifetime Likes by Language
## Weekly Reach Demographics
## Weekly Reach By Country
## Weekly Reach By City
## Weekly Reach By Language
## Daily Demographics: People Talking About This

# Grab specific column names of interest 

# Identify first 100 rows
saku_page %>% 
    names() %>%
    head(n = 50)

# Select 10 Columns of Interest
saku_page %>%
    select(Date, 
           `Lifetime Total Likes`, 
           `Daily Page Engaged Users`, 
           `Weekly Page Engaged Users`, 
           `28 Days Total Reach`, 
           `28 Days Organic Reach`, 
           `28 Days Total Impressions`,
           `28 Days Organic impressions`,
           `28 Days Paid Impressions`,
           `28 Days Viral impressions`) %>%
    view()


# exploring saku_post data ----
