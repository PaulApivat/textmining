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
saku_page_select <- saku_page %>%
    select(Date, 
           `Lifetime Total Likes`, 
           `Daily Page Engaged Users`, 
           `Weekly Page Engaged Users`, 
           `28 Days Total Reach`, 
           `28 Days Organic Reach`, 
           `28 Days Total Impressions`,
           `28 Days Organic impressions`,
           `28 Days Paid Impressions`,
           `28 Days Viral impressions`)



# visualize saku_page data ----
library(lubridate)
library(glue)

# Lifetime Total Likes
saku_page %>%
    select(Date, `Lifetime Total Likes`) %>% 
    mutate(lifetime_total_likes = as.numeric(`Lifetime Total Likes`)) %>%
    mutate(lifetime_total_likes = if_else(is.na(lifetime_total_likes), 0, lifetime_total_likes)) %>%
    slice(-1) %>%
    mutate(Date = as.Date(Date) %>% ymd()) %>%
    
    ggplot(aes(x = Date, y = lifetime_total_likes)) +
    geom_vline(xintercept = as.Date("2020-06-01"), na.rm = FALSE, linetype = 'dashed') +
    geom_vline(xintercept = as.Date("2020-07-01"), na.rm = FALSE, linetype = 'dashed') +
    geom_vline(xintercept = as.Date("2020-08-01"), na.rm = FALSE, linetype = 'dashed') +
    scale_x_date(breaks = '5 day') +
    geom_line(color = 'red') +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
        title = paste0("`Lifetime Total Likes`")
    )

# Daily Page Enagement
saku_page_select %>%
    select(Date, `Daily Page Engaged Users`) %>% 
    mutate(`Daily Page Engaged Users` = as.numeric(`Daily Page Engaged Users`)) %>%
    mutate(`Daily Page Engaged Users` = if_else(is.na(`Daily Page Engaged Users`), 0, `Daily Page Engaged Users`)) %>%
    slice(-1) %>%
    mutate(Date = as.Date(Date) %>% ymd()) %>%
    
    ggplot(aes(x = Date, y = `Daily Page Engaged Users`)) +
    geom_vline(xintercept = as.Date("2020-06-01"), na.rm = FALSE, linetype = 'dashed') +
    geom_vline(xintercept = as.Date("2020-07-01"), na.rm = FALSE, linetype = 'dashed') +
    geom_vline(xintercept = as.Date("2020-08-01"), na.rm = FALSE, linetype = 'dashed') +
    scale_x_date(breaks = '5 day') +
    geom_line(color = 'red') +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
        title = paste0("`Daily Page Engaged Users`")
    )

# Weekly Page Engaged Users
saku_page_select %>%
    select(Date, `Weekly Page Engaged Users`) %>% 
    mutate(`Weekly Page Engaged Users` = as.numeric(`Weekly Page Engaged Users`)) %>%
    mutate(`Weekly Page Engaged Users` = if_else(is.na(`Weekly Page Engaged Users`), 0, `Weekly Page Engaged Users`)) %>%
    slice(-1) %>%
    mutate(Date = as.Date(Date) %>% ymd()) %>%
    
    ggplot(aes(x = Date, y = `Weekly Page Engaged Users`)) +
    geom_vline(xintercept = as.Date("2020-06-01"), na.rm = FALSE, linetype = 'dashed') +
    geom_vline(xintercept = as.Date("2020-07-01"), na.rm = FALSE, linetype = 'dashed') +
    geom_vline(xintercept = as.Date("2020-08-01"), na.rm = FALSE, linetype = 'dashed') +
    scale_x_date(breaks = '5 day') +
    geom_line(color = 'red') +
    theme_classic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
        title = paste0("`Weekly Page Engaged Users`")
    )




# create a general function ----




# General Function for line chart
create_line_chart_fn_2 <- function(dataset, col_name){
    col_name <- enquo(col_name)
    # key for grabbing the column name of the parameter
    column_name <- paste0("Metric: ", as_label(col_name))
    
    dataset %>%
        select(Date, !!(col_name)) %>% 
        mutate(local_col_name = as.numeric(!!(col_name))) %>%
        mutate(local_col_name = if_else(is.na(local_col_name), 0, local_col_name)) %>%
        slice(-1) %>%
        mutate(Date = as.Date(Date) %>% ymd()) %>%
        
        ggplot(aes(x = Date, y = local_col_name)) +
        geom_vline(xintercept = as.Date("2020-06-01"), na.rm = FALSE, linetype = 'dashed') +
        geom_vline(xintercept = as.Date("2020-07-01"), na.rm = FALSE, linetype = 'dashed') +
        geom_vline(xintercept = as.Date("2020-08-01"), na.rm = FALSE, linetype = 'dashed') +
        scale_x_date(breaks = '5 day') +
        geom_line(color = 'green') +
        theme_classic() +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1)
        ) +
        labs(
            title = glue('{column_name}'),
            y = 'Number of People'
        )
}

create_line_chart_fn_2(saku_page_select, `28 Days Total Reach`)
create_line_chart_fn_2(saku_page_select, `28 Days Organic Reach`)
create_line_chart_fn_2(saku_page_select, `28 Days Total Impressions`)
create_line_chart_fn_2(saku_page_select, `28 Days Organic impressions`)
create_line_chart_fn_2(saku_page_select, `28 Days Paid Impressions`)
create_line_chart_fn_2(saku_page_select, `28 Days Viral impressions`)
create_line_chart_fn_2(saku_page_select, `Weekly Page Engaged Users`)
create_line_chart_fn_2(saku_page, `Daily Page Consumptions`)
create_line_chart_fn_2(saku_page, `Weekly Viral Reach`)
create_line_chart_fn_2(saku_page, `Weekly Logged-in Page Views`)
create_line_chart_fn_2(saku_page, `Weekly Total Impressions of your posts`)

# Explore Saku Post separately
library(lubridate)


names(saku_post)

# Explore Lifetime Post Total Reach
saku_post %>%
    select(`Type`, `Posted`, `Lifetime Post Total Reach`) %>%
    slice(-1) %>%
    mutate(
        Date = as.Date(`Posted`, "%m/%d/%Y %H:%M:%S"),
        Date2 = as.Date(Date) %>% ymd(),
        lifetime_post_total_reach = as.numeric(`Lifetime Post Total Reach`)
    ) %>%
    ggplot(aes(x = Date2, y = lifetime_post_total_reach, fill = Type)) +
    geom_col() 
    

# Boxplot
saku_post %>%
    select(`Type`, `Posted`, `Lifetime Post Total Reach`) %>%
    slice(-1) %>%
    mutate(
        Date = as.Date(`Posted`, "%m/%d/%Y %H:%M:%S"),
        Date2 = as.Date(Date) %>% ymd(),
        lifetime_post_total_reach = as.numeric(`Lifetime Post Total Reach`)
    ) %>%
    ggplot(aes(x = Type, y = lifetime_post_total_reach, fill = Type)) +
    geom_boxplot()


# Filter out outliers
# Boxplot
saku_post %>%
    select(`Type`, `Posted`, `Lifetime Post Total Reach`) %>%
    slice(-1) %>%
    mutate(
        Date = as.Date(`Posted`, "%m/%d/%Y %H:%M:%S"),
        Date2 = as.Date(Date) %>% ymd(),
        lifetime_post_total_reach = as.numeric(`Lifetime Post Total Reach`)
    ) %>%
    filter(lifetime_post_total_reach < 8000) %>%
    ggplot(aes(x = Type, y = lifetime_post_total_reach, fill = Type)) +
    geom_boxplot()







