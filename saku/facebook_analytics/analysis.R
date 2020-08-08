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

names(saku_page)

# Saku Post
saku_post <- read_csv("./data/Facebook Insights Data Export (Post Level) - Saku - 2020-08-03.csv")
glimpse(saku_post)