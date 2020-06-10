# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# download from data_full.csv (see Notion project notes; i.e., Department of Employment)
# original called data.csv 

# package
library(tidyverse)

# load and save data
load(file = "saku.RData")

# read data_full.csv (originally called data.csv)
data_full <- read.csv("data_full.csv")

# quick exploratory
# data SHAPE: 5,595 rows, 190 columns

# check column names (compare to smart_job_data)
# very different from smart_job_data
# data_full has JobAnnounce_ and Employer_ (presumably from employer side)
# smart_job_data is from perspective of Job Seeker
names(data_full)  # compare names(smart_job_data)