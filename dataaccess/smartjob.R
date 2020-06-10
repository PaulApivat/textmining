# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# download from smart_job_data.csv (see Notion project notes; i.e., Department of Employment)

# package
library(tidyverse)

# load and save data
load(file = "saku.RData")

# read smart_job_data.csv
smart_job_data <- read.csv("smart_job_data.csv")

# quick exploratory
# data SHAPE: 5,944 rows, 175 columns
summary(smart_job_data)

# check column names (compare to data_full)
names(smart_job_data)  # compare names(data_full)

## NOTES
# - jobseeker can create multiple entries in smart_job_data (see UserID)
