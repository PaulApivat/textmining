# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# download from google sheet at CSV
# read into RStudio

library(tidyverse)

# Worker
jobbot <- read.csv("jobbot.csv")

# Job Categories
jobcat <- read.csv('jobcat.csv')

# Employer
employer <- read.csv('employer.csv')

####### Clean, Transform, EDA #######