# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# download from google sheet at CSV
# read into RStudio

library(tidyverse)

# load data
load(file = "saku.RData")

# Worker
jobbot <- read.csv("jobbot.csv")
jobseeker <- jobbot

# Job Categories
jobcat <- read.csv('jobcat.csv')

# Employer
employer <- read.csv('employer.csv')

####### Clean, Transform, EDA JOB SEEKER #######

# keep only distinct Full.name in jobbot (worker); 548 rows (from 1525)
View(distinct(jobseeker, Full.name, .keep_all = TRUE)) 

# keep only distinct facebook.id in jobbot (worker); 552 rows (from 1525)
View(distinct(jobseeker, facebook.id, .keep_all = TRUE))

# group by GENDER
# female: 334, male: 208
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>%
    group_by(sex) %>%
    tally(sort = TRUE)

# group by STUDY.LEVEL (Education)
# undergrad (360); master (96); grade-12 (46); grade-9 (16); อนุปริญญา (14); phd (10)
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(study.level) %>% 
    tally(sort = TRUE)

# group by AGE
# highest frequency 20's, then 30's, then 40's, then 50's & 60's (expected)
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(age) %>% 
    tally(sort = TRUE) %>% 
    view()

# group by JOB CATEGORY (job.cat)
# mostly missing data or '0' (no category)
# งานอื่นๆ (n = 47); การขาย การตลาด (n = 42); วิศวกร (n = 30)
# note: need to join jobseeker and jobcat dataframes
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(job.cat) %>% 
    tally(sort = TRUE) %>% 
    view()

# group by WORK EXPERIENCE (YEARS)
# this is a Factor; consider convert to numeric
# mostly of missing data
# work experience by frequency: 0 (118), 1 (45), 2 (33)
# many first jobber profiles
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(work.exp) %>% 
    tally(sort = TRUE) %>% 
    view()


# understand distribution of salary from job-seekers
# salary is a factor so must use group_by
# NOTE consider changing to integer/numeric
# if keep factor then do salary ranges
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(min.salary) %>% 
    tally(sort = TRUE)

# A tibble: 42 x 2
#   min.salary     n
#   <fct>      <int>
# 1 ""           431
# 2 "15000"       15
# 3 "20000"       13
# 4 "30000"        9
# 5 "25000"        7
# 6 "50000"        7
# 7 "35000"        6
# 8 "40000"        6

####### Clean, Transform,  EMPLOYER #######

# By Distinct Facebook.ID (n = 48)
distinct(employer, facebook.id, .keep_all = TRUE) %>% view()

# Description of "employer" by role
# note: reduce redundancy by getting rid of emoji or have data dictionary
# เจ้าของกิจการ (32); HR (9); ผู้จัดการแผนก (3); อื่นๆ (4)
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(role) %>% 
    tally(sort = TRUE) %>% 
    view()



