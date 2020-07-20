library(tidyverse)

events <- read_csv("events.csv")
glimpse(events)

events %>% view()

# change column name
colnames(events)[3] <- 'count'

# data manipulation
event_count <- events %>%
    group_by(Event) %>%
    summarize(total_count = sum(count)) %>%
    ungroup()

# visualization
event_count %>%
    ggplot(aes(Event, total_count)) +
    geom_col(aes(fill=Event)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


# access postgresql ----
library(DBI)
library(RPostgres)
library(RPostgreSQL)
library(tidyverse)

con <- dbConnect(RPostgres::Postgres(),dbname = 'dbname', 
                 host = 'localhost', 
                 port = 5432, 
                 user = 'user', 
                 password = 'password')


dbListTables(con)

# write table to dataframe
employer <- dbReadTable(con, 'employer')
glimpse(employer)

jobpost <- dbReadTable(con, 'jobpost')
glimpse(jobpost)

# explore dataframes ----

View(jobpost)
View(employer)

# prep for text analytics
# jobpost - character data types: job_name, study_field, job_qualification, job_description, location
# employer - character data types: company_name, role, name
str(jobpost)
str(employer)


# distinct manychat id (employer: 62, jobpost: 52)
employer %>% 
    distinct(manychat_id)

jobpost %>%
    distinct(manychat_id)

jobpost %>%
    distinct(jobpost_id)

# emp_jobpost: join employer + jobpost by manychat_id
emp_jobpost <- employer %>%
    left_join(jobpost, by = 'manychat_id') %>% 
    mutate(
        major_group = NA,
        major_group_id = NA
        ) %>% 
    select(job_name, major_group, major_group_id, job_qualification, job_description, study_field, everything())

View(emp_jobpost)

# Onet Classification ----

# load onet_job_classification

onet_job_classification <- read_csv("SOC_Structure.csv")

View(onet_job_classification)

# Subset Major Group classification by renaming variables
major_group_classification <- onet_job_classification %>%
    rename(
        major_group_id = `Major Group`,
        major_group = `SOC or O*NET-SOC 2019 Title`
        ) %>%
    group_by(major_group_id, major_group) %>%
    count() %>% 
    filter(!is.na(major_group_id))

View(major_group_classification)    





