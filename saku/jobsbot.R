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


# access postgresql
library(DBI)
library(RPostgres)
library(RPostgreSQL)



dbListTables(con)

# write table to dataframe
employer <- dbReadTable(con, 'employer')
glimpse(employer)

jobpost <- dbReadTable(con, 'jobpost')
glimpse(jobpost)


