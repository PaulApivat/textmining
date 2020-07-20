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
        major_group_id = NA,
        broad_occupation = NA,
        onet_title = NA
        ) %>% 
    select(job_name, major_group_id, major_group, broad_occupation, onet_title, job_qualification, job_description, study_field, everything())

View(emp_jobpost)

# Onet Classification ----

# load onet_job_classification

onet_job_classification <- read_csv("SOC_Structure.csv")

View(onet_job_classification)

onet_job_classification %>%
    distinct(`Broad Occupation`, `SOC or O*NET-SOC 2019 Title`) %>% view()

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

# Impute Missing Values  ---- 
# onet_job_classification
# 23 Major Groups

onet_job_classification[1:94,1] <- '11-0000'
onet_job_classification[95:170,1] <- '13-0000'
onet_job_classification[171:223,1] <- '15-0000'
onet_job_classification[224:307,1] <- '17-0000'
onet_job_classification[308:404,1] <- '19-0000'
onet_job_classification[405:431,1] <- '21-0000'
onet_job_classification[432:446,1] <- '23-0000'
onet_job_classification[447:547,1] <- '25-0000'
onet_job_classification[548:613,1] <- '27-0000'
onet_job_classification[614:740,1] <- '29-0000'
onet_job_classification[741:770,1] <- '31-0000'
onet_job_classification[771:817,1] <- '33-0000'
onet_job_classification[818:851,1] <- '35-0000'
onet_job_classification[852:869,1] <- '37-0000'
onet_job_classification[870:931,1] <- '39-0000'
onet_job_classification[932:975,1] <- '41-0000'
onet_job_classification[976:1085,1] <- '43-0000'
onet_job_classification[1086:1112,1] <- '45-0000'
onet_job_classification[1113:1220,1] <- '47-0000'
onet_job_classification[1221:1296,1] <- '49-0000'
onet_job_classification[1297:1472,1] <- '51-0000'
onet_job_classification[1473:1570,1] <- '53-0000'
onet_job_classification[1571:1596,1] <- '55-0000'



# Manual Classification ----

emp_jobpost[1,2:3] <- major_group_classification[16, 1:2]
emp_jobpost[1,4] <- '41-3010'
emp_jobpost[1,5] <- 'Sales Representatives, Services'

emp_jobpost[2,2:3] <- major_group_classification[16, 1:2]
emp_jobpost[2,4] <- '41-2010'
emp_jobpost[2,5] <- 'Cashiers'

emp_jobpost[3,2:3] <- major_group_classification[9,1:2]
emp_jobpost[3,4] <- '27-1020'
emp_jobpost[3,5] <- 'Graphic Designers'

emp_jobpost[4,2:3] <- major_group_classification[12,1:2]
emp_jobpost[4,4] <- '33-9030'
emp_jobpost[4,5] <- 'Security Guards'

emp_jobpost[5,2:3] <- major_group_classification[17,1:2]
emp_jobpost[5,4] <- '43-4050'
emp_jobpost[5,5] <- 'Customer Service Representatives'

emp_jobpost[6,2:3] <- major_group_classification[1,1:2]
emp_jobpost[6,4] <- '11-3120'
emp_jobpost[6,5] <- 'Human Resources Managers'

emp_jobpost[7,2:3] <- major_group_classification[17,1:2]
emp_jobpost[7,4] <- '43-5070'
emp_jobpost[7,5] <- 'Shipping, Receiving, and Inventory Clerks'

emp_jobpost[8,2:3] <- major_group_classification[2,1:2]
emp_jobpost[8,4] <- '13-2010'
emp_jobpost[8,5] <- 'Accountants and Auditors'

emp_jobpost[9,2:3] <- major_group_classification[4,1:2]
emp_jobpost[9,4] <- '17-2050'
emp_jobpost[9,5] <- 'Civil Engineers'
    
emp_jobpost[10,2:5] <- emp_jobpost[2,2:5]

emp_jobpost[11,2:5] <- emp_jobpost[7,2:5]

emp_jobpost[12,2:3] <- major_group_classification[16,1:2]
emp_jobpost[12,4] <- '41-2030'
emp_jobpost[12,5] <- 'Retail Salespersons'

emp_jobpost[13,2:3] <- major_group_classification[17,1:2]
emp_jobpost[13,4] <- '43-6010'
emp_jobpost[13,5] <- 'Secretaries and Administrative Assistants'

emp_jobpost[14,2:3] <- major_group_classification[2,1:2]
emp_jobpost[14,4] <- '13-1160'
emp_jobpost[14,5] <- 'Search Marketing Strategists'

## after re-organize onet_job_classification

emp_jobpost[15,2:3] <- major_group_classification[13,1:2]
emp_jobpost[15,4] <- '35-3020'
emp_jobpost[15,5] <- 'Fast Food and Counter Workers'

emp_jobpost[16,2:3] <- major_group_classification[3,1:2]
emp_jobpost[16,4] <- '15-1250'
emp_jobpost[16,5] <- 'Software and Web Developers, Programmers, and Tester'

emp_jobpost[17,2:5] <- emp_jobpost[2,2:5]

emp_jobpost[18,2:3] <- major_group_classification[1,1:2]
emp_jobpost[18,4] <- '11-2020'
emp_jobpost[18,5] <- 'Marketing and Sales Managers'

emp_jobpost[19,2:3] <- major_group_classification[2,1:2]
emp_jobpost[19,4] <- '13-1080'
emp_jobpost[19,5] <- 'Project Management Specialists'

# skip row 20 emp_jobpost

emp_jobpost[21,2:3] <- major_group_classification[20,1:2]
emp_jobpost[21,4] <- '49-2090'
emp_jobpost[21,5] <- 'Miscellaneous Electrical and Electronic Equipment Mechnics, Installers and Repairers'

emp_jobpost[22,2:3] <- major_group_classification[18,1:2]
emp_jobpost[22,4] <- '45-2020'
emp_jobpost[22,5] <- 'Animal Breeders'



emp_jobpost[23,2:3] <- major_group_classification[2,1:2]
emp_jobpost[23,4] <- '13-1160'
emp_jobpost[23,5] <- 'Search Marketing Strategists'


emp_jobpost[25,2:3] <- major_group_classification[17,1:2]
emp_jobpost[25,4] <- '43-5070'
emp_jobpost[25,5] <- 'Shipping, Receiving, and Inventory Clerks'


emp_jobpost[26,2:3] <- major_group_classification[16,1:2] 
emp_jobpost[26,4] <- '41-3090'
emp_jobpost[26,5] <- 'Miscellaneous Sales Representatives, Services'

emp_jobpost[27,2:3] <- major_group_classification[22,1:2]
emp_jobpost[27,4] <- '53-3030'
emp_jobpost[27,5] <- 'Driver/Sales Workers and Truck Drivers'

emp_jobpost[28,2:5] <- emp_jobpost[16,2:5]


emp_jobpost[29,2:3] <- major_group_classification[3,1:2]
emp_jobpost[29,4] <- '15-2030'
emp_jobpost[29,5] <- 'Business Intelligence Analysts'

emp_jobpost[30,2:3] <- major_group_classification[13,1:2]
emp_jobpost[30,4] <- '35-3020'
emp_jobpost[30,5] <- 'Baristas'

emp_jobpost[31,2:3] <- major_group_classification[3,1:2] 
emp_jobpost[31,4] <- '15-1240'
emp_jobpost[31,5] <- 'Telecommunications Engineering Specialists'

emp_jobpost[32,2:3] <- major_group_classification[1,1:2] 
emp_jobpost[32,4] <- '11-1020'
emp_jobpost[32,5] <- 'General and Operations Managers'

# skip 33-34

emp_jobpost[35,2:3] <- major_group_classification[19,1:2] 
emp_jobpost[35,4] <- '47-4090'
emp_jobpost[35,5] <- 'Miscellaneous Construction and Related Workers'

emp_jobpost[36,2:3] <- major_group_classification[16,1:2]
emp_jobpost[36,4] <- '41-2030'
emp_jobpost[36,5] <- 'Retail Salespersons'

# skip 37-38

emp_jobpost[39,2:3] <- major_group_classification[17,1:2] 
emp_jobpost[39,4] <- '43-6010'
emp_jobpost[39,5] <- 'Secretaries and Administrative Assistants'

emp_jobpost[41,2:5] <- emp_jobpost[39,2:5]


emp_jobpost[44,2:5] <- emp_jobpost[23,2:5]



emp_jobpost[46,2:3] <- major_group_classification[2,1:2]
emp_jobpost[46,4] <- '13-2010'
emp_jobpost[46,5] <- 'Accountants and Auditors'


emp_jobpost[47,2:5] <- emp_jobpost[46,2:5]


emp_jobpost[50,2:5] <- major_group_classification[1,1:2]
emp_jobpost[50,4] <- '11-2020'
emp_jobpost[50,5] <- 'Sales Managers'

emp_jobpost[51,2:5] <- major_group_classification[4,1:2]
emp_jobpost[51,4] <- '17-2070'
emp_jobpost[51,5] <- 'Electrical and Electronics Engineers'

emp_jobpost[52,2:5] <- emp_jobpost[28,2:5]


emp_jobpost[53,2:3] <- major_group_classification[17,1:2]
emp_jobpost[53,4] <- '43-3030'
emp_jobpost[53,5] <- 'Bookkeeping, Accounting, and Auditing Clerks'

emp_jobpost[54,2:3] <- major_group_classification[2,1:2]
emp_jobpost[54,4] <- '13-2010'
emp_jobpost[54,5] <- 'Accountants and Auditors'

emp_jobpost[55,2:5] <- emp_jobpost[30,2:5]


emp_jobpost[56,2:5] <- emp_jobpost[25,2:5]


emp_jobpost[57,2:5] <- emp_jobpost[15,2:5]


emp_jobpost[58,2:3] <- major_group_classification[2,1:2]
emp_jobpost[58,4] <- '13-1070'
emp_jobpost[58,5] <- 'Human Resources Workers'

emp_jobpost[59,2:3] <- major_group_classification[17,1:2]
emp_jobpost[59,4] <- '43-9190'
emp_jobpost[59,5] <- 'Miscellaneous Office and Administrative Support Workers'

emp_jobpost[60,2:3] <- major_group_classification[16,1:2]
emp_jobpost[60,4] <- '41-2030'
emp_jobpost[60,5] <- 'Retail Salespersons'

emp_jobpost[61,2:3] <- major_group_classification[1,1:2]
emp_jobpost[61,4] <- '11-3030'
emp_jobpost[61,5] <- 'Financial Managers'

# Exploring major_group and onet_title of each jobpost_name ----
# Content / Theme analysis of initial Saku 1.0 jostpost / employers

# Group_by major_group in descending order
emp_jobpost %>%
    mutate(major_group = as.factor(major_group)) %>%
    filter(!is.na(major_group)) %>%
    group_by(major_group) %>%
    tally(sort = TRUE) %>% 
    ggplot(aes(x = major_group, y = n)) +
    geom_col(aes(fill = major_group)) +
    scale_fill_discrete()



emp_jobpost2 <- emp_jobpost %>% 
    mutate(major_group = as.factor(major_group))

str(emp_jobpost2)

# Visualization ----

# Bar chart is major_group is a factor

emp_jobpost2 %>%
    filter(!is.na(major_group)) %>%
    group_by(major_group) %>%
    tally(sort = TRUE) %>%
    ggplot(aes(x = reorder(major_group,n), y = n, fill = major_group)) +
    geom_col() +
    geom_text(aes(label = n), color = 'black', hjust = 2) +
    scale_fill_brewer(palette = 'Set3') +
    theme_dark() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1,)) +
    coord_flip()
    

    

# Group by onet_title in descending order
emp_jobpost %>%
    filter(!is.na(onet_title)) %>%
    group_by(onet_title) %>%
    tally(sort = TRUE) %>%
    ggplot(aes(x = reorder(onet_title, n), y = n)) +
    geom_segment(aes(x = reorder(onet_title, n), xend = onet_title, y = 0, yend = n), size = 1, color = 'blue', linetype = 'dotdash') +
    geom_point(aes(size = n)) +
    theme(axis.text.x = element_text(hjust = 1)) +
    coord_flip()






