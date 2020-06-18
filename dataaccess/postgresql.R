# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

### PREREQUISITE: Need PostgreSQL database up and running

# load data
load(file = "saku.RData")

### RPostgres
install.packages('RPostgres')

### Connecting to specific Postgres Instance
library(DBI)



con <- dbConnect(RPostgres::Postgres(), 
    dbname = 'jobsbot', 
    host = '35.234.17.105',     # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
    port= 5432,                 # or any other port specified by your DBA
    user = 'USER', 
    password = 'PASSWORD')

# check if connection is established
# return 'character(0)' means 'no tables' are stored in the database, so we must create one
dbListTables(con)

# practice inserting data / running queries

# save mtcars into my_data, change first column to character
my_data <- data.frame(carname = rownames(mtcars), mtcars, row.names = NULL)
my_data$carname <- as.character(my_data$carname)

# write to 'con' database (or allows overwriting of table)
dbWriteTable(con, name = 'cars', value = my_data)
dbWriteTable(con, name = 'cars', value = my_data, overwrite = TRUE)

# list tables written into the db, re-run and should see "cars"
dbListTables(con)

# for reading a data table in the console
dbReadTable(con, 'cars')
# alternative way to do the same thing
dbGetQuery(con, 'SELECT * FROM cars;')


### ---------- ###
# note: signing off, terminates connection to postgresql database. 
# re-signing on, I can still access the 'cars' dataframe we wrote in last time
# the dataframe persists
### ---------- ###


# Querying Data
# fetch(carQuery) did not work
# cannot save to a variable 
RPostgres::dbFetch(carQuery)

# Clear results before next query
dbClearResult(carQuery)

####### Querying Data that works #######
#### KEY: use dbGetQuery() INSTEAD of dbSendQuery()
query_res <- dbGetQuery(con, "select * from cars where mpg > 20")

# save as data.frame
res_data_frame <- as.data.frame(query_res)

# dbGetQuery instead of dbSendQuery
carQuery <- dbGetQuery(con, 'select carname, cyl, gear from cars where cyl >= 6 and gear >= 5;')
result <- as.data.frame(carQuery)

# store BACK into Compose Database 
# when run dbListTables(con), should see two tables 'cars' and 'my_new_table'
dbWriteTable(con, 'my_new_table', result)

###### TRY AGAIN (Ben Login) Specific Postgres Instance #######

library(DBI)

# changed to B's credentials
con <- dbConnect(RPostgres::Postgres(),dbname = 'jobsbot', 
                host = '35.234.17.105', 
                port = 5432, 
                user = 'USER', 
                password = 'PASSWORD')

## success, saving all 7 tables into their own objects in R
dbListTables(con)

# primarily Data Dictionary
[1] "definition_study_level" "jobpost"                "employer"              
[4] "definition_work_exp"    "definition_job_type"    "definition_job_sex"    
[7] "smartjob"               "test_user" 

# separate from previous smart_job_data frame
smart_job_data2 <- dbReadTable(con, "smartjob")
definition_study_level <- dbReadTable(con, "definition_study_level")
jobpost <- dbReadTable(con, "jobpost")
employer <- dbReadTable(con, "employer")
definition_work_exp <- dbReadTable(con, "definition_work_exp")
definition_job_type <- dbReadTable(con, "definition_job_type")
definition_job_sex <- dbReadTable(con, "definition_job_sex")
test_user <- dbReadTable(con, "test_user")




# areas for table joins
definition_study_level & jobpost
definition_work_exp & jobpost
definition_job_type & jobpost
definition_job_sex & jobpost
test_user & employer

# definition_study_level & jobpost
jobpost_join <- jobpost %>%
    inner_join(definition_study_level, by = c('study_level' = 'id'))

# definition_work_exp & jobpost
jobpost_join <- jobpost_join %>%
    inner_join(definition_work_exp, by = c('work_exp' = 'n_year')) 

# definition_job_type & jobpost
jobpost_join <- jobpost_join %>%
    inner_join(definition_job_type, by = c('job_type' = 'id')) 

# definition_job_sex & jobpost
jobpost_join <- jobpost_join %>%
    inner_join(definition_job_sex, by = c('job_sex' = 'id'))

# test_user & employer
# note: left_join instead of inner_join
employer_test <- employer %>% 
    left_join(test_user, by = 'manychat_id')

# change column name
names(jobpost_join)[25:28] <- c("studylevel", "workexp", "jobtype", "jobsex")

### Visualizations ###

# Job Posting by Gender
jobpost_join %>% 
    ggplot(aes(x= jobsex, fill=jobsex)) 
    + geom_bar(stat = 'count', width = 0.7) 
    + theme(axis.text.x = element_text(family = 'Krub'), 
            legend.text = element_text(family = 'Krub')) 
    + labs(title = 'Job Posting by Gender', x = 'Gender', fill = 'Gender')

# Job Posting by Education Level requirement
jobpost_join %>% 
    group_by(studylevel) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x=reorder(studylevel, n), y=n, fill=studylevel)) 
    + geom_bar(stat = 'identity', width = 0.7) 
    + theme(axis.text.x = element_text(family = 'Krub'), 
            legend.text = element_text(family = 'Krub')) 
    + labs(title = 'Job Posting by Education Requirement (n=52)', 
            x = 'Education Level', 
            y = 'Number of Posts', 
            fill = 'Education Level')

# Job Posting by Work Experience requirement
jobpost_join %>% 
    group_by(workexp) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x=reorder(workexp, n), y=n, fill=workexp)) 
    + geom_bar(stat = 'identity', width = 0.7) 
    + theme(axis.text.x = element_text(family = 'Krub'), 
            legend.text = element_text(family = 'Krub')) 
    + labs(title = 'Job Posting by Work Experience (n=52)', 
            x = 'Work Experience', 
            y = 'Number of Posts', 
            fill = 'Work Experience')

# Job Posting by Job Type
jobpost_join %>% 
    ggplot() 
    + aes(x=jobtype) 
    + geom_bar(stat = 'count') 
    + theme(axis.text.x = element_text(family = 'Krub'))


#### Other Visualizations ####

# filter by unique jobpost_id
# distribution of minimum age required
# substitute AGE_MAX
# need to .keep_all to have age_min to analyze
jobpost_join %>% 
    distinct(jobpost_id, .keep_all = TRUE) %>% 
    ggplot(aes(x=age_min)) 
    + geom_density()

## job qualification group_by and tally
jobpost_join %>% 
    group_by(job_qualification) %>% 
    tally(sort = TRUE)

####### ADDITIONAL JOIN: jobpost_join with employer_test by manychat_id
### select only variable of interest in employer_test
# manychat_id, company_name (that made a post), role and name of person who made the post?



# separate thing
smart_job_data2 

