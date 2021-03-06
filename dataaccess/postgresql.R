# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

### PREREQUISITE: Need PostgreSQL database up and running

# load data
load(file = "saku.RData")

### RPostgres
install.packages('RPostgres')
install.packages('IMmailgun')

### Connecting to specific Postgres Instance
library(DBI)
library(tidyverse)
library(IMmailgun)


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

# look at job_posts by employer_Role

employer_test_1 <- employer_test

employer_test_1$role <- ifelse((employer_test_1$role=='🙋‍♀️เจ้าของกิจการ'), "เจ้าของกิจการ", employer_test_1$role)


# treemap, employer_Role superseding job_qualification

# bar chart of job_qualifications, grouped by role
ggplot(post_join_employer, aes(fill=role, y=age_min, x=job_qualification)) + geom_bar(position = 'dodge', stat = 'identity') + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust = 1), legend.text = element_text(family = 'Krub'))

# basic treemap (no thai font!)
treemap(tree_post, index = c("role", "job_qualification"), vSize = 'n', type = "index") 

#### NOTE: there is no good way to visualize role and job_qualification
#### NOTE: treemap package does not have Thai alphabet


#### DATA CLEANING #####

# REMOVE EMOJI Business Owner
remove_emoticon <- post_join_employer$role[2]
rmSpec <- "🙋|♀️" 
s.rem <- gsub(rmSpec, "", remove_emoticon)
post_join_employer$role <- gsub(rmSpec, "", post_join_employer$role)

# REMOVE EMOJI อื่นๆ
# put | between every emoji, 'or' logical
rmSpec2 <- "💁|‍|♂️"
post_join_employer$role <- gsub(rmSpec2, "", post_join_employer$role)

### BEFORE EMOJI

employer_test %>% group_by(role) %>% tally(sort = TRUE)
# A tibble: 8 x 2
  role              n
  <chr>         <int>
1 🙋‍♀️เจ้าของกิจการ    28
2 เจ้าของกิจการ       9
3 🙋‍♂️HR              6
4 อื่นๆ               6
5 💁‍♂️อื่นๆ             4
6 HR                4
7 💁‍♀️ผู้จัดการแผนก      2
8 ผู้จัดการแผนก        2

### AFTER EMOJI

post_join_employer %>% group_by(role) %>% tally(sort = TRUE)
# A tibble: 4 x 2
  role            n
  <chr>       <int>
1 ‍เจ้าของกิจการ    36
2 ‍HR             10
3 ‍อื่นๆ             4
4 ‍ผู้จัดการแผนก      2


##### ALTERNATIVE TO TREEMAP - Facet_Grid Bar Chart

# See preferences of "role" re "workexp" (can also switch)
ggplot(tree_post, aes(x=role, y=n, fill=role)) 
    + geom_bar(stat = 'identity') 
    + facet_grid(~workexp) 
    + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust=1), 
            strip.text.x = element_text(family = 'Krub'), 
            legend.text = element_text(family = 'Krub'))
    + labs(title = "Work Experience Requirement by Hiring Manager", 
            x = "Type of Hiring Manager", 
            y = "Number of Job Posts", 
            fill = "Hiring Roles")


#### Salary Ranges
# Table of Avg Salary, Min, Max by Role
post_join_employer %>% 
    group_by(role) %>% 
    summarize(avg_salary = mean(min_salary), 
                    min = min(min_salary), 
                    max = max(min_salary))

# Facet Bar Chart: Salary Types by Role
# tidy data
post_join_employer %>% 
    group_by(role) %>% 
    summarize(avg_salary = mean(min_salary), min = min(min_salary), max = max(min_salary)) %>% 
    gather("avg_salary", "min", "max", key = "salary", value = "amount") %>% 
    ggplot(aes(x=role, y=amount, fill=role)) 
        + geom_bar(stat = 'identity') 
        + facet_grid(~salary) 
        + theme(axis.text.x = element_text(family = 'Krub', angle=45, hjust=1), legend.text = element_text(family = 'Krub')) 
        + labs(title = 'Salary Requirement by Role', y = 'Monthly Salary (Baht)', x = 'Hiring Person Role')

# Facet Bar Chart: Gender Requirement by Role
post_join_employer %>% 
    group_by(role, jobsex) %>% 
    tally(sort = T) %>% 
    ggplot(aes(x=role, y=n, fill=role)) 
        + geom_bar(stat = 'identity') 
        + facet_grid(~jobsex) 
        + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust=1), legend.text = element_text(family = 'Krub'), strip.text.x = element_text(family = 'Krub')) 
        + labs(title = 'Gender Requirement by Hiring Role', y = 'Number of Posts', x = 'Hiring Roles', fill = 'Hiring Roles')

# Facet Bar Chart: Education Requirements by Hiring Role
post_join_employer %>% 
    group_by(role, studylevel) %>% 
    tally(sort = T) %>% 
    ggplot(aes(x=role, y=n, fill=role)) 
        + geom_bar(stat = 'identity') 
        + facet_grid(~studylevel) 
        + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust=1), 
                legend.text = element_text(family = 'Krub'), 
                strip.text.x = element_text(family = 'Krub')) 
        + labs(title = 'Education Requirement by Study Level', 
                y = 'Number of Posts', 
                x = 'Hiring Roles', 
                fill = 'Hiring Roles')


# different salary levels
post_join_employer %>% 
    summarize(mean = mean(min_salary), min = min(min_salary), max = max(min_salary)) %>% 
    gather("mean", "min", "max", key = "stat", value = "amount") %>% 
    ggplot(aes(x=stat, y=amount, fill=stat)) + geom_bar(stat = 'identity', position = 'dodge') 
        + geom_text(aes(label=format(round(amount, 2), nsmall = 2)), vjust=-0.5) 
        + labs(x="Minimum Salary Spread", y="Monthly Salary (Baht)") 
        + labs(title = "Job Postings by Minimum Salary Statistics")


# different Age Min and Max
post_join_employer %>% 
    select(role, age_min, age_max) %>% 
    gather("age_max", "age_min", key = 'age_type', value = 'age') %>% 
    ggplot(aes(x=age_type, y=age, color=age_type)) 
        + geom_point(position = 'jitter') 
        + geom_hline(yintercept = 22.27, color='#339966') 
        + geom_hline(yintercept = 39.83, color='red') 
        # Annotation
        + annotate("text", x = 2, y = 45, color = "red", label = "Average Max Age: 39 yrs") 
        + annotate("text", x = 1, y = 15, color = "#339966", label = "Average Min Age: 22 yrs") 
        + labs(x="Max and Min Age", y='Age', fill='Age Range', title = "Job Posting Age Spread")


# Qualifications Wordcloud
install.packages('wordcloud')
library(wordcloud)

qual_vector <- as.vector(post_join_employer$job_qualification)

# create dataframe specifically for wordcloud
post_join_employer %>% 
    group_by(job_qualification) %>% 
    tally(sort = TRUE) -> job_qual

# black background (base r)
par(bg='black')

# the wordcloud
wordcloud(job_qual$job_qualification, 
                        job_qual$n, 
                        colors = terrain.colors(length(job_qual$job_qualification), 
                        alpha=0.9), 
                        rot.per = 0.3, 
                        family = 'Krub')



# separate thing
smart_job_data2 

### Filtering SPAM Email ###

## create dataframe with just Employer_Email, Employer_EmployerName
## n = 6428
employer_info <- smart_job_data2 %>% 
    select(Employer_OrganizationName, 
            Employer_EmployerName, 
            Employer_Telephone, 
            Employer_Email)

## filter out xxx@xxx.com
## filter out 1026 rows
employer_info %>% filter(grepl("xx", employer_info$Employer_Email)) %>% view()


## filter out 111@hotmail, but potential high type-2 error
## filter out 483
employer_info %>% filter(grepl("([0-9])\\1", tolower(Employer_Email))) %>% view()

## filter out 20 1111@hotmail.com, 000000@hotmail.com but also aifa88888@gmail.com
employer_info %>% filter(grepl("([0-9])\\1([0-9])\\2", tolower(Employer_Email))) %>% view()

## filter out (only) 5 ; 000000@hotmail.com
employer_info %>% filter(grepl("([0-9])\\1([0-9])\\2([0-9])\\3", tolower(Employer_Email))) %>% view()


##### NOTE !grepl()  not  grepl()
    # 6428 total
employer_info %>% 
    # 5402 left
    filter(!grepl("xx", employer_info$Employer_Email)) %>% 
    # 5397 left
    # filtering out consecutive numbers
    filter(!grepl("([0-9])\\1([0-9])\\2([0-9])\\3", tolower(Employer_Email))) %>% 
    # 5382 left
    filter(!grepl("([0-9])\\1([0-9])\\2", tolower(Employer_Email))) %>% 
    # 4922 left
    filter(!grepl("([0-9])\\1", tolower(Employer_Email))) %>%
    # 4732 left (filter out Thai alphabets)
    filter(!grepl("^[ก-๙]", tolower(Employer_Email))) %>% 
    # 4569 left 
    # 2890 left 
    filter(!grepl("123", tolower(Employer_Email))) %>% 
    # filtering out consecutive letters
    filter(!grepl("([a-z])\\1([a-z])\\2([a-z])\\3", tolower(Employer_Email))) %>% 
    filter(!grepl("([a-z])\\1([a-z])\\2", tolower(Employer_Email))) %>% 
    filter(!grepl("([a-z])\\1", tolower(Employer_Email))) %>% 
    filter(!grepl("-@", tolower(Employer_Email))) %>% 
    view()


# 1st Method Email Filter (# rows left from 6428)
employer_info %>% 
    filter(!grepl("xx", employer_info$Employer_Email)) %>%    # 5402 left (1026 out)
    # regex \\1 indicates first remembered pattern of ([a-zA-Z0-9_]); 
    # Three consecutive letters and numbers
    filter(!grepl("([a-z0-9\\d])\\1\\1", tolower(Employer_Email))) %>%     # 5080 left  (1348 out)
    # filter(!grepl("^[ก-๙]", tolower(Employer_Email))) %>% 
    filter(!grepl("123", tolower(Employer_Email))) %>%      # 4915 left   (1513 out)
    filter(!grepl("-@", tolower(Employer_Email)))            # 4892 left   (1536 out)
    -> employer_info1



# take out (not filter out) all Thai alphabets in a separate step
# 4892 entries
employer_info1$Employer_Email <- gsub("[ก-๙]", "", employer_info1$Employer_Email)

# filter only unique emails (but consider 4892 - legit email)
View(distinct(employer_info1, employer_info1$Employer_Email, .keep_all = TRUE))


# prepare to test emails as Ham or Spam
# create email_quality column based on my filter
# 0 = spam, 1 = ham
# note use of grepl() instead of !grepl()
employer_info$email_quality <- ifelse(grepl("xx", employer_info$Employer_Email), 0, employer_info$email_quality)
# Three consecutive letters and numbers
employer_info$email_quality <- ifelse(grepl("([a-z0-9\\d])\\1\\1", tolower(employer_info$Employer_Email)), 0, employer_info$email_quality)
employer_info$email_quality <- ifelse(grepl("123", tolower(employer_info$Employer_Email)), 0, employer_info$email_quality)
employer_info$email_quality <- ifelse(grepl("-@", tolower(employer_info$Employer_Email)), 0, employer_info$email_quality)
# change all NA to 1
employer_info$email_quality <- ifelse(is.na(employer_info$email_quality), 1, employer_info$email_quality)

# remove thai alphabet
employer_info$Employer_Email <- gsub("[ก-๙]", "", employer_info$Employer_Email)


#### Figuring total portions to compare

# Compare Distinct Employer Name vs Email
View(employer_info %>% distinct(Employer_EmployerName))     # 3208 unique Names
View(employer_info %>% distinct(Employer_Email))            # 2275 unique Emails

View(employer_info %>% group_by(Employer_EmployerName, Employer_Email) %>% tally(sort = TRUE))    # 3227 unique Names with Emails

# Even with email_quality, 3227 unique Names and Emails remain (2397 valid, 830 spam)
# Next: Filter for email_quality == 1, read to csv
View(employer_info %>% group_by(Employer_EmployerName, Employer_Email, email_quality) %>% tally(sort = TRUE))
# Filter by email_quality
View(employer_info %>% group_by(Employer_EmployerName, Employer_Email, email_quality) %>% tally(sort = TRUE) %>% filter(email_quality==1))
# save into new data frame
## employer_info1 contains 2397 valid emails
employer_info %>% group_by(Employer_EmployerName, Employer_Email, email_quality) %>% 
    tally(sort = TRUE) %>% 
    filter(email_quality==1) -> employer_info1


## employer_info0 contains 830 INVALID emails
employer_info %>% 
    group_by(Employer_EmployerName, Employer_Email, email_quality) %>% 
    tally(sort = TRUE) %>% 
    filter(email_quality==0) -> employer_info0


### Prepare CSV file for testing of emails
valid_email <- employer_info1$Employer_Email
invalid_email <- employer_info0$Employer_Email


## CLEAN TELEPHONE NUMBERS
# get rid of hyphens
gsub("-", "", employer_info$Employer_Telephone)