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
    tally(sort = TRUE) -> jobseeker_gender



# group by STUDY.LEVEL (Education)
# undergrad (360); master (96); grade-12 (46); grade-9 (16); อนุปริญญา (14); phd (10)
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(study.level) %>% 
    tally(sort = TRUE) -> jobseeker_edu

# group by AGE
# highest frequency 20's, then 30's, then 40's, then 50's & 60's (expected)
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(age) %>% 
    tally(sort = TRUE) -> jobseeker_age

## age is a 'factor'
## create another factor for 'age range'
## note: minimum working age in TH is 15
jobseeker_age[,'age_bracket'] <- NA
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age < 15, 'minor', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 15 & jobseeker_age$age < 20, 'teens', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 19 & jobseeker_age$age < 30, '20s', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 29 & jobseeker_age$age < 40, '30s', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 39 & jobseeker_age$age < 50, '40s', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 49 & jobseeker_age$age < 60, '50s', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 59 & jobseeker_age$age < 70, '60s', jobseeker_age$age_bracket)
jobseeker_age$age_bracket <- ifelse(jobseeker_age$age > 69, '70+', jobseeker_age$age_bracket)



# group by JOB CATEGORY (job.cat)
# mostly missing data or '0' (no category)
# งานอื่นๆ (n = 47); การขาย การตลาด (n = 42); วิศวกร (n = 30)
# note: need to join jobseeker and jobcat dataframes
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(job.cat) %>% 
    tally(sort = TRUE) -> jobseeker_jobcat   # n = 35


# Need to merge with jobcat2 to access job_category and job_cluster (n = 35)
jobseeker_jobcat <- merge(x=jobseeker_jobcat, 
                          y=jobcat2, 
                          by='job.cat', 
                          all.x = TRUE)








# group by WORK EXPERIENCE (YEARS)
# this is a Factor; consider convert to numeric
# mostly of missing data
# work experience by frequency: 0 (118), 1 (45), 2 (33)
# many first jobber profiles
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(work.exp) %>% 
    tally(sort = TRUE) -> jobseeker_we

# change work.exp from factor to numeric
jobseeker_we$work.exp <- as.numeric(jobseeker_we$work.exp)

jobseeker_we[,'wk_exp_fct'] <- NA

jobseeker_we$wk_exp_fct <- ifelse(jobseeker_we$work.exp < 4, '1-3', jobseeker_we$wk_exp_fct)
jobseeker_we$wk_exp_fct <- ifelse(jobseeker_we$work.exp > 3 & jobseeker_we$work.exp < 7, '4-6', jobseeker_we$wk_exp_fct)
jobseeker_we$wk_exp_fct <- ifelse(jobseeker_we$work.exp > 6 & jobseeker_we$work.exp < 10, '7-9', jobseeker_we$wk_exp_fct)
jobseeker_we$wk_exp_fct <- ifelse(jobseeker_we$work.exp > 9, '10+', jobseeker_we$wk_exp_fct)

# change wk_exp_fct from character to factor (also set levels)
# better than trying to re-order levels later

x <- jobseeker_we$wk_exp_fct

jobseeker_we$wk_exp_fct <- factor(x, levels = c("1-3", "4-6", "7-9", "10+"), 
                                    labels = c("1-3", "4-6", "7-9", "10+"))






# understand distribution of salary from job-seekers
# salary is a factor so must use group_by
# NOTE consider changing to integer/numeric
# if keep factor then do salary ranges
distinct(jobseeker, facebook.id, .keep_all = TRUE) %>% 
    group_by(min.salary) %>% 
    tally(sort = TRUE) -> jobseeker_min

# change min.salary to numeric from factor WITHOUT losing information
# can use as.numeric(paste(x)), but need to get rid of commas first
# use gsub() function within as.numeric() - from base R
jobseeker_min$min.salary <- as.numeric(gsub(",", "", jobseeker_min$min.salary))

jobseeker_min[,'salary_fct'] <- NA

jobseeker_min$salary_fct <- ifelse(jobseeker_min$min.salary < 15001, '0-15000', jobseeker_min$salary_fct)
jobseeker_min$salary_fct <- ifelse(jobseeker_min$min.salary > 14999 & jobseeker_min$min.salary < 30001, '15000-30000', jobseeker_min$salary_fct)
jobseeker_min$salary_fct <- ifelse(jobseeker_min$min.salary > 29999 & jobseeker_min$min.salary < 45001, '30001-45000', jobseeker_min$salary_fct)
jobseeker_min$salary_fct <- ifelse(jobseeker_min$min.salary > 44999 & jobseeker_min$min.salary < 60001, '45001-60000', jobseeker_min$salary_fct)
jobseeker_min$salary_fct <- ifelse(jobseeker_min$min.salary > 59999 & jobseeker_min$min.salary < 75001, '60001-75000', jobseeker_min$salary_fct)
jobseeker_min$salary_fct <- ifelse(jobseeker_min$min.salary > 75000, '75000+', jobseeker_min$salary_fct)

# still in chr, need to change to factor, and set levels
y <- jobseeker_min$salary_fct

jobseeker_min$salary_fct <- factor(y, levels = c("0-15000","15000-30000", "30001-45000", "45001-60000", "60001-75000","75000+"), 
                                    labels = c("0-15000","15000-30000", "30001-45000", "45001-60000", "60001-75000","75000+"))


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
### NOTE: IF System Permits employer to log multiple times to input multiple requirements
### Then might need to distinct by two variables.


# By Distinct Facebook.ID (n = 48)
distinct(employer, facebook.id, .keep_all = TRUE) %>% view()

# Description of "employer" by role
# note: reduce redundancy by getting rid of emoji or have data dictionary
# เจ้าของกิจการ (32); HR (9); ผู้จัดการแผนก (3); อื่นๆ (4)
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(role) %>% 
    tally(sort = TRUE) %>% 
    view()

# List of "employer" by company.name (n = 48)
# same as # of distinct facebook.id
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(company.name) %>% 
    tally(sort = TRUE) %>% 
    view()

# Description of 'employer' by job.name 
# note: redundant with role?
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(job.name) %>% 
    tally(sort = TRUE) %>% 
    view()

# Employer Requirement by 'job categories' employers seeking
# note: some facebook.id could be redundant
# note: want to avoid employers logging in more than once to fill in info in piecemeal fashion
# OR scane for latest time-stamp, then run distinct() function
employer %>%
    group_by(job.cat) %>%
    tally(sort = TRUE)


######### Join jobcat and employer dataframe
### Assumption Field.ID == job.cat
jobcat %>% select(Field.ID, job_cat, กลุ่มงาน) -> jobcat2
colnames(jobcat2)[1] <- 'job.cat'
colnames(jobcat2)[2] <- 'job_category'
colnames(jobcat2)[3] <- 'job_cluster'

### Need to join data, but ignore missing value
### NOT have new data frame shrink (merge instead inner_join)
employer2 <- merge(x=employer, y=jobcat2, by='job.cat', all.x = TRUE)

### Employer description JOB CATEGORY
### Mostly missing data
employer2 %>% 
    group_by(job_category, job_cluster) %>% 
    tally(sort = TRUE)




# Employer requirements by gender
# ไม่ระบุ (27); หญิง (12); ชาย (6); เพศไหนก็ได้	(3)
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(sex) %>% 
    tally(sort = TRUE) %>% 
    view()

# Employer requirement by study.level (Education)
# ป.ตรี (16), อนุปริญญา (diploma) (10), ม.3 (8), 
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(study.level) %>% 
    tally(sort = TRUE) %>% 
    view()

# Employer requirement by study.field
# mostly missing data (n = 152)
# why is this even a field? นอนสบาย ๆ  
# note: facebook.id could be REDUNDANT
employer %>% 
    group_by(study.field) %>% 
    tally(sort = T)

# Employer requirement by work.experience
# ไม่ต้องมี (20), 1-2 ปี (13), 3-5ปี (4), มากกว่า 5 ปี (4), ไม่ระบุ	 (7)
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(work.exp) %>% 
    tally(sort = TRUE) %>% 
    view()

### Not sure how job.name and jd need to be two separate columns
### Are we letting employers post multiple jobs? perhaps a Jobs ID is needed

# Employer requirement by max.salary
# mostly missing data
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(max.salary) %>% 
    tally(sort = TRUE) %>% 
    view()

# Employer requirement by age.max 
# 40 (12), 35 (9), 45 (6)
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(age.max) %>% 
    tally(sort = TRUE) %>% 
    view()

# Employer requirement by age.min
# 20 (7), 25 (6), 18 (5), 22 (5), 30 (4)
distinct(employer, facebook.id, .keep_all = TRUE) %>% 
    group_by(age.min) %>% 
    tally(sort = TRUE) %>% 
    view()

# Employer requirement by drive.license, own.car, bike.license
# milatary (?) (misspelled), com.skill (?), own.com (?)
# mostly missing data


########--------- Visualization (Exploratory)------- #########
### ASSUMPTION: UNIQUE FACEBOOK_ID

####------- JOB SEEKERS ------ #####

### GENDER
ggplot(data = jobseeker_gender, 
       mapping = aes(x=reorder(sex,n), 
       y=n, 
       fill = sex)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(family = 'Krub', size = 15), 
            legend.text = element_text(family = 'Krub')) 
    + labs(x = 'Gender', 
           y = 'Number of People', 
           fill = 'Gender', 
           title = 'Job Seekers by Gender') 
    + geom_text(aes(label=n), vjust=-0.5)

### EDUCATION
# note: should not allow users to input whole sentences.
jobseeker_edu %>% 
    # filter OUT: ฝั่งดำเนินการในร้านหนังสือออนไลน์ คอยจัดการระบบภายใน... 
    filter(n > 1) %>% 
    ggplot(aes(x=reorder(study.level,n), y=n, fill = study.level)) 
        + geom_bar(stat = 'identity') 
        + theme(axis.text.x = element_text(angle = 45, 
            hjust = 1, 
            color = 'black', 
            family = 'Krub', 
            size = 10), 
            legend.position = 'none', 
            legend.text = element_text(family = 'Krub')) 
        + labs(x = 'Education Levels', 
                y = 'Number of People', 
                fill = 'Edu', 
                title = 'Job Seekers by Education Level') 
        + geom_text(aes(label=n), vjust=-0.5)


### AGE BRACKET
# note: should not allow 500
ggplot(data = jobseeker_age, mapping = aes(x=reorder(age,n), y=n, fill = age_bracket)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(angle = 45, color = 'black', family = 'Krub', size = 10), 
            legend.text = element_text(family = 'Krub')) 
    + labs(x = 'Age Leves', 
           y = 'Number of People', 
           fill = 'Age Bracket', 
           title = 'Job Seekers by Age') 
    + geom_text(aes(label=n), vjust=-0.5)

### JOB CATEGORY
# once you coord_flip(): 
# axis.text.x -> axis.text.y; 
# geom_text(vjust) -> geom_text(hjust)

ggplot(data = jobseeker_jobcat, mapping = aes(x=reorder(job_category,n), y=n, fill = job_cluster)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.y = element_text(color = 'black', family = 'Krub', size = 10), 
            legend.text = element_text(family = 'Krub')) 
    + labs(x = 'Job Categories', 
           y = 'Number of People', 
           fill = 'Job Cluster', 
           title = 'Job Seekers by Job Categories') 
    + geom_text(aes(label=n), hjust=-0.10) 
    + coord_flip()


### WORK EXPERIENCE
# note neede to properly set factor levels in wk_exp_fct
ggplot(data = jobseeker_we, mapping = aes(x=reorder(work.exp,n), y=n, fill = wk_exp_fct)) 
    + geom_bar(stat = 'identity') 
    + theme(axis.text.x = element_text(hjust = 1, color = 'black', family = 'Krub', size = 10), 
            legend.text = element_text(family = 'Krub')) 
    + labs(x = 'Work Experience (yrs)', 
           y = 'Number of People', 
           fill = 'Year Range', 
           title = 'Job Seekers by Work Experience') 
    + geom_text(aes(label=n), vjust=-0.5, color = 'white') 
    + scale_fill_manual(values = c("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45")) 
    + theme(panel.background = element_rect(fill = 'black'), 
            panel.grid.major = element_line(color = 'black'), 
            panel.grid.minor = element_line(color = 'black'))

### MINIMUM SALARY
# ---------------------------- NEEDS WORK -----------------------#
jobseeker_min %>% 
    filter(min.salary != is.na(min.salary)) %>% 
    filter(n > 1) %>% 
    ggplot(aes(x=reorder(min.salary, min.salary), y=n, fill = salary_fct)) 
        + geom_bar(stat = 'identity') 
        + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = 'black', family = 'Krub', size = 10), 
                legend.text = element_text(family = 'Krub')) 
        + labs(x = 'Minimum Salary', 
               y = 'Number of People', 
               fill = 'Min Salary Ranges', 
               title = 'Job Seekers by Desired Mininum Salary') 
        + geom_text(aes(label=n), vjust=-0.5)

#### dataframes of importance
jobseeker
jobseeker_we
jobseeker_age
jobseeker_edu
jobseeker_gender
jobseeker_jobcat
jobseeker_min

