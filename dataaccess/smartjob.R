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

# Unique UserID:
# data SHAPE: 2,956 rows, 175 columns
View(distinct(smart_job_data, UserID, .keep_all = TRUE))

# check column names (compare to data_full)
names(smart_job_data)  # compare names(data_full)

## NOTES
# - jobseeker can create multiple entries in smart_job_data (see UserID)

##### Subset data based on unique UserID #####
# data SHAPE: 2,956 rows, 175 columns
distinct(smart_job_data, UserID, .keep_all = TRUE) -> smartjob_unique

smartjob_unique

##### NOTE some UserID had multiple entries
## 188338 had 17 entries (most), 14798488 had 15 entries (second) etc.

####### Exploratory Data Visualization (Descriptive) ##########
# note: assumption of unique UserID
# note: each unique UserID had corresponding unique EmployerID (NO overlap in jobs?)
# note: will try not to create additional data frames aside from smartjob_unique

### Employment Types Sought
smartjob_unique %>% 
    group_by(EmploymentType) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x = reorder(EmploymentType, n), y=n, fill=EmploymentType)) 
        + geom_bar(stat = 'identity') 
        + geom_text(aes(label=n), vjust=-0.5) 
        # changing Legend text labels
        + scale_fill_discrete(name = "Employment Type", 
                              labels = c("Part-Time", "Full-Time", "Temporary")) 
        + labs(x = 'Employment Type', 
               y = 'Number of People', 
               title = "Smart Job Data: Job Seeker by Employment Type")

### Applicant Types of Job Seekers
smartjob_unique %>% 
    group_by(ApplicantTypeName) %>% 
    tally(sort = TRUE) %>% 
    ggplot(aes(x = reorder(ApplicantTypeName, n), y=n, fill=ApplicantTypeName)) 
        + geom_bar(stat = 'identity') 
        + geom_text(aes(label=n), vjust=-0.5) 
        + scale_fill_discrete(name = "Applicant Type Name", labels = c("Disability m.33", "Elder", "General Job Seeker")) 
        + labs(x = 'Applicant Type Name', y = 'Number of People', title = "Smart Job Data: Job Seeker by Applicant Type") 
        + theme(axis.text.x = element_text(family = 'Krub', angle = 45, hjust = 1))

