# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

### PREREQUISITE: Need PostgreSQL database up and running

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
# return character(0) means 'no tables' are stored in the database, so we must create one
dbListTables(con)




