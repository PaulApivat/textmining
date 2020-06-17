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