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
# return 'character(0)' means 'no tables' are stored in the database, so we must create one
dbListTables(con)

# practice inserting data / running queries

# save mtcars into my_data, change first column to character
my_data <- data.frame(carname = rownames(mtcars), mtcars, row.names = NULL)
my_data$carname <- as.character(my_data$carname)

# write to 'con' database (or allows overwriting of table)
dbWriteTable(con, name = 'cars', value = my_data)
dbWriteTable(con, name = 'cars', value = my_data, overwrite = TRUE)

# re-run and should see "cars"
dbListTables(con)

# reading
dbReadTable(con, 'cars')

# Querying Data







