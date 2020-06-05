# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

###### Interact with ManyChat API ######

## NOTE: below is simple API request; 
## some APIs require user-info - password, username

# packages and libraries
install.packages(c('httr', 'jsonlite', 'tidyverse'))

# package and libraries
library(httr)
library(jsonlite)
library(tidyvers)

# GET() request from httr package
getinfo = GET('https://api.manychat.com/fb/page/getInfo')
getgrowthtools = GET('https://api.manychat.com/fb/page/getGrowthTools')

# NOTE: status 401 error for both api endpoints. 