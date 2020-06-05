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


# Process:
# 1. manychat -> settings -> API -> Get API Key
# 2. go to api.manychat.com/swagger# -> Authorize -> paste API 
# 3. Request URL https://api.manychat.com/fb/page/getInfo
# 4. Request URL https://api.manychat.com/fb/page/getGrowthTools 

# NOTE: status 401 ERROR for both api endpoints. 

# UPDATE: Success (status 200)
# 2. go to api.manychat.com/swagger# --> Authorize -> paste "Bearer API Key"
# can only download specific users 1 at a time (need way to download in bulk)
