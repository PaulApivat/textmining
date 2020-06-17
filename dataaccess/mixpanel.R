# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# load data
file(load = "saku.RData")

# installing package
install.packages('RMixpanel')
library(RMixpanel)

# set-up account in R
# Fill in API token, key and secret as found on
# www.mixpanel.com - Account -> Projects.

account = mixpanelCreateAccount("Jobsbot",
                                  token="Token",
                                  secret="API Secret", 
                                  key="API Key")

# should see [1] "mixpanelAccount"
class(account)

# identify data type of 'account' - List of 4
str(account)

# convert string to data frame
df <- data.frame(matrix(unlist(account), nrow = length(account), byrow = T))

# no funnel or list data
mixpanelGetData(account, method="funnels/list/", args=list(), data=TRUE)
## Download https://mixpanXXXXXXXXXXX2.0/funnels/list/?... parse data...
## [1] "[]"

### Weekly retention as percentages
# launch event: May 18, 2020
retentions <- mixpanelGetRetention(account, 
                            born_event="AppInstall", 
                            event = "WatchedItem", 
                            from = 20200517, 
                            to = 20200617, 
                            unit = "week")

# Error: number of items to replace is not a multiple of replacement length

mixpanelGetFunnelList(account)
## Download https://mixpanXXXXXXXXXXX2.0/funnels/list/?... parse data...
list()


#### NOTE creating Mixpanel account is simply a list with four fields....

#### Alternative Route: Download CSV data directly: June 17, 2020

# Data Management -> Lexicon (All Events and Properties; All User Profile Properties) # does NOT appear helpful
# Users -> 132 Profile (csv)
