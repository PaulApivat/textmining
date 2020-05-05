# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

# library and packages
library(tidyverse)
library(jsonlite)

# read JSON data
json_data <- fromJSON("april.json", flatten = TRUE)
json_data2 <- fromJSON("april.json", flatten = FALSE)   # subtle difference

# this is a list
json_data$posts

# list converted to a character (not advised)
posts <- as.character(json_data$posts)

# list converted to data.frame (recommended)
# note: some cells contain other lists or variables
posts_df <- as.data.frame(json_data$posts)





