# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042


# library and packages
library(tidyverse)
library(jsonlite)

# load data
file(load = "upski.RData")


# read JSON data
json_data <- fromJSON("april.json", flatten = TRUE)
json_data2 <- fromJSON("april.json", flatten = FALSE)   # subtle difference

# this is a list
# note: aside from 'posts' other columns are either list of one or not informative
# note: pageUrL suggest the entirety of 'posts' came from one FB page
json_data$posts

# list converted to a character (not advised)
posts <- as.character(json_data$posts)

# list converted to data.frame (recommended)
# note: some cells contain other lists or variables
# note: postImages, postLinks and postsUrl contain data about a specific FB job-listing post
posts_df <- as.data.frame(json_data$posts)

# These 4 columns: postStats.comments, postStats.reactions, postStats.shares, postComments.count
# indicate how "popular/engaging" a ROW in posts_df dataframe is.
# NOTE: each ROW is a post for a Job Position (there are 187 Rows or Job Position posted)
# includes description of job and of the FB post itself 
# (# reactions, # comments, # shares)


### Explore postText ###

# arranged desc order postStats.reactions - top 23 are *not* job posts, but posting of screen shots (humor, thought provoking)
# screen shot of pantip or twitter (@BFkumkom)
View(posts_df %>% arrange(desc(postStats.reactions)))

# arrange desc order postStats.comments - 50/50 job posts & screen shots
# top 10 job posts include: online gamer, lazada (inventory mgmt), online fiction writer, online teaching, thai post office
# tesco lotus (operations), farmhouse, audio engineer / sound mix, 
# cross-posting: parttime pantip
View(posts_df %>% arrange(desc(postStats.comments)))
View(posts_df %>% arrange(desc(postComments.count)))


# arrange desc order postStats.shares
# only 2 job posting in top twenty; rest screen shots similar to 'reactions'
# 
View(posts_df %>% arrange(desc(postStats.shares)))



# actual comments to each posting
# filtered by 'RECENT_ACTIVITY' (newest) - response to each posting
# ERROR: argument imply differing number of rows (i.e., each posts getting different number of actual comments)
# SOLUTION: manual - go through each of 187 one-by-one to convert to dataframe

comments_post_1 <- as.data.frame(posts_df$postComments.comments[1])





