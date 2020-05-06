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


############# EXPLORATORY DATA ANALYSIS ################

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
View(posts_df %>% arrange(desc(postStats.shares)))

########### Specific Word Search #############
# 'Part Time' (24)
# 'Full Time' (7)
# 'Pizza' (9) // 6 = Pizza Company, 2 = Domino's, 1 = Call Center
# 'Lazada' (3)
# 'Call Center' (5)
# 'Kerry Express' (4)
# 'Grab Bike' (1)
# 'Tesco Lotus' (3)
# 'KFC' (2)
# 'พนักงานปฏิบัติการ' (Tesco) (1)
# 'ออนไลน์' (23) (apply online)
# 'Admin' (6) 
# 'แอดมิน' (11)
# 'Office' (4)
# 'Content' (1)
# 'นักแปล' (1)
# 'แม่บ้าน' (6)
# 'ประจำร้าน' (15)
# '10,000' (8) *10,000 bht salary in range
# '11,000' (5) *11,000 bht salary in range
# '13,000' (9) *13,000 bht salary in range
# '15,000' (37) *15,000 bht salary in range
# '18,000' (9) *18,000 bht salary in range
# '20,000' (15) *20,000 bht salary in range
# '30,000' (12) *30,000 bht salary in range
# '40,000' (1) *40,000 bht salary in range
# 'อายุ 25' (9)
# 'คอมมิชชั่น' (8)
# 'Commission' (4)
# 'แพ็คสินค้า' (5)
# 'จีน' (5)
# 'ร้านซักรีด' (1)
# 'Telesales' (4)
# 'ประกันสังคม' (social benefit) (23)
# '📍' (96) 
# 'วุฒิ ม.3' (19)
# 'สาขา' (38) Retail
# 'ร้าน' (40) Retail
# 'พนักงานประจำร้าน' (13) Retail
# 'ร้านอาหาร' (restaurant) (6)
# 'ไลฟ์สด' (Video Jockey) (5)
# 'อาหาร' (20)



# Example specific text search
View(posts_df %>% filter(grepl('Pizza', postText)))
View(posts_df %>% filter(grepl('Lazada', postText)))
View(posts_df %>% filter(grepl('ออนไลน์', postText)))
View(posts_df %>% filter(grepl('แม่บ้าน', postText)))
View(posts_df %>% filter(grepl('ประจำร้าน', postText)))
View(posts_df %>% filter(grepl('11,000', postText)))
View(posts_df %>% filter(grepl('อายุ 25', postText)))
View(posts_df %>% filter(grepl('ประกันสังคม', postText)))
View(posts_df %>% filter(grepl('📍', postText)))   
View(posts_df %>% filter(grepl('วุฒิ ม.3', postText)))
View(posts_df %>% filter(grepl('สาขา', postText)))
View(posts_df %>% filter(grepl('อาหาร', postText)))


#### Top 30 Job Posting by *Number_of_Comments
## non-traditional jobs descriptions: นักเล่นเกมส์, นักเขียนนิยายออนไลน์, แคสออนไลน์,
## ผู้เข้าร่วมทดสอบเกม, งานอัดเสียงที่บ้าน, สอนภาษาจีนออนไลน์, เปิดแคสติ้งออนไลน์, รับสมัครแม่บ้านออนไลน์ 
View(posts_df %>% arrange(desc(postStats.comments)))





# 24 of 187 includes "Part Time"
View(filter(posts_df, grepl('Part Time', postText)))

# same filter through dplyr (24 of 187 includes "Part Time")
View(posts_df %>% filter(grepl('Part Time', postText)))

####### 24 'Part Time' job posts arranged in descending order by reactions (can repeat for comments, shares)
View(posts_df %>% filter(grepl('Part Time', postText)) %>% arrange(desc(postStats.reactions)))





# actual comments to each posting
# filtered by 'RECENT_ACTIVITY' (newest) - response to each posting
# ERROR: argument imply differing number of rows (i.e., each posts getting different number of actual comments)
# SOLUTION: manual - go through each of 187 one-by-one to convert to dataframe

comments_post_1 <- as.data.frame(posts_df$postComments.comments[1])





