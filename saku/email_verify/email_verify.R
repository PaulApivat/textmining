# R version 3.6.3 (2020-02-29) -- "Holding the Windsock"
# RStudio Version 1.2.5042

library(tidyverse)

# read csv
verified_valid <- read_csv("verified_valid_email.csv")
verified_spam <- read_csv("verified_spam_email.csv")

View(verified_valid)
View(verified_spam)

# verified valid count second column to figure out false positives
# 2313 valid emails, 84 false positives (0.04%)
str(verified_valid)
verified_valid %>% count(is.na(x_1))

# verified spam count second column to figure out false negatives
# 730 valid emails, 100 false negatives (12%)
verified_spam %>% count(is.na(x_1))

