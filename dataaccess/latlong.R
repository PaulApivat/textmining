# Convert UTM coordinate data into Longitude - Latitude data ----

# Preparation ----
install.packages('sp')
library(data.table)
library(sp)
library(tidyverse)
library(raster)

load(file = 'saku.RData')

# Data Manipulation ----

# load relevant dataframes
View(jobpost)

# subset into dataframe
utm <- data.frame(jobpost$utm_x, jobpost$utm_y)
str(utm)

# handle missing values before creating spatial objects
View(utm)
utm <- utm[-50,]

# create spatial object
coordinates(utm) <- ~jobpost.utm_x + jobpost.utm_y
str(utm)

# Visualization ----

# Conclusion ----