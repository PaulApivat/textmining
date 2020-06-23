# Convert UTM coordinate data into Longitude - Latitude data ----

# Preparation ----
install.packages('sp')
library(data.table)
library(sp)
library(tidyverse)
library(raster)
library(maps)

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
# the utm data doesn't have CRS
proj4string(utm)

# add CRS (coordinates reference system)
# Thailand's ESPG code is 1231
# source: http://www.epsg-registry.org/

proj4string(utm) <- CRS("+init=epsg:2358")
head(utm)

# convert to Longitude-Latitude data
dist.location <- spTransform(utm, CRS("+init=epsg:4326"))
dist.location


# since we're referencing back to jobpost, need to delete 50th row before transform data set
jobpost <- jobpost[-50,]


# transform dataset
thai.map <- data.frame(Location = jobpost$location, 
                       lat = dist.location$jobpost.utm_x, 
                       long = dist.location$jobpost.utm_y)


# Visualization ----
library(maps)

world.map <- map_data("world")
THAI.map <- world.map %>% filter(region=='Thailand')
THAI.map %>%
    ggplot() +
    geom_map(map = THAI.map, 
             aes(x = long, y = lat, map_id = region),
             fill="white", colour = "black") +
    coord_map()
    geom_point(data = thai.map, 
               aes(x=lat, y=long, color="red", alpha = .9))

# could not see red dots
# use Bangkok Map from another project
    

# Conclusion ----
thai.map %>% head()
    
    