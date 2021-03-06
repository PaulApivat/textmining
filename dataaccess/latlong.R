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

proj4string(utm) <- CRS("+init=epsg:1231")
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
    coord_map() +
    geom_point(data = thai.map, 
               aes(x=lat, y=long, color="red", alpha = .9))

# could not see red dots
# use Bangkok Map from another project
library(rgdal)
library(broom)

# download BMASubDistrict_Polygon folder (whole folder) into working directory
###### 180 SUBDISTRICTS
# must contain: .shp, .shx, .dbf files
# layer = is file name infront of .shp , .shx .dbf 
bkk <- readOGR("./BMASubDistrict_Polygon", layer = 'BMA_ADMIN_SUB_DISTRICT')
View(bkk)

# quick plot in base R
plot(bkk, col="#f2f2f2", bg="skyblue", lwd=0.25, border=0 )

# IMPORTANT - to get longitude and lattitude you need convert to data frame
# using broom package
library(broom)

bkk_fortified <- tidy(bkk)

# more detailed plot with ggplot()
ggplot() + geom_polygon(data = bkk_fortified, mapping = aes(x = long, y = lat, group = group), fill="#69b3a2", color="white", alpha = .5) + geom_point(data = thai.map, aes(x=long, y=lat, color="red"), inherit.aes = FALSE)  

ggplot() + geom_point(data = thai.map, aes(x=lat, y=long, color="red")) 

# What if original utm_x and utm_y were already in lat/long?
thai.map$lat <- jobpost$utm_y
thai.map$long <- jobpost$utm_x

# ----- final conversion utm ----- #
sputm <- SpatialPoints(utm, proj4string=CRS("+proj=utm +zone=47N +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84"))                     

# transform again
thai.map2 <- data.frame(Location = jobpost$location, 
                       lat = spgeo$jobpost.utm_y, 
                       long = spgeo$jobpost.utm_x)

# thai map
THAI.map %>%
    ggplot() +
    geom_map(map = THAI.map, 
             aes(x = long, y = lat, map_id = region),
             fill="white", colour = "black") +
    coord_map() +
    geom_point(data = thai.map2, 
               aes(x=lat, y=long, color="red", alpha = .9))

ggplot() + geom_polygon(data = bkk_fortified, mapping = aes(x = long, y = lat, group = group), fill="#69b3a2", color="white", alpha = .5) + geom_point(data = thai.map2, aes(x=lat, y=long, color="red", alpha = .9))

# Conclusion ----
# best result, note: thai.maps2
THAI.map %>% 
    ggplot() 
    + geom_map(map = THAI.map, aes(x=long, y=lat, map_id = region), fill='white', color='black') 
    + geom_point(data = thai.map2, aes(x=long, y=lat, color="red", alpha = .9))
    
# get rid of outlier in thai.map2 (zambia africa)
thai.map2a <- thai.map2[-11,]
View(thai.map2a)

# successfully convert utm to lat/long data for visualization
THAI.map %>% ggplot() + geom_map(map = THAI.map, aes(x=long, y=lat, map_id = region), fill='white', color='black') + geom_point(data = thai.map2a, aes(x=long, y=lat, color="red", alpha = .9))

# Summary ----

# NOTE: key to properly converting utm to lat/long is recognition of the range of possible values
# Caution: lat/long use to convert from utm is based on library(maps), filter region=='Thailand'
# using lat/long from SHAPE file did NOT work (bkkfortified)
library(maps)

world.map <- map_data("world")
THAI.map <- world.map %>% filter(region=='Thailand')

# note in THAI.map, longitude range is 97.37392 - 105.641
# note in THAI.map, lattitude range is 5.636767 - 20.42441
# note in thai.map2a longitude range is 96.10385 - 101.5921 (largely overlap with THAI.map)
# note in thai.map2a lattitude range is 7.1756 - 17.10503 (within range of THAI.map)



# start with jobpost
View(jobpost)
write.csv(jobpost, "jobpost.csv") # for Rmarkdown

# subset into dataframe
utm <- data.frame(jobpost$utm_x, jobpost$utm_y)
str(utm)

# handle missing values before creating spatial objects
View(utm)
utm <- utm[-50,]

# handle outliers as well (ie zambia africa)
# utm <- utm[-11,]

# ----- final conversion utm into lat/long ----- #
# note: +zone=???  Thailand's zone is 47N
# source: https://stackoverflow.com/questions/30018098/how-to-convert-utm-coordinates-to-lat-and-long-in-r/30018607

sputm <- SpatialPoints(utm, proj4string=CRS("+proj=utm +zone=47N +datum=WGS84"))  
spgeo <- spTransform(sputm, CRS("+proj=longlat +datum=WGS84")) 

# transform  using spgeo
thai.map2 <- data.frame(Location = jobpost$location, 
                        lat = spgeo$jobpost.utm_y, 
                        long = spgeo$jobpost.utm_x)


# successfully convert utm to lat/long data for visualization 
# note: 
THAI.map %>% ggplot() 
    + geom_map(map = THAI.map, aes(x=long, y=lat, map_id = region), fill='white', color='black') 
    + geom_point(data = thai.map2a, aes(x=long, y=lat, color="red", alpha = .9))


