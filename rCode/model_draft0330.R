Sys.setenv(LANG = "en")

#avoid scientific notation
options(scipen = 999)

#packages
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)
library(viridis)
library(ggplot2)
library(kableExtra)
library(stargazer)
library(FNN)

##====Import and Clean Data====##

#import trip data
Aug_pnts <- st_read("E:/NYCDOT/riderpoint_shp/Aug20_pnts/tripPoints_Aug20.shp")

#filter out dates in 1-15
Aug_pnts <- Aug_pnts %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)))

Aug_pnts <- Aug_pnts %>%
  filter(day <= 15)

borough <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp") %>%
  st_transform(st_crs(Aug_pnts))

#borough-->not Staten Island
borough <- borough %>%
  filter(boro_name != "Staten Island")

#find out pnts inside 2 boroughs
Aug_pnts <- Aug_pnts[borough,]


##=====Roads and Bikelanes====##
#import Lion bikelane data
bike20d_buffer <- st_read("E:/NYCDOT/Lion data/lion2020d/lion20d_buffer/lion20d_Buffer.shp") %>%
  st_transform(st_crs(Aug_pnts))

#select useful column
bike20d_buffer <- bike20d_buffer %>%
  select(Street, BikeLane, SegmentID, geometry)

#only roads inside four boroughs
bike20d_buffer <- bike20d_buffer[borough,]

#create a new column of whether it is a bikelane
bike20d_buffer <- bike20d_buffer %>%
  mutate(bikeline = ifelse(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9", "yes", "no"))

#assign NA with no
bike20d_buffer$bikeline[is.na(bike20d_buffer$bikeline)] <- "no"


#other features

##Protected bikelanes
bike20d_buffer <- bike20d_buffer %>%
  mutate(protected = ifelse(BikeLane == "1", "yes", "no"))
#assign NA with no
bike20d_buffer$protected[is.na(bike20d_buffer$protected)] <- "no"


##Unprotected bikelanes
bike20d_buffer <- bike20d_buffer %>%
  mutate(unprotected = ifelse(BikeLane == "2", "yes", "no"))
#assign NA with no
bike20d_buffer$unprotected[is.na(bike20d_buffer$unprotected)] <- "no"

#Find out the points inside bikelanes
Aug.in <- Aug_pnts[bike20d_buffer,]

#allocate street names
Aug.in <- st_join(Aug.in, bike20d_buffer, join=st_intersects, left=TRUE)

#Group Points inside Same Street and Same ID as linestring
ls.Aug <- Aug.in %>%
  st_drop_geometry() %>%
  group_by(index, Street, SegmentID) %>%
  summarise()

#count the number of trips on each Street segment
count.Aug <- ls.Aug %>%
  group_by(Street, SegmentID) %>%
  summarise(Count = n())

#merge the info that whether the road is bikelane or not
#keep the order to makesure sf
info.Aug <- merge(bike20d_buffer, count.Aug)


##=====Feature Engineering========##

##nn function
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

##distance to nearest bikelanes

#change projection
info.Aug <- info.Aug %>% st_transform('ESRI:102711')
bike20d_buffer <- bike20d_buffer %>% st_transform('ESRI:102711')
bikeslines <- bike20d_buffer %>% filter(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9")

###Distance to nearest bikelanes
info.Aug <- info.Aug %>%
  mutate(dist.lane = nn_function(st_coordinates(st_centroid(info.Aug$geometry)),
                                 st_coordinates(st_centroid(bikeslines$geometry)), 1))

#test
#test <- info.Aug[1:5,]

#test <- test %>%
  #mutate(dist.lane = nn_function(st_coordinates(st_centroid(test$geometry)),
                                 #st_coordinates(st_centroid(bikes$geometry)), 1))

         
##====Regression====###
#Make the regression
reg <- lm(Count ~ bikeline + protected + unprotected + dist.lane, data=info.Aug)
summary(reg)
