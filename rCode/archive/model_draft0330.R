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


#neighborhood DATA
#reference:https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-nynta.page
neighborhood <- st_read("E:/NYCDOT/NYCneighborhood/nynta.shp") %>%
  st_transform(st_crs(Aug_pnts))

#mapview(neighborhood)

#only neighboor in 4 boro
neighborhood <- neighborhood[borough,]

#Station Extent
#station <- st_read("E:/NYCDOT/Station_Extent_2021/station_extent_2021.shp") %>%
  #st_transform(st_crs(Aug_pnts))

#mapview(station)


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

#find out the number of trips on bikelanes
bikeslines <- bike20d_buffer %>% filter(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9")
bike.Aug <- merge(bikeslines, count.Aug)

#find out the number of trips in each neighborhood
bike.nh <- st_join(info.Aug, neighborhood %>% st_transform(st_crs(info.Aug)), join=st_intersects, left=TRUE)

bike.nh <- bike.nh %>%
  select(NTAName, Count) %>%
  group_by(NTAName) %>%
  summarise(nhCount = sum(Count))




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



#change projection
info.Aug <- info.Aug %>% st_transform('ESRI:102711')
bike20d_buffer <- bike20d_buffer %>% st_transform('ESRI:102711')
bike.Aug <- bike.Aug %>% st_transform('ESRI:102711')
bikeslines <- bikeslines %>% st_transform('ESRI:102711')
bike.nh <- bike.nh %>% st_transform('ESRI:102711')



##distance to nearest bikelanes

###Distance to nearest bikelanes
info.Aug <- info.Aug %>%
  mutate(dist.lane = nn_function(st_coordinates(st_centroid(info.Aug$geometry)),
                                 st_coordinates(st_centroid(bikeslines$geometry)), 1))




##Biketrips at cloest roads

#function to get cloest idx
nn_idx<- nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.index[,k]
  return(nn)  
}

#N1
#get the index
info.Aug <- info.Aug %>%
  mutate(nnidx = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                      st_coordinates(st_centroid(bike.Aug$geometry)), 1))


#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$n1Count[i] =bike.Aug[info.Aug$nnidx[i],]$Count
}

#N2
#get the index
info.Aug <- info.Aug %>%
  mutate(nnidx2 = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                      st_coordinates(st_centroid(bike.Aug$geometry)), 2))


#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$n2Count[i] =bike.Aug[info.Aug$nnidx2[i],]$Count
}


#N3
#get the index
info.Aug <- info.Aug %>%
  mutate(nnidx3 = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                         st_coordinates(st_centroid(bike.Aug$geometry)), 3))

#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$n3Count[i] =bike.Aug[info.Aug$nnidx3[i],]$Count
}


#The number of biketrips in each neighborhood

info.Aug <- st_join(info.Aug, bike.nh, join=st_intersects, left=TRUE)



#the number of biketrips on the surrounding  (closest) 2 roads
info.Aug2 <- info.Aug

#C1
#get the index
info.Aug <- info.Aug %>%
  mutate(cidx = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                        st_coordinates(st_centroid(info.Aug2$geometry)), 1))
#C2
info.Aug <- info.Aug %>%
  mutate(cidx2 = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                       st_coordinates(st_centroid(info.Aug2$geometry)), 2))

#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$c1Count[i] =info.Aug2[info.Aug$cidx[i],]$Count
}


for (i in 1:nrow(info.Aug)) {
  info.Aug$c2Count[i] =info.Aug2[info.Aug$cidx2[i],]$Count
}

info.Aug <- info.Aug %>%
  mutate(C2 = c1Count + c2Count)



##====Regression====###
#Make the regression
reg <- lm(Count ~ bikeline + dist.lane + protected + unprotected +
            n1Count + n2Count + n3Count + nhCount + C2, data=info.Aug)

summary(reg)

stargazer(reg, type = "html",
          title = "Regression results",
          single.row = TRUE)


#neighborhood effect?
reg.nh <- lm(Count ~ bikeline + dist.lane + protected + unprotected +
            n1Count + n2Count + n3Count + nhCount, data=info.Aug)

stargazer(reg.nh, type = "html",
          title = "Regression results",
          single.row = TRUE)

##Modeling results in Different borough
Manhattan <- borough %>%
  filter(boro_name == "Manhattan") %>%
  st_transform(st_crs(info.Aug))


info.Manhattan <- info.Aug[Manhattan,]

reg.M <- lm(Count ~ bikeline + dist.lane + protected + unprotected +
            n1Count + n2Count + n3Count + nhCount + C2, data=info.Manhattan)

summary(reg.M)

stargazer(reg.M, type = "html",
          title = "Regression results",
          single.row = TRUE)


#==========TEST===========#

test <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.index
  return(nn)  
}



info.test <- info.Aug[1:100,]
bike.test <- bikeslines[10:15,]
info.test2 <- info.Aug[115:300,]


test(st_coordinates(st_centroid(info.test$geometry)), 
     st_coordinates(st_centroid(bikeslines$geometry)), 1)

as.matrix(st_coordinates(st_centroid(info.test$geometry)))


get.knnx(st_coordinates(st_centroid(info.test$geometry)), 
         st_coordinates(st_centroid(bikeslines$geometry)), 1)


get.knn(info.test, k=2)$nn.index
get.knn(info.test, k=2)$nn.dist

as.matrix(info.test)
as.matrix(bikeslines)
get.knn(as.matrix(info.test$geometry), as.matrix(bikeslines$geometry), k=1)


get.knn(cbind(1:10, 1:10), 2)$nn.index[,2]

info.test <- cbind(1:10, 1:10)


test(st_coordinates(st_centroid(info.test$geometry)),
            st_coordinates(st_centroid(bike.test$geometry)), 1)


mapview(list(info.test, bike.test), color=c("red", "blue"), lwd=10)

mapview(bike.test, lwd = 10)


info.test <- info.test %>%
  mutate(nnidx = test(st_coordinates(st_centroid(info.test$geometry)),
                      st_coordinates(st_centroid(info.test2$geometry)), 1))

bike.test[info.test$nnidx]

info.test <- info.test %>%
  mutate(test = bike.test[info.test$nnidx])

bike.test[1,]
info.test$nnidx[2]
bike.test[info.test$nnidx[1],]$SegmentID

for (i in 1:nrow(info.test)) {
  info.test$test =info.test2[info.test$nnidx[i],]$Count
}

for (i in 1:nrow(info.test)) {
  print(info.test$nnidx[i])
}

for (i in 1:nrow(info.test)) {
  print(info.test2[info.test$nnidx[i],]$Count)
}

for (i in 1:nrow(info.test)) {
  info.test$test[i] =info.test2[info.test$nnidx[i],]$Count
}




mapview(list(info.test, info.test2), color=c("red", "blue"), lwd=10)

#test
#test <- info.Aug[1:5,]

#test <- test %>%
  #mutate(dist.lane = nn_function(st_coordinates(st_centroid(test$geometry)),
                                 #st_coordinates(st_centroid(bikes$geometry)), 1))

         

