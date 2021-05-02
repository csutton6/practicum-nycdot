setwd('C:/Users/xinti/Box/MUSA_800_Practicum/Data')
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



################ read in data and clean the data #############################
Aug2020 <- st_read("C:/Users/xinti/Box/MUSA_800_Practicum/Data/BikeTrips/pointData/Aug20_pnts/tripPoints_Aug20.shp") %>% st_transform(crs = 4326)

road2020 <- st_read("Infrastructure/Lion/lion2020d_buffer15.shp")%>% st_transform(crs = 4326)

#filter out dates in 1-15
Aug2020 <- Aug2020 %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)))


AugFir2Wks <- Aug2020 %>%
  filter(day <= 15)

extent <- st_read("OtherData/Station_Extent_2020/station_extent_Aug2020.shp") %>%
  st_transform(crs = 4326)

road2020 <- road2020[extent,]
AugFir2Wks <- AugFir2Wks[extent,]
rm(Aug2020)


######################### Calculate ridership #####################
# clip pnts outside of the road buffers
clnPnt2020 <- AugFir2Wks[road2020,] %>% 
  mutate(pnt_id = seq.int(nrow(.))) %>% 
  st_transform(crs = 4326) %>% 
  st_join(.,road2020,join = st_intersects, left = T)

rm(AugFir2Wks)

countTrip <- function(pnt){
  tripCount <- pnt %>%
    as.data.frame() %>%
    select(-geometry) %>%
    group_by(id_1,SegmentID) %>%
    summarise() %>%
    group_by(SegmentID) %>%
    summarise(tripCount = n())
  return(tripCount)
}

tripCount <- countTrip(clnPnt2020)

road2020 <- road2020 %>% 
  merge(.,tripCount,by = "SegmentID",all.x = T) %>% 
  rename(tripAugFir2=tripCount) 


################################# Feature engineering #############################
road2020_se <- road2020 %>% select(SegmentID,Street,FeatureTyp,RW_TYPE,BikeLane,Number_Tra,geometry,tripAugFir2,SegmentTyp) %>% 
  mutate_all(~replace(.,is.na(.),0)) 
# filter out by featuretype and rw_type
road2020_se <- road2020_se %>%  filter(FeatureTyp != 1 &
                                         FeatureTyp != 2 &
                                         FeatureTyp != 3 &
                                         FeatureTyp != 5 &
                                         FeatureTyp != 7 &
                                         FeatureTyp != 8 &
                                         FeatureTyp != 9 &
                                         RW_TYPE != 2 &
                                         RW_TYPE != 4 &
                                         RW_TYPE != 7 &
                                         RW_TYPE != 12 &
                                         RW_TYPE != 14 )

# add dummy variable for trail and bikelane
road2020_se <- road2020_se %>% 
  mutate(
    trailOrNot = case_when(
      RW_TYPE == 6 ~ 1,
      TRUE~0
    ),
    bikeLaneLv = case_when(
      BikeLane == 1 | BikeLane == 5 | BikeLane == 8 | BikeLane == 9 | BikeLane == 10 ~ "Protected",
      BikeLane == 2 | BikeLane == 3 | BikeLane == 4 | BikeLane == 6 | BikeLane == 11 ~ "Unprotected",
      TRUE ~ "noBikeLane"
    ))

# add borough id
borough<- st_read('https://data.cityofnewyork.us/resource/7t3b-ywvw.geojson')%>%
  st_transform(crs = 4326)

road2020_se <- road2020_se%>% st_join(.,borough %>% select(boro_name,boro_code),join = st_intersects, left = T)


# collect bike lane information
bikeslines <- road2020_se %>% filter(bikeLaneLv!="noBikeLane")
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

road2020_se <- road2020_se %>% st_transform('ESRI:102711')
bikeslines <- bikeslines %>% st_transform('ESRI:102711')

# distance to nearest bikelane
road2020_se <- road2020_se %>% 
  mutate(dist.lane = nn_function(st_coordinates(st_centroid(road2020_se$geometry)),
                                 st_coordinates(st_centroid(bikeslines$geometry)), 1))

# we dont use spatial lag this time
# #function to get cloest idx
# nn_idx<- nn_function <- function(measureFrom,measureTo,k) {
#   measureFrom_Matrix <- as.matrix(measureFrom)
#   measureTo_Matrix <- as.matrix(measureTo)
#   nn <-   
#     get.knnx(measureTo, measureFrom, k)$nn.index[,k]
#   return(nn)  
# }
# road2020_se2 <- road2020_se
# #C1
# #get the index
# road2020_se <- road2020_se %>%
#   mutate(cidx = nn_idx(st_coordinates(st_centroid(road2020_se$geometry)),
#                        st_coordinates(st_centroid(road2020_se2$geometry)), 1))
# #C2
# road2020_se <- road2020_se %>%
#   mutate(cidx2 = nn_idx(st_coordinates(st_centroid(road2020_se$geometry)),
#                         st_coordinates(st_centroid(road2020_se2$geometry)), 2))
# 
# #extract count based on index
# for (i in 1:nrow(road2020_se)) {
#   road2020_se$c1Count[i] =road2020_se2[road2020_se$cidx[i],]$Count
# }
# 
# 
# for (i in 1:nrow(road2020_se)) {
#   road2020_se$c2Count[i] =road2020_se2[road2020_se$cidx2[i],]$Count
# }
# 
# road2020_se <- road2020_se %>%
#   mutate(C2 = c1Count + c2Count)

names(road2020_se)
reg <- lm(tripAugFir2 ~ bikeLaneLv + 
            dist.lane + 
            boro_code + 
            Number_Tra + 
            trailOrNot, 
          data=road2020_se)
summary(reg)

