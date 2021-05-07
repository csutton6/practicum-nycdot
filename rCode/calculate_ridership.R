setwd('C:/Users/katee/Box Sync/Practicum/shp/')


library(sf)
library(tidyverse)
library(mapview)

#read in data
points_new<-st_read('Aug1_14_points.shp')

extent <- st_read("station_extent_Aug2020.shp") %>%
  st_transform(st_crs(points_new))

cd<-st_read('https://data.cityofnewyork.us/resource/jp9i-3b7y.geojson') %>%
  st_transform(st_crs(points_new))

cd_extent<-st_intersection(cd, extent)


#clip points to study area extent
points_new_extent<-st_intersection(points_new, extent)

#bring in roads
bike20d_buffer <- st_read("lion2020d_buffer15.shp") %>%
  st_transform(st_crs(points_new))

# filter out non-bikeable LION segments (e.g. highways and ferry routes)
bike20d_buffer<- subset(bike20d_buffer, RW_TYPE != 12 &
                RW_TYPE != 7 &
                RW_TYPE != 14 &
                RW_TYPE != 2 &
                RW_TYPE != 4 &
                FeatureTyp != 1 &
                FeatureTyp != 2 &
                FeatureTyp != 3 &
                FeatureTyp != 5 &
                FeatureTyp != 7 &
                FeatureTyp != 8 &
                FeatureTyp != 9 &
                SegmentTyp !='E'&
                SegmentTyp !='G'&
                SegmentTyp !='F'&
                SegmentTyp !='T')

#segment ID is not unique, so select distinct segment IDs
bike20d_buffer <- distinct(bike20d_buffer,SegmentID,.keep_all = T)

#clip street segments to study area
bike20d_buffer<- st_intersection(bike20d_buffer, extent)

save.image(file = "pointsClean_raw.RData")


#function that cleans points for one community district
cleaner<- function(cb){
  cd_chosen<- cd %>%
    filter(boro_cd==cb)
  streets <- st_intersection(bike20d_buffer, cd_chosen)
  points_extent <- st_intersection(points_new_extent, cd_chosen)
  points_clean <- points_extent[streets,]%>%
    mutate(pnt_id = seq.int(nrow(.))) %>%
    st_join(., streets, join=st_intersects, left=T, largest=TRUE)
  return(points_clean)
}

#run the function for each cd
#Manhattan
points_clean_101<- cleaner('101')
points_clean_102<- cleaner('102')
points_clean_103<- cleaner('103')
points_clean_104<- cleaner('104')
points_clean_105<- cleaner('105')
points_clean_106<- cleaner('106')
points_clean_108<- cleaner('108')
points_clean_164<- cleaner('164')
points_clean_107<- cleaner('107')
points_clean_111<- cleaner('111')
points_clean_110<- cleaner('110')
points_clean_109<- cleaner('109')
points_clean_112<- cleaner('112')

save.image(file = "pointsClean_Manhattan.RData")

points_clean_201<- cleaner('201')
points_clean_202<- cleaner('202')
points_clean_203<- cleaner('203')
points_clean_204<- cleaner('204')

save.image(file = "pointsClean_MHBX.RData")


points_clean_301<- cleaner('301')
points_clean_302<- cleaner('302')
points_clean_303<- cleaner('303')
points_clean_304<- cleaner('304')
points_clean_306<- cleaner('306')
points_clean_308<- cleaner('308')
points_clean_307<- cleaner('307')
points_clean_355<- cleaner('355')
points_clean_309<- cleaner('309')
points_clean_316<- cleaner('316')

save.image(file = "pointsClean_MHBXBK.RData")

points_clean_405<- cleaner('405')
points_clean_402<- cleaner('402')
points_clean_401<- cleaner('401')

save.image(file = "pointsClean_MHBXBKQN.RData")


#function for counting trips
countTrip <- function(pnt){
  tripCount <- pnt %>%
    as.data.frame() %>%
    select(-geometry) %>%
    group_by(FID_1,SegmentID) %>%
    summarise() %>%
    group_by(SegmentID) %>%
    summarise(tripCount = n())
  return(tripCount)
}

#count trips in the previous dataframe
count_101 <- countTrip(points_clean_101)
count_102 <- countTrip(points_clean_102)
count_103 <- countTrip(points_clean_103)
count_104 <- countTrip(points_clean_104)
count_105 <- countTrip(points_clean_105)
count_106 <- countTrip(points_clean_106)


count_107 <- countTrip(points_clean_107)
count_108 <- countTrip(points_clean_108)
count_109 <- countTrip(points_clean_109)
count_110 <- countTrip(points_clean_110)
count_111 <- countTrip(points_clean_111)
count_112 <- countTrip(points_clean_112)
count_164 <- countTrip(points_clean_164)

count_301 <- countTrip(points_clean_301)
count_302 <- countTrip(points_clean_302)
count_303 <- countTrip(points_clean_303)
count_304 <- countTrip(points_clean_304)
count_306 <- countTrip(points_clean_306)
count_307 <- countTrip(points_clean_307)
count_308 <- countTrip(points_clean_308)
count_309 <- countTrip(points_clean_309)
count_316 <- countTrip(points_clean_316)
count_355 <- countTrip(points_clean_355)


count_201 <- countTrip(points_clean_201)
count_202 <- countTrip(points_clean_202)
count_203 <- countTrip(points_clean_203)
count_204 <- countTrip(points_clean_204)


count_401 <- countTrip(points_clean_401)
count_402 <- countTrip(points_clean_402)
count_405 <- countTrip(points_clean_405)

save.image(file = "pointsCounted.RData")


#join the counted trips to the road data
#Manhattan
new_info.Aug <- lion %>% 
  merge(.,new_tripCount,by = "SegmentID",all.x = T) %>% 
  rename(Count=tripCount)

info.Aug_101<- bike20d_buffer %>% 
  merge(., count_101, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)

info.Aug_102<- bike20d_buffer %>% 
  merge(., count_102, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)

info.Aug_103<- bike20d_buffer %>% 
  merge(., count_103, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_104<- bike20d_buffer %>% 
  merge(., count_104, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_105<- bike20d_buffer %>% 
  merge(., count_105, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_106<- bike20d_buffer %>% 
  merge(., count_106, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_107<- bike20d_buffer %>% 
  merge(., count_107, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_108<- bike20d_buffer %>% 
  merge(., count_108, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_109<- bike20d_buffer %>% 
  merge(., count_109, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_110<- bike20d_buffer %>% 
  merge(., count_110, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_111<- bike20d_buffer %>% 
  merge(., count_111, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_112<- bike20d_buffer %>% 
  merge(., count_112, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_164<- bike20d_buffer %>% 
  merge(., count_164, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)

#bronx
info.Aug_201<- bike20d_buffer %>% 
  merge(., count_201, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_202<- bike20d_buffer %>% 
  merge(., count_202, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_203<- bike20d_buffer %>% 
  merge(., count_203, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_204<- bike20d_buffer %>% 
  merge(., count_204, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)

#brooklyn
info.Aug_301<- bike20d_buffer %>% 
  merge(., count_301, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_302<- bike20d_buffer %>% 
  merge(., count_302, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_303<- bike20d_buffer %>% 
  merge(., count_303, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_304<- bike20d_buffer %>% 
  merge(., count_304, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_306<- bike20d_buffer %>% 
  merge(., count_306, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_307<- bike20d_buffer %>% 
  merge(., count_307, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_308<- bike20d_buffer %>% 
  merge(., count_308, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_309<- bike20d_buffer %>% 
  merge(., count_309, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_316<- bike20d_buffer %>% 
  merge(., count_316, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_355<- bike20d_buffer %>% 
  merge(., count_355, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)

#queens
info.Aug_401<- bike20d_buffer %>% 
  merge(., count_401, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_402<- bike20d_buffer %>% 
  merge(., count_402, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)
info.Aug_405<- bike20d_buffer %>% 
  merge(., count_405, by='SegmentID', all.y=T)%>%
  rename(Count=tripCount)

#rbind everything back together

all_test<- rbind(info.Aug_101, info.Aug_102, info.Aug_103, info.Aug_104, info.Aug_105,
                 info.Aug_106, info.Aug_107, info.Aug_108, info.Aug_109, info.Aug_110,
                 info.Aug_111, info.Aug_112, info.Aug_164, info.Aug_201, info.Aug_202, 
                 info.Aug_203, info.Aug_204, info.Aug_301, info.Aug_302, info.Aug_303,
                 info.Aug_304, info.Aug_306, info.Aug_307, info.Aug_308, info.Aug_309,
                 info.Aug_316, info.Aug_355, info.Aug_401, info.Aug_402, info.Aug_405)

glimpse(all_test)

#group by street and segment id and sum ridership (some streets are split by community districts)
all_grouped<-all_test%>%
  group_by(Street, SegmentID)%>%
  summarize(Count=sum(Count))
all_grouped<-st_set_geometry(all_grouped, NULL)


#join it back to the original bike20d_buffer or somehow bring back the segments with count0
mergeCols <- c("Street", "SegmentID")
ridership<- bike20d_buffer%>%
  merge(., all_grouped, by=mergeCols, all.x=T)
ridership$Count[is.na(ridership$Count)] <- 0

#export as RData
save.image(file = "ridership.RData")
