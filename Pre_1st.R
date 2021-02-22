#Set Up
library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(RSocrata)
library(mapview)
library(httr)
library(rgdal)


##===========Import data===================#
#NYC boundary
setwd("C:\\Xintian\\Upenn_Xintian\\1Courses\\MUSA_801_repo\\practicum-nycdot\\DATA\\NYS_Civil_Boundaries_SHP")
NYC <- st_read("Cities_Towns.shp") %>% filter(NAME == "New York") %>% st_transform(crs = 4326)
NYC_LINE <- st_cast(NYC,"MULTILINESTRING")

setwd("C:\\Users\\xinti\\Box\\MUSA_800_Practicum\\Data")
July2018 <- st_read("BikeTrips/rideData_cleaned/July2018/July18_cleaned.shp")
July2019 <- st_read("BikeTrips/rideData_cleaned/July2019/July19_cleaned.shp")

#neighborhood
chelsea <- st_read("OtherData/Chelsea_for7thAve/Chealse.shp") %>% st_transform(st_crs(July2018))
eastHarlem <- st_read("OtherData/East_Harlem_CD/East_Harlem_CD.shp")%>% st_transform(st_crs(July2018))

#15 ft road buffer
road15 <- st_read("OtherData/roadBuffer/lion_buffer15/lion_buffer15.shp") %>% st_transform(crs = 4326)
#15 ft road buffer within the neighborhood boundary
road15_chel <- road15[chelsea,]
road15_har <- road15[eastHarlem,]


##===================Clean Data=======================##
##--------------Clip Data to the study region-----------------##
chelsea_LINE <- st_cast(chelsea,"MULTILINESTRING")
eastHarlem_LINE <- st_cast(eastHarlem,"MULTILINESTRING")
clipToBoundary <- function(outline,polygon,trip){
  outliers <- trip[outline,]
  clippedData <- trip[polygon,] %>% filter(!(id %in% outliers$id))
  return(clippedData)
}

chel_07_2018 <- clipToBoundary(chelsea_LINE,chelsea,July2018) %>% st_transform(crs=3857)
chel_07_2019 <- clipToBoundary(chelsea_LINE,chelsea,July2019) %>% st_transform(crs=3857)
Har_07_2018 <- clipToBoundary(eastHarlem_LINE,eastHarlem,July2018) %>% st_transform(crs=3857)
Har_07_2019 <- clipToBoundary(eastHarlem_LINE,eastHarlem,July2019) %>% st_transform(crs=3857)

# visualize to check data
ggplot()+
  geom_sf(data=pnts_chel_1907) +
  geom_sf(data=chelsea,col ="red",fill=NA)

ggplot()+
  geom_sf(data=road15_har)+
  geom_sf(data=eastHarlem_LINE,col="green")

st_write(chel_07_2018,"chelsea_1807.shp")
st_write(chel_07_2019,"chelsea_1907.shp")
st_write(Har_07_2018,"harlem_1807.shp")
st_write(Har_07_2019,"harlem_1907.shp")


##===================Linestring to point=======================##
##--------------This step in arcgis-----------------##
## in R, the original trip id for each pnt will not be saved so we use ArcGIS
# pnts_chel_1807 <- st_line_sample(st_transform(chel_07_2018,3857),density = 1/150)
# pnts_chel_1907 <- st_line_sample(st_transform(chel_07_2019,3857),density = 1/150)

##===================point to line=======================##
# read in pnt data
pnts_chel_1807 <- st_read("BikeTrips/pointData/pnts_chel_1807.shp") %>% st_transform(crs=4326)
pnts_chel_1907 <- st_read("BikeTrips/pointData/pnts_chel_1907.shp") %>% st_transform(crs=4326)
pnts_har_1807 <- st_read
("BikeTrips/pointData/pnts_har_1807.shp") %>% st_transform(crs=4326)
pnts_har_1907 <- st_read("BikeTrips/pointData/pnts_har_1907.shp") %>% st_transform(crs=4326)  
  
# clean data: pnts within 15ft buffer, add a column for pnt id
clnp_chel_18 <- pnts_chel_1807[road15_chel,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)
clnp_chel_19 <- pnts_chel_1907[road15_chel,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)
clnp_har_18 <- pnts_har_1807[road15_har,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)
clnp_har_19 <- pnts_har_1907[road15_har,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)

# spatial join
clnp_chel_18 <- clnp_chel_18 %>% st_join(.,road15_chel,join = st_intersects, left = T)
clnp_chel_19 <- clnp_chel_19 %>% st_join(.,road15_chel,join = st_intersects, left = T)
clnp_har_18 <- clnp_har_18 %>% st_join(.,road15_har,join = st_intersects,left = T)
clnp_har_19 <- clnp_har_19 %>% st_join(.,road15_har,join = st_intersects,left = T)


##===================Count trip by road=======================##
# Count trip on each road
countTrip <- function(pnt){
  tripCount <- pnt %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    group_by(id_1,Street) %>% 
    summarise(pntCount = n()) %>% 
    group_by(Street) %>% 
    summarise(tripCount = n())
  return(tripCount)
}
 

trip_chel_18 <- countTrip(clnp_chel_18)
trip_chel_19 <- countTrip(clnp_chel_19) 
trip_har_18 <- countTrip(clnp_har_18)
trip_har_19 <- countTrip(clnp_har_19)

# add column in the road
addTripToRoad = function(roadBuffer, tripCount18,tripCount19,byName){
  roadBuffer <- merge(roadBuffer,tripCount18,by=byName,all.x = T) %>% 
    rename(trip18=tripCount) %>% 
    merge(.,tripCount19,by=byName,all.x = T) %>% 
    rename(trip19 = tripCount)
  return(roadBuffer)
}

road15_chel <- addTripToRoad(road15_chel,trip_chel_18,trip_chel_19,"Street")
road15_har <- addTripToRoad(road15_har,trip_har_18,trip_har_19,"Street")

##===================Visualizations and Exploratory Analysis=======================##
# bike ridership by hour by day
grid.arrange(
  ggplot(chel_07_2018 %>% 
           st_drop_geometry() %>%
           mutate(time = ymd_hms(rcrd_tm),
                  interval60 = floor_date(time,unit = "hour")) %>% 
           select(-time) %>% group_by(interval60) %>% tally() %>% 
           mutate(year = 2018))+geom_line(aes(x=interval60,y=n))+
    labs(title = "Ridership pr hour pr day, Chelsea, July,2018")+plotTheme,
  ggplot(chel_07_2019 %>% 
           st_drop_geometry() %>%
           mutate(time = ymd_hms(rcrd_tm),
                  interval60 = floor_date(time,unit = "hour")) %>% 
           select(-time) %>% group_by(interval60) %>% tally() %>% 
           mutate(year = 2019))+geom_line(aes(x=interval60,y=n))+
    labs(title = "Ridership pr hour pr day, Chelsea, July,2019")+plotTheme,
  ggplot(Har_07_2018 %>% 
           st_drop_geometry() %>%
           mutate(time = ymd_hms(rcrd_tm),
                  interval60 = floor_date(time,unit = "hour")) %>% 
           select(-time) %>% group_by(interval60) %>% tally() %>% 
           mutate(year = 2018))+geom_line(aes(x=interval60,y=n))+
    labs(title = "Ridership pr hour pr day, East Harlem, July,2018")+plotTheme,
  ggplot(Har_07_2019 %>% 
           st_drop_geometry() %>%
           mutate(time = ymd_hms(rcrd_tm),
                  interval60 = floor_date(time,unit = "hour")) %>% 
           select(-time) %>% group_by(interval60) %>% tally() %>% 
           mutate(year = 2019))+geom_line(aes(x=interval60,y=n))+
    labs(title = "Ridership pr hour pr day, East Harlem, July,2019")+plotTheme
)

# chel_07_2018 <- chel_07_2018 %>% 
#   mutate(time = ymd_hms(rcrd_tm),
#          interval60 = floor_date(time,unit = "hour"))
# 

# bike ridership by hour 
ggplot(rbind(
  chel_07_2018 %>%
         st_drop_geometry() %>%
         group_by(rhour) %>% tally() %>%
    mutate(year = "2018",
           neighborhood = "Chelsea",
           gr = paste(year,neighborhood,sep = "_")),
  chel_07_2019 %>%
    st_drop_geometry() %>%
    group_by(rhour) %>% tally() %>%
    mutate(year = "2019",
           neighborhood = "Chelsea",
           gr = paste(year,neighborhood,sep = "_")),
  Har_07_2018 %>%
    st_drop_geometry() %>%
    group_by(rhour) %>% tally() %>%
    mutate(year = "2018",
           neighborhood = "East Harlem",
           gr = paste(year,neighborhood,sep = "_")),
  Har_07_2019 %>%
    st_drop_geometry() %>%
    group_by(rhour) %>% tally() %>%
    mutate(year = "2019",
           neighborhood = "East Harlem",
           gr = paste(year,neighborhood,sep = "_"))))+
    geom_line(aes(x=rhour,y = n, group = gr,color = gr,binwidth = 1))+plotTheme
  
# bike ridership by day of week
ggplot(rbind(
  chel_07_2018 %>%
    st_drop_geometry() %>%
    mutate(dotw = wday(rcrd_tm,label = T)) %>% 
    group_by(dotw) %>% tally() %>%
    mutate(year = "2018",
           neighborhood = "Chelsea",
           gr = paste(year,neighborhood,sep = "_")),
  chel_07_2019 %>%
    st_drop_geometry() %>%
    mutate(dotw = wday(rcrd_tm,label = T)) %>% 
    group_by(dotw) %>% tally() %>%
    mutate(year = "2019",
           neighborhood = "Chelsea",
           gr = paste(year,neighborhood,sep = "_")),
  Har_07_2018 %>%
    st_drop_geometry() %>%
    mutate(dotw = wday(rcrd_tm,label = T)) %>% 
    group_by(dotw) %>% tally() %>%
    mutate(year = "2018",
           neighborhood = "East Harlem",
           gr = paste(year,neighborhood,sep = "_")),
  Har_07_2019 %>%
    st_drop_geometry() %>%
    mutate(dotw = wday(rcrd_tm,label = T)) %>% 
    group_by(dotw) %>% tally() %>%
    mutate(year = "2019",
           neighborhood = "East Harlem",
           gr = paste(year,neighborhood,sep = "_"))))+
  geom_line(aes(x=dotw,y = n, group = gr,color = gr,binwidth = 1))+plotTheme



# bike trip in 2018.2019, Chelsea and East Harlem
chel18 <- road15_chel %>% 
  select(Street,trip18,geometry) %>% 
  mutate(Year = 2018) %>% 
  rename(trip=trip18)

chel19 <- road15_chel %>% 
  select(Street,trip19,geometry) %>% 
  mutate(Year = 2019) %>% 
  rename(trip=trip19)

har18 <- road15_har %>% 
  select(Street,trip18,geometry) %>% 
  mutate(Year = 2018) %>% 
  rename(trip=trip18)

har19 <- road15_har %>% 
  select(Street,trip19,geometry) %>% 
  mutate(Year = 2019) %>% 
  rename(trip=trip19)

har_Diff1819 <- road15_har %>% 
  select(Street,trip18,trip19,geometry) %>% 
  mutate(tripDiff = trip19-trip18) %>% 
  select(-trip18,-trip19)

har_Diff1819 <- road15_har %>% 
  select(Street,trip18,trip19,geometry) %>% 
  mutate(tripDiff = trip19-trip18) %>% 
  select(-trip18,-trip19)

har_Diff1819 <- road15_har %>% 
  select(Street,trip18,trip19,geometry) %>% 
  mutate(tripDiff = trip19-trip18,
         Neighborhood = "East Harlem") %>% 
  select(-trip18,-trip19) 

chel_Diff1819 <- road15_chel %>% 
  select(Street,trip18,trip19,geometry) %>% 
  mutate(tripDiff = trip19-trip18,
         Neighborhood = "Chelsea") %>% 
  select(-trip18,-trip19) 

#Chelsea
ggplot() + 
  geom_sf(data = chelsea,fill = "#f7f5f5",col = NA)+
  geom_sf(data = rbind(chel18,chel19),aes(color = trip))+
  scale_color_viridis(direction = 1,
                      discrete = F, option = "viridis",
                      na.value = "#D4D4D4")+
  labs(title = "Trip Count per road, Chelsea, July, 2018") +
  facet_wrap(~Year)+
  mapTheme()

#East Harlem
ggplot() + 
  geom_sf(data = eastHarlem,fill = "#f7f5f5",col = NA)+
  geom_sf(data = rbind(har18,har19),aes(color = trip))+
  scale_color_viridis(direction = 1,
                      discrete = F, option = "viridis",
                      na.value = "#D4D4D4")+
  labs(title = "Trip Count per road, East Harlem, July, 2018") +
  facet_wrap(~Year)+
  mapTheme()


# Diff in two years
ggplot()+
  geom_sf(data = chelsea,fill = "#f7f5f5",col = NA)+
  geom_sf(data = chel_Diff1819,aes(color = tripDiff))+
  scale_color_viridis(direction = 1,
                      discrete = F, option = "viridis",
                      na.value = "#D4D4D4")+
  labs(title = "Trip Count Difference between 2018 & 2019, Chelsea") +
  mapTheme()

ggplot()+
  geom_sf(data = eastHarlem,fill = "#f7f5f5",col = NA)+
  geom_sf(data = har_Diff1819,aes(color = tripDiff))+
  scale_color_viridis(direction = 1,
                      discrete = F, option = "viridis",
                      na.value = "#D4D4D4")+
  labs(title = "Trip Count Difference between 2018 & 2019, East Harlem") +
  mapTheme()


