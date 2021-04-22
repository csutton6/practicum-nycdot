setwd('C:/Users/katee/Box Sync/Practicum/shp/')
setwd('C:/Users/xinti/Box/MUSA_800_Practicum/Data')

#avoid scientific notation
options(scipen = 999)

#### packages ####
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)
library(viridis)
library(ggplot2)
library(kableExtra)
library(stargazer)
library(FNN)
library(ggcorrplot)
library(spdep)
library(plotly)
library(car)
library(ranger)
library(tidycensus)
library(corpcor)
library(caret)
library(rjson)
library(tidyr)
library(sp)
library(osmdata)

#### palettes etc ####
palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

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

####====Import and Clean Data====####

#import trip data
Aug_pnts <- st_read("tripPoints_Aug20.shp")
Aug_pnts <- st_read("BikeTrips/pointData/Aug20_pnts/tripPoints_Aug20.shp") %>% st_transform(crs = 4326)

#filter out dates in 1-15
Aug_pnts <- Aug_pnts %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)))

Aug_pnts <- Aug_pnts %>%
  filter(day <= 15)

#NYC boroughs
borough<- st_read('https://data.cityofnewyork.us/resource/7t3b-ywvw.geojson')%>%
  st_transform(st_crs(Aug_pnts))

boros_4<-subset(borough, boro_code<5)

#citibike stations
station<- st_read('Stations_Aug_2020.shp')%>%
  st_transform(st_crs(Aug_pnts))

station<- st_read('Infrastructure/Citibike_Stations/Stations_Aug_2020.shp')%>%
  st_transform(st_crs(Aug_pnts))

#Station Extent
extent <- st_read("station_extent_Aug2020.shp") %>%
  st_transform(st_crs(Aug_pnts))

extent <- st_read('OtherData/Station_Extent_2020/station_extent_Aug2020.shp')%>%
  st_transform(st_crs(Aug_pnts))


#find out pnts inside citibike extent
Aug_pnts <- Aug_pnts[extent,]


#neighborhoods
neighborhood <- st_read('https://data.cityofnewyork.us/resource/q2z5-ai38.geojson')%>%
  st_transform(st_crs(Aug_pnts))


#only neighborhood in citibike extent
neighborhood <- neighborhood[extent,]


##=====Roads and Bikelanes====##
#import Lion bikelane data
bike20d_buffer <- st_read("lion2020d_buffer15.shp") %>%
  st_transform(st_crs(Aug_pnts))
bike20d_buffer <- st_read("Infrastructure/Lion/lion2020d_buffer15.shp") %>%
  st_transform(st_crs(Aug_pnts))


#clip roads to citibike extent
bike20d_buffer<- bike20d_buffer[extent,]

#get rid of non-road data (eg shorelines and boundaries)
bike20d_buffer_drop<- subset(bike20d_buffer, RW_TYPE !=12 &
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


#add bike lane level
bike20d_buffer_drop <- bike20d_buffer_drop %>% 
  mutate(
    bikeLaneLv = case_when(
      BikeLane == 1 | BikeLane == 5 | BikeLane == 8 | BikeLane == 9 | BikeLane == 10 ~ "Protected",
      BikeLane == 2 | BikeLane == 3 | BikeLane == 4 | BikeLane == 6 | BikeLane == 11 ~ "Unprotected",
      TRUE ~ "noBikeLane"
    ))

bike20d_buffer<- bike20d_buffer_drop
bike20d_buffer <- distinct(bike20d_buffer,SegmentID,.keep_all = T)

#select useful columns
bike20d_buffer <- bike20d_buffer %>%
  select(Street, BikeLane, SegmentID, FeatureTyp, SegmentTyp, NonPed,
         TrafDir, SegCount, LBoro, XFrom, YFrom, RW_TYPE, Snow_Prior,
         Number_Tra, Number_Par, Number_Tot, BIKE_TRAFD, 
         StreetWidt, StreetWi_1, StreetWi_2, TRUCK_ROUT, POSTED_SPE,  
         bikeLaneLv, geometry)


#### Calculate Ridership ####
# clip points to the road buffers
bike20d_buffer<- st_transform(bike20d_buffer, crs=4326)

points_clean <- Aug_pnts[bike20d_buffer,] %>% 
  mutate(pnt_id = seq.int(nrow(.))) %>% 
  st_transform(crs = 4326) %>% 
  st_join(.,bike20d_buffer,join = st_intersects, left = T)


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

tripCount <- countTrip(points_clean)

info.Aug <- bike20d_buffer %>% 
  merge(.,tripCount,by = "SegmentID",all.x = T) %>% 
  rename(Count=tripCount) 

#set NA Count equal to zero
info.Aug$Count[is.na(info.Aug$Count)] <- 0


# collect bike lane information
bikelanes <- bike20d_buffer %>% filter(bikeLaneLv!="noBikeLane")


####=====Feature Engineering========####
#change projection
info.Aug <- info.Aug %>% st_transform('ESRI:102711')
bike20d_buffer <- bike20d_buffer %>% st_transform('ESRI:102711')
bikelanes <- bikelanes %>% st_transform('ESRI:102711')
station <- station %>% st_transform('ESRI:102711')


###Distance to nearest bikelanes
info.Aug <- info.Aug %>%
  mutate(dist.lane = nn_function(st_coordinates(st_centroid(info.Aug$geometry)), st_coordinates(st_centroid(bikelanes$geometry)), 1))



#borough
boxplot(Count ~ LBoro,
        data=info.Aug)

info.Aug$isMH<-0
info.Aug$isMH[info.Aug$LBoro==1]<-1

#travel lanes
info.Aug$Number_Tra <- as.numeric(info.Aug$Number_Tra)


#snow route
info.Aug$MinorSnowRoute<-0
info.Aug$MinorSnowRoute[info.Aug$Snow_Prior=='S']<-1

#speed limit
info.Aug$POSTED_SPE<-as.numeric(info.Aug$POSTED_SPE)

#citibike stations
#drop all columns but geometry
station<-station%>%
  select()

info.Aug$citibike.Buffer_small =
  st_buffer(info.Aug, 250) %>% 
  aggregate(mutate(station, counter = 1),., sum) %>%
  pull(counter)
info.Aug$citibike.Buffer_small[is.na(info.Aug$citibike.Buffer_small)] <- 0

#edge effect (not working right now)
extent.point <- st_cast(extent,"LINESTRING") 

extent.point<- st_cast(extent, 'POINT')

mapview(extent.point)

info.Aug$dist.edge_2<- st_cast(extent, 'LINESTRING')%>% st_distance(y=info.Aug$geometry)

info.Aug <- info.Aug %>%
  mutate(dist.edge = nn_function((st_coordinates(st_centroid(info.Aug))),
                                 st_coordinates(extent.point), 1))

#fixNAs
#find averages
median_lanes <- median(info.Aug$Number_Tra, na.rm=TRUE)
median_width<- median(info.Aug$StreetWidt, na.rm = TRUE)
median_speed<- median(info.Aug$POSTED_SPE, na.rm=TRUE)


info.Aug$Number_Tra[is.na(info.Aug$Number_Tra)] <- median_lanes
info.Aug$StreetWidt[is.na(info.Aug$StreetWidt)] <- median_width
info.Aug$POSTED_SPE[is.na(info.Aug$POSTED_SPE)] <- median_speed
info.Aug$TRUCK_ROUT[is.na(info.Aug$TRUCK_ROUT)] <- 0


#### Census Features ####
#variables available at: https://api.census.gov/data/2019/acs/acs5/variables.html
v19 <- load_variables(2019, "acs5", cache = TRUE)

census_df <- data.frame(vars = c("B01003_001E",
                                 'B02001_002E',
                                 'B01001A_002E',
                                 'B01001A_017E',
                                 'B01002_001E',
                                 'B19013_001E',
                                 'B25031_001E',
                                 'B25044_001E',
                                 'B08006_003E',
                                 'B08006_010E',
                                 'B08131_001E',
                                 'B08141_002E',
                                 'B08141_001E',
                                 'B08303_001E',
                                 'B03001_003E',
                                 'B26001_001E',
                                 'B25007_012E',
                                 'B08201_001E',
                                 'B25002_001E',
                                 'B25002_003E',
                                 'B08006_014E'),
                        colNames2 = c('TotPop',
                                      'WhitePop',
                                      'TotMale',
                                      'TotFemale',
                                      'MedianAge',
                                      'MedHHInc',
                                      'MedRent',
                                      'Vehicles_Avail',
                                      'Commute_DriveAlone',
                                      'Commute_Subway',
                                      'Travel_Time',
                                      'No_Vehicle',
                                      'Means_of_Transport_pop',
                                      'Travel_Time_pop',
                                      'LatinoPop',
                                      'GroupQuarters',
                                      'NumRenters',
                                      'AvgHHSize',
                                      'TotUnits',
                                      'VacUnits',
                                      'Commute_Bike'),
                        stringsAsFactors = FALSE)

census_vars <- census_df$vars
census_colNames <- census_df$colNames

# Function for renaming columns after collecting census data
rename_census_cols <- function(x){
  
  output <- x %>% 
    rename_at(vars(census_vars), 
              ~ census_colNames)
  
  output
}

#set census api key
census_key <- 'a7e8cf22d61cfaeca8e855304a07e8f35b139d06'
census_api_key(census_key, overwrite = TRUE, install = TRUE)

# Collect census data and geometries
Census_raw <- get_acs(geography = "tract", 
                      variables = census_vars, 
                      year = 2019, 
                      state = "NY", 
                      geometry = TRUE, 
                      county = c("New York", "Kings", "Queens", "Bronx"),
                      output = "wide") %>%
  rename_census_cols %>%
  dplyr::select(GEOID, 
                geometry,
                census_colNames) %>% 
  st_transform(2263)

Census_raw <- Census_raw%>%
  st_transform(2263)%>%
  mutate(AREA=st_area(geometry))
Census_raw <- Census_raw%>%
  st_transform(2263)%>%
  mutate(sq_mi=AREA/27878400)

#fix NAs 
colSums(is.na(Census_raw))

median_age<-median(Census_raw$MedianAge, na.rm = TRUE)
median_rent<-median(Census_raw$MedRent, na.rm=TRUE)

Census_raw$MedianAge[is.na(Census_raw$MedianAge)] <- median_age
Census_raw$MedRent[is.na(Census_raw$MedRent)] <- median_rent


# calculate some vars
Census <- Census_raw %>% 
  st_transform(2263) %>% 
  mutate(pWhite = WhitePop / TotPop,
         PopDens = TotPop/sq_mi,
         Mean_Commute_Time = Travel_Time / Travel_Time_pop,
         pSubway = Commute_Subway / Means_of_Transport_pop,
         pDrive = Commute_DriveAlone/Means_of_Transport_pop,
         pBike = Commute_Bike/Means_of_Transport_pop,
         pNoVeh = No_Vehicle / TotPop,
         pLatino = LatinoPop / TotPop,
         pQuarters = GroupQuarters / TotPop,
         pRenters = NumRenters / TotPop,
         pVacant = VacUnits / TotUnits)

#fix weird values in calculated vars
#set infinity values to NA
Census_geo<-Census%>%
  select(GEOID)

Census_st<- st_set_geometry(Census, NULL)
Census_fix <- do.call(data.frame, lapply(Census_st, function(x) replace(x, is.infinite(x), NA)))

#fix NAs in calculated vars
colSums(is.na(Census_fix))

median_pWhite<-median(Census_fix$pWhite, na.rm=TRUE)
median_pLatino<-median(Census_fix$pLatino, na.rm=TRUE)
median_commute<-median(Census_fix$Mean_Commute_Time, na.rm = TRUE)
median_subway<-median(Census_fix$pSubway, na.rm=TRUE)
median_noveh<-median(Census_fix$pNoVeh, na.rm=TRUE)
median_drive<-median(Census_fix$pDrive, na.rm=TRUE)
median_quarters<-median(Census_fix$pQuarters, na.rm=TRUE)
median_renters<-median(Census_fix$pRenters, na.rm=TRUE)
median_vacant<-median(Census_fix$pVacant, na.rm=TRUE)
median_bike<-median(Census_fix$pBike, na.rm=TRUE)


Census_fix$pWhite[is.na(Census_fix$pWhite)] <- median_pWhite
Census_fix$pLatino[is.na(Census_fix$pLatino)] <- median_pLatino
Census_fix$Mean_Commute_Time[is.na(Census_fix$Mean_Commute_Time)] <- median_commute
Census_fix$pSubway[is.na(Census_fix$pSubway)] <- median_subway
Census_fix$pNoVeh[is.na(Census_fix$pNoVeh)] <- median_noveh
Census_fix$pDrive[is.na(Census_fix$pDrive)] <- median_drive
Census_fix$pQuarters[is.na(Census_fix$pQuarters)] <- median_quarters
Census_fix$pRenters[is.na(Census_fix$pRenters)] <- median_renters
Census_fix$pVacant[is.na(Census_fix$pVacant)] <- median_vacant


#if % greater than 1, set value equal to median
Census_fix$pSubway[Census_fix$pSubway>1.1] <- median_subway

#get geometry back
Census<- inner_join(Census_geo, Census_fix)

#select the variables of interest
Census <- Census %>%
  select(GEOID, sq_mi, TotPop, PopDens, MedianAge, MedHHInc, AvgHHSize,
                pWhite, pLatino, MedRent, Mean_Commute_Time, pSubway, pDrive, pBike, pNoVeh,
                pQuarters, pRenters, pVacant)



#join with info.Aug
Census<-st_transform(Census, crs=st_crs(info.Aug))
info.Aug_census<-st_join(st_centroid(info.Aug), left=TRUE, Census)


#fill NAs for the 16 records that didn't join to a census tract
info.Aug_census$PopDens[is.na(info.Aug_census$PopDens)] <- 0
info.Aug_census$MedianAge[is.na(info.Aug_census$MedianAge)] <- 0
info.Aug_census$MedHHInc[is.na(info.Aug_census$MedHHInc)] <- 0
info.Aug_census$AvgHHSize[is.na(info.Aug_census$AvgHHSize)] <- 0
info.Aug_census$pWhite[is.na(info.Aug_census$pWhite)] <- 0
info.Aug_census$pLatino[is.na(info.Aug_census$pLatino)] <- 0
info.Aug_census$MedRent[is.na(info.Aug_census$MedRent)] <- 0
info.Aug_census$Mean_Commute_Time[is.na(info.Aug_census$Mean_Commute_Time)] <- 0
info.Aug_census$pSubway[is.na(info.Aug_census$pSubway)] <- 0
info.Aug_census$pDrive[is.na(info.Aug_census$pDrive)] <- 0
info.Aug_census$pBike[is.na(info.Aug_census$pBike)] <- 0
info.Aug_census$pNoVeh[is.na(info.Aug_census$pNoVeh)] <- 0
info.Aug_census$pQuarters[is.na(info.Aug_census$pQuarters)] <- 0
info.Aug_census$pRenters[is.na(info.Aug_census$pRenters)] <- 0
info.Aug_census$pVacant[is.na(info.Aug_census$pVacant)] <- 0

info.Aug<-info.Aug_census

#### Environment Features ####

#make new dataset to play with
info.Aug_env<- info.Aug

#sidewalk cafes
sidewalk_cafe<- read.csv('https://data.cityofnewyork.us/resource/qcdj-rwhu.csv?$limit=2000')
sidewalk_cafe<-subset(sidewalk_cafe, lic_status =='Active')
sidewalk_cafe <- st_as_sf(sidewalk_cafe, coords=c('longitude', 'latitude'), crs=4326)


#nn version
sidewalk_cafe<-st_transform(sidewalk_cafe, crs=st_crs(info.Aug))

info.Aug_env<- info.Aug_env %>%
  mutate(
    sidewalk_caf_nn1 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(sidewalk_cafe), 1),
    sidewalk_caf_nn2 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(sidewalk_cafe), 2),
    sidewalk_caf_nn3 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(sidewalk_cafe), 3))



#big parks
parks<- st_read('for_basemap/geo_export_8469ffba-1951-4e52-916c-c9c4dfa54c18.shp')
big_parks<-subset(parks, acres>100)

big_parks<-st_transform(big_parks, crs=st_crs(info.Aug_env))

info.Aug_env<- info.Aug_env %>% 
  mutate(bigPark = lengths(st_within(geometry, big_parks)))
info.Aug_env$bigPark[info.Aug_env$bigPark==2]<-1


#greenway
info.Aug_env$isGreenway<-ifelse(grepl('greenway', info.Aug_env$Street, ignore.case=T), 1, 0)

#distance to subway stations
subway<-st_read('stops_nyc_subway_may2019.shp')
subway<-st_transform(subway, st_crs(info.Aug))


info.Aug_env<- info.Aug_env %>%
  mutate(
    subway_nn1 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(subway), 1),
    subway_nn2 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(subway), 2),
    subway_nn3 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(subway), 3),
    subway_nn4 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(subway), 4),
    subway_nn5 = nn_function(st_coordinates(st_centroid(info.Aug_env)), st_coordinates(subway), 5))


#reset whole dataset to include these features
info.Aug<-info.Aug_env

# jobs
jobs_mh<-st_read('LODES_WAC/manhattan/points_2018.shp')
jobs_bx<-st_read('LODES_WAC/bronx/points_2018.shp')
jobs_bk<-st_read('LODES_WAC/brooklyn/points_2018.shp')
jobs_qn<-st_read('LODES_WAC/queens/points_2018.shp')

jobs<-rbind(jobs_mh, jobs_bx, jobs_bk, jobs_qn)

WAC <- jobs %>% 
  dplyr::select(id, c000) %>% 
  mutate(GEOID = as.character(substr(id, 1, 11))) %>% 
  group_by(GEOID) %>% 
  summarize(jobs_in_tract = sum(c000, na.rm = TRUE)) %>% 
  filter(GEOID %in% info.Aug$GEOID)

WAC<-st_set_geometry(WAC, NULL)

info.Aug_jobs<- left_join(info.Aug, WAC, left=TRUE)

#get rid of NAs
colSums(is.na(info.Aug_jobs))
median_jobs <- median(info.Aug_jobs$jobs_in_tract, na.rm=TRUE)
info.Aug_jobs$jobs_in_tract[is.na(info.Aug_jobs$jobs_in_tract)] <- median_jobs

info.Aug<-info.Aug_jobs

#### OSM Features ####
info.Aug_osm<-info.Aug

#retail
extent<- st_transform(extent, st_crs(info.Aug))

retail <- opq ("New York City USA") %>%
  add_osm_feature(key = 'shop') %>%
  osmdata_sf(.)

retail <- st_geometry(retail$osm_points) %>%
  st_transform(st_crs(info.Aug)) %>%
  st_sf() %>%
  st_intersection(extent) %>%
  mutate(Legend = 'Retail') %>%
  dplyr::select(Legend, geometry)

#office
office <- opq ("New York City USA") %>%
  add_osm_feature(key = 'office') %>%
  osmdata_sf(.)

office <- st_geometry(office$osm_points) %>%
  st_transform(st_crs(info.Aug)) %>%
  st_sf() %>%
  st_intersection(extent) %>%
  mutate(Legend = 'Office') %>%
  dplyr::select(Legend, geometry)

#add nn features
info.Aug_osm<- info.Aug_osm %>%
  mutate(
    office_nn1 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(office), 1),
    office_nn2 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(office), 2),
    office_nn3 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(office), 3),
    office_nn4 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(office), 4),
    office_nn5 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(office), 5))

info.Aug_osm<- info.Aug_osm %>%
  mutate(
    retail_nn1 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(retail), 1),
    retail_nn2 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(retail), 2),
    retail_nn3 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(retail), 3),
    retail_nn4 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(retail), 4),
    retail_nn5 = nn_function(st_coordinates(st_centroid(info.Aug_osm)), st_coordinates(retail), 5))

#add density features
# retail
retail_ct <- st_join(info.Aug_osm, retail) %>%
  group_by(GEOID, sq_mi) %>%
  summarise(count_retail= n())

retail_ct$density_retail <- retail_ct$count_retail/retail_ct$sq_mi

# office
office_ct <- st_join(info.Aug_osm, office) %>%
  group_by(GEOID, sq_mi) %>%
  summarise(count_office= n())

office_ct$density_office <- office_ct$count_office/office_ct$sq_mi

#join back to info.Aug
retail_ct<- retail_ct%>%
  select(GEOID, count_retail, density_retail)%>%
  st_set_geometry(NULL)

office_ct<- office_ct%>%
  select(GEOID, count_office, density_office)%>%
  st_set_geometry(NULL)

info.Aug_retail<- left_join(info.Aug_osm, retail_ct)
info.Aug_office<- left_join(info.Aug_retail, office_ct)

info.Aug<-info.Aug_office

#fix NAs
colSums(is.na(info.Aug))
median_lanes <- median(info.Aug$Number_Tra, na.rm=TRUE)

median_retail<-median(info.Aug$density_retail, na.rm=TRUE)
median_office<-median(info.Aug$density_retail, na.rm=TRUE)

info.Aug$density_retail[is.na(info.Aug$density_retail)] <- median_retail
info.Aug$density_office[is.na(info.Aug$density_office)] <- median_office


#### Correlations ####
#correlation of numeric independent variables against each other
numericVars <- 
  select_if(st_drop_geometry(info.Aug[, -c(1:2, 30)]), is.numeric) %>% 
  na.omit()

corr<-round(cor(numericVars), 1)
p.mat<- cor_pmat(numericVars)

ggcorrplot(corr, hc.order = TRUE, type='lower', insig='blank')

#qualitative vars scatter plot
#all possible variables
st_drop_geometry(info.Aug) %>% 
  dplyr::select(Count, bikeLaneLv, POSTED_SPE, Number_Tra, Number_Tot,
                Snow_Prior, TRUCK_ROUT, RW_TYPE, Number_Par, BIKE_TRAFD,
                FeatureTyp, NonPed,
                StreetWidt, trailOrNot, bridgeOrNot) %>%
  gather(Variable, Value, -Count) %>% 
  ggplot(aes(Value, Count)) +
  geom_point(size = .5, shape=20, alpha = 0.5) + 
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(caption = "Count as a Function Some Variables")

#boxplots
boxplot(Count~MinorSnowRoute,
        data=info.Aug)

boxplot(Count~bikeLaneLv,
        data=info.Aug)

boxplot(Count~POSTED_SPE,
        data=info.Aug)

boxplot(Count~StreetWidt,
        data=info.Aug)

boxplot(Count~Number_Tra,
        data=info.Aug)

boxplot(Count~citibike.Buffer_small, 
        data=info.Aug)


#matrix of cor plots: LION and citibike vars
correlation.long <-
  select(st_drop_geometry(info.Aug), c(Number_Tra, POSTED_SPE, StreetWidt, dist.lane,
                                       citibike.Buffer_small,
                                       Count)) %>%
  gather(Variable, Value, -Count)

correlation.cor <-
  correlation.long %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, Count, use = "complete.obs"))


ggplot(correlation.long, aes(Value, Count)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Count of trips as a function of LION and citibike features")

#matrix of cor plots: census variables
colSums(is.na(info.Aug_census))

correlation.long_c <-
  select(st_drop_geometry(info.Aug_census), c(MedHHInc, AvgHHSize,
                                              pWhite, pLatino, MedRent, Mean_Commute_Time, pSubway, pDrive, pNoVeh, pBike,
                                              Count)) %>%
  gather(Variable, Value, -Count)

correlation.cor_c <-
  correlation.long_c %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, Count, use = "complete.obs"))


ggplot(correlation.long_c, aes(Value, Count)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor_c, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1, color='blue') +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Count of trips as a function of census features")

#matrix of corplots: environment vars
correlation.long_e <-
  select(st_drop_geometry(info.Aug), c(sidewalk_caf_nn1, sidewalk_caf_nn2, sidewalk_caf_nn3,
                                              sidewalk_caf.Buffer,
                                           jobs_in_tract, 
                                       subway_nn1, subway_nn2, subway_nn3, subway_nn4, subway_nn5,
                                              Count)) %>%
  gather(Variable, Value, -Count)

correlation.cor_e <-
  correlation.long_e %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, Count, use = "complete.obs"))


ggplot(correlation.long_e, aes(Value, Count)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor_e, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1, color='blue') +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Count of trips as a function of environmental features")

#matrix of corplots: Open Street Map vars
correlation.long_osm <-
  select(st_drop_geometry(info.Aug), c(retail_nn1, retail_nn2,retail_nn3,retail_nn4,retail_nn5,
                                       office_nn1, office_nn2, office_nn3, office_nn4, office_nn5,
                                       density_retail, density_office,
                                       Count)) %>%
  gather(Variable, Value, -Count)

correlation.cor_osm <-
  correlation.long_osm %>%
  group_by(Variable) %>%
  summarize(correlation = cor(Value, Count, use = "complete.obs"))


ggplot(correlation.long_osm, aes(Value, Count)) +
  geom_point(size = 0.1) +
  geom_text(data = correlation.cor_osm, aes(label = paste("r =", round(correlation, 2))),
            x=-Inf, y=Inf, vjust = 1.5, hjust = -.1, color='blue') +
  geom_smooth(method = "lm", se = FALSE, colour = "blue") +
  facet_wrap(~Variable, ncol = 2, scales = "free") +
  labs(title = "Count of trips as a function of OpenStreetMap features")

#check for multicolinearity among census variables
glimpse(info.Aug_census)
mc_check<-info.Aug_census[,35:44]
mc_check<-st_set_geometry(mc_check, NULL)
glimpse(mc_check)
cor2pcor(cov(mc_check))

####====Regression====####
#split data into train and test
## training set = 75% of the sample
smp_size <- floor(0.75 * nrow(info.Aug))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(info.Aug)), size = smp_size)

train <- info.Aug[train_ind, ]
test <- info.Aug[-train_ind, ]


#Make the regression
reg_basic<-lm(Count ~ bikeLaneLv + dist.lane + Number_Tra + StreetWidt + 
                + TRUCK_ROUT + citibike.Buffer_small + isMH +
                pWhite + MedRent + pNoVeh + pSubway + 
                sidewalk_caf_nn3 + jobs_in_tract + bigPark + isGreenway + subway_nn5
                , data=train)
summary(reg_basic)

reg_compare<-lm(Count ~ bikeLaneLv + dist.lane + Number_Tra + StreetWidt + 
                  + TRUCK_ROUT + citibike.Buffer_small + isMH +
                  pWhite + MedRent + pNoVeh + pSubway + 
                  sidewalk_caf_nn3  + bigPark + isGreenway + subway_nn5 +
                  jobs_in_tract + office_nn5, data=train)
summary(reg_compare)


#make predictions
predictions<-reg_basic %>% predict(test)
predictions_comp<-reg_compare %>% predict(test)

#model performance
perf_basic<-data.frame(
  RMSE=RMSE(predictions, test$Count),
  R2 = R2(predictions, test$Count)
)

perf_compare <-data.frame(
  RMSE=RMSE(predictions_comp, test$Count),
  R2 = R2(predictions_comp, test$Count)
)


#### Explore Errors ####
#### add predictions and errors to df ####
# test set prediction 
reg_predict <- predict(reg_compare, newdata = test)

#training set prediction
reg_predict_train <- predict(reg_compare, newdata = train)

# training MAE
rmse.train <- caret::MAE(reg_predict_train, train$Count)

# test MAE
rmse.test  <- caret::MAE(reg_predict, test$Count)

test <- cbind(test, reg_all_predict)
train <- cbind(train, reg_all_predict_train)


#add error types for each prediction
test <-
  test %>%
  mutate(Count.Predict = reg_predict,
         Count.Error = reg_predict - Count,
         Count.AbsError = abs(reg_predict - Count),
         Count.APE = (abs(reg_predict - Count)) / Count)

train <-
  train %>%
  mutate(Count.Predict = reg_predict_train,
         Count.Error = reg_predict_train - Count,
         Count.AbsError = abs(reg_predict_train - Count),
         Count.APE = (abs(reg_predict_train - Count)) / Count)

MAPE_Train <- mean(train$Count.APE)
MAPE_Test <- mean(test$Count.APE)

table4.1.2 <- matrix(data = c(rmse.train, rmse.test, MAPE_Train, MAPE_Test), nrow = 2, ncol = 2)

rownames(table4.1.2) <- c("train", "test")
colnames(table4.1.2) <- c("MAE", "MAPE")

# show table
table4.1.2 %>% kable(caption = "Table 4.1.2 - Training and Testing MAE and MAPE")

#### true value vs. predicted value ####
preds.train <- data.frame(pred   = reg_predict_train,
                          actual = train$Count,
                          source = "training data")
preds.test  <- data.frame(pred   = reg_predict,
                          actual = test$Count,
                          source = "testing data")
preds <- rbind(preds.train, preds.test)

#plot of predicted vs observed
#I set the xlim to 0,200 because our predictions are very low
ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
  xlim(0,200)+
  geom_smooth(method = "lm", color = "green") +
  geom_abline(color = "orange") +
  coord_equal() +
  theme_bw() +
  facet_wrap(~source, ncol = 2) +
  labs(title = "Comparing predictions to actual values",
       x = "Predicted Value",
       y = "Actual Value",
       caption = "Figure 4.1.3 - Comparing predictions to actual values") +
  theme(
    legend.position = "none"
  )

#### skipping cross validation ####

#### spatial distribution of errors ####
# map errors from the test set
ggplot() +
  geom_sf(data = test, aes(colour = Count.Error),
          show.legend = "point", size = 1) +
  labs(title="Spatial Distribution of Residuals from Test Set", 
       subtitle = "Both over-prediction and under-prediction included",
       caption = "Spatial Distribution of Residuals from Test Set")

# map large errors
high_err_test<- subset(test, test$Count.AbsError>100)
high_err_train<- subset(train, Count.AbsError>100)

med_err_test<- subset(test, test$Count.AbsError>10)
med_err_train<- subset(train, Count.AbsError>10)

boroughs_clip<-st_intersection(borough, extent)

ggplot()+
  geom_sf(data=boroughs_clip, fill='white', color='white')+
  geom_sf(data=high_err_test, aes(color=Count.Error), size=0.7)+
  geom_sf(data=high_err_train, aes(color=Count.Error), size =0.7)+
  scale_color_continuous(type='viridis')+
  labs(title='Residuals Larger than 100')

ggplot()+
  geom_sf(data=boroughs_clip, fill='white', color='white')+
  geom_sf(data=med_err_test, aes(color=Count.Error), size=0.7)+
  geom_sf(data=med_err_train, aes(color=Count.Error), size =0.7)+
  scale_color_continuous(type='viridis')+
  labs(title='Residuals Larger than 10')

#see it there is spatial autocorrelation in the errors
#coords
coords <- st_coordinates(st_centroid(info.Aug))

# k nearest neighbors, k= 5
neighborList <- knn2nb(knearneigh(coords, 5))
spatialWeights <- nb2listw(neighborList, style="W")
info.Aug$lagCount <- lag.listw(spatialWeights, info.Aug$Count)

#errors
coords.test <-  st_centroid(test)
neighborList.test <- knn2nb(knearneigh(coords.test, 5))
spatialWeights.test <- nb2listw(neighborList.test, style="W")
test$lagCountError <- lag.listw(spatialWeights.test, test$Count.AbsError)

#spatial lag of count
count_lag_plot <- ggplot(info.Aug, aes(x=lagCount, y=Count,  label=Street)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Count as a function of the spatial lag of count",
       caption = "count as a function of the spatial lag of count",
       x = "Spatial lag of count (Mean count of 5 nearest neighbors)",
       y = "Trip Count")
count_lag_plot

ggplotly(count_lag_plot)

#spatial lag vs error
error_lag_plot<- ggplot(test, aes(x=lagCountError, y=Count, label=Street)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Count as a function of the spatial lag of error",
       caption = "Count as a function of the spatial lag of error",
       x = "Spatial lag of errors (Mean error of 5 nearest neighbors)",
       y = "Trip Count")
error_lag_plot

ggplotly(error_lag_plot)

