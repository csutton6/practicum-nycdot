Sys.setenv(LANG = "en")


#packages
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)
library(viridis)


##===========Import data===================#

#trip data
March2018 <- st_read("E:/NYCDOT/RideInsights_trim/ride2018FebMar/ride2018March.shp")

March2019 <- st_read("E:/NYCDOT/RideInsights_trim/ride2019Mar/ride2019March_sf.shp")

#add day and hour
March2018 <- March2018 %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)),
         time = ymd_hms(rcrd_tm),
         rhour = hour(time))

March2019 <- March2019 %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)),
         time = ymd_hms(rcrd_tm),
         rhour = hour(time))

#borough
borough <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp") %>%
  filter(boro_name == "Manhattan") %>%
  st_transform(st_crs(March2018))



#bike line (15 feet buffer)
lion15 <- st_read("E:/NYCDOT/nyclion_20d/lion_buffer15/lion_buffer15.shp") %>%
  st_transform(st_crs(March2018)) %>%
  select(Street, FeatureTyp, SegmentTyp, FaceCode, SeqNum, StreetCode, LGC1, SegmentID,
         SHAPE_Leng, SHAPE_Area, geometry)



##===================Clean Data=======================##

#clean road data into Manhattan
lion15M <- lion15[borough,]

#clean trip data into Manhattan

##March2018
#find out intersection points
pntsMarch18 <- st_intersection(March2018, 
                             st_cast(borough, "MULTILINESTRING", group_or_split = FALSE))


#select out the index of the points and then make a list
pntsMarchList18 <- pntsMarch18[, 1] %>% #only select the index column
  st_drop_geometry() #drop geometry

pntsMarchList18 <- as.vector(unlist(pntsMarchList18), mode = "numeric") #change to vector


#clean
March18_cleaned <- subset(March2018[borough,], !(index %in% pntsMarchList18))


##March 2019
pntsMarch19 <- st_intersection(March2019, 
                               st_cast(borough, "MULTILINESTRING", group_or_split = FALSE))


#select out the index of the points and then make a list
pntsMarchList19 <- pntsMarch19[, 1] %>% #only select the index column
  st_drop_geometry() #drop geometry

pntsMarchList19 <- as.vector(unlist(pntsMarchList19), mode = "numeric") #change to vector

#clean
March19_cleaned <- subset(March2019[borough,], !(index %in% pntsMarchList19))


#check
mapview(March18_cleaned)
mapview(March19_cleaned)



##===========Visualizations====================##

##Functions
#Plot functions
plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}



#Hourly Trend
ggplot(rbind(March18_cleaned, March19_cleaned) %>%
         as.data.frame() %>%
         group_by(rhour, ryear) %>%
         tally()) +
  geom_line(aes(x = rhour, y = n, color = ryear)) + 
  scale_x_continuous(breaks = seq(0, 23, by = 1)) +
  labs(title = "Hourly Trend in year 2018 vs year 2019",
       x = "Hour",
       y = "Count") + 
  plotTheme +
  theme(plot.title = element_text(size=14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#Daily Trend
ggplot(rbind(March18_cleaned, March19_cleaned) %>%
         as.data.frame() %>%
         group_by(day, ryear) %>%
         tally()) +
  geom_line(aes(x = day, y = n, color = ryear)) +
  labs(title = "Daily Trend in year 2018 vs year 2019",
       x = "Hour",
       y = "Count") + 
  plotTheme +
  theme(plot.title = element_text(size=14),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12))


#no clear daily trend --> pick Marth10th and Martch 20th for comparision
March10th18 <- March18_cleaned %>%
  filter(day == 10)

March10th19 <- March19_cleaned %>%
  filter(day == 10)

March20th18 <- March18_cleaned %>%
  filter(day == 20)

March20th19 <- March19_cleaned %>%
  filter(day == 20)

#save and change linestring points in ArcGIS Pro 
#use Generate points along line tool in ArcGIS: by percentage, 5%, include end point
st_write(March10th18, "March10th18.shp", driver = "ESRI Shapefile")
st_write(March10th19, "March10th19.shp", driver = "ESRI Shapefile")
st_write(March20th18, "March20th18.shp", driver = "ESRI Shapefile")
st_write(March20th19, "March20th19.shp", driver = "ESRI Shapefile")



###====March 10th====###
March10th18_pnts <- st_read("E:/NYCDOT/riderpoint_shp/March/generated/March10th18_pnts.shp")

March10th19_pnts <- st_read("E:/NYCDOT/riderpoint_shp/March/generated/March10th19_pnts.shp")


#select only the points within the roads
inrpnts_031018 <- March10th18_pnts[lion15M,]

#check
mapview(list(inrpnts_031018, lion15M), color = list("red", "blue"))


inrpnts_031019 <- March10th19_pnts[lion15M,]


#allocate street info on points
inrpnts_031018 <- st_join(inrpnts_031018, lion15M, join = st_intersects, left = TRUE)
inrpnts_031019 <- st_join(inrpnts_031019, lion15M, join = st_intersects, left = TRUE)


#group points inside same street as linestring
#Reference:https://stackoverflow.com/questions/50908771/create-multilines-from-points-grouped-by-id-with-sf-package
pnts2Line_031018 <- inrpnts_031018 %>%
  group_by(index, Street) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")



pnts2Line_031019 <- inrpnts_031019 %>%
  group_by(index, Street) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


#Count number of trips on each street
ntrips_031018 <- pnts2Line_031018 %>%
  st_drop_geometry() %>%
  group_by(Street) %>%
  summarise(Count = n())


ntrips_031019 <- pnts2Line_031019 %>%
  st_drop_geometry() %>%
  group_by(Street) %>%
  summarise(Count = n())


#bind the trips to road table
road <- lion15M %>%
  select(Street, geometry)

#March10th, 2018
road_031018 <- merge(road, ntrips_031018, by = ("Street"), all.x=TRUE)

mapview(road_031018, zcol = "Count")


ggplot()+
  geom_sf(data=road_031018, aes(color = Count)) + 
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option="viridis",
                       na.value="#D4D4D4") +
  mapTheme()


#March10th, 2019
road_031019 <- merge(road, ntrips_031019, by = ("Street"), all.x=TRUE)

ggplot()+
  geom_sf(data=road_031019, aes(color = Count)) + 
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option="viridis",
                       na.value="#D4D4D4") +
  mapTheme()


#add year
road_031018$year <- 2018
road_031019$year <- 2019


ggplot() +
  geom_sf(data = rbind(road_031018, road_031019), aes(color = Count), alpha = 0.9)+ 
  facet_wrap(~year) +
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option="viridis",
                       na.value="#D4D4D4") +
  labs(title = "Bike Trip Counts in March 10th") +
  mapTheme()



###=======March20th==========###
March20th18_pnts <- st_read("E:/NYCDOT/riderpoint_shp/March/generated/March20th18_pnts.shp")

March20th19_pnts <- st_read("E:/NYCDOT/riderpoint_shp/March/generated/March20th19_pnts.shp")

#select on the points within the roads
inrpnts_032018 <- March20th18_pnts[lion15M,]

inrpnts_032019 <- March20th19_pnts[lion15M,]


#allocate street info on points
inrpnts_032018 <- st_join(inrpnts_032018, lion15M, join = st_intersects, left = TRUE)
inrpnts_032019 <- st_join(inrpnts_032019, lion15M, join = st_intersects, left = TRUE)


#group points inside same street as linestring
#Reference:https://stackoverflow.com/questions/50908771/create-multilines-from-points-grouped-by-id-with-sf-package
pnts2Line_032018 <- inrpnts_032018 %>%
  group_by(index, Street) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


pnts2Line_032019 <- inrpnts_032019 %>%
  group_by(index, Street) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


#Count number of trips on each street
ntrips_032018 <- pnts2Line_032018 %>%
  st_drop_geometry() %>%
  group_by(Street) %>%
  summarise(Count = n())


ntrips_032019 <- pnts2Line_032019 %>%
  st_drop_geometry() %>%
  group_by(Street) %>%
  summarise(Count = n())


#bind the trips to road table

#March20th, 2018
road_032018 <- merge(road, ntrips_032018, by = ("Street"), all.x=TRUE)
#March20th, 2019
road_032019 <- merge(road, ntrips_032019, by = ("Street"), all.x=TRUE)

#add year
road_032018$year <- 2018
road_032019$year <- 2019


#map
ggplot() +
  geom_sf(data = rbind(road_032018, road_032019), aes(color = Count), alpha = 0.9)+ 
  facet_wrap(~year) +
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option="viridis",
                       na.value="#D4D4D4") +
  labs(title = "Bike Trip Counts in March 20th") +
  mapTheme()




###================Hourly===================###
#peaks at 23
#lowest at 8

March23pm_18 <- March18_cleaned %>%
  filter(rhour == 23)

March23pm_19 <- March19_cleaned %>%
  filter(rhour == 23)


March8am_18 <- March18_cleaned %>%
  filter(rhour == 8)

March8am_19 <- March19_cleaned %>%
  filter(rhour == 8)

#not enough data for low hours--> use 23pm
#save and change linestring in ArcGIS pro
#same as before
st_write(March23pm_18, "March23pm_18.shp", driver = "ESRI Shapefile")
st_write(March23pm_19, "March23pm_19.shp", driver = "ESRI Shapefile")


##VISUALIZE
#read file
March23pm_18pnts <- st_read("E:/NYCDOT/riderpoint_shp/March/generated/March23pm_18pnts.shp")

March23pm_19pnts <- st_read("E:/NYCDOT/riderpoint_shp/March/generated/March23pm_19pnts.shp")


#select on the points within the roads
inrpnts_23pm_18 <- March23pm_18pnts[lion15M,]

inrpnts_23pm_19 <- March23pm_19pnts[lion15M,]


#allocate street info on points
inrpnts_23pm_18 <- st_join(inrpnts_23pm_18, lion15M, join = st_intersects, left = TRUE)
inrpnts_23pm_19 <- st_join(inrpnts_23pm_19, lion15M, join = st_intersects, left = TRUE)

#group points inside same street as linestring
pnt2Line_23pm18 <- inrpnts_23pm_18 %>%
  group_by(index, Street) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


pnt2Line_23pm19 <- inrpnts_23pm_19 %>%
  group_by(index, Street) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


#Count number of trips on each street
ntrips_23pm_18 <- pnt2Line_23pm18 %>%
  st_drop_geometry() %>%
  group_by(Street) %>%
  summarise(Count = n())

#Count number of trips on each street
ntrips_23pm_19 <- pnt2Line_23pm19 %>%
  st_drop_geometry() %>%
  group_by(Street) %>%
  summarise(Count = n())


#bind the trips to road table

#23pm in March, 2018
road23pm_18 <- merge(road, ntrips_23pm_18, by = ("Street"), all.x=TRUE)
#23pm in March, 2019
road23pm_19 <- merge(road, ntrips_23pm_19, by = ("Street"), all.x=TRUE)

#add year
road23pm_18$year <- 2018
road23pm_19$year <- 2019


#map
ggplot() +
  geom_sf(data = rbind(road23pm_18, road23pm_19), aes(color = Count), alpha = 0.9)+ 
  facet_wrap(~year) +
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option="magma",
                       na.value="#D4D4D4") +
  labs(title = "Bike Trip Counts at 23pm in March") +
  mapTheme()



