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


##Import Data and Slice Data

#Trips data
Jun18 <- st_read("E:/NYCDOT/riderpoint_shp/June/June18_pnts.shp")

#import borough
borough <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp") %>%
  st_transform(st_crs(Jun18))

#import lion data
lion <- st_read("E:/NYCDOT/Lion data/lion19d/lion19a_buffer.shp") %>%
  st_transform(st_crs(Jun18))


##========Brooklyn========##
Brooklyn <- borough %>%
  filter(boro_name == "Brooklyn")

##clip the trip pnts into Brooklyn
Jun18_Bk <- Jun18[Brooklyn,]

##clip the bikelanes into Brooklyn
lion_Bk <- lion[Brooklyn,]

#select useful column
lion_Bk <- lion_Bk %>%
  select(Street, BikeLane, SegmentID, geometry)

#create a new column of whether it is a bikelane
lion_Bk <- lion_Bk %>%
  mutate(bikeline = ifelse(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9", "yes", "no"))

#assign NA with no
lion_Bk$bikeline[is.na(lion_Bk$bikeline)] <- "no"


#Find out the points inside lanes
Jun18_Bk.in <- Jun18_Bk[lion_Bk,]


#allocate street names
Jun18_Bk.in <- st_join(Jun18_Bk.in, lion_Bk, join=st_intersects, left=TRUE)


#Group Points inside Same Street and Same ID as linestring
ls.June18_Bk <- Jun18_Bk.in %>%
  st_drop_geometry() %>%
  group_by(id, Street, SegmentID) %>%
  summarise()

#count the number of trips on each Street segment
count.Jun18_Bk <- ls.June18_Bk %>%
  group_by(Street, SegmentID) %>%
  summarise(Count = n())

#merge the info that whether the road is bikelane or not
info.Jun18_Bk <- merge(count.Jun18_Bk, lion_Bk, all.x=TRUE)

#Make the regression
reg_Bk <- lm(Count ~ bikeline, data=info.Jun18_Bk)
summary(reg_Bk)

stargazer(reg_Bk, type = "text",
          title = "Regression results",
          single.row = TRUE)




#test <- Jun18_Bk.in %>%
  #filter(index == 2929 | index == 3385)

#test1 <- test %>%
  #group_by(id, Street, SegmentID) %>%
  #group_by(Street, SegmentID) %>%
  #summarise(n = n())

#test %>% filter(Street == "6 STREET")

#test2 <- test %>%
  #group_by(id, Street, SegmentID) %>%
  #summarise(do_union = FALSE)

#test3 <- test2 %>%
  #group_by(Street, SegmentID) %>%
  #summarise(n = n())

#test4 <- test %>%
  #group_by(id, Street, SegmentID) %>%
  #summarise()


#test5 <- anti_join(test2 %>% st_drop_geometry(), test4 %>% st_drop_geometry())
#--> two methods are the same


