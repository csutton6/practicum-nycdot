Sys.setenv(LANG = "en")


#packages
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)


##===================Import data=====================##
##Trip Data
FebMarch <- st_read("E:/NYCDOT/RideInsights_trim/ride2018FebMar/ride2018FebMar_sf.shp")

#March
March <- FebMarch %>%
  filter(rmonth == "03")

#add day and hour
March <- March %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)),
         time = ymd_hms(rcrd_tm),
         rhour = hour(time))


##Get NYC data
#Reference: http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927
NYC <- st_read("E:/NYCDOT/NYS_Civil_Boundaries_SHP/Cities_Towns.shp") %>%
  st_transform(st_crs(March)) %>%
  filter(NAME == "New York")


##=================Clean Data========================##

#EXAMPLE:

#first type: March 5th --> not connected
March5th <- March %>%
  filter(day == 5)


mapview(March5th[NYC,]) #use clip to clean up


#second type: March 25th --> connected
#need to make a NYC buffer

March25th <- March %>%
  filter(day == 25)

#make buffer
buffercor <- NYC[, 13]
NYCbuffer <- st_buffer(buffercor, dist = 0.02)


#find out the intersection
pntsAll <- st_intersection(March25th, 
                           st_cast(NYCbuffer, "MULTILINESTRING", group_or_split = FALSE))


#select out the index of the points and then make a list
pntsList <- pntsAll[, 1] %>% #only select the index column
  st_drop_geometry() #drop geometry

pntsList <- as.vector(unlist(pntsList), mode = "numeric") #change to vector


#clean March25th
March25th_cleaned <- subset(March25th[NYC,], !(index %in% pntsList))

#check
mapview(March25th_cleaned)




##Clean UP ALL MARCH

#find out intersection points
pntsMarch <- st_intersection(March, 
                            st_cast(NYCbuffer, "MULTILINESTRING", group_or_split = FALSE))


#select out the index of the points and then make a list
pntsMarchList <- pntsMarch[, 1] %>% #only select the index column
  st_drop_geometry() #drop geometry

pntsMarchList <- as.vector(unlist(pntsMarchList), mode = "numeric") #change to vector


#clean
March_cleaned <- subset(March[NYC,], !(index %in% pntsMarchList))


#check
mapview(March_cleaned)

