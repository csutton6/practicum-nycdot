Sys.setenv(LANG = "en")


#packages
library(tidyverse)
library(sf)
library(lubridate)
library(mapview)


##===================Import data=====================##
##Trip Data
FebMarch <- st_read("E:/NYCDOT/RideInsights_trim/ride2018FebMar/ride2018FebMar_sf.shp")

##Feb
Feb <- FebMarch %>%
  filter(rmonth == "02")

##add day
Feb <- Feb %>%
  mutate(day = substr(rdate, 9, 10))


#March
March <- FebMarch %>%
  filter(rmonth == "03")

#add day
March <- March %>%
  mutate(day = substr(rdate, 9, 10))


##Get NYC data
#Reference: http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=927
NYC <- st_read("E:/NYCDOT/NYS_Civil_Boundaries_SHP/State.shp") %>%
  st_transform(st_crs(Feb))


##======================Clean Data=========================##
#First type: the data is not connected

#if error, run: remotes::install_github("r-spatial/mapview")
mapshot(mapview(Feb), url = paste0(getwd(), "/map.html")) #save image

Feb_cleaned <- Feb[NYC,]
mapview(Feb_cleaned, zcol="day", color=viridisLite::plasma)



#Second type: the data is connected

MarchTrip <- March[NYC,] # only works for if the outside string is not connected

mapview(MarchTrip, zcol="day", color=viridisLite::plasma)


March5th <- March %>%
  filter(day == "05")

March25th <- March %>%
  filter(day == "25")


#make buffer
buffercor <- NYC[, 13]
NYCbuffer <- st_buffer(buffercor, dist = 1)

mapview(March25th[NYCbuffer,])


#find out intersections
pnts  <- st_intersection(March25th, 
                         st_cast(NYCbuffer, "MULTILINESTRING", group_or_split = FALSE))

March25th[pnts,]


pntsAll <- st_intersection(March, 
                           st_cast(NYCbuffer, "MULTILINESTRING", group_or_split = FALSE))


#cleaned data
March_cleaned <- March[NYC,] %>%
  filter(index != 36215 & index != 40804)

mapview(March_cleaned, zcol="day", color=viridisLite::plasma, alpha=0.7)
mapview(March_cleaned, color="#3399FF", alpha=0.1)


#change days as numeric

Feb_cleaned$day <- as.numeric(Feb_cleaned$day)

March_cleaned$day <- as.numeric(March_cleaned$day)


#trips over days


ggplot(Feb_cleaned %>%
         as.data.frame() %>%
         group_by(day) %>%
         tally())+
  geom_line(aes(x = day, y = n))


ggplot(March_cleaned %>%
         as.data.frame() %>%
         group_by(day) %>%
         tally())+
  geom_line(aes(x = day, y = n))


#add hour to March
March_cleaned <- March_cleaned %>%
  mutate(time = ymd_hms(rcrd_tm)) %>%
  mutate(rhour = hour(time))

#trips over hour
ggplot(March_cleaned %>%
         as.data.frame() %>%
         group_by(rhour) %>%
         tally())+
  geom_line(aes(x = rhour, y = n))
