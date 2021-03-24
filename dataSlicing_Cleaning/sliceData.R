Sys.setenv(LANG = "en")

#load data
library(RSQLite)
library(tidyverse)
library(sf)
library(lubridate)
library(data.table)
library(RPostgreSQL)

setwd('C:/Users/katee/Box Sync/Practicum/shp/')

#Connect to SQLite
#riderdata <- "E:/NYCDOT/RideInsights_trim/RideInsights_trim.db"
riderdata <- 'RideInsights_trim.db'


sqlite.driver <- dbDriver("SQLite")

db <- dbConnect(sqlite.driver,
                dbname = riderdata)


## list all tables
tables <- dbListTables(db)

dbGetQuery(conn=db, 
           statement=
             paste("SELECT *, date(record_timestamp) AS rdate FROM rental_paths LIMIT 5"))

a <- as.data.frame(dbGetQuery(conn=db, 
                              statement=
                                paste("SELECT *, date(record_timestamp) AS rdate FROM rental_paths")))

a$ryear <- substr(a$rdate, 1, 4)

ride2020 <- a %>%
  filter(ryear == "2020")

ride2019 <- a %>%
  filter(ryear == "2019")

ride2018 <- a %>%
  filter(ryear == "2018")

ride2021 <- a %>%
  filter(ryear == "2021")

ride2021.sf <- st_as_sfc(lapply(ride2021$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2021 %>% select(-rental_path))


st_write(ride2021.sf, "ride2021_sf.shp", driver = "ESRI Shapefile")


test <- st_read("E:/NYCDOT/RideInsights_trim/ride2021/ride2021_sf.shp")

test %>%
  filter()


#Year 2018
ride2018$rmonth <- substr(ride2018$rdate, 6, 7)

ride2018FebMar <- ride2018 %>%
  filter(rmonth == "02" | rmonth == "03")


ride2018FebMar.sf <- st_as_sfc(lapply(ride2018FebMar$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018FebMar %>% select(-rental_path))

st_write(ride2018FebMar.sf, "ride2018FebMar_sf.shp", driver = "ESRI Shapefile")

test2 <- st_read("E:/NYCDOT/RideInsights_trim/ride2018FebMar/ride2018FebMar_sf.shp")


ride2018Apr <- ride2018 %>%
  filter(rmonth == "04")

ride2018Apr.sf <- st_as_sfc(lapply(ride2018Apr$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Apr %>% select(-rental_path))

st_write(ride2018Apr.sf, "ride2018Apr_sf.shp", driver = "ESRI Shapefile")

ride2018May <- ride2018 %>%
  filter(rmonth == "05")

ride2018May.sf <- st_as_sfc(lapply(ride2019May$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019May %>% select(-rental_path))

st_write(ride2018May.sf, "ride2019May_sf.shp", driver = "ESRI Shapefile")

ride2018Jun <- ride2018 %>%
  filter(rmonth == "06")

rider2018Jun_sf <- st_as_sfc(lapply(ride2018Jun$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Jun %>% select(-rental_path))

st_write(rider2018Jun_sf, "ride2018Jun_sf.shp", driver = "ESRI Shapefile")


ride2018July <- ride2018 %>%
  filter(rmonth == "07")

ride2018July_sf <- st_as_sfc(lapply(ride2018July$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018July %>% select(-rental_path))


st_write(ride2018July_sf, "ride2018July_sf.shp", driver = "ESRI Shapefile")

ride2018Aug <- ride2018 %>%
  filter(rmonth == "08")

ride2018Aug$date <- substr(ride2018Aug$rdate, 9, 10)

ride2018Aug15 <- ride2018Aug %>%
  filter(date == "01" | date == "02" | date == "03" | date == "04" | date == "05"|
           date == "06" | date == "07" | date == "08" | date == "09" | date == "10" |
           date == "11" | date == "12" | date == "13" | date == "14" | date == "15")

ride2018Aug15_sf <- st_as_sfc(lapply(ride2018Aug15$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Aug15 %>% select(-rental_path))

st_write(ride2018Aug15_sf, "ride2018Aug15_sf.shp", driver = "ESRI Shapefile")


ride2018Aug31 <- ride2018Aug %>%
  filter(date == "16" | date == "17" | date == "18" | date == "19" | date == "20" |
           date == "21" | date == "22" | date == "23" | date == "24" | date == "25" |
           date == "26" | date == "27" | date == "28" | date == "29" | date == "30" |
           date == "31")


ride2018Aug31_sf <- st_as_sfc(lapply(ride2018Aug31$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Aug31 %>% select(-rental_path))

st_write(ride2018Aug31_sf, "ride2018Aug31_sf.shp", driver = "ESRI Shapefile")


ride2018Sep <- ride2018 %>%
  filter(rmonth == "09")

ride2018Sep_sf <- st_as_sfc(lapply(ride2018Sep$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Sep %>% select(-rental_path))

st_write(ride2018Sep_sf, "ride2018Sep_sf.shp", driver = "ESRI Shapefile")

ride2018Oct <- ride2018 %>%
  filter(rmonth == "10")

ride2018Oct_sf <- st_as_sfc(lapply(ride2018Oct$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Oct %>% select(-rental_path))


ride2018Nov <- ride2018 %>%
  filter(rmonth == "11")

ride2018Nov_sf <- st_as_sfc(lapply(ride2018Nov$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Nov %>% select(-rental_path))

st_write(ride2018Nov_sf, "ride2018Nov_sf.shp", driver = "ESRI Shapefile")

ride2018Dec <- ride2018 %>%
  filter(rmonth == "12")

ride2018Dec_sf <- st_as_sfc(lapply(ride2018Dec$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2018Dec %>% select(-rental_path))

st_write(ride2018Dec_sf, "ride2018Dec_sf.shp", driver = "ESRI Shapefile")

#year 2020
ride2020$rmonth <- substr(ride2020$rdate, 6, 7)

ride2020Aug <- ride2020 %>%
  filter(rmonth == "08")

ride2020Aug.sf <- st_as_sfc(lapply(ride2020Aug$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2020Aug %>% select(-rental_path))

st_write(ride2020Aug.sf, "ride2020Aug_sf.shp", driver = "ESRI Shapefile")

testAug2020 <- st_read("ride2020Aug_sf.shp")
#back to doing things

ride2020Sep <- ride2020 %>%
  filter(rmonth == "09")

ride2020Sep.sf <- st_as_sfc(lapply(ride2020Sep$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2020Sep %>% select(-rental_path))

st_write(ride2020Sep.sf, "ride2020Sep_sf.shp", driver = "ESRI Shapefile")

#october
ride2020Oct <- ride2020 %>%
  filter(rmonth == "10")

ride2020Oct.sf <- st_as_sfc(lapply(ride2020Oct$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2020Oct %>% select(-rental_path))

st_write(ride2020Oct.sf, "ride2020Oct_sf.shp", driver = "ESRI Shapefile")

#2019
ride2019$rmonth <- substr(ride2019$rdate, 6, 7)

#May
ride2019May <- ride2019 %>%
  filter(rmonth == "05")

ride2019May.sf <- st_as_sfc(lapply(ride2019May$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019May %>% select(-rental_path))

st_write(ride2019May.sf, "ride2019May_sf.shp", driver = "ESRI Shapefile")

#June
ride2019June <- ride2019 %>%
  filter(rmonth == "06")

ride2019June.sf <- st_as_sfc(lapply(ride2019June$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019June %>% select(-rental_path))

st_write(ride2019June.sf, "ride2019June_sf.shp", driver = "ESRI Shapefile")


#July
ride2019July <- ride2019 %>%
  filter(rmonth == "07")

ride2019July.sf <- st_as_sfc(lapply(ride2019July$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019July %>% select(-rental_path))

st_write(ride2019July.sf, "ride2019July_sf.shp", driver = "ESRI Shapefile")


#August
ride2019Aug <- ride2019 %>%
  filter(rmonth == "08")

ride2019Aug.sf <- st_as_sfc(lapply(ride2019Aug$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019Aug %>% select(-rental_path))

st_write(ride2019Aug.sf, "ride2019Aug_sf.shp", driver = "ESRI Shapefile")

#October
ride2019Oct <- ride2019 %>%
  filter(rmonth == "10")

ride2019Oct.sf <- st_as_sfc(lapply(ride2019Oct$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019Oct %>% select(-rental_path))

st_write(ride2019Oct.sf, "ride2019Oct_sf.shp", driver = "ESRI Shapefile")

#November
ride2019Nov <- ride2019 %>%
  filter(rmonth == "11")

ride2019Nov.sf <- st_as_sfc(lapply(ride2019Nov$rental_path, sf:::hex_to_raw), EWKB = TRUE) %>%
  cbind(., ride2019Nov %>% select(-rental_path))

st_write(ride2019Nov.sf, "ride2019Nov_sf.shp", driver = "ESRI Shapefile")

