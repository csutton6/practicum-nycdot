setwd('C:/Users/katee/Box Sync/Practicum/shp/')

install.packages("kableExtra")


library(sf)
library(tidyverse)
library(ggplot2)
library(mapview)
library(RSocrata)
library(ggthemes)
library(kableExtra)

bikelane_17d<- st_read('bikelane_17d.shp')
bikelane_20d<- st_read('bikelane_20d.shp')

new_18 <- st_read('bikelane_1718Diff_2.shp')
new_19 <- st_read('bikelane_1819Diff_2.shp')
new_20 <- st_read('bikelane_1920Diff_2.shp')

boroughs<- st_read('https://data.cityofnewyork.us/resource/7t3b-ywvw.geojson')
ntas<- st_read('https://data.cityofnewyork.us/resource/q2z5-ai38.geojson')

parks<- st_read('for_basemap/geo_export_8469ffba-1951-4e52-916c-c9c4dfa54c18.shp')
big_parks<-subset(parks, parks$acres>30)

#get rid of excess columns
new_18<- new_18%>%
  select(120:242)

new_19<- new_19%>%
  select(120:241)

new_20<- new_20%>%
  select(120:244)

# add lane category
new_18$LaneType <- '3: Combination'
new_18$LaneType[new_18$BikeLane_1=='1']<- '1: Protected Lane'
new_18$LaneType[new_18$BikeLane_1=='2']<- '2: Unprotected Lane'
new_18$LaneType[new_18$BikeLane_1=='3']<- '4: Sharrow'

new_19$LaneType <- '3: Combination'
new_19$LaneType[new_19$BikeLane_1=='1']<- '1: Protected Lane'
new_19$LaneType[new_19$BikeLane_1=='2']<- '2: Unprotected Lane'
new_19$LaneType[new_19$BikeLane_1=='3']<- '4: Sharrow'

new_20$LaneType <- '3: Combination'
new_20$LaneType[new_20$BikeLane_1=='1']<- '1: Protected Lane'
new_20$LaneType[new_20$BikeLane_1=='2']<- '2: Unprotected Lane'
new_20$LaneType[new_20$BikeLane_1=='3']<- '4: Sharrow'

bikelane_17d$LaneType <- '3: Combination'
bikelane_17d$LaneType[bikelane_17d$BikeLane=='1']<- '1: Protected Lane'
bikelane_17d$LaneType[bikelane_17d$BikeLane=='2']<- '2: Unprotected Lane'
bikelane_17d$LaneType[bikelane_17d$BikeLane=='3']<- '4: Sharrow'

bikelane_20d$LaneType <- '3: Combination'
bikelane_20d$LaneType[lane2_20d$BikeLane=='1']<- '1: Protected Lane'
bikelane_20d$LaneType[lane2_20d$BikeLane=='2']<- '2: Unprotected Lane'
bikelane_20d$LaneType[lane2_20d$BikeLane=='3']<- '4: Sharrow'


#add boro name
bikelane_17d$Borough <- 'Manhattan'
bikelane_17d$Borough[bikelane_17d$LBoro_1=='2']<- 'Bronx'
bikelane_17d$Borough[bikelane_17d$LBoro_1=='3']<- 'Brooklyn'
bikelane_17d$Borough[bikelane_17d$LBoro_1=='4']<- 'Queens'
bikelane_17d$Borough[bikelane_17d$LBoro_1=='5']<- 'Staten Island'

new_18$Borough <- 'Manhattan'
new_18$Borough[new_18$LBoro_1=='2']<- 'Bronx'
new_18$Borough[new_18$LBoro_1=='3']<- 'Brooklyn'
new_18$Borough[new_18$LBoro_1=='4']<- 'Queens'
new_18$Borough[new_18$LBoro_1=='5']<- 'Staten Island'

new_19$Borough <- 'Manhattan'
new_19$Borough[new_19$LBoro_1=='2']<- 'Bronx'
new_19$Borough[new_19$LBoro_1=='3']<- 'Brooklyn'
new_19$Borough[new_19$LBoro_1=='4']<- 'Queens'
new_19$Borough[new_19$LBoro_1=='5']<- 'Staten Island'

new_20$Borough <- 'Manhattan'
new_20$Borough[new_20$LBoro_1=='2']<- 'Bronx'
new_20$Borough[new_20$LBoro_1=='3']<- 'Brooklyn'
new_20$Borough[new_20$LBoro_1=='4']<- 'Queens'
new_20$Borough[new_20$LBoro_1=='5']<- 'Staten Island'


#if sum of road <1000 ft, drop all segments with that road name
lane_17d<- bikelane_17d%>%
  group_by(Street, LaneType)%>%
  summarize(Length=sum(SHAPE_Leng),
            Borough=first(Borough))

new_18_clean<- new_18%>%
  group_by(Street_1, LaneType)%>%
  summarize(Length=sum(SHAPE_Le_1),
            Borough=first(Borough))%>%
  filter(Length>1000)

new_19_clean<- new_19%>%
  group_by(Street_1, LaneType)%>%
  summarize(Length=sum(SHAPE_Le_1),
            Borough=first(Borough))%>%
  filter(Length>1000)

new_20_clean<- new_20%>%
  group_by(Street_1, LaneType)%>%
  summarize(Length=sum(SHAPE_Le_1),
            Borough=first(Borough))%>%
  filter(Length>1000)

#names(my_data)[names(my_data) == "Sepal.Length"] <- "sepal_length"
names(new_18_clean)[names(new_18_clean) == "Street_1"] <- "Street"
names(new_19_clean)[names(new_19_clean) == "Street_1"] <- "Street"
names(new_20_clean)[names(new_20_clean) == "Street_1"] <- "Street"


lane_17d$Year<- 2017
new_18_clean$Year<- 2018
new_19_clean$Year<- 2019
new_20_clean$Year<- 2020

st_crs(new_18_clean)

combo<- rbind(lane_17d, new_18_clean, new_19_clean, new_20_clean)
combo_sf<-inner_join(bikelane_20d, combo, by='SegmentID')

#current lanes by type
ggplot()+
  geom_sf(data=boroughs, color='#222222', fill='#222222')+
  geom_sf(data=big_parks, color='#4c6350', fill='#4c6350')+
  geom_sf(data=bikelane_20d, aes(color = LaneType), size=0.5)+
  scale_colour_manual(values = c('#F55536','#f57f36', '#f7ac2a', '#fcd93a'))+
  labs(title='New York City Bike Lanes by Type 2021')

#new lanes by type each year
ggplot()+
  geom_sf(data=boroughs, color='#222222', fill='#222222')+
  geom_sf(data=big_parks, color='#4c6350', fill='#4c6350')+
  geom_sf(data=new_18_clean, aes(color = LaneType), size=0.9)+
  scale_colour_manual(values = c('#F55536','#f57f36', '#f7ac2a', '#fcd93a'))+
  labs(title='2018: New Bike Lanes by Type')

ggplot()+
  geom_sf(data=boroughs, color='#222222', fill='#222222')+
  geom_sf(data=big_parks, color='#4c6350', fill='#4c6350')+
  geom_sf(data=new_19_clean, aes(color = LaneType), size=0.9)+
  scale_colour_manual(values = c('#F55536','#f57f36', '#f7ac2a', '#fcd93a'))+
  labs(title='2019: New Bike Lanes by Type')

ggplot()+
  geom_sf(data=boroughs, color='#222222', fill='#222222')+
  geom_sf(data=big_parks, color='#4c6350', fill='#4c6350')+
  geom_sf(data=new_20_clean, aes(color = LaneType), size=0.9)+
  scale_colour_manual(values = c('#F55536','#f57f36', '#f7ac2a', '#fcd93a'))+
  labs(title='2020: New Bike Lanes by Type')

#### possible colors ####
# oranges: '#F55536','#f57f36', '#f7ac2a', '#fcd93a'
#old green: #d5e8be
#viridis: "#481567", "#2d708e", "#158c4a", '#f2db0c'
#coolors1: '#F55536', '#FAC05E', '#A4D4B4', '#5E7CE2'
#blues: '#0A369D', '#4472CA', '#5E7CE2', '#92B4F4'
#starting w teal: '#52ffdc', '#52f6ff', '#52cbff', '#5294ff'
#purples: '#cebdff', '#aa7ff0', '#d069f5', '#f569e9'
#pinks: '#c51b8a', '#f768a1', '#fbb4b9', '#feebe2' 



#### tables of new lanes each year ####
new_18_table<- new_18_clean%>%
  as.data.frame()%>%
  select(Street_1, Borough, LaneType, Length)%>%
  filter(Borough!= 'Staten Island')%>%
  arrange(desc(Length))%>%
  head(20)%>%
  kbl(caption='Longest New Bike Lanes in 2018')%>%
  kable_styling()
new_18_table

#don't use this
new_18_protect_table<- new_18_clean%>%
  as.data.frame()%>%
  select(Street_1, Borough, LaneType, Length)%>%
  filter(Borough!= 'Staten Island')%>%
  arrange(desc(Length))%>%
  filter(LaneType=='1: Protected Lane')%>%
  head(20)%>%
  kbl(caption='Longest New Protected Bike Lanes in 2018')%>%
  kable_styling()
new_18_protect_table


new_19_table<- new_19_clean%>%
  as.data.frame()%>%
  select(Street_1, Borough, LaneType, Length)%>%
  filter(Borough!= 'Staten Island')%>%
  arrange(desc(Length))%>%
  head(20)%>%
  kbl(caption='Longest New Bike Lanes in 2019')%>%
  kable_styling()
new_19_table

new_19_protect_table<- new_19_clean%>%
  as.data.frame()%>%
  select(Street_1, Borough, LaneType, Length)%>%
  arrange(desc(Length))%>%
  filter(LaneType=='1: Protected Lane')%>%
  head(10)%>%
  kbl(caption='Longest New Protected Bike Lanes in 2019')%>%
  kable_styling()
new_19_protect_table

new_20_table<- new_20_clean%>%
  as.data.frame()%>%
  select(Street_1, Borough, LaneType, Length)%>%
  arrange(desc(Length))%>%
  head(20)%>%
  kbl(caption='Longest New Bike Lanes in 2020')%>%
  kable_styling()
new_20_table

new_20_protect_table<- new_20_clean%>%
  as.data.frame()%>%
  select(Street_1, Borough, LaneType, Length)%>%
  arrange(desc(Length))%>%
  filter(LaneType=='1: Protected Lane')%>%
  head(10)%>%
  kbl(caption='Longest New Protected Bike Lanes in 2020')%>%
  kable_styling()
new_20_protect_table



