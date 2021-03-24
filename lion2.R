setwd('C:/Users/katee/Box Sync/Practicum/shp/')

install.packages("kableExtra")
install.packages('RJSONIO')
install.packages('gdata')

library(sf)
library(tidyverse)
library(ggplot2)
library(mapview)
library(RSocrata)
library(ggthemes)
library(kableExtra)
library(RJSONIO)
library(gdata)

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
bikelane_20d$LaneType[bikelane_20d$BikeLane=='1']<- '1: Protected Lane'
bikelane_20d$LaneType[bikelane_20d$BikeLane=='2']<- '2: Unprotected Lane'
bikelane_20d$LaneType[bikelane_20d$BikeLane=='3']<- '4: Sharrow'


#add boro name
bikelane_17d$Borough <- 'Manhattan'
bikelane_17d$Borough[bikelane_17d$LBoro=='2']<- 'Bronx'
bikelane_17d$Borough[bikelane_17d$LBoro=='3']<- 'Brooklyn'
bikelane_17d$Borough[bikelane_17d$LBoro=='4']<- 'Queens'
bikelane_17d$Borough[bikelane_17d$LBoro=='5']<- 'Staten Island'

bikelane_20d$Borough <- 'Manhattan'
bikelane_20d$Borough[bikelane_20d$LBoro=='2']<- 'Bronx'
bikelane_20d$Borough[bikelane_20d$LBoro=='3']<- 'Brooklyn'
bikelane_20d$Borough[bikelane_20d$LBoro=='4']<- 'Queens'
bikelane_20d$Borough[bikelane_20d$LBoro=='5']<- 'Staten Island'

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

#combine all new lanes into one dataframe (not filtered by length)
names(bikelane_17d)[names(bikelane_17d) == "SHAPE_Leng"] <- "Length"
names(new_18)[names(new_18) == "SHAPE_Le_1"] <- "Length"
names(new_19)[names(new_19) == "SHAPE_Le_1"] <- "Length"
names(new_20)[names(new_20) == "SHAPE_Le_1"] <- "Length"

names(new_18)[names(new_18) == "Street_1"] <- "Street"
names(new_19)[names(new_19) == "Street_1"] <- "Street"
names(new_20)[names(new_20) == "Street_1"] <- "Street"

names(new_18)[names(new_18) == "SegmentID_"] <- "SegmentID"
names(new_19)[names(new_19) == "SegmentID_"] <- "SegmentID"
names(new_20)[names(new_20) == "SegmentID_"] <- "SegmentID"

bikelane_17d$Year<- 2017
new_18$Year<- 2018
new_19$Year<- 2019
new_20$Year<- 2020

lane_17d_st<-bikelane_17d%>%
  as.data.frame()%>%
  select(Street, SegmentID, LaneType, Length, Borough, Year)

new_18_st<-new_18%>%
  as.data.frame()%>%
  select(Street, SegmentID, LaneType, Length, Borough, Year)

new_19_st<-new_19%>%
  as.data.frame()%>%
  select(Street, SegmentID, LaneType, Length, Borough, Year)

new_20_st<-new_20%>%
  as.data.frame()%>%
  select(Street, SegmentID, LaneType, Length, Borough, Year)


combo<- rbind(lane_17d_st, new_18_st, new_19_st, new_20_st)
combo_sf<-inner_join(bikelane_20d, combo, by='SegmentID')

combo_2<- combo%>%
  count(Year=factor(Year))%>%
  mutate(pct=prop.table(n))



#group current network by borough and lanetype
## SOMETHING WRONG with group_20_boro in md
names(bikelane_20d)[names(bikelane_20d) == "SHAPE_Leng"] <- "Length"

group_20_boro<-bikelane_20d%>%
  as.data.frame()%>%
  group_by(Borough, LaneType)%>%
  summarize(Count = n(), 
            Length=sum(SHAPE_Leng))%>%
  arrange(desc(Length))

group_20_boro

# bike lane addtions by type and year
ggplot(data=combo_2, aes(x=Year, y=pct, label=scales::percent(pct)))+
  geom_col()+
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c('Before 2018', '2018', '2019', '2020'))+
  labs(title='Bike Lanes Added During the Study Period', x='Year Built', y = 'Percent of Network')

#map of additions by year
ggplot()+
  geom_sf(data=boroughs, color='#222222', fill='#222222')+
  geom_sf(data=big_parks, color='#4c6350', fill='#4c6350')+
  geom_sf(data=combo_sf, aes(color=LaneType.y))+
  scale_colour_manual(values = c('#F55536','#f57f36', '#f7ac2a', '#fcd93a'))+
  facet_wrap(~Year)+
  labs(title='New Bike Lanes During Study Period', subtitle='By Year and Type')


#plots
ggplot(data=group_20_boro)+
  geom_col(aes(x=Borough, y=Length))+
  labs(title='NYC Bike Lanes by Borough - 2021')

ggplot(data=group_20_boro)+
  geom_col(aes(x=LaneType, y=Length))+
  labs(title='NYC Bike Lanes by Type - 2021')

ggplot(data=group_20_boro)+
  geom_col(aes(x=Borough, y=Length, fill=LaneType), position='dodge')+
  labs(title='NYC Bike Lanes by Borough and Type - 2021')


# maps
# current lanes by type
ggplot()+
  geom_sf(data=boroughs, color='#222222', fill='#222222')+
  geom_sf(data=big_parks, color='#4c6350', fill='#4c6350')+
  geom_sf(data=bikelane_20d, aes(color = LaneType), size=0.5)+
  scale_colour_manual(values = c('#F55536','#f57f36', '#f7ac2a', '#fcd93a'))+
  labs(title='New York City Bike Lanes by Type 2021')+
  facet_wrap(~LaneType)

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
  head(15)%>%
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


#citibike
station_info<- fromJSON('https://gbfs.citibikenyc.com/gbfs/en/station_information.json')
stations<- station_info[['data']]

grabInfo<-function(var){
  print(paste("Variable", var, sep=" "))  
  sapply(stations, function(x) returnData(x, var)) 
  
  
}


returnData<-function(x, var){
  if(!is.null( x[[var]])){
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

stationDataDF<-data.frame(sapply(1:22, grabInfo), stringsAsFactors=FALSE)

#with geo
grabGeoInfo<-function(val){
  
  l<- length(stations[[1]][[val]])
  tmp<-lapply(1:l, function(y) 
    
    sapply(stations, function(x){
      
      if(!is.null(x[[val]][[y]])){
        return(x[[val]][[y]])
      }else{
        return(NA)
      }
      
    })     
  )
}


stationDataGeo<-grabGeoInfo(23)
stationDataGeo<-data.frame(do.call("cbind", stationDataGeo), stringsAsFactors=FALSE)

stationDataDF<-cbind(stationDataDF, stationDataGeo)


