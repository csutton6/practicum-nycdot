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
bike20d_buffer <- st_read("lion_buffer15/lion_buffer15.shp") %>%
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

#highways
highways<- subset(bike20d_buffer, RW_TYPE==2 | SegmentTyp=='E')

#divided streets
divided<-subset(bike20d_buffer, SegmentTyp=='G' | SegmentTyp=='R' | SegmentTyp=='T' | SegmentTyp=='C')
divided_better<-subset(bike20d_buffer, SegmentTyp=='R' | SegmentTyp=='T' |SegmentTyp=='C')
mapview(divided_better)

#tunnels
tunnels<- subset(bike20d_buffer, RW_TYPE==4)
mapview(tunnels)

#path/trail
trail<- subset(bike20d_buffer, RW_TYPE==6)
mapview(trail)

mapview(bike20d_buffer_drop)

#add a highway, trail, and tunnel 
bike20d_buffer_drop <- bike20d_buffer_drop %>% 
  mutate(trailOrNot = case_when(
    RW_TYPE == 6 ~ 1,
    TRUE~0
  ),
  bikeLaneLv = case_when(
    BikeLane == 1  ~ "Protected",
    BikeLane == 2 | BikeLane == 4 | BikeLane == 5 | BikeLane == 8 | BikeLane == 9 | BikeLane == 10 ~ 'Unprotected', 
    TRUE ~ 'None'
  ),
  bridgeOrNot = case_when(
    RW_TYPE == 3 ~ 1,
    TRUE~0
  ))

bike20d_buffer<- bike20d_buffer_drop
bike20d_buffer <- distinct(bike20d_buffer,SegmentID,.keep_all = T)
# ggplot()+geom_sf(data = bike20d_buffer %>% st_as_sf(),aes(color = "red"))

#select useful column
bike20d_buffer <- bike20d_buffer %>%
  select(Street, BikeLane, SegmentID, FeatureTyp, SegmentTyp, NonPed,
         TrafDir, SegCount, LBoro, XFrom, YFrom, RW_TYPE, Snow_Prior,
         Number_Tra, Number_Par, Number_Tot, BIKE_TRAFD, 
         StreetWidt, StreetWi_1, StreetWi_2, TRUCK_ROUT, POSTED_SPE, trailOrNot, 
         bikeLaneLv, bridgeOrNot, geometry)

glimpse(bike20d_buffer)

#summing ridership onto streets
#Find out the points inside bikelanes
Aug.in <- Aug_pnts[bike20d_buffer,]

#allocate street names
Aug.in <- st_join(Aug.in, bike20d_buffer, join=st_intersects, left=TRUE)

#Group Points inside Same Street and Same ID as linestring
ls.Aug <- Aug.in %>%
  st_drop_geometry() %>%
  group_by(index, Street, SegmentID) %>%
  summarise()

#count the number of trips on each Street segment
count.Aug <- ls.Aug %>%
  group_by(Street, SegmentID) %>%
  summarise(Count = n())

#merge the info that whether the road is bikelane or not
#keep the order to makesure sf
info.Aug <- merge(bike20d_buffer, count.Aug, all.x = T)

#find out the number of trips on bikelanes
bikelanes <- bike20d_buffer %>% filter(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9" |
                                         BikeLane == '4' | BikeLane == '8' | BikeLane == '10')
# bike.Aug <- merge(bikelanes, count.Aug, all.x = T)

#find out the number of trips in each neighborhood
bike.nh <- st_join(info.Aug, neighborhood %>% st_transform(st_crs(info.Aug)), join=st_intersects, left=TRUE)

bike.nh <- bike.nh %>%
  select(ntaname, Count) %>%
  group_by(ntaname) %>%
  summarise(nhCount = sum(Count))




####=====Feature Engineering========####
#change projection
info.Aug <- info.Aug %>% st_transform('ESRI:102711')
bike20d_buffer <- bike20d_buffer %>% st_transform('ESRI:102711')
# bike.Aug <- bike.Aug %>% st_transform('ESRI:102711')
bikelanes <- bikelanes %>% st_transform('ESRI:102711')
bike.nh <- bike.nh %>% st_transform('ESRI:102711')
station <- station %>% st_transform('ESRI:102711')


###Distance to nearest bikelanes
info.Aug <- info.Aug %>%
  mutate(dist.lane = nn_function(st_coordinates(st_centroid(info.Aug$geometry)),
                                 st_coordinates(st_centroid(bikelanes$geometry)), 1))


#borough
boxplot(Count ~ LBoro,
        data=info.Aug)

info.Aug$isMH<-0
info.Aug$isMH[info.Aug$LBoro==1]<-1
# 
# info.Aug$Boro_F<-as.factor(info.Aug$LBoro)

#travel lanes
info.Aug$Number_Tra <- as.numeric(info.Aug$Number_Tra)

#snow route
critical<-subset(info.Aug, Snow_Prior=='C')
mapview(critical)

sector<-subset(info.Aug, Snow_Prior=='S')
mapview(sector)

haulster<-subset(info.Aug, Snow_Prior=='H')
mapview(haulster)

snow_v<-subset(info.Aug, Snow_Prior=='V')
mapview(snow_v)

info.Aug$MinorSnowRoute<-0
info.Aug$MinorSnowRoute[info.Aug$Snow_Prior=='S']<-1

#speed limit
info.Aug$POSTED_SPE<-as.numeric(info.Aug$POSTED_SPE)
ggplot(info.Aug, aes(x=POSTED_SPE, y=Count))+
  geom_point(size=0.5, alpha=0.2)+
  geom_smooth(method='lm')

boxplot(Count~POSTED_SPE,
        data=info.Aug)


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


#### Correlations ####
#corplot of numeric variables against each other
# create a correlation matrix of all numerical variables
numericVars <- 
  select_if(st_drop_geometry(info.Aug[, -c(1:2, 30)]), is.numeric) %>% 
  na.omit()


glimpse(numericVars)

corr<-round(cor(numericVars), 1)
p.mat<- cor_pmat(numericVars)

ggcorrplot(corr, hc.order = TRUE, type='lower', insig='blank')

#qualitative vars scatter plot
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
boxplot(Count~bridgeOrNot,
        data=info.Aug)

boxplot(Count~bikeLaneLv,
        data=info.Aug,
        col='orange',
        border='brown')

boxplot(Count~POSTED_SPE,
        data=info.Aug)

boxplot(Count~StreetWidt,
        data=info.Aug)

boxplot(Count~Number_Tra,
        data=info.Aug)

ggplot()+
  geom_point(data=info.Aug, aes(x=dist.lane, y=Count), alpha=0.2)+
  geom_smooth(data= info.Aug, aes(x=dist.lane, y=Count), method='lm')

ggplot()+
  geom_point(data=info.Aug, aes(x=XFrom, y=Count), alpha=0.2)+
  geom_smooth(data= info.Aug, aes(x=XFrom, y=Count), method='lm')

#facet cor plot
select_if(st_drop_geometry(info.Aug[, -c(1:2, 30)]), is.numeric) %>% 
  na.omit()
glimpse(info.Aug)
correlation.long <-
  st_drop_geometry(info.Aug) %>%
  dplyr::select(-Street, -SegmentID) %>%
  gather(Variable, Value, -Count)

correlation.long <-
  select_if(st_drop_geometry(info.Aug[, -c(1:2, 30)]), is.numeric) %>%
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
  labs(title = "Count of trips as a function of some features")

#map of some variables
ggplot()+
  geom_sf(data=info.Aug, aes(color=dist.lane))

ggplot()+
  geom_sf(data=info.Aug, aes(color=C2))

ggplot()+
  geom_sf(data=info.Aug, aes(color=nhCount))

####====Regression====####
glimpse(info.Aug)

info.Aug$Centroid <- st_centroid(info.Aug$geometry)

st_coordinates(info.Aug$Centroid)

#Make the regression
reg_b <- lm(Count ~ bikeLaneLv + dist.lane + Number_Tra + StreetWidt + MinorSnowRoute + POSTED_SPE  + TRUCK_ROUT
            + YFrom*isMH + XFrom*isMH + nhCount + citibike.Buffer_small, data=info.Aug)
summary(reg_b)

reg_p <- lm(Count ~ bikeLaneLv + dist.lane + Number_Tra + StreetWidt + MinorSnowRoute + POSTED_SPE  + TRUCK_ROUT
             + YFrom*isMH + XFrom*isMH + nhCount + citibike.Buffer_small , data=info.Aug)
summary(reg_p)

reg_p2<-lm(Count ~ bikeLaneLv + isMH + dist.lane + Number_Tra + StreetWidt + SnowLv + POSTED_SPE + TRUCK_ROUT
           + YFrom*isMH + XFrom*isMH + nhCount + citibike.Buffer_small, data=info.Aug)
summary(reg_p2)

info.Aug_test <- info.Aug %>% mutate_all(~replace(.,is.na(.),0)) %>% st_drop_geometry()
reg_rf <- ranger::ranger(Count ~ bikeLaneLv + isMH + dist.lane + Number_Tra + StreetWidt + MinorSnowRoute + POSTED_SPE + TRUCK_ROUT 
                         + nhCount + citibike.Buffer_small, data=info.Aug_test)
summary(reg_rf)

info.Aug_test$pred <- predict(reg_rf,data = info.Aug_test)$predictions
info.Aug_test <- info.Aug_test %>% mutate(
  Error = pred - Count,
  AbsError = abs(pred - Count),
  APE = ((abs(pred - Count))/pred)
)

ggplot()+geom_histogram(data = info.Aug_test,aes(AbsError),binwidth = 10)

stargazer(reg, type = "html",
          title = "Regression results",
          single.row = TRUE)


#plotting residuals some more
info.Aug$res_reg1 <- residuals(reg)
info.Aug$fitted_reg1<- fitted(reg)

ggplot()+ 
  geom_point(aes(x = obs, y = pred))

ggplot()+
  geom_sf(data=info.Aug, aes(color=res_reg1))

info.Aug$res_reg2 <- residuals(reg_2)
info.Aug$fitted_reg2<- fitted(reg_2)

#select only those with higher value residuals
high_err<- subset(info.Aug, res_reg1>10)

ggplot()+
  geom_sf(data=high_err, aes(color=res_reg1), size = 1.2)

ggplot(info.Aug, aes(x=res_reg1))+
  geom_density()+
  labs(title='Density of Residuals',
       xlab='Residual')

hist(info.Aug$res_reg1, col='skyblue3', breaks = 50, main='Histogram of Residuals')


#### more detailed modeling - train/test and errors ####




#retrain model into training and test sets
# create the training set
## 75% of the sample size
smp_size <- floor(0.75 * nrow(info.Aug))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(info.Aug)), size = smp_size)

train <- info.Aug[train_ind, ]
test <- info.Aug[-train_ind, ]

# Regression  
reg_all <- lm(Count ~ bikeline + dist.lane + C2, data = st_drop_geometry(train))

summary(reg_all)

# test set prediction 
reg_all_predict <- predict(reg_all, newdata = test)

#training set prediction
reg_all_predict_train <- predict(reg_all, newdata = train)

# training MAE
rmse.train <- caret::MAE(reg_all_predict_train, train$Count)

# test MAE
rmse.test  <- caret::MAE(reg_all_predict, test$Count)

test <- cbind(test, reg_all_predict)
train <- cbind(train, reg_all_predict_train)


#add error types for each prediction
test <-
  test %>%
  mutate(Count.Predict = reg_all_predict,
         Count.Error = reg_all_predict - Count,
         Count.AbsError = abs(reg_all_predict - Count),
         Count.APE = (abs(reg_all_predict - Count)) / Count)

train <-
  train %>%
  mutate(Count.Predict = reg_all_predict_train,
         Count.Error = reg_all_predict_train - Count,
         Count.AbsError = abs(reg_all_predict_train - Count),
         Count.APE = (abs(reg_all_predict_train - Count)) / Count)

MAPE_Train <- mean(train$Count.APE)
MAPE_Test <- mean(test$Count.APE)

table4.1.2 <- matrix(data = c(rmse.train, rmse.test, MAPE_Train, MAPE_Test), nrow = 2, ncol = 2)

rownames(table4.1.2) <- c("train", "test")
colnames(table4.1.2) <- c("MAE", "MAPE")

# show table
table4.1.2 %>% kable(caption = "Table 4.1.2 - Training and Testing MAE and MAPE")

# true value vs. predicted value 
preds.train <- data.frame(pred   = reg_all_predict_train,
                          actual = train$Count,
                          source = "training data")
preds.test  <- data.frame(pred   = reg_all_predict,
                          actual = test$Count,
                          source = "testing data")
preds <- rbind(preds.train, preds.test)

#plot of predicted vs observed
ggplot(preds, aes(x = pred, y = actual, color = source)) +
  geom_point() +
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

#skipping cross validation

#spatial distribution of errors
glimpse(test)
# plot errors from the test set on a map
ggplot() +
  geom_sf(data = test, aes(colour = Count.Error),
          show.legend = "point", size = 1) +
  #scale_colour_manual(values = palette5,
  #                    labels=qBr(test,"SalePrice.Error"),
  #                    name="Quintile Breaks\nof Abs Error") +
  labs(title="Spatial Distribution of Residuals from Test Set", 
       subtitle = "Both over-prediction and under-prediction included",
       caption = "Spatial Distribution of Residuals from Test Set")

#testing residuals for spatial lag with moran's i
# 4.3.2 Moran Test and Spatial Lag
#prices
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

#mapping errors
glimpse(test)
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

ggplot()+
  geom_sf(data=boroughs_clip, fill='white', color='white')+
  geom_sf(data=info.Aug, aes(color=res_reg1), size=0.5)+
  scale_color_continuous(type='viridis')+
  labs(title='All Residuals')


glimpse(info.Aug)


# maps of filtering data

