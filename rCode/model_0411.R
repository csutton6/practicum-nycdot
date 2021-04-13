setwd('C:/Users/katee/Box Sync/Practicum/shp/')
#avoid scientific notation
options(scipen = 999)

install.packages('ggcorrplot')
install.packages('spdep')

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

#palettes etc
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


##====Import and Clean Data====##

#import trip data
Aug_pnts <- st_read("tripPoints_Aug20.shp")

#filter out dates in 1-15
Aug_pnts <- Aug_pnts %>%
  mutate(day = as.numeric(substr(rdate, 9, 10)))

Aug_pnts <- Aug_pnts %>%
  filter(day <= 15)

borough<- st_read('https://data.cityofnewyork.us/resource/7t3b-ywvw.geojson')%>%
  st_transform(st_crs(Aug_pnts))

#borough-->not Staten Island
borough <- borough %>%
  filter(boro_name != "Staten Island")

#find out pnts inside 2 boroughs
Aug_pnts <- Aug_pnts[borough,]


#neighborhood DATA
#reference:https://www1.nyc.gov/site/planning/data-maps/open-data/dwn-nynta.page
neighborhood <- st_read('https://data.cityofnewyork.us/resource/q2z5-ai38.geojson')%>%
  st_transform(st_crs(Aug_pnts))

#mapview(neighborhood)

#only neighboor in 4 boro
neighborhood <- neighborhood[borough,]

#Station Extent
extent <- st_read("station_extent_Aug2020.shp") %>%
  st_transform(st_crs(Aug_pnts))

#mapview(station)


##=====Roads and Bikelanes====##
#import Lion bikelane data
bike20d_buffer <- st_read("lion_buffer15/lion_buffer15.shp") %>%
  st_transform(st_crs(Aug_pnts))

#select useful column
bike20d_buffer <- bike20d_buffer %>%
  select(Street, BikeLane, SegmentID, geometry)

#only roads inside four boroughs
#bike20d_buffer <- bike20d_buffer[borough,]

# OR only roads inside citibike extent
bike20d_buffer<- bike20d_buffer[extent,]

mapview(bike20d_buffer_2)

#create a new column of whether it is a bikelane
bike20d_buffer <- bike20d_buffer %>%
  mutate(bikeline = ifelse(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9", "yes", "no"))

#assign NA with no
bike20d_buffer$bikeline[is.na(bike20d_buffer$bikeline)] <- "no"


#other features

##Protected bikelanes
bike20d_buffer <- bike20d_buffer %>%
  mutate(protected = ifelse(BikeLane == "1", "yes", "no"))
#assign NA with no
bike20d_buffer$protected[is.na(bike20d_buffer$protected)] <- "no"


##Unprotected bikelanes
bike20d_buffer <- bike20d_buffer %>%
  mutate(unprotected = ifelse(BikeLane == "2", "yes", "no"))
#assign NA with no
bike20d_buffer$unprotected[is.na(bike20d_buffer$unprotected)] <- "no"

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
info.Aug <- merge(bike20d_buffer, count.Aug)

#find out the number of trips on bikelanes
bikeslines <- bike20d_buffer %>% filter(BikeLane == "1"| BikeLane == "2" | BikeLane == "5" | BikeLane == "9")
bike.Aug <- merge(bikeslines, count.Aug)

#find out the number of trips in each neighborhood
bike.nh <- st_join(info.Aug, neighborhood %>% st_transform(st_crs(info.Aug)), join=st_intersects, left=TRUE)

bike.nh <- bike.nh %>%
  select(ntaname, Count) %>%
  group_by(ntaname) %>%
  summarise(nhCount = sum(Count))




##=====Feature Engineering========##

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



#change projection
info.Aug <- info.Aug %>% st_transform('ESRI:102711')
bike20d_buffer <- bike20d_buffer %>% st_transform('ESRI:102711')
bike.Aug <- bike.Aug %>% st_transform('ESRI:102711')
bikeslines <- bikeslines %>% st_transform('ESRI:102711')
bike.nh <- bike.nh %>% st_transform('ESRI:102711')



##distance to nearest bikelanes

###Distance to nearest bikelanes
info.Aug <- info.Aug %>%
  mutate(dist.lane = nn_function(st_coordinates(st_centroid(info.Aug$geometry)),
                                 st_coordinates(st_centroid(bikeslines$geometry)), 1))




##Biketrips at cloest roads

#function to get cloest idx
nn_idx<- nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <- as.matrix(measureFrom)
  measureTo_Matrix <- as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.index[,k]
  return(nn)  
}

#N1
#get the index
info.Aug <- info.Aug %>%
  mutate(nnidx = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                        st_coordinates(st_centroid(bike.Aug$geometry)), 1))


#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$n1Count[i] =bike.Aug[info.Aug$nnidx[i],]$Count
}

#N2
#get the index
info.Aug <- info.Aug %>%
  mutate(nnidx2 = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                         st_coordinates(st_centroid(bike.Aug$geometry)), 2))


#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$n2Count[i] =bike.Aug[info.Aug$nnidx2[i],]$Count
}


#N3
#get the index
info.Aug <- info.Aug %>%
  mutate(nnidx3 = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                         st_coordinates(st_centroid(bike.Aug$geometry)), 3))

#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$n3Count[i] =bike.Aug[info.Aug$nnidx3[i],]$Count
}


#The number of biketrips in each neighborhood

info.Aug <- st_join(info.Aug, bike.nh, join=st_intersects, left=TRUE)



#the number of biketrips on the surrounding  (closest) 2 roads
info.Aug2 <- info.Aug

#C1
#get the index
info.Aug <- info.Aug %>%
  mutate(cidx = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                       st_coordinates(st_centroid(info.Aug2$geometry)), 1))
#C2
info.Aug <- info.Aug %>%
  mutate(cidx2 = nn_idx(st_coordinates(st_centroid(info.Aug$geometry)),
                        st_coordinates(st_centroid(info.Aug2$geometry)), 2))

#extract count based on index
for (i in 1:nrow(info.Aug)) {
  info.Aug$c1Count[i] =info.Aug2[info.Aug$cidx[i],]$Count
}


for (i in 1:nrow(info.Aug)) {
  info.Aug$c2Count[i] =info.Aug2[info.Aug$cidx2[i],]$Count
}

info.Aug <- info.Aug %>%
  mutate(C2 = c1Count + c2Count)


mapview(info.Aug)

##====Regression====###
#Make the regression
reg <- lm(Count ~ bikeline + dist.lane + nhCount + C2, data=info.Aug)

summary(reg)

stargazer(reg, type = "html",
          title = "Regression results",
          single.row = TRUE)

reg.res = resid(reg)

plot(info.Aug$Count, reg.res)+
  abline(0,0)


#plotting residuals some more
info.Aug$res_reg1 <- residuals(reg)
info.Aug$fitted_reg1<- fitted(reg)

ggplot()+ 
  geom_point(aes(x = obs, y = pred))

ggplot()+
  geom_sf(data=info.Aug, aes(color=res_reg1))

ggplot(info.Aug, aes(x=res_reg1))+
  geom_density()


#### more detailed modeling - train/test and errors ####


#corplot of numeric variables against each other
# create a correlation matrix of all numerical variables
numericVars <- 
  select_if(st_drop_geometry(info.Aug[, -c(1:2, 5)]), is.numeric) %>% 
  na.omit()

glimpse(numericVars)

corr<-round(cor(numericVars), 1)
p.mat<- cor_pmat(numericVars)

ggcorrplot(corr, method='circle')

ggcorrplot(corr, hc.order = TRUE, type='lower', insig='blank')

#correlation plots against dependent var
st_drop_geometry(info.Aug) %>% 
  dplyr::select(Count, bikeline, BikeLane, dist.lane, C2, nhCount) %>%
  gather(Variable, Value, -Count) %>% 
  ggplot(aes(Value, Count)) +
  geom_point(size = .5) + geom_smooth(method = "lm", se=F, colour = "#FA7800") +
  facet_wrap(~Variable, ncol = 3, scales = "free") +
  labs(caption = "Count as a Function Some Numerical Variables")

#map of some variables
ggplot()+
  geom_sf(data=info.Aug, aes(color=dist.lane))

ggplot()+
  geom_sf(data=info.Aug, aes(color=C2))

ggplot()+
  geom_sf(data=info.Aug, aes(color=nhCount))

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

#working with the errors
rmse.train <- caret::MAE(reg_all_predict_train, train$Count)

rmse.test  <- caret::MAE(reg_all_predict, test$Count)

miami_test <- cbind(test, reg_all_predict)
miami_training <- cbind(train, reg_all_predict_train)

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
ggplot(info.Aug, aes(x=lagCount, y=Count)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Count as a function of the spatial lag of count",
       caption = "count as a function of the spatial lag of count",
       x = "Spatial lag of count (Mean count of 5 nearest neighbors)",
       y = "Trip Count") 

#spatial lag vs error
error_lag_plot<- ggplot(test, aes(x=lagCountError, y=Count, label=ntaname)) +
  geom_point(colour = "#FA7800") +
  geom_smooth(method = "lm", se = FALSE, colour = "#25CB10") +
  labs(title = "Error as a function of the spatial lag of count",
       caption = "Error as a function of the spatial lag of count",
       x = "Spatial lag of errors (Mean error of 5 nearest neighbors)",
       y = "Trip Count")
error_lag_plot

glimpse(test)
ggplotly(error_lag_plot,
         tooltip=c('Street', 'bikeline', 'Count', 'lagCountError', 'ntaname'))

ggplotly(error_lag_plot)


