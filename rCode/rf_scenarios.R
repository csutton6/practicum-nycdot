####====Regression====####

options(scipen = 999)
rm(list = ls())
Sys.setenv(LANG = "en")

#Package installs -------------------------------------------------------------
load.fun <- function(x) { 
  x <- as.character(x) 
  if(isTRUE(x %in% .packages(all.available=TRUE))) { 
    eval(parse(text=paste("require(", x, ")", sep=""))) 
    print(paste(c(x, " : already installed; requiring"), collapse=''))
  } else { 
    #update.packages()
    print(paste(c(x, " : not installed; installing"), collapse=''))
    eval(parse(text=paste("install.packages('", x, "')", sep=""))) 
    print(paste(c(x, " : installed and requiring"), collapse=''))
    eval(parse(text=paste("require(", x, ")", sep=""))) 
  } 
} 

########### Required Packages ###########
packages = c("bayesplot", "lme4", "rstan", "shinystan", "RcppEigen",
             "tidyverse", "tidyr", "AmesHousing", "broom", "caret", "dials", "doParallel", "e1071", "earth",
             "ggrepel", "glmnet", "ipred", "klaR", "kknn", "pROC", "rpart", "randomForest",
             "sessioninfo", "tidymodels","ranger", "recipes", "workflows", "themis","xgboost",
             "sf", "nngeo", "mapview")

for(i in seq_along(packages)){
  packge <- as.character(packages[i])
  load.fun(packge)
}

#load data
load("C:/Users/pc/Downloads/model_clean_xl03.RData")
load("C:/Users/pc/Downloads/randomF_workflow4.RData")

rm(list=setdiff(ls(), "info.Aug"))
info.Aug <- info.Aug %>% filter(Count<400)
selectedP <- c("bikeLaneLv" , 'dist.lane' , 'Number_Tra' , 'StreetWidt' , 'TRUCK_ROUT' , 'citibike.Buffer_small' , 'isMH' ,
               'pWhite' , 'MedRent' , 'pNoVeh' , 'pSubway' , 
               'sidewalk_caf_nn3'  , 'bigPark' , 'isGreenway' , 'subway_nn5' ,
               'jobs_in_tract','density_retail','density_office','ntaname',"avgCount5")
selectedF <- c("bikeLaneLv" , 'dist.lane' , 'Number_Tra' , 'StreetWidt' , 'TRUCK_ROUT' , 'citibike.Buffer_small' , 'isMH' ,
               'pWhite' , 'MedRent' , 'pNoVeh' , 'pSubway' , 
               'sidewalk_caf_nn3'  , 'bigPark' , 'isGreenway' , 'subway_nn5' ,
               'jobs_in_tract','density_retail','density_office','ntaname','Count',"avgCount5",'SegmentID')
#Check NAs
info.Aug.model <- info.Aug %>% st_drop_geometry() %>% dplyr::select(selectedF)
info.Aug.geo <- info.Aug %>% dplyr::select(SegmentID,geometry)
sum(is.na(info.Aug.model))

#Create dataset without NAs
data <- info.Aug.model %>%
  drop_na()
#Set seed to make your practive producible
set.seed(717)

#set theme
theme_set(theme_bw())


"%!in%" <- Negate("%in%")
g <- glimpse


#Slipt the data into training and testing sets
data_split <- rsample::initial_split(data, strata = "Count", prop = 0.75)
train.set <- rsample::training(data_split)
test.set  <- rsample::testing(data_split)

train.set <- train.set %>% dplyr::select(selectedP,Count) 
test.set <- test.set %>% dplyr::select(selectedP,Count) 

cv_splits <- rsample::vfold_cv(train.set,  strata = "Count", k = 10)
print(cv_splits)


#Create recipe
model_rec <- recipe(Count ~ ., data = train.set) %>% #the "." means using every variable we have in the training dataset
  update_role(ntaname, new_role = "ntaname") %>% #This is more like to keep the neighborhood variable out of the model
  step_other(ntaname, threshold = 0.005) %>%
  step_dummy(all_nominal(),-ntaname) %>%
  step_zv(all_predictors()) %>%
  step_center(dist.lane, StreetWidt,MedRent,sidewalk_caf_nn3,
              subway_nn5, jobs_in_tract,density_retail,density_office,avgCount5) %>%
  step_scale(dist.lane, StreetWidt,MedRent,sidewalk_caf_nn3,
             subway_nn5, jobs_in_tract,density_retail,density_office,avgCount5) #%>% #put on standard deviation scale

library(poissonreg)
lm_plan <- 
  poisson_reg() %>% 
  set_engine("glm")

glmnet_plan <- 
  parsnip::linear_reg() %>% 
  parsnip::set_args(penalty  = tune()) %>%
  parsnip::set_args(mixture  = tune()) %>%
  parsnip::set_engine("glmnet")

rf_plan <- parsnip::rand_forest() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 2000) %>% 
  parsnip::set_engine("ranger", importance = "impurity") %>% 
  parsnip::set_mode("regression")

XGB_plan <- parsnip::boost_tree() %>%
  parsnip::set_args(mtry  = tune()) %>%
  parsnip::set_args(min_n = tune()) %>%
  parsnip::set_args(trees = 100) %>% 
  parsnip::set_engine("xgboost") %>% 
  parsnip::set_mode("regression")



# GridSearch
glmnet_grid <- expand.grid(penalty = seq(0, 1, by = .25), 
                           mixture = seq(0,1,0.25))

rf_grid <- expand.grid(mtry = c(3,6,9), 
                       min_n = c(100,500,800))
xgb_grid <- expand.grid(mtry = c(3,6,9), 
                        min_n = c(100,500,800))



#Create workflow
lm_wf <-
  workflows::workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(lm_plan)

glmnet_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(glmnet_plan)

rf_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(rf_plan)

xgb_wf <-
  workflow() %>% 
  add_recipe(model_rec) %>% 
  add_model(XGB_plan)


# fit model to workflow and calculate metrics
control <- tune::control_resamples(save_pred = TRUE, verbose = TRUE)

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits,#cv_splits_geo
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))


show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)


##===============Predict the test set=============####
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)


rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- data.frame(dplyr::select(rf_best_OOF_preds, .pred, Count), model = "RF") %>%
  mutate(#.pred = exp(.pred),
    #Count = exp(Count),
    RMSE = yardstick::rmse_vec(Count, .pred),
    MAE  = yardstick::mae_vec(Count, .pred),
    MAPE = yardstick::mape_vec((Count+1), .pred)) %>% 
  ungroup()


#Scenarios

scenarios <- st_read("E:/NYCDOT/Scenarios/scenariosShort.shp")

neighborhoods <- st_read("E:/NYCDOT/NYCneighborhood/nynta.shp")
mapview(list(scenarios, scenario1))

scenario1_range <- neighborhoods %>% filter(NTACode == "MN99")

scenario1 <- scenarios[scenario1_range,] %>% 
  filter(SegmentID != "0137647" & SegmentID != "0134012" &
           SegmentID != "0150953" & SegmentID != "0150951" &
           SegmentID != "0071072" & SegmentID != "0071082")

scenario1 <- scenario1 %>%
  filter(SegmentID != "0111014" & SegmentID != "0071078" & SegmentID != "0108162")

scenario1 <- scenario1 %>%
  filter(SegmentID != "9000012")

scenario1 <- scenario1 %>%
  filter(SegmentID != "0071084")

scenario1 <- scenario1 %>%
  filter(SegmentID != "9000011")

mapview(scenario_23)


scenario_23 <- scenarios %>%
  filter(! SegmentID %in% scenario1$SegmentID)

boro <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp")

boro <- boro %>%
  filter(boro_name == "Brooklyn")
boro <- boro %>% st_transform(st_crs(scenario_23))

boro2 <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp")
boro2 <- boro2 %>%
  filter(boro_name == "Manhattan" | boro_name == "Bronx")
boro2 <- boro2 %>% st_transform(st_crs(scenario_23))

mapview(list(boro2, scenario_23))


scenario2 <- scenario_23[boro2,]


scenario3 <- scenario_23 %>%
  filter(! SegmentID %in% scenario2$SegmentID)

sample1 <- info.Aug %>%
  filter(SegmentID %in% scenario1$SegmentID)

mapview(list(scenario3, scenario2), color=list("red", "blue"))
mapview(list(scenario2, boro))

sample2 <- info.Aug %>%
  filter(SegmentID %in% scenario2$SegmentID)

sample3 <- info.Aug %>% filter(SegmentID %in% scenario3$SegmentID)

mapview(list(scenario1, scenario2, scenario3), color = list("red", "blue", "purple"))

st_write(scenario3, "scenario3.shp")


mapview(list(sample1, sample2, sample3), color = list("red", "blue", "purple"))

sample1 <- sample1 %>%
  filter(bikeLaneLv == "noBikeLane")
scenario1 <- scenario1 %>%
  filter(SegmentID %in% sample1$SegmentID)

mapview(sample2, zcol="bikeLaneLv")

mapview(sample3, zcol="bikeLaneLv")

sample3 <- sample3 %>%
  filter(bikeLaneLv != "Protected")

scenario3 <- scenario3 %>%
  filter(SegmentID %in% sample3$SegmentID)

#read buffered
scenario1_buffer <- st_read("E:/NYCDOT/Scenarios/buffered/scenario1_Buffer.shp")
scenario2_buffer <- st_read("E:/NYCDOT/Scenarios/buffered/scenario2_Buffer.shp")
scenario3_buffer <- st_read("E:/NYCDOT/Scenarios/buffered/scenario3_Buffer.shp")

mapview(list(scenario1_buffer, scenario2_buffer, scenario3_buffer))

#### Predictions

#reference:https://hansjoerg.me/2020/02/09/tidymodels-for-machine-learning/
Brooklyn <- boro %>% st_transform(st_crs(info.Aug))

Manhattan <- boro2 %>%
  filter(boro_name == "Manhattan") %>% st_transform(st_crs(info.Aug))

info.Brooklyn <- info.Aug[Brooklyn,] 

info.Manhattan <- info.Aug[Manhattan,]

info.Bk_model <- info.Brooklyn %>% st_drop_geometry() %>% dplyr::select(selectedF)
info.M_model <- info.Manhattan %>% st_drop_geometry() %>% dplyr::select(selectedF)

sample1 <- sample1 %>% dplyr::select(selectedF)
sample2 <- sample2 %>% dplyr::select(selectedF)
sample3 <- sample3 %>% dplyr::select(selectedF)

#Predict scenario1

rf_Manhattan_fit <- fit(rf_best_wf, data = info.M_model)

rf_final_Mfit <- pull_workflow_fit(rf_Manhattan_fit)
dia_rec3_M    <- pull_workflow_prepped_recipe(rf_Manhattan_fit)

#make different versions of sample 1 data
sample1_protect <- sample1 %>%
  mutate(bikeLaneLv = "Protected") %>% st_drop_geometry()
sample1_unprotect <- sample1 %>%
  mutate(bikeLaneLv = "Unprotected") %>% st_drop_geometry()


#make predictions

#Scenario1
sample1_protect$.pred <- predict(rf_final_Mfit, 
                          new_data = bake(dia_rec3_M , sample1_protect))$.pred


sample1_unprotect$.pred <- predict(rf_final_Mfit, 
                                 new_data = bake(dia_rec3_M , sample1_unprotect))$.pred

#join back and calculate diff
sample1$pre_protect <- sample1_protect$.pred
sample1 <- sample1 %>%
  mutate(diff_protect = ((pre_protect - Count) / Count) * 100)

sample1$pre_unprotect <- sample1_unprotect$.pred
sample1 <- sample1 %>%
  mutate(diff_unprotect = ((pre_unprotect - Count) / Count) * 100)

sample1.info <- sample1 %>%
  dplyr::select(SegmentID, Count, pre_protect, diff_protect, pre_unprotect, diff_unprotect)

sample1.info <- sample1.info %>% st_transform(st_crs(scenario1))

#join back to road segment
scenario1_info <- merge(scenario1, sample1.info %>% st_drop_geometry(), by="SegmentID", left=TRUE)

scenario1_info <- distinct(scenario1_info,SegmentID,.keep_all = T)

scenario1_info <- scenario1_info %>%
  dplyr::select(Street, SegmentID,Count,pre_protect, diff_protect, pre_unprotect, diff_unprotect)

scenario1_info <- scenario1_info %>% st_transform(st_crs("EPSG:4326"))

mapview(scenario1_info) 



scenario1_infonew <- st_join(scenario1_buffer %>% st_transform(st_crs(sample1.info)),
                             sample1.info, left=TRUE, join = st_intersects)

scenario1_infonew <- distinct(scenario1_infonew,SegmentID.x,.keep_all = T)

scenario1_infonew <- scenario1_infonew %>%
  dplyr::select(Street, SegmentID.x,Count,pre_protect, diff_protect, pre_unprotect, diff_unprotect)

scenario1_infonew <- scenario1_infonew %>% st_transform(st_crs("EPSG:4326"))

names(scenario1_infonew)[2] <- "SegmentID"

st_write(scenario1_infonew, "scenario1.geojson")







##Predict Scenario2

#make different versions of sample 1 data
sample2_protect <- sample2 %>%
  mutate(bikeLaneLv = "Protected") %>% st_drop_geometry()

sample2_protect$.pred <- predict(rf_final_Mfit, 
                                 new_data = bake(dia_rec3_M ,sample2_protect))$.pred

sample2_protect <- sample2_protect %>%
  mutate(Count, ifelse(Count == 0, 0.01, Count))

names(sample2_protect)[24] <- "Count_c"

#join back and calculate diff
sample2$pre_protect <- sample2_protect$.pred
sample2$Count_c <- sample2_protect$Count_c

sample2 <- sample2 %>%
  mutate(diff_protect = ((pre_protect - Count) / Count_c) * 100)

sample2.info <- sample2 %>%
  dplyr::select(SegmentID, Count, pre_protect, diff_protect)

#change back to segment

scenario2_info <- st_join(scenario2_buffer %>% st_transform(st_crs(sample2.info)),
                             sample2.info, left=TRUE, join = st_intersects)

scenario2_info <- distinct(scenario2_info, SegmentID.x,.keep_all = T)

scenario2_info <- scenario2_info %>%
  dplyr::select(Street, SegmentID.x,Count,pre_protect, diff_protect)

scenario2_info <- scenario2_info %>% st_transform(st_crs("EPSG:4326"))

names(scenario2_info)[2] <- "SegmentID"

mapview(scenario2_info)

st_write(scenario2_info, "scenario2.geojson")


#Scenario3
rf_Brooklyn_fit <- fit(rf_best_wf, data = info.Bk_model)

rf_final_Bfit <- pull_workflow_fit(rf_Brooklyn_fit)
dia_rec3_B    <- pull_workflow_prepped_recipe(rf_Brooklyn_fit)

#make different versions of sample 3 data
mapview(sample3, zcol="bikeLaneLv")

sample3_protect <- sample3 %>%
  mutate(bikeLaneLv = "Protected") %>% st_drop_geometry()

sample3_unprotect <- sample3 %>%
  filter(bikeLaneLv != "Unprotected") %>%
  mutate(bikeLaneLv = "Unprotected") %>% st_drop_geometry()


#make predictions

#Scenario1
sample3_protect$.pred <- predict(rf_final_Bfit, 
                                 new_data = bake(dia_rec3_B, sample3_protect))$.pred


sample3_unprotect$.pred <- predict(rf_final_Bfit, 
                                   new_data = bake(dia_rec3_B, sample3_unprotect))$.pred


sample3_protect <- sample3_protect %>%
  mutate(Count, ifelse(Count == 0, 0.01, Count))

names(sample3_protect)[24] <- "Count_c"


sample3_unprotect <- sample3_unprotect %>%
  mutate(Count, ifelse(Count == 0, 0.01, Count))

names(sample3_unprotect)[24] <- "Count_c"

#join back and calculate diff
sample3$pre_protect <- sample3_protect$.pred
sample3$Count_c <-sample3_protect$Count_c

sample3 <- sample3 %>%
  mutate(diff_protect = ((pre_protect - Count) / Count_c) * 100)

sample3 <- merge(sample3, sample3_unprotect %>% dplyr::select(SegmentID, .pred),
                 by="SegmentID", all.x=TRUE)

sample3 <- sample3 %>%
  mutate(diff_unprotect = ((.pred - Count) / Count_c) * 100)

names(sample3)[26] <- "pre_unprotect"


sample3$pre_unprotect[is.na(sample3$pre_unprotect)] <- 0
sample3$diff_unprotect[is.na(sample3$diff_unprotect)] <- 0

sample3.info <- sample3 %>%
  dplyr::select(SegmentID, Count, pre_protect, diff_protect, pre_unprotect, diff_unprotect)



#change back to segment

scenario3_info <- st_join(scenario3_buffer %>% st_transform(st_crs(sample3.info)),
                          sample3.info, left=TRUE, join = st_intersects)

scenario3_info <- distinct(scenario3_info, SegmentID.x,.keep_all = T)

scenario3_info <- scenario3_info %>%
  dplyr::select(Street, SegmentID.x,Count, pre_protect, diff_protect, pre_unprotect, diff_unprotect)

scenario3_info <- scenario3_info %>% st_transform(st_crs("EPSG:4326"))

names(scenario3_info)[2] <- "SegmentID"

mapview(scenario3_info)

st_write(scenario3_info, "scenario3.geojson")