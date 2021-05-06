####====Regression====####

options(scipen = 999)
rm(list = ls())

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


#read in data from feature engineering
load("C:/Users/xinti/Box/MUSA_800_Practicum/Data/RHistoryData/model_clean_xl03.RData")

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

theme_set(theme_bw())

"%!in%" <- Negate("%in%")
g <- glimpse

#Slipt the data into training and testing sets
data_split <- rsample::initial_split(data, strata = "Count", prop = 0.75)
train.set <- rsample::training(data_split)
test.set  <- rsample::testing(data_split)

train.set <- train.set %>% dplyr::select(selectedP,Count) 
# %>% mutate(cvID = sample(round(nrow(train.set) / 1415), size=nrow(train.set), replace = TRUE))
test.set <- test.set %>% dplyr::select(selectedP,Count) 
# %>% mutate(cvID = sample(round(nrow(test.set) / 472), size=nrow(test.set), replace = TRUE))
# cv_splits <- rsample::group_vfold_cv(train.set,  strata = "Count", group = "cvID")
# print(cv_splits)
# cv_splits_geo <- rsample::group_vfold_cv(train.set,  strata = "Count", group = "ntaname")
# print(cv_splits_geo)
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


#Build the model
# lm_plan <- 
#   parsnip::linear_reg() %>% 
#   parsnip::set_engine("lm")

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



lm_tuned <- lm_wf %>%
  fit_resamples(.,
                resamples = cv_splits,#cv_splits_geo
                control   = control,
                metrics   = metric_set(rmse, rsq))

glmnet_tuned <- glmnet_wf %>%
  tune_grid(.,
            resamples = cv_splits,#cv_splits_geo
            grid      = glmnet_grid,
            control   = control,
            metrics   = metric_set(rmse, rsq))

rf_tuned <- rf_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits,#cv_splits_geo
                  grid      = rf_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

xgb_tuned <- xgb_wf %>%
  tune::tune_grid(.,
                  resamples = cv_splits,#cv_splits_geo
                  grid      = xgb_grid,
                  control   = control,
                  metrics   = metric_set(rmse, rsq))

show_best(lm_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(glmnet_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(rf_tuned, metric = "rmse", n = 15, maximize = FALSE)
show_best(xgb_tuned, metric = "rmse", n = 15, maximize = FALSE)

lm_best_params     <- select_best(lm_tuned, metric = "rmse", maximize = FALSE)
glmnet_best_params <- select_best(glmnet_tuned, metric = "rmse", maximize = FALSE)
rf_best_params     <- select_best(rf_tuned, metric = "rmse", maximize = FALSE)
xgb_best_params    <- select_best(xgb_tuned, metric = "rmse", maximize = FALSE)

# Pull best hyperparam preds from out-of-fold predictions
lm_best_OOF_preds <- collect_predictions(lm_tuned) 
glmnet_best_OOF_preds <- collect_predictions(glmnet_tuned) %>% 
  filter(penalty  == glmnet_best_params$penalty[1] & mixture == glmnet_best_params$mixture[1])
rf_best_OOF_preds <- collect_predictions(rf_tuned) %>% 
  filter(mtry  == rf_best_params$mtry[1] & min_n == rf_best_params$min_n[1])

xgb_best_OOF_preds <- collect_predictions(xgb_tuned) %>% 
  filter(mtry  == xgb_best_params$mtry[1] & min_n == xgb_best_params$min_n[1])


OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, Count), model = "lm"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, Count), model = "RF")) %>% 
  group_by(model) %>% 
  mutate(#.pred = exp(.pred),
    #Count = exp(Count),
    RMSE = yardstick::rmse_vec(Count, .pred),
    MAE  = yardstick::mae_vec(Count, .pred),
    MAPE = yardstick::mape_vec((Count+1), .pred)) %>% 
  ungroup()

# average error for each model
ggplot(data = OOF_preds %>% 
         dplyr::select(model, MAPE) %>% 
         distinct() , 
       aes(x = model, y = MAPE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = paste0(round(MAPE,1),"%"))) +
  theme_bw()

ggplot(data = OOF_preds %>% 
         dplyr::select(model, MAE) %>% 
         distinct() , 
       aes(x = model, y = MAE, group = 1)) +
  geom_path(color = "red") +
  geom_label(aes(label = round(MAE,1))) +
  theme_bw()

ggplot(lm_best_OOF_preds, aes(y=.pred, x = Count))+ 
  geom_point() +
  coord_equal() +
  geom_abline(color = "red") +
  geom_smooth(method="lm") +
  theme_bw()

yardstick::mae_vec(lm_best_OOF_preds$Count, lm_best_OOF_preds$.pred)

##===============Predict the test set=============####

#Final workflow
lm_best_wf     <- finalize_workflow(lm_wf, lm_best_params)
glmnet_best_wf <- finalize_workflow(glmnet_wf, glmnet_best_params)
rf_best_wf     <- finalize_workflow(rf_wf, rf_best_params)
xgb_best_wf    <- finalize_workflow(xgb_wf, xgb_best_params)

lm_val_fit_geo <- lm_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))
glmnet_val_fit_geo <- glmnet_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

rf_val_fit_geo <- rf_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

xgb_val_fit_geo <- xgb_best_wf %>% 
  last_fit(split     = data_split,
           control   = control,
           metrics   = metric_set(rmse, rsq))

####################################Model Validation
# collect validation set predictions from last_fit model
lm_val_pred_geo     <- collect_predictions(lm_val_fit_geo)
glmnet_val_pred_geo <- collect_predictions(glmnet_val_fit_geo)
rf_val_pred_geo     <- collect_predictions(rf_val_fit_geo)
xgb_val_pred_geo    <- collect_predictions(xgb_val_fit_geo)

# Aggregate OOF predictions (they do not overlap with Validation prediction set)
OOF_preds <- rbind(data.frame(dplyr::select(lm_best_OOF_preds, .pred, Count), model = "lm"),
                   data.frame(dplyr::select(glmnet_best_OOF_preds, .pred, Count), model = "glmnet"),
                   data.frame(dplyr::select(rf_best_OOF_preds, .pred, Count), model = "RF"),
                   data.frame(dplyr::select(xgb_best_OOF_preds, .pred, Count), model = "xgb")) %>% 
  group_by(model) %>% 
  mutate(#.pred = exp(.pred),
         #Count = exp(Count),
         RMSE = yardstick::rmse_vec(Count, .pred),
         MAE  = yardstick::mae_vec(Count, .pred),
         MAPE = yardstick::mape_vec((Count+1), .pred)) %>% 
  ungroup()










