### Following the codes of random forest

###===Scenarios===##

##Read Data
scenarios <- st_read("E:/NYCDOT/Scenarios/scenariosShort.shp")
neighborhoods <- st_read("E:/NYCDOT/NYCneighborhood/nynta.shp")
boro <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp")

boro <- boro %>%
  filter(boro_name == "Brooklyn")
boro <- boro %>% st_transform(st_crs(scenarios))

boro2 <- st_read("E:/NYCDOT/Borough Boundaries/geo_export_4c045526-dcb9-4e1a-b602-59b848e20e6a.shp")
boro2 <- boro2 %>%
  filter(boro_name == "Manhattan" | boro_name == "Bronx")
boro2 <- boro2 %>% st_transform(st_crs(scenarios))

##Find and Clean Range
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


scenario_23 <- scenarios %>%
  filter(! SegmentID %in% scenario1$SegmentID)
scenario2 <- scenario_23[boro2,]


scenario3 <- scenario_23 %>%
  filter(! SegmentID %in% scenario2$SegmentID)

#check
mapview(list(scenario1, scenario2, scenario3), color = list("red", "blue", "purple"))

#Make Samples
sample1 <- info.Aug %>%
  filter(SegmentID %in% scenario1$SegmentID)

sample2 <- info.Aug %>%
  filter(SegmentID %in% scenario2$SegmentID)

sample3 <- info.Aug %>% filter(SegmentID %in% scenario3$SegmentID)

#Edit
sample1 <- sample1 %>%
  filter(bikeLaneLv == "noBikeLane")
scenario1 <- scenario1 %>%
  filter(SegmentID %in% sample1$SegmentID)

#check
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
scenario1_infonew <- st_join(scenario1_buffer %>% st_transform(st_crs(sample1.info)),
                             sample1.info, left=TRUE, join = st_intersects)

scenario1_infonew <- distinct(scenario1_infonew,SegmentID.x,.keep_all = T)

scenario1_infonew <- scenario1_infonew %>%
  dplyr::select(Street, SegmentID.x,Count,pre_protect, diff_protect, pre_unprotect, diff_unprotect)

scenario1_infonew <- scenario1_infonew %>% st_transform(st_crs("EPSG:4326"))

names(scenario1_infonew)[2] <- "SegmentID"

st_write(scenario1_infonew, "scenario1.geojson")



##==Predict Scenario2

#make different versions of sample 1 data
sample2_protect <- sample2 %>%
  mutate(bikeLaneLv = "Protected") %>% st_drop_geometry()

sample2_protect$.pred <- predict(rf_final_Mfit, 
                                 new_data = bake(dia_rec3_M ,sample2_protect))$.pred

sample2_protect <- sample2_protect %>%
  mutate(Count, ifelse(Count == 0, 0.01, Count)) #avoid INF

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


##==Predict Scenario3
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

#Scenario3
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
