####=== EastHarlem ===###
##=======Cleaning Data===========##
#Clip Tirp Data in EastHarlem

Sep18 <- st_read("E:/NYCDOT/rideData_cleaned/Sep2018/Sep18_cleaned.shp")
Sep19 <- st_read("E:/NYCDOT/rideData_cleaned/Sep2019/Sep19_cleaned.shp")

#EastHarlem shp file
EastHarlem <- st_read("E:/NYCDOT/Feb23/data/East_Harlem_CD/East_Harlem_CD.shp") %>%
  st_transform(st_crs(Sep18))

#clip trip data inside EastHarlem
EastHarlem_line <- st_cast(EastHarlem, "MULTILINESTRING")

#find out intersection points
Sep18_int <- Sep18[EastHarlem_line,]
Sep19_int <- Sep19[EastHarlem_line,]

#only in EastHarlem
Sep18_e <- Sep18[EastHarlem,] %>%
  filter(!id %in% Sep18_int$id)

Sep19_e <- Sep19[EastHarlem,] %>%
  filter(!id %in% Sep19_int$id)

mapview(Sep19_e)


#save --> Do Line to Point Transformation in ArcGIS
st_write(Sep18_e, "Sep18_eh.shp", driver = "ESRI Shapefile")
st_write(Sep19_e, "Sep19_eh.shp", driver = "ESRI Shapefile")


#PlotTheme
plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.2),
  axis.ticks=element_blank())

##=========Import Data=======##
#import pnts data
Sep18_eh_pnts <- st_read("E:/NYCDOT/DiffinDiff/trips_eh_pnts/Sep18_eh_pnts.shp")
Sep19_eh_pnts <- st_read("E:/NYCDOT/DiffinDiff/trips_eh_pnts/Sep19_eh_pnts.shp")


#import EastHarlem shp file
EastHarlem <- st_read("E:/NYCDOT/Feb23/data/East_Harlem_CD/East_Harlem_CD.shp") %>%
  st_transform(st_crs(Sep18_eh_pnts))

#import bikelanes
#control
eh_control <- st_read("E:/NYCDOT/DiffinDiff/EastHarlem_Control/buffer/EastHarlem_Control_Buffer.shp") %>%
  st_transform(st_crs(Sep18_eh_pnts))
#treatment
eh_treat <- st_read("E:/NYCDOT/DiffinDiff/EastHarlem_Treatment/buffer/EastHarlem_Treatment_Buffer.shp") %>%
  st_transform(st_crs(Sep18_eh_pnts))


#select used columns
eh_control <- eh_control %>%
  dplyr::select(OBJECTID, Street, Shape_Le_1, geometry)

eh_treat <- eh_treat %>%
  dplyr::select(NodeIDTo_1, Street_1, Shape_Le_3, geometry)


##=====Find out trips fall within Bike routes=======##
ctrl.inrpnts_Sep18 <- Sep18_eh_pnts[eh_control,]
treat.inrpnts_Sep18 <- Sep18_eh_pnts[eh_treat,]

ctrl.inrpnts_Sep19 <- Sep19_eh_pnts[eh_control,]
treat.inrpnts_Sep19 <- Sep19_eh_pnts[eh_treat,]


##========Allocate Street Names, IDs on trip points=============##
#allocate Street Name on roads

ctrl.inrpnts_Sep18 <- st_join(ctrl.inrpnts_Sep18,eh_control,join = st_intersects, left = TRUE)
treat.inrpnts_Sep18 <- st_join(treat.inrpnts_Sep18, eh_treat,join = st_intersects, left = TRUE)


ctrl.inrpnts_Sep19 <- st_join(ctrl.inrpnts_Sep19,eh_control,join = st_intersects, left = TRUE)
treat.inrpnts_Sep19 <- st_join(treat.inrpnts_Sep19, eh_treat,join = st_intersects, left = TRUE)


##==============Group Points inside Same Street and Same ID as linestring======###

#Sep18
ctrl.pnts2Line_Sep18 <- ctrl.inrpnts_Sep18 %>%
  group_by(id, Street, OBJECTID) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

treat.pnts2Line_Sep18 <- treat.inrpnts_Sep18 %>%
  group_by(id, NodeIDTo_1, Street_1) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

#Sep19
ctrl.pnts2Line_Sep19 <- ctrl.inrpnts_Sep19 %>%
  group_by(id, Street, OBJECTID) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

treat.pnts2Line_Sep19 <- treat.inrpnts_Sep19 %>%
  group_by(id, NodeIDTo_1, Street_1) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")


#===Count the number of trips on Each Street===#
#Sep18
ctrl.ntrips_Sep18 <- ctrl.pnts2Line_Sep18 %>%
  st_drop_geometry() %>%
  group_by(OBJECTID, Street) %>%
  summarise(Count_Sep18 = n())


treat.ntrips_Sep18 <- treat.pnts2Line_Sep18 %>%
  st_drop_geometry() %>%
  group_by(NodeIDTo_1, Street_1) %>%
  summarise(Count_Sep18 = n())

#Sep19
ctrl.ntrips_Sep19 <- ctrl.pnts2Line_Sep19 %>%
  st_drop_geometry() %>%
  group_by(OBJECTID, Street) %>%
  summarise(Count_Sep19 = n())


treat.ntrips_Sep19 <- treat.pnts2Line_Sep19 %>%
  st_drop_geometry() %>%
  group_by(NodeIDTo_1, Street_1) %>%
  summarise(Count_Sep19 = n())



##============Diff in Diff Table==================##

#Treatment
treatment <- merge(treat.ntrips_Sep18, treat.ntrips_Sep19, all=TRUE)
treatment[is.na(treatment)] <- 0
treatment <- treatment %>%
  mutate(Pre = Count_Sep18, Post = Count_Sep19) %>%
  dplyr::select(-Count_Sep18, -Count_Sep19) %>%
  mutate(Diff = Post - Pre)

#Control
control <- merge(ctrl.ntrips_Sep18, ctrl.ntrips_Sep19, all=TRUE)
control[is.na(control)] <- 0
control <- control %>%
  mutate(Pre = Count_Sep18, Post = Count_Sep19) %>%
  dplyr::select(-Count_Sep18, -Count_Sep19) %>%
  mutate(Diff = Post - Pre)



treatment %>%
  summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff))

control %>%
  summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff))


#Diff in Diff
diff <- rbind(treatment %>%
                summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff)),
              control %>%
                summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff)))
rownames(diff) <- c("Treatment", "Control")
diff <- diff %>% rbind(list(diff[2, 1]- diff[1, 1], diff[2, 2] - diff[1, 2], diff[2, 3] - diff[1, 3]))
rownames(diff)[3] <- "Difference"

#Vis
ggplot() + geom_segment(aes(x=1, xend=2,y=3786, yend=7046, color="Treatment"), size=1.1)+
  geom_segment(aes(x=1, xend=2, y=1809, yend=2679, color="Control"), size=1.1)+
  geom_point(aes(x=1, y=3786), size=3)+
  geom_point(aes(x=2, y=7046), size=3)+
  geom_point(aes(x=1, y=1809), size=3) +
  geom_point(aes(x=2, y=2679), size=3)+
  labs(title = "Differences between Treatment and Control Groups",
       y = "Trip Counts") + plotTheme +
  scale_x_continuous(breaks = seq(0, 1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())




###===Lower Manhattan===###

##Clean Data
# read in neighbor boundary
chelsea <- st_read("E:/NYCDOT/DiffinDiff/Chelsea/Chel_large_boundary.shp") %>% st_transform(crs = 4326)
chelsea_LINE <- st_cast(chelsea,"MULTILINESTRING")

# read in road buffer
Ave7 <- st_read("E:/NYCDOT/DiffinDiff/7thAve_Treatment/7thAve_Treatment_bff15.shp") %>% st_transform(crs = 4326) %>% select(contains("_"))
Ave3 <- st_read("E:/NYCDOT/DiffinDiff/3rdAve_Control/3rdAve_Control_bff15.shp") %>% st_transform(crs = 4326)
park <- st_read("E:/NYCDOT/DiffinDiff/ParkAve_Control/ParkAve_Control_bff151.shp") %>% st_transform(crs = 4326)


#read ridership data

Sep18 <- st_read("E:/NYCDOT/rideData_cleaned/Sep2018/Sep18_cleaned.shp")
Sep19 <- st_read("E:/NYCDOT/rideData_cleaned/Sep2019/Sep19_cleaned.shp")


clipToBoundary <- function(outline,polygon,trip){
  outliers <- trip[outline,]
  clippedData <- trip[polygon,] %>% filter(!(id %in% outliers$id))
  return(clippedData)
}


chel_05_2018 <- clipToBoundary(chelsea_LINE,chelsea,Sep18) %>% st_transform(crs=3857)
chel_06_2018 <- clipToBoundary(chelsea_LINE,chelsea,Sep19) %>% st_transform(crs=3857)


st_write(chel_05_2018, "chelsea_sep18.shp")
st_write(chel_06_2018, "chelsea_sep19.shp")


##==Analysis==##
pnts_chelsea18 <- st_read("E:/NYCDOT/DiffinDiff/trips_ch_pnts/chelsea_sep18_pnts.shp") %>% st_transform(crs=4326)
pnts_chelsea19 <- st_read("E:/NYCDOT/DiffinDiff/trips_ch_pnts/chelsea_sep19_pnts.shp") %>% st_transform(crs=4326)

Ave7_18 <- pnts_chelsea18[Ave7,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)
Ave7_19 <- pnts_chelsea19[Ave7,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)

Ave3_18 <- pnts_chelsea18[Ave3,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)
Ave3_19 <- pnts_chelsea19[Ave3,] %>% mutate(pnt_id = seq.int(nrow(.)))%>% st_transform(crs = 4326)

clnp_A7_18 <- Ave7_18 %>% st_join(.,Ave7,join = st_intersects, left = T)
clnp_A7_19 <- Ave7_19 %>% st_join(.,Ave7,join = st_intersects, left = T)

clnp_A3_18 <- Ave3_18 %>% st_join(.,Ave3,join = st_intersects, left = T)
clnp_A3_19 <- Ave3_19 %>% st_join(.,Ave3,join = st_intersects, left = T)


countTrip <- function(pnt){
  tripCount <- pnt %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    group_by(id,SegmentID_) %>% 
    summarise(pntCount = n()) %>% 
    group_by(SegmentID_) %>% 
    summarise(tripCount = n())
  return(tripCount)
}


countTrip2 <- function(pnt){
  tripCount <- pnt %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    group_by(id,SegmentID) %>% 
    summarise(pntCount = n()) %>% 
    group_by(SegmentID) %>% 
    summarise(tripCount = n())
  return(tripCount)
}

trip_chel_18 <- countTrip(clnp_A7_18)
trip_chel_19 <- countTrip(clnp_A7_19)

colnames(trip_chel_18) <- c("SegmentID", "Count18")
colnames(trip_chel_19) <- c("SegmentID", "Count19")

cntrl_18 <- countTrip2(clnp_A3_18)
cntrl_19 <- countTrip2(clnp_A3_19) 

colnames(cntrl_18) <- c("SegmentID", "Count18")
colnames(cntrl_19) <- c("SegmentID", "Count19")

##Diff in Diff Table
#Treatment
treatment <- merge(trip_chel_18, trip_chel_19, all=TRUE)
treatment <- treatment %>%
  mutate(Pre = Count18, Post = Count19) %>%
  select(-Count18, -Count19) %>%
  mutate(Diff = Post - Pre)

control <- merge(cntrl_18,cntrl_19, all=TRUE)
control <- control %>%
  mutate(Pre = Count18, Post = Count19) %>%
  select(-Count18, -Count19) %>%
  mutate(Diff = Post - Pre)


treatment %>%
  summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff))

control %>%
  summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff))


diff <- rbind(treatment %>%
                summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff)),
              control %>%
                summarise(Pre = sum(Pre), Post = sum(Post), Difference = sum(Diff)))

rownames(diff) <- c("Treatment", "Control")
diff <- diff %>% rbind(list(diff[2, 1]- diff[1, 1], diff[2, 2] - diff[1, 2], diff[2, 3] - diff[1, 3]))
rownames(diff)[3] <- "Difference"


#Vis
ggplot() + geom_segment(aes(x=1, xend=2,y=3786, yend=8397, color="Treatment"), size=1.1)+
  geom_segment(aes(x=1, xend=2, y=1713, yend=2903, color="Control"), size=1.1)+
  geom_point(aes(x=1, y=3786), size=3)+
  geom_point(aes(x=2, y=8397), size=3)+
  geom_point(aes(x=1, y=1713), size=3) +
  geom_point(aes(x=2, y=2903), size=3)+
  labs(title = "Differences between Treatment and Control Groups",
       y = "Trip Counts") + plotTheme +
  scale_x_continuous(breaks = seq(0, 1))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
