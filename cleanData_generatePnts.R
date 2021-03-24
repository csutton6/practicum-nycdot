# boroughs
borough <- st_read("OtherData/BoroughBoundaries/Boroughs.shp") %>% st_transform(crs=4326)
brooklyn <- borough %>% filter(boro_name == "Brooklyn") %>% st_transform(crs=4326)
brooklyn_L <- st_cast(brooklyn,"MULTILINESTRING")
manhattan <- borough %>% filter(boro_name == "Manhattan") %>% st_transform(crs=4326)
manhattan_L <- st_cast(manhattan,"MULTILINESTRING")
queens <- borough %>% filter(boro_name == "Queens") %>% st_transform(crs=4326)
queens_L <- st_cast(queens,"MULTILINESTRING")
bronx <- borough %>% filter(boro_name == "Bronx") %>% st_transform(crs=4326)
bronx_L <- st_cast(bronx,"MULTILINESTRING")


# ridership in August 2019
Aug2019 <- st_read("BikeTrips/slicedData/ride2019Aug/ride2019Aug_sf.shp") %>% st_transform(crs = 4326)

# road infrastructure (15 feet road buffer)
roads <- st_read("OtherData/roadBuffer/lion_buffer15/lion_buffer15.shp") %>% st_transform(crs = 4326)
roads <- roads %>% select(Street,StreetCode,SegmentID)

# delete dupilicated rows
road15 <- distinct(roads,SegmentID,.keep_all = T)
rm(roads)

# clip trips and clean data
clipToBoundary <- function(outline,polygon,trip){
  outliers <- trip[outline,]
  clippedData <- trip[polygon,] %>% filter(!(id %in% outliers$id))
  return(clippedData)
}
# clip and clean trip data in Brooklyn in 2019 August.
trip_Broo_198<- clipToBoundary(brooklyn_L,brooklyn,Aug2019) %>% st_transform(crs=3857)
# Do the same thing for other boroughs


# write data to .shp and do the following steps in ArcGIS
st_write(trip_Broo_198,"trip_Broo_198.shp")

# Next steps;
# in ArcGIS:
# Generate points along Line:
#   input Features:trip_Broo_198.shp
#   output Feature class: give it a name
#   point placement: by distance
#   distance: 150 Meter
#   check the box of "include end points"



