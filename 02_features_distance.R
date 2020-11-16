#install.packages("cyclestreets")
library(cyclestreets) # https://cran.r-project.org/web/packages/cyclestreets/cyclestreets.pdf
# network analysis
library(ggmap)
library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(sf)
library(sp)
library(tmap)    # for static and interactive maps
library(stplanr)

# TODO: get right dataset from server
# load data ---------------------------------------------------------------------------------------------------
stations <- readRDS("data/stationshh.rds")

# ROUTE DISTANCE -------------------------------------------------------------------------

bike_df_clean <- readRDS("data/bike_df_clean.rds")
# remove loop trips (not needed for bike infrastructure)
bike_inter <- bike_df_clean %>%
  filter(from_id != to_id) %>%
  dplyr::group_by(from_id, to_id) %>%
  dplyr::summarise(trip_count = mean(trip_count))
rm(bike_df_clean, bike_df, bike_features, M1)

# stations as spatial object -------------------------------------------------------------------------
stations_coord <- stations
coordinates(stations_coord) <- ~lon+lat

# create straight connection
desire_lines = od2line(bike_inter, stations_coord)

# convert to sf (newer format)
desire_lines_sf <- st_as_sf(desire_lines)
saveRDS(desire_lines_sf, "data/desire_lines_sf.rds")
# get routes ------------------------------------------------------------------------------
# get API-key for cyclestreets.net
api_cycle <- Sys.getenv("CYCLESTREETS")

# use cyclestreets.net API to get shortest bicycle route for each station-pair
# source: https://geocompr.robinlovelace.net/transport.html
routes <- line2route(
  desire_lines_sf,
  route_fun = stplanr::route_cyclestreets,
  pat = api_cycle
)

desire_lines_sf$geom_bike <- st_geometry(routes)
saveRDS(desire_lines_sf, "data/desire_lines_sf_routes.rds")
saveRDS(routes, "data/routes.rds")
# plot routes ------------------------------------------------------------------------------
# make only with subset, otherwise kills server
leaflet(routes[1,]) %>%
  addTiles() %>%
  addPolylines() #%>%
  addMarkers(stations$lon, stations$lat)#, color = "red")

# sanity check ----------------------------------------------------------------------------
  # explore timediff (rental time)  -------------------------------------------------------------------------
  bike_trip_level_clean <- readRDS("data/bike_trip_level_clean.rds") # df with timediff and every sep trip
  
# do rental times & bike time work? 
# merge bike_od with route data

# get time for routes from API as variable 
desire_lines_sf$route_time <- (routes$time/60)

# merge trip-data with routed data
route_time <- desire_lines_sf %>%
  ungroup() %>%
  dplyr::select(from_id,to_id, geom_bike, route_time) 

st_length(routes[1,])

route_time$dist_route <- st_length(route_time$geom_bike)
saveRDS(route_time, "data/distance_time_route.rds")


## GREAT CIRCLE DISTANCE ------------------------------------------------------------------
# other distances to compare with route time
bike_features <- readRDS("data/bike_features.rds")


stations_clean <- stations %>% dplyr::select(rental_zone_hal_id, lon, lat)
station_pairs <- bike_features %>%
  dplyr::select(from_id, to_id) %>%
  distinct() %>%
  left_join(stations_clean, by = c("from_id" = "rental_zone_hal_id")) %>%
  dplyr::rename(from_lon = lon, from_lat = lat) %>%
  left_join(stations_clean, by = c("to_id" = "rental_zone_hal_id")) %>%
  dplyr::rename(to_lon = lon, to_lat = lat)

station_from_sf <- st_as_sf(station_pairs, coords = c("from_lon", "from_lat"))
station_to_sf <- st_as_sf(station_pairs, coords = c("to_lon", "to_lat"))
station_pairs$from_geom <- st_geometry(station_from_sf)
station_pairs$to_geom <- st_geometry(station_to_sf)

glimpse(station_pairs)
saveRDS(station_pairs, "data/station_pairs.rds")
station_pairs <- readRDS("data/station_pairs.rds")

bike_list <- split(station_pairs, as.factor(station_pairs$from_id))

bike_loop_list <- lapply(bike_list, function(x){
  print(unique(x$from_id))
  x$dist_circle <- st_distance(x$from_geom, x$to_geom, by_element = TRUE)
  return(x)
})

stations_dist_circle <- ldply(bike_loop_list, data.frame)

saveRDS(stations_dist_circle, "data/stations_distcircle.rds")

# COMBINE -----------------------------------------------------------
## read in both distance dfs & clean
stations_dist_circle <- readRDS( "data/stations_distcircle.rds")
stations_dist_circle_clean <- stations_dist_circle %>%
  dplyr::select(from_id, to_id, dist_circle)
st_geometry(stations_dist_circle_clean) <- NULL
glimpse(stations_dist_circle_clean)

station_time_route_clean <- station_time_route %>%
  dplyr::select(from_id, to_id, route_time, dist_route)
st_geometry(station_time_route_clean) <- NULL
glimpse(station_time_route_clean)

# join all distances  ------------------------------------------------------
station_distances <- station_time_route %>%
  left_join(stations_dist_circle_clean, by = c("from_id", "to_id"))
glimpse(station_distances)  

# remove geometry  ------------------------------------------------------
st_geometry(station_distances)  <- NULL
station_distances$geom_bike <- NULL
saveRDS(station_distances, "data/station_distances.rds")

# get some measures to check sanity of time difference
hist(route_info$route_time_difference, breaks = 100)
quantile(route_info$route_time_difference, seq(0,1,0.05))
mean(route_info$route_time_difference) # mean of difference between rented time & estimated bike path time ~ 1.5 min
median(route_info$route_time_difference) # median of difference between rented time & estimated bike path time ~ 1.5 min
sd(route_info$route_time_difference)

# get cycle paths -------------------------------------------------------
# with geofabrik
library(sf)
roads_org <- shapefile("data/roads/gis_osm_roads_free_1.shp")

# transform to sf object & set correct map projection
roads_sf <- st_transform(st_as_sf(roads_org), crs = "+proj=longlat +datum=WGS84")

# keep cycle ways only
cycle_sf <- roads_sf %>%
  dplyr::filter(fclass == "cycleway") %>%
  dplyr::select(osm_id, geometry)

saveRDS(cycle_sf, "data/cycle_sf.rds")

cycle_sf <- readRDS("data/cycle_sf.rds")

# get data frame which contains route of shortest paths & transform to correct format
desire_lines_sf <- readRDS("data/desire_lines_sf_routes.rds")
st_geometry(desire_lines_sf) <- NULL

bike_routes <- desire_lines_sf %>%
  dplyr::select(from_id, to_id)
st_geometry(bike_routes) <- desire_lines_sf$geom_bike


bike_routes <- st_set_crs(bike_routes, "+proj=longlat +datum=WGS84")
bike_routes <- st_transform(st_as_sf(bike_routes), crs = "+proj=longlat +datum=WGS84")

## check if projection is the same
st_crs(bike_routes) == st_crs(cycle_sf)

# get all overlays of shortest routes & bike paths
cycle_route_intersect_raw <- st_intersection(bike_routes, cycle_sf)

# some conversion is needed for mrege
cycle_route_intersect <- cycle_route_intersect_raw %>%
  st_cast("MULTIPOINT") %>%
  st_cast("LINESTRING") %>% st_cast("MULTILINESTRING")

# calculate length of each cycle way that intersects with shortest routes
cycle_route_intersect$length_cycleway <- st_length(cycle_route_intersect$geometry)

# do not need geometry anymore
st_geometry(cycle_route_intersect) <- NULL

# calculate length of overall route
bike_routes$length_route <- st_length(bike_routes$geometry)

# do not need geometry anymore
st_geometry(bike_routes) <- NULL
saveRDS(cycle_route_intersect, "data/cycle_intersect.rds")

# aggregate all intersecting bike paths by from and to station id to get all bike paths per 
## each shortest path
cycle_route_intersect_agg <- cycle_route_intersect %>%
  group_by(from_id, to_id) %>%
  dplyr::summarise(length_cycleway = sum(as.numeric(length_cycleway)))

# join shortest routes & cycle paths
bike_routes_path <- bike_routes %>%
  left_join(cycle_route_intersect_agg, by = c("from_id", "to_id")) %>%
  mutate(length_route = as.numeric(length_route),
         length_cycleway = as.numeric(length_cycleway),
         length_cycleway = replace(length_cycleway, is.na(length_cycleway),0),
         perc_cylceway = (length_cycleway/length_route)*100, # calculate percentage
         perc_cylceway = replace(length_cycleway, length_cycleway > length_route),50)
  dplyr::select(-length_route)

  saveRDS(bike_routes_path, "data/bike_routes_path.rds")

  
  
# distance to city center & within city centre ---------------------------------------------------------------
  city_center_df <- data.frame("lat" = 53.550005, "lon" = 9.990657)
  
  xy <- city_center_df[,c("lon", "lat")]
  
  spdf <- SpatialPointsDataFrame(coords = xy, data = city_center_df,
                                 proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  city_center_df <- st_transform(st_as_sf(spdf, coords = c("lon", "lat")), crs = 25832)
  
  city_buff = st_buffer(city_center_df, dist = 1500)
  
  ggplot(landuse_small) +
    #geom_sf(color = "blue", fill = "blue") +
    geom_sf(data = city_buff, color = "red") +
    geom_sf(data = stations_buff200m, fill = alpha("yellow", 0.2)) #+ #,  fill = alpha("yellow", 0.5)) +
  geom_sf(data= intersection_raw, color = "red", fill = "red")   
  
  
  city_center_var <- city_center_df[rep(seq_len(nrow(city_center_df)), each = 208),]
  
  stations_sf$city_center <- st_geometry(city_center_var)
  stations_sf$dist_city_center <-   as.numeric(st_distance(st_geometry(stations_sf), stations_sf$city_center, by_element = T))
  
  # add dummy variable in city center not in city center
  stations_sf <- stations_sf %>%
    mutate(in_city_center = case_when(
      dist_city_center <= 2000 ~ 1,
      TRUE ~ 0
    ) )
  
  city_center_final <- dplyr::select(stations_sf, rental_zone_hal_id, dist_city_center, in_city_center)
  st_geometry(city_center_final) <- NULL
  
  saveRDS(city_center_final, "data/city_center_final.rds")