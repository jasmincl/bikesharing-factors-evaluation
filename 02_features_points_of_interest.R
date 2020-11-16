# distance to POIs (transport, leisure)

library(sp)
library(RANN)
library(rgdal)
library(tidyverse)
library(tidyr)
library(geosphere)
library(leaflet)

# DATA PREP ------------------------------------------------------------------------------------
source("src/functions_nn2.R")
# transport  ------------------------------------------------------------------------------------
# from transport get: 5601 - railway_station, 5602 - railway_halt, 5603 - tram_stop, 5621 - bus_stop, 
# 5622 - bus_station, 5651 - airport,
# 5661 - ferry_terminal

# shapefile from geofabrik download for Hamburg
transport_org <- shapefile("data/osm/gis_osm_transport_free_1.shp")

# bring in right format
transport_df <- cbind(transport_org@data, transport_org@coords)
code_vector <- c(5601:5603, 5621, 5622, 5651, 5661)

# filter for correct codes & clean
transport <- transport %>%
  filter(code %in% code_vector) %>%
  mutate(fclass_categories = case_when(
    grepl("bus", fclass) ~ "bus_stop",
    grepl("rail", fclass) ~ "rail_stop",
    TRUE ~ as.character(fclass)
  )) %>%
  dplyr::select(-fclass, -name)

# pois ------------------------------------------------------------------------
# get: 2005 - post_office, 2007 - library, 2008 - town_hall, 2009 - courthouse, 2012 - community_centre,
# 2013 - nursing_home, 2014 - arts_centre, 2016 - market_place, 2081 - 84 (uni, school, kindergarten, college)
# 2099 - public_building, 2101 - 2121 - health (pharmacy, hospital, doctor, dentist)
# leisure: 2201-2206: theatre, nightclub, cinema, park playground, dog_park
# sports: 2251 - sports_centre, 2252 - pitch, 2253 - swimming_pool 54 - tennis, 55 golf, 56 -stadium
# catering: 2301 - restaurant, 2302 - fast_food, 2303 - cafe, 2304 -pub, 2305 - bar, 2307 - biergarten
# accommodation - 2401 - 2406 (hotel, motel, hostel, ...)
# shopping: 2501 - 2561 : supermarkets, malls, kiosk, bakery ...
# bank 2601
# tourism: 2721 - 2744

poi_org <- shapefile("data/osm/gis_osm_pois_free_1.shp")
code_vector_poi <- c(2005, 2007:2009, 2012:2014, 2081:2084, 2099, 2101:2121, 2201:2206, 2251:2256,2301:2305,
        2307, 2401:2406, 2501:2561, 2601, 2721:2744)

# bring in correct format
poi_df <- cbind(poi_org@data, poi_org@coords)

# remove unnecessary pois & categorize them in larger categories
poi <- poi_df %>%
  filter(code %in% code_vector_poi) %>%
  filter(code != 2083 & code != 2012 & code != 2724 
         & code != 2099 & code != 2013) %>% # remove unimportant codes
  mutate(cat = "poi", # recode into larger catgories
         fclass_categories = case_when(
           code > 2720 & code < 2745 ~ "tourism", 
           code > 2080 & code < 2085 ~ "education", 
           code > 2303 & code < 2306 | code == 2202 ~ "nightlife", 
           code > 2300 & code < 2304 ~ "food", 
           code > 2250 & code < 2257 ~ "sports", 
           code > 2101 & code < 2122 ~ "health", 
           code > 2500 & code < 2562 | code == 2601 | code == 2101 | code == 2005 ~ "shopping", 
           code > 2400 & code < 2407 ~ "accommodation", 
           code > 2200 & code < 2207 & !(code == 2202)| code == 2307 | code == 2014~ "leisure_culture", 
           code > 2006 & code < 2010 | code == 2012 ~ "government", 
           TRUE ~ as.character(fclass)
         )) %>%
  dplyr::select(-name, -fclass)

# check number of POIs in each category
poi_catn <- poi %>%
  #group_by(code, fclass, fclass_detailed) %>% 
  group_by(fclass_detailed) %>% 
  summarise(n = n()) #%>% 
  ungroup() %>% 
  mutate(n_detailed = n())

 
# combine all in one data frame ----------------------------------------------------------------------------
points_all <- poi %>%
  bind_rows(list(pofw, transport)) %>%
  dplyr::rename(poi_lon = coords.x1, poi_lat = coords.x2)

saveRDS(points_all, "data/points_all.rds")

## GET CLOSEST POIS  ----------------------------------------------------------------------------
## use nearest neighbor algorithm to find closest points around stations
stations <- readRDS("data/stationshh.rds")

# prepare data 
## need ids for merge later
stations <- set_center_id(stations)
points_all <- set_close_point_id(points_all)

# self-written function (see below)
stations.xy <- nn2_prep(stations, "lon", "lat")
poi.xy <- nn2_prep(points_all, "poi_lon", "poi_lat")

# find all points within radius -----------------------------------------------------------------
## output: df with 3 columns: station_id & id of close poi & distance to that poi
pois_nn2 <- closest_radius(stations.xy, poi.xy, stations, points_all, 400, 500)

## add station & poi information
pois_info <- pois_nn2 %>%
  left_join(stations, by = "center_id") %>%
  left_join(points_all, by = "close_point_id") %>%
  dplyr::select(rental_zone_hal_id, close_point_id, fclass_categories, close_point_dist)

# clean & get distance to closest POI of each category
pois_close <- pois_info %>%
  group_by(rental_zone_hal_id, fclass_categories) %>%
  mutate(n_close = n()) %>%
  top_n(-1, close_point_dist) %>%
  dplyr::select(-close_point_id)

# bring data set in wide format (one colum for each category + distance and number of that category close)
pois_wide <- pois_close %>%
  pivot_wider(names_from = fclass_categories,
              values_from = c(n_close, close_point_dist)
              ) %>%
  # replace all missing categories with 0
  mutate_at(vars(contains("n_close")), ~replace(., is.na(.), 0)) %>%
  # replace all missing distances with 400
  mutate_at(vars(contains("close_point_dist")), ~replace(., is.na(.), 400))

saveRDS(pois_wide, "data/pois_final.rds")

# GET CLOSE STATIONS  ------------------------------------------------------------------------------------
# get inside stations data frame all closest stations to each other
stations <- readRDS("data/stationshh.rds")
stations$center_id <- seq.int(1,nrow(stations))
stations$close_point_id <- seq.int(1,nrow(stations))
stations.xy <- convert_to_utm(stations, "lon", "lat")

# find all points within radius -----------------------------------------------------------------
## output: df with 3 columns: station_id & id of close poi & distance to that poi
stations_nn2 <- closest_radius_distance(stations.xy, stations.xy, stations, stations, 8000, 208)

stations_close <- stations_nn2 %>%
  left_join(stations, by = "center_id") %>%
  dplyr::select(-close_point_id.y) %>%
  dplyr::rename(close_point_id = close_point_id.x) %>%
  filter(center_id != close_point_id) %>%
  group_by(rental_zone_hal_id, name, lon, lat) %>%
  top_n(-1,close_point_dist)

stations_close_feature <- stations_close %>%#
  ungroup() %>%
  dplyr::select(rental_zone_hal_id, close_point_dist) %>%
  dplyr::rename(closest_station_dist = close_point_dist)

saveRDS(stations_close_feature, "data/stations_close_feature.rds")
stations_close_feature <- readRDS("data/stations_close_feature.rds")


## FUNCTIONS ------------------------------------------------------------------------------------
# SET ROW_ID
set_center_id <- function(df) {
  df$center_id <- seq.int(1,nrow(df))
  return(df)
}
set_close_point_id <- function(df) {
  df$close_point_id <- seq.int(1,nrow(df))
  return(df)
}

# PREP BREAKS ---------------------------------------------------------------------------------

nn2_prep <- function(df, lonchar, latchar) {
  ## a longitude to its UTM zone
  long2UTM <- function(long) {
    (floor((long + 180)/6) %% 60) + 1
  }
  
  ## Assuming that all points are within a zone (within 6 degrees in longitude ~ 600 km),
  z <- long2UTM(df[1,lonchar])
  
  ## convert the df lat/long coordinates to UTM for the computed zone
  df2 <- df
  coordinates(df2) <- c(lonchar, latchar)
  proj4string(df2) <- CRS("+proj=longlat +datum=WGS84")
  
  df.xy <- spTransform(df2, CRS(paste0("+proj=utm +zone=",z," ellps=WGS84")))
  
  return(df.xy)
}

# FIND CLOSE POINTS WITHIN RADIUS -----------------------------------------------------------------------------------------
## convert the points lat/long coordinates to UTM for the computed zone
## using the other Josh O'Brien linked answer

closest_radius <- function(df1.xy, df2.xy, df1, df2, radius, nn_number) {
  
  # return df with two columns, 1 is row_id of center point, 2 is matching ids of close points within certain radius
  
  # df1: coordinates to find closest points to
  # df2: list of points to search through and check if they are closest
  
  # calc nn2 --------------------------------------------------------------------
  ## find the nearest neighbor in df2.xy@coords for each df1.xy@coords
  # look for 10 nearest neighbors within 500 m
  res <- nn2(df2.xy@coords, df1.xy@coords, nn_number, searchtype = "radius", radius = radius)
  ## res$nn.dist is a matrix of k neighbors of the distance (in m) to the nearest pois.xy@coords for each breaks.xy@coords
  ## res$nn.idx is a matrix of k neighbors of indices to pois.xy of the nearest pois.xy@coords for each breaks.xy@coords
  
  
  # clean nn2 output ---------------------------------------------------------------------------------
  
  # output is a matrix so with k columns 
  res_idx_org <- res$nn.idx %>% data.frame()
  ## get distance matrix also
  res_dist_org <- res$nn.dist %>% data.frame()
  
  # first check what the maximum # of POIs is
  res_idx_org$close_points_count <- rowSums(res_idx_org!=0)
  res_dist_org$close_points_count <- rowSums(res_idx_org!=0)
  print(max(res_idx_org$close_points_count))
  # would have been enough to calculate nn2 for max(..) nearest neigbors
  
  # if ----------------------------------------------------------------------------
  # only do if pois are close:
  if(max(res_idx_org$close_points_count) > 0) {
    
    ## for ids
    # I only need the neighbors found so for each center I only want the columns != 0
    res_idx <- res_idx_org[,1:max(res_idx_org$close_points_count)]
    res_dist <- res_dist_org[,1:max(res_dist_org$close_points_count)]
    
    # add row id
    res_idx$center_id <- seq.int(nrow(res_idx))
    res_dist$center_id <- seq.int(nrow(res_dist))
    
    # add column names
    res_ids_gathered <- res_idx %>%
      gather(key = old, value = "close_point_id", -center_id) %>%
      filter(close_point_id != 0) %>%
      dplyr::select(-old)
    
    # add column names
    res_dist_gathered <- res_dist %>%
      gather(key = old, value = "close_point_dist", -center_id) %>%
      filter(close_point_dist < 400) %>%
      dplyr::select(-old, -center_id) 
    
    res_dist_ids <- cbind(res_ids_gathered, res_dist_gathered)
    
    return(res_dist_ids)
    
  } else {print("no nearest points found.")}# end if
} # end function

