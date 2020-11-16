# combine all features

# add station level variables ----------------------------------------------------------------------------------------
## read in all data sets
city_center <- readRDS("data/city_center_final.rds")
landuse_old <- readRDS("data/landuse_features.rds")
landuse <- readRDS("data/lu_long.rds")
pois <- readRDS("data/pois_final.rds")
stations_close <- readRDS("data/stations_close_feature.rds")
demography <- readRDS("data/stations_demo_pop.rds")
population <- readRDS("data/stations_population.rds")
route_time <- readRDS("data/distance_time_route.rds")

## clean them up & only keep variables needed for analysis
route_time_final <- route_time %>%
  dplyr::select(from_id, to_id, route_time, dist_route)
st_geometry(route_time_final) <- NULL

demo_final <- demography %>%
  dplyr::rename(citizen_DE = n_pop_STAATSANGE_GRP_1_sum) %>%
  dplyr::mutate(citizen_notDE = 100 - citizen_DE) %>%
  ungroup() %>%
  dplyr::select(rental_zone_hal_id, citizen_notDE)

landuse_add <- data.frame( ## add station not intersecting with any landuse
    "rental_zone_hal_id" = 218635,
    "park" = 0,
    "residential" = 0,
    "mixed" = 0,
    "commercial" = 0,
    "other" = 0
    )

landuse_final <- landuse %>%
  bind_rows(landuse_add)

pois_final <- pois %>%
  mutate( # make categories smaller
    n_close_transport = n_close_bus_stop+ 
      n_close_rail_stop+
      n_close_ferry_terminal,
    n_close_leisure =
      n_close_leisure_culture+ 
      n_close_nightlife+ 
      n_close_tourism+ 
      n_close_food+ 
      n_close_sports,
  ) %>%
  dplyr::select(-n_close_government, -n_close_bus_stop, 
                  -n_close_rail_stop, -n_close_leisure_culture, 
                  -n_close_nightlife,  -n_close_tourism, 
                  -n_close_food, -n_close_sports,
                  -n_close_ferry_terminal, -n_close_accommodation) %>%
  data.frame()


population_final <- population %>%
  ungroup() %>%
  dplyr::select(rental_zone_hal_id, population)

## join all station-level features into one data frame
station_features <- demo_final %>%
  left_join(landuse_old, by = c("rental_zone_hal_id" = "rental_zone_hal_id")) %>%
  left_join(city_center, by = c("rental_zone_hal_id" = "rental_zone_hal_id"))%>%
  left_join(pois_final, by = c("rental_zone_hal_id" = "rental_zone_hal_id"))%>%
  left_join(stations_close, by = c("rental_zone_hal_id" = "rental_zone_hal_id"))%>%
  left_join(population_final, by = c("rental_zone_hal_id" = "rental_zone_hal_id")) %>%
  dplyr::rename(id = rental_zone_hal_id) 

## create two data frames for origin and destination for merge
station_features_from <- station_features
station_features_to <- station_features
colnames(station_features_to) <- paste0("to_", colnames(station_features_to))
colnames(station_features_from) <- paste0("from_", colnames(station_features_from))

# sanity check
names(station_features_from)
names(station_features_to)


# merge with trip data -----------------------------
trip_clean <- readRDS("data/trip_clean.rds")


trip_joined <- trip_clean %>%
  left_join(station_features_from, by = "from_id") %>%
  left_join(station_features_to, by = "to_id") %>%
  left_join(route_time_final, by = c("from_id", "to_id")) %>%
  mutate( 
    route_time = replace(route_time, is.na(route_time), 0),
    dist_route = replace(dist_route, is.na(dist_route), 0),
    reachable_30 = case_when( # add reachable within 30 minutes dummy
      route_time >30 ~ 0,
      route_time <= 30  ~ 1, 
      TRUE ~ route_time
    ),
    weekday_cat = case_when( # add weekend variable
      weekday == "Sunday" ~ "weekend",
      weekday == "Saturday" ~ "weekend",
      TRUE ~ "weekday"
    )
    ) %>% # filter for wrong station pairs
  filter(from_id != 245452 & to_id != 245452) %>%
  filter(from_id != 218635 & to_id != 218635) 


# check missings (only name is missing now)
sapply(trip_features, function(x) sum(is.na(x)))

saveRDS(trip_features, "data/trip_features.rds")

# scale for better fit ---------------------------------------------------------
trip_features <- readRDS("data/trip_features.rds")

# scaling function to substract mean & divide by standard deviation
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# scale numeric variables
trip_features <- trip_features %>%
  mutate(
     from_perc_residential_scaled = scale_this(from_perc_residential),
     to_perc_residential_scaled = scale_this(to_perc_residential),
     from_perc_commercial_scaled = scale_this(from_perc_commercial),
     to_perc_commercial_scaled = scale_this(to_perc_commercial),
     from_perc_retail_scaled = scale_this(from_perc_retail),
     to_perc_retail_scaled = scale_this(to_perc_retail),
     from_perc_industrial_scaled = scale_this(from_perc_industrial),
     to_perc_industrial_scaled = scale_this(to_perc_industrial),
     from_closest_station_dist_scaled = scale_this(from_closest_station_dist),  
     to_closest_station_dist_scaled = scale_this(to_closest_station_dist),        
     from_dist_city_center_scaled = scale_this(from_dist_city_center),
     to_dist_city_center_scaled = scale_this(to_dist_city_center),       
     from_population_scaled = scale_this(from_population),
     to_population_scaled = scale_this(to_population),             
     route_time_scaled = scale_this(route_time),
     from_n_close_transport_scaled = scale_this(from_n_close_transport),
     from_n_close_shopping_scaled = scale_this(from_n_close_shopping),
     from_n_close_education_scaled = scale_this(from_n_close_education),
     from_n_close_health_scaled = scale_this(from_n_close_health),
     from_n_close_leisure_scaled = scale_this(from_n_close_leisure),  
     to_n_close_leisure_scaled = scale_this(to_n_close_leisure),
     to_n_close_transport_scaled = scale_this(to_n_close_transport),
     to_n_close_shopping_scaled = scale_this(to_n_close_shopping),
     to_n_close_education_scaled = scale_this(to_n_close_education),
     to_n_close_health_scaled = scale_this(to_n_close_health)
  ) 

saveRDS(trip_features, "data/trip_features.rds")
