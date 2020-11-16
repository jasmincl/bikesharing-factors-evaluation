library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)
library(sp)
library(leaflet)
library(sf)

# STATIONS ------------------------------------------------------------------------------------
stations_org <-  readRDS("data/stationshh.rds") 
# # used to have some test data, missing lats & lons, wrongly spelled stations (e.g. Grindelberg / Bezirksamt Eimsbuettel)
# # lat & lon is also wrong way in original dataset

# clean up ------------------------------------------------------------------------------
stations_clean <- stations_org %>%
  filter(latitude != "" & longitude != "") %>%
  dplyr::select(rental_zone_hal_id, name, latitude, longitude) %>%
  mutate(
    latitude = gsub(",", ".", latitude),
    longitude = gsub(",", ".", longitude),
    latitude = as.numeric(latitude),
    longitude =  as.numeric(longitude)
  ) 


# add two missing stations
grindel_add <- data.frame(
  "rental_zone_hal_id" = c(138370, 242731),
  "name" = c("Grindelberg / Bezirksamt Eimsbuettel", "Maretstrasse / Baererstrasse"),
  "lon" = c(9.977883, 9.982299),
  "lat" = c(53.575384, 53.453659)
)
grindel_add$name <- as.character(grindel_add$name)

# add to stations data frame
stations <- stations_clean %>%
  dplyr::select(rental_zone_hal_id, name, latitude, longitude) %>%
  dplyr::rename(lat = longitude, lon = latitude) %>%
  mutate(lat = as.numeric(lat),
         lon = as.numeric(lon)) %>%
  bind_rows(grindel_add)

# save
saveRDS(stations, "data/stations_df.rds")

# plot all stations to check
leaflet(stations) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat)

# make sf & spobject for later analysis -----------------------------------------------------------------------------------
stations <- readRDS("data/stations_df.rds")

# to sf, correct projection ------------------------------------------------------------------------------
# projection used is European Terrestrial Reference System 1989 (ETRS89/UTM zone 32N).
xy <- stations[,c(3,4)]

# sp format for plotting
stations_sp <- SpatialPointsDataFrame(coords = xy, data = stations,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# sf format for landuse & routing
stations_sf <- st_transform(st_as_sf(stations_sp, coords = c("lon", "lat")), crs = 25832)

# save both
saveRDS(stations_sf,"data/stations_sf.rds")
saveRDS(stations_sp,"data/stations_sp.rds")

# plot to check
plot(stations_sp)
plot(stations_sf)

# OD-TRIP DATA -------------------------------------------------------------------------------------
#bike <- fread("data/bike.csv")
bike_hh <- fread("data/bike_hh_clean.csv")

# keep relevant variables only, filter for one week for analysis
bike_oneweek_raw <- bike_hh %>%
 # rename(datetime_from = date_from, datetime_to = date_to) %>%
  mutate( # add date & hour variables
    date_from = as.Date(as.POSIXct(datetime_from)),
    date_to = as.Date(as.POSIXct(datetime_to))) %>%
  filter(date_from >= "2016-09-16" & date_to <= "2016-09-30")

# save
saveRDS(bike_oneweek_raw, "data/bike_oneweek_raw.rds")


# add variables & clean up on trip level -------------------------------------------------------------------
bike_oneweek_raw <- readRDS("data/bike_oneweek_raw.rds")

# add trip length
bike_hh_timediff <- bike_oneweek_raw %>%
  mutate(timediff = difftime(as.POSIXct(datetime_to),as.POSIXct(datetime_from),units = "mins"))
saveRDS(bike_hh_timediff, "data/bike_hh_timediff.rds")

# filter out unrealistic durations, add minute & weekday variables
bike_hh_timediff <- readRDS("data/bike_hh_timediff.rds")
bike_trip_level <- bike_hh_timediff %>%
  filter(timediff > 3 & timediff < 300) %>% # filter out unrealistic rental durations
  dplyr::rename(date = date_from)

# only keep trips that have station info added (207 stations)
bike_trip_level_clean <- bike_trip_level %>%
  filter(start_rental_zone_hal_id %in% stations$rental_zone_hal_id) %>%
  filter(end_rental_zone_hal_id %in% stations$rental_zone_hal_id) %>%
  dplyr::rename(from_id = start_rental_zone_hal_id, to_id = end_rental_zone_hal_id)

# save
saveRDS(bike_trip_level_clean, "data/bike_trip_level_clean.rds")


# aggregate to day ------------------------------------------------------------------------------------
bike_trip_level_clean <- readRDS("data/bike_trip_level_clean.rds")
bike_day <- bike_trip_level_clean %>%
  group_by(from_id, to_id, start_rental_zone, end_rental_zone, date) %>%
  dplyr::summarise(trip_count=n())

saveRDS(bike_day, "data/bike_day.rds")

# sanity check
quantile(bike_day$trip_count, seq(0,1,0.01))

# add missing zero trips ------------------------------------------------  
## dataset contains only station combinations where trips took place, add all combination with
## zero trips
stations <- readRDS("data/stations_df.rds")
bike_day <- readRDS("data/bike_day.rds")

# get all trip properties that need to be included as vectors
from_id <- stations$rental_zone_hal_id
to_id <- stations$rental_zone_hal_id
date <- unique(bike_day$date)

# make all combinations
stations_all <- expand.grid(from_id, to_id, 
                            date
                            )
stations_all$Var3 <- as.character(stations_all$Var3)
stations_all$Var3 <- as.Date(stations_all$Var3)

saveRDS(stations_all, "data/stations_all.rds")

# join bike_per_day data frame with expanded grid data frame for missing combinations
bike_complete <- bike_day %>%
  full_join(stations_all, by = c("from_id" = "Var1","to_id" ="Var2", "date" = "Var3")) %>%
  mutate(
    weekday = weekdays(as.Date(date)),
    loop_trip = case_when(
      from_id == to_id ~ 1,
      TRUE ~ 0
    ))

saveRDS(bike_complete, "data/bike_complete.rds")

# check if merge worked
bike_complete <- readRDS("data/bike_complete.rds")

# needs to be TRUE
nrow(bike_complete) - sum(is.na(bike_complete$trip_count)) == nrow(bike_day)
quantile(bike_complete$trip_count)
rm(stations_all)

# correct trip count (zero trips are NA atm)
trip_clean <- bike_complete %>%
  mutate(
    trip_count = replace(trip_count, is.na(trip_count), 0)
    )

quantile(trip_clean$trip_count)
check <- filter(trip_clean, trip_count > 10)
hist(check$trip_count,breaks = 15)

# check for missings
sapply(trip_clean, function(x) sum(is.na(x)))

# save
saveRDS(trip_clean, "data/trip_clean.rds")
