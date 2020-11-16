# read in land use file and make data frame from it
library(tidyverse)
library(rgdal)
library(lubridate)
library(leaflet)
library(ggplot2)
library(magrittr)
library(ggmap)
library(raster)
library(maptools)
library(sf)

# load original dfs ------------------------------------------------------------------------------
landuse_org <- shapefile("data/landusehh/landuse/gis_osm_landuse_a_free_1.shp")
stations_sf <- readRDS("data/stations_sf.rds")

# to sf, correct projection ------------------------------------------------------------------------------
# projection used is European Terrestrial Reference System 1989 (ETRS89/UTM zone 32N).
landuse_sf <- st_transform(st_as_sf(landuse_org), crs = 25832)

# check if projection is the same -> convert correctly
#st_crs(stations_sf) <- "+proj=longlat +datum=WGS84 +no_defs"
st_crs(stations_sf) == st_crs(landuse_sf)

saveRDS(landuse_sf,"data/landuse_sf.rds")

# get data  ------------------------------------------------------------------------------
landuse_sf <- readRDS("data/landuse_sf.rds")
stations_sf <- readRDS("data/stations_sf.rds")

# create buffer around station  -------------------------------------
stations_buff200m = st_buffer(stations_sf, dist = 200)

# create IDs to identify intersecting polygons
stations_buff200m$station_id <- seq.int(nrow(stations_buff200m))
landuse_sf$landuse_id <- seq.int(nrow(landuse_sf))

# plot to check
ggplot(landuse_sf) +
  geom_sf(fill = "blue") +
  #geom_sf(data = city_center_df, color = "red") +
  geom_sf(data = stations_buff200m, fill = alpha("yellow", 0.2)) #+ #,  fill = alpha("yellow", 0.5)) +
  geom_sf(data= intersection_raw, color = "red", fill = "red")   
  
# get intersections  -------------------------------------
intersection_raw <- st_intersection(stations_buff200m, landuse_sf)

# add in an area size column to the tibble (area of each landuse poly inside a point buffer)
intersection_raw$area_intersection <- as_tibble(st_area(intersection_raw$geometry))

# add area & landuse type variables -------------------------------------
service_area <- st_area(stations_buff200m[1,]$geometry)

# add intersecting area to stations and caclualte percentages
stations_landuse_info <- intersection_raw %>%
  dplyr::select(rental_zone_hal_id, name, landuse_id, area_intersection, geometry) %>%
  dplyr::mutate(area_station_buffer400 = rep(as.numeric(service_area), nrow(.)), # add area of buffer zone around station
         area_intersection = as.numeric(st_area(geometry)), # add area of intersecting landuse inside station radius
         perc_intersection = (area_intersection/area_station_buffer400)*100)  # calculate percentage

# check whether precentages work
check <- intersection_raw %>%
  dplyr::select(station_id, landuse_id, geometry) %>%
  group_by(landuse_id) %>% dplyr::mutate(n = n()) %>%
  filter(landuse_id == 3)

# clean up ---------------------------------------------------------------
# get one column of percentage of each landuse type in station radius
# recode into larger categories
landuse_df <- landuse_sf
st_geometry(landuse_df) <- NULL
stations_landuse_info_df <- stations_landuse_info
st_geometry(stations_landuse_info_df) <- NULL

landuse_stations_clean <- stations_landuse_info_df %>%
  left_join(landuse_df, by = "landuse_id") %>%
  dplyr::select(rental_zone_hal_id, perc_intersection, fclass) %>%
  mutate(fclass = case_when(
    fclass == "cemetery" ~ "other",
    fclass == "military" ~ "other",
    fclass == "vineyard" ~ "other",
    fclass == "scrub" ~ "other",
    fclass == "farm" ~ "other",
    fclass == "heath" ~ "other",
    fclass == "orchard" ~ "other",
    fclass == "meadow" ~ "other",
    fclass == "farm" ~ "other",
    fclass == "quarry" ~ "other",
    fclass == "nature_reserve" ~ "other",
    fclass == "forest" ~ "park",
    fclass == "recreation_ground" ~ "park",
    fclass == "grass" ~ "other",
    fclass == "allotments" ~ "other",
    TRUE ~ as.character(fclass)
    
  )) %>%
  group_by(rental_zone_hal_id, fclass) %>%
  dplyr::summarise(perc_intersection = sum(perc_intersection)
              ) %>%
  pivot_wider(names_from = fclass, 
              values_from = perc_intersection,
              names_prefix = "perc_") %>%
  mutate_at(vars(contains("perc_")), 
            ~replace(., is.na(.), 0)) %>%
  mutate(
    perc_park = replace(perc_park, rental_zone_hal_id == 213688, 96),
    perc_industrial = replace(perc_industrial, rental_zone_hal_id == 239807, 100)
  ) %>%
  mutate(perc_landuse_sum = perc_other + perc_park + perc_residential + 
           perc_commercial + perc_retail + perc_industrial,
         perc_nothing = 100 - perc_landuse_sum) 

