library(data.table)
library(tidyverse)
library(lubridate)
library(leaflet)
library(ggplot2)
library(rgdal)
library(raster)j
library(sp)
library(leaflet)
library(sf)
library(RANN)

# POPULATION ---------------------------------------------------------------------------------
# source: https://www.statistik-nord.de/fileadmin/maps/zensus2011_hh/index.html
pop_org <- fread("data/zensushh11_100m.csv")

# convert from ETRS: 3035 to utm32
pop <- pop_org %>%
  dplyr::rename(lon = x_mp_100m, lat = y_mp_100m) %>% 
  mutate(
    lon = as.numeric(gsub(",", "", lon)),
    lat = as.numeric(gsub(",", "", lat))
  )

coordinates(pop) <- c("lon", "lat")

# overview projections: https://epsg.io/3035
proj4string(pop) <- CRS("+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

CRS.new <- CRS("+proj=longlat +datum=WGS84")

# convert lat/lon to raster: 
pop_trans <- spTransform(pop, CRS.new)

pop_df_trans <- as.data.frame(pop_trans)


# population around stations ------------------------------------------------------------------------------
# filter for cell-centers within 500 m of station
stations <- readRDS("data/stationshh.rds")

source("src/functions_nn2.R") # file that contains all functions

# need id for nearest neighbors
stations$station_id <- seq.int(1, nrow(stations))
pop_df_trans$close_point_id <- seq.int(1, nrow(pop_df_trans))

# function from functions_nn2
## prepare for nn2-function, needs to be in utm
stations.xy <- convert_to_utm(stations, "lon", "lat")
pop.xy <- convert_to_utm(pop_df_trans, "lon", "lat")

# detect all centers of census-raster within 350 m of a station
pop_nn2 <- detect_nearest_latlon(stations.xy, pop.xy, stations, pop_df_trans, 40, 350)
saveRDS(pop_nn2, "data/pop_nn2.rds")

# keep only rasters that where found to be close to a station
pop_info <- pop_nn2 %>%
  left_join(pop_df_trans, by = "close_point_id") %>%
  dplyr::rename(demo_lon = lon, demo_lat = lat) %>%
  left_join(stations, by = "station_id")

saveRDS(pop_info, "data/pop_info.rds")

# check on map
leaflet() %>%
  addTiles() %>%
  addCircles(pop_close$lon, pop_close$lat, popup = pop_close$Einwohner) %>%
  addMarkers(stations$lon, stations$lat) 

# merge population numbers to get population around stations  ------------------------------------------------------------------------------
station_pop_final <- pop_info %>%
  dplyr::select(-OBJECTID, -CellCode) %>%
  dplyr::rename(pop_cens = Einwohner) %>%
  mutate(
    pop_cens = replace(pop_cens, pop_cens == "-1", NA),
    pop_cens = gsub(",", "", pop_cens),
    pop_cens = as.numeric(pop_cens)
         ) %>%
  distinct() %>%
  group_by(name, rental_zone_hal_id, lat, lon) %>%
  summarise(
    sumNA = sum(is.na(pop_cens)),
    population = sum(pop_cens, na.rm = T)
            )

saveRDS(station_pop_final, "data/stations_population.rds")
station_pop_final <- readRDS("data/stations_population.rds")

# report deviation  ------------------------------------------------------------------------------
quantile(station_pop_final$sumNA) # not always correct, up to 150 people difference

# PERCENTAGE MIGRANTS ------------------------------------------------------------------------------
demo_org <- fread("data/demographie100m.csv", header = T) # zensus data
pop_info <- readRDS("data/pop_info.rds") # from previous steps

demodf <- as.data.frame(demo_org)

## only keep Hamburg, this is still Germany with 50 million rows
demo_close <- subset(demodf, Gitter_ID_100m %in% pop_info$Gitter_ID_100m)

saveRDS(demo_close, "data/demo_close.rds")


# join with population data to get coordinates  ------------------------------------------------------------------------------
## coordinates not contained in demography data
demo_close_info <- demo_close %>%
  left_join(pop_info, by = "Gitter_ID_100m") %>%
  filter(!is.na(lon)) %>%
  dplyr::select(-Einwohner, -Gitter_ID_100m_neu) %>%
  dplyr::rename(cat_code = Auspraegung_Code, cat_name = Auspraegung_Text, n_pop = Anzahl, n_dev = Anzahl_q) %>%
  filter(
    Merkmal == "ALTER_KURZ"  | 
    Merkmal == "GESCHLECHT"  | 
    (Merkmal == "STAATSANGE_GRP" & cat_code != 24)
         )

saveRDS(demo_close_info, "data/demo_close_info.rds")

# clean & transform  ------------------------------------------------------------------------------
code_translation <- dplyr::select(demo_close_info, Merkmal, cat_code, cat_name) %>% distinct()
code_translation
saveRDS(code_translation, "data/code_translation.rds")

# put each category in separate column of number of category
## df: one line for each station - gitter - pair
stations_demo_info <- demo_close_info %>%
  dplyr::select(-cat_name) %>%
  pivot_wider(names_from = c(Merkmal,cat_code), 
              values_from = c(n_pop, n_dev)) %>%
  # replace all missing categories with 0
  mutate_at(vars(contains("n_")), ~replace(., is.na(.), 0)) 

saveRDS(stations_demo_info, "data/stations_demo_info.rds")

# calcuate percentages  ------------------------------------------------------------------------------
## now merge to population & station data frame to get number of people per station

# final data frame with one line per stations & # of people in certain, age, gender or citizenship group
stations_demo_agg <- stations_demo %>%
  group_by(name, rental_zone_hal_id, ) %>%
  dplyr::select(starts_with("n_")) %>%
  summarise_all(list(sum = sum))

saveRDS(stations_demo_agg, "data/stations_demo_agg.rds")

# add population data & percentages
stations_demo_pop <- stations_demo_agg %>%
  left_join(station_pop_final, by = c("rental_zone_hal_id", "name")) %>%
  mutate_at(vars(contains("n_")),
            ~(./population)*100
            )

saveRDS(stations_demo_pop, "data/stations_demo_pop.rds")
# check if numbers match witch example station


stations_demo_pop <- readRDS("data/stations_demo_pop.rds")
