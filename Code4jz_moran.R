library(sf)
library(dplyr)
library(lubridate)
library(sp)
library(spdep)

# Read the census shapefile
census <- st_read(dsn="./data/nyc2020_census/nyct2020.shp", layer="nyct2020")

# Load the population csv data
population <- read.csv("./data/nyc_censusdata_2020.csv")
population$BCT2020 <- as.character(population$BCT2020)

# Join population data to census data
census_joined <- census %>%
  left_join(population, by= c("BoroCT2020"="BCT2020"))

# Filter out Staten Island
census_joined <- census_joined %>%
  filter(BoroName != "Staten Island")

# Ensure geometry is valid
census_joined <- st_make_valid(census_joined)

# Ensure CRS is consistent
census_joined <- st_transform(census_joined, crs = 2263)  # Use a projected CRS for NYC

# Load Citibike data
citibike_1 <- read.csv("./data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_1.csv")
citibike_2 <- read.csv("./data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_2.csv")
citibike_3 <- read.csv("./data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_3.csv")

citibike_july <- bind_rows(citibike_1, citibike_2, citibike_3)

# Convert starttime and stoptime into DateTime format
citibike_july$starttime <- ymd_hms(citibike_july$starttime)
citibike_july$stoptime <- ymd_hms(citibike_july$stoptime)

# Create the station dataset with attributes on station id/lat/long, ride counts
start_station_july <- citibike_july %>%
  group_by(start.station.id, start.station.latitude, start.station.longitude) %>%
  summarise(
    ride_start_count = n(),
    .groups = 'drop'
  ) %>%
  rename(
    station_id = start.station.id,
    station_latitude = start.station.latitude,
    station_longitude = start.station.longitude
  )

# Calculate end station counts  
end_station_july <- citibike_july %>%
  group_by(end.station.id, end.station.latitude, end.station.longitude) %>%
  summarise(
    ride_end_count = n(),
    .groups = 'drop'
  ) %>%
  rename(
    station_id = end.station.id,
    station_latitude = end.station.latitude,
    station_longitude = end.station.longitude
  )

# Combine start and end to return ride activity by station
station_july <- full_join(start_station_july, end_station_july,
                          by= c("station_id", "station_latitude", "station_longitude")) %>%
  mutate(
    ride_start_count = ifelse(is.na(ride_start_count), 0, ride_start_count),
    ride_end_count = ifelse(is.na(ride_end_count), 0, ride_end_count),
    ride_activity = ride_start_count + ride_end_count
  )

# Convert station_id to character for joining
station_july$station_id <- as.character(station_july$station_id)

# Join station data to census data
agg_citibike_july <- census_joined %>%
  left_join(station_july, by = c("BoroCT2020" = "station_id")) %>%
  mutate(total_ride_activity = ifelse(is.na(ride_activity), 0, ride_activity))

print(agg_citibike_july)