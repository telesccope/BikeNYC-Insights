library(sf)
library(tmap)
library(tmaptools)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gstat)
library(sp)

# Create map directory if it doesn't exist
if (!dir.exists("./map")) {
  dir.create("./map")
}

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

# Plot census polygons
png("./map/census_polygons.png")
plot(census_joined["CT2020"])
dev.off()

# Load Citibike data
citibike_1 <- read.csv("./data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_1.csv")
citibike_2 <- read.csv("./data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_2.csv")
citibike_3 <- read.csv("./data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_3.csv")

citibike_july <- bind_rows(citibike_1, citibike_2, citibike_3)

# Convert starttime and stoptime into DateTime format
citibike_july$starttime <- ymd_hms(citibike_july$starttime)
citibike_july$stoptime <- ymd_hms(citibike_july$stoptime)

# Cleaning up for weekday/weekend and hour information
citibike_july <- citibike_july %>%
  mutate(
    # Weekday or weekend
    is_weekend = ifelse(wday(starttime) %in% c(1,7), TRUE, FALSE),
    # Start time hour
    start_hour = hour(starttime)
  )

# Create the station dataset with attributes on station id/lat/long, ride counts, user counts and time-based counts
start_station_july <- citibike_july %>%
  # Group by start.station and user type
  group_by(start.station.id, start.station.latitude, start.station.longitude) %>%
  summarise(
    ride_start_count = n(),
    usertype_subscriber_count = sum(usertype == "Subscriber"),
    usertype_customer_count = sum(usertype == "Customer"),
    
    # Weekday counts for time intervals AM/PM Peak/Off-Peak - ref Liu et al
    ride_start_weekday_7_10 = sum(!is_weekend & start_hour>=7 & start_hour<10),
    ride_start_weekday_10_17 = sum(!is_weekend & start_hour>=10 & start_hour<17),
    ride_start_weekday_17_0 = sum(!is_weekend & start_hour>=17 & start_hour<24),
    ride_start_weekday_0_7 = sum(!is_weekend & start_hour>=0 & start_hour<7),
    
    # Weekend counts for leisure/others - ref Liu et al
    ride_start_weekend_10_0 = sum(is_weekend & start_hour>=10 & start_hour<24),
    ride_start_weekend_0_10 = sum(is_weekend & start_hour>=0 & start_hour<10)
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
    ride_end_count = n()
  ) %>%
  rename(
    station_id = end.station.id,
    station_latitude = end.station.latitude,
    station_longitude = end.station.longitude
  )

# Combine start and end to return ride activity by station
station_july <- full_join(start_station_july, end_station_july,
                          by= c("station_id", "station_latitude", "station_longitude")) %>%
  # Deal with NA values
  mutate(
    ride_start_count = ifelse(is.na(ride_start_count), 0, ride_start_count),
    ride_end_count = ifelse(is.na(ride_end_count), 0, ride_end_count),
    ride_activity = ride_start_count + ride_end_count
  )

# Add median trip duration for start stations
start_station_duration <- citibike_july %>%
  group_by(start.station.id) %>%
  summarise(median_trip_duration_start = median(tripduration, na.rm=TRUE)) %>%
  rename(station_id = start.station.id)

# Add median trip duration for end stations
end_station_duration <- citibike_july %>%
  group_by(end.station.id) %>%
  summarise(median_trip_duration_end = median(tripduration, na.rm=TRUE)) %>%
  rename(station_id = end.station.id)

# Merge median trip duration with station_july
station_july <- station_july %>%
  left_join(start_station_duration, by= "station_id") %>%
  left_join(end_station_duration, by= "station_id")

# Convert cleaned July dataset into sf object
station_july_sf <- station_july %>%
  st_as_sf(coords = c("station_longitude", "station_latitude"), crs=4326, remove=FALSE)

# Plot map of points
map1 <- tm_shape(station_july_sf) +
  tm_bubbles(size=0.1, col="ride_activity", palette="YlOrRd") +
  tm_layout(main.title = "Citibike July 2019",
            main.title.size=1.5, frame=FALSE, legend.position = c("left", "top"))
tmap_save(map1, "./map/citibike_july_2019.png")

# Convert Census polygon crs4269 to crs4329
census_joined <- st_transform(census_joined, crs=st_crs(station_july_sf))

# Plot interim data
map2 <- tm_shape(census_joined) +
  tm_borders() +
  tm_shape(station_july_sf) +
  tm_bubbles(size=0.1, col="ride_activity", palette="YlOrRd") + 
  tm_layout(main.title = "Citibike July 2019 and Census Polygons",
            main.title.size=1, frame=FALSE, legend.position= c("left", "top"))
tmap_save(map2, "./map/citibike_july_2019_census.png")

# Creating an aggregated station dataset at census tract level
station_july_nyc <- station_july_sf %>%
  # Left join to remove stations outside NYC (do not have census tract)
  st_join(census_joined, join= st_within, left=FALSE)
ct_station_july <- station_july_nyc %>%
  group_by(BoroCT2020) %>%
  summarise(
    num_stations = n(),
    total_ride_start_count = sum(ride_start_count, na.rm=TRUE),
    total_ride_end_count = sum(ride_end_count, na.rm=TRUE),
    total_ride_activity = sum(ride_activity, na.rm=TRUE),
    median_trip_duration_start = median(median_trip_duration_start, na.rm=TRUE),
    median_trip_duration_end = median(median_trip_duration_end, na.rm=TRUE),
    usertype_subscriber_count = sum(usertype_subscriber_count, na.rm=TRUE),
    usertype_customer_count = sum(usertype_customer_count, na.rm=TRUE),
    # Adding in the time factor
    weekday_7_10 = sum(ride_start_weekday_7_10, na.rm=TRUE),
    weekday_10_17 = sum(ride_start_weekday_10_17, na.rm=TRUE),
    weekday_17_0 = sum(ride_start_weekday_17_0, na.rm=TRUE),
    weekday_0_7 = sum(ride_start_weekday_0_7, na.rm=TRUE),
    weekend_10_0 = sum(ride_start_weekend_10_0, na.rm=TRUE),
    weekend_0_10 = sum(ride_start_weekend_0_10, na.rm=TRUE)
  )

# Converting to df to allow for inner join and drop column for polygons without Citibike docks
ct_station_july_df <- as.data.frame(ct_station_july)
agg_citibike_july <- census_joined %>%
  inner_join(ct_station_july_df, by= "BoroCT2020")

# Plotting the map with context
map3 <- tm_shape(census_joined) +
  tm_borders(col= "white") +
  tm_fill(col= "lightgrey") +
  tm_shape(agg_citibike_july) +
  tm_polygons(col= "median_trip_duration_start",
              style= "quantile",
              palette= "YlOrRd",
              title= "Median Trip Duration") +
  tm_layout(title= "Citibike Ride Activity by Census Tract (July 2019)",
            title.position= c("left", "top"),
            legend.position= c("left", "top"),
            legend.outside= FALSE,
            frame= FALSE)
tmap_save(map3, "./map/citibike_ride_activity_census_tract.png")

# Plot histogram
histogram_plot <- ggplot(data=agg_citibike_july, aes(total_ride_activity)) +
  geom_histogram(breaks=seq(0,72000, 1000))
ggsave("./map/total_ride_activity_histogram.png", plot = histogram_plot)

# Plot semivariogram
station_july_nyc_sp <- as(station_july_nyc, "Spatial")
citibike_semivar <- variogram(ride_activity~1, station_july_nyc_sp)
citibike_semivar_fit <- fit.variogram(citibike_semivar, model= vgm("Sph"))

png("./map/semivariogram.png")
plot(citibike_semivar, main= "Semivariogram of Ride Activity", xlab= "Distance", ylab= "Semivariance")
dev.off()

library(spdep)

# 将 sf 对象转换为 Spatial 对象
census_sp <- as(census_joined, "Spatial")

# 创建邻接矩阵，增加 snap 参数以处理无邻居
nb <- poly2nb(census_sp, snap = 0.01)

# 创建空间权重矩阵，允许无邻居多边形
listw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# 计算全局 Moran's I
moran_test <- moran.test(agg_citibike_july$total_ride_activity, listw, zero.policy = TRUE)

# 查看结果
print(moran_test)
