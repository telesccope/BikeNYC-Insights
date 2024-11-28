library(sf)
library(tmap)
library(tmaptools)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gstat)
library(sp)

# Read the census shapefile
census <- st_read(dsn="Assignment/Data/nyc2020_census/nyct2020.shp", layer="nyct2020")

# Load the population csv data
population <- read.csv("Assignment/Data/nyc_censusdata_2020.csv")
population$BCT2020 <- as.character(population$BCT2020)

# Join population data to census data
census_joined <- census %>%
  left_join(population, by= c("BoroCT2020"="BCT2020"))

# Plot census polygons
plot(census_joined["CT2020"])

# Load Citibike data
citibike_jan <- read.csv("Assignment/Data/2019-citibike-tripdata/1_January/201901-citibike-tripdata_1.csv")

# If there are multiple csv files e.g. summer months = more rides
# citibike_1 <- read.csv("Assignment/Data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_1.csv")
# citibike_2 <- read.csv("Assignment/Data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_2.csv")
# citibike_3 <- read.csv("Assignment/Data/2019-citibike-tripdata/7_July/201907-citibike-tripdata_3.csv")
# citibike_july <- bind_rows(citibike_1, citibike_2, citibike_3)

# Convert starttime and stoptime into DateTime format
citibike_jan$starttime <- ymd_hms(citibike_jan$starttime)
citibike_jan$stoptime <- ymd_hms(citibike_jan$stoptime)

# Cleaning up for weekday/weekend and hour information
citibike_jan <- citibike_jan %>%
  mutate(
    # Weekday or weekend
    is_weekend = ifelse(wday(starttime) %in% c(1,7), TRUE, FALSE),
    # Start time hour
    start_hour = hour(starttime)
  )

# Create the station dataset with attributes on station id/lat/long, ride counts, user counts and time-based counts
start_station_jan <- citibike_jan %>%
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
end_station_jan <- citibike_jan %>%
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
station_jan <- full_join(start_station_jan, end_station_jan,
                          by= c("station_id", "station_latitude", "station_longitude")) %>%
  # Deal with NA values
  mutate(
    ride_start_count = ifelse(is.na(ride_start_count), 0, ride_start_count),
    ride_end_count = ifelse(is.na(ride_end_count), 0, ride_end_count),
    ride_activity = ride_start_count + ride_end_count
  )

# Add median trip duration for start stations
start_station_duration <- citibike_jan %>%
  group_by(start.station.id) %>%
  summarise(median_trip_duration_start = median(tripduration, na.rm=TRUE)) %>%
  rename(station_id = start.station.id)

# Add median trip duration for end stations
end_station_duration <- citibike_jan %>%
  group_by(end.station.id) %>%
  summarise(median_trip_duration_end = median(tripduration, na.rm=TRUE)) %>%
  rename(station_id = end.station.id)

# Merge median trip duration with station_jan
station_jan <- station_jan %>%
  left_join(start_station_duration, by= "station_id") %>%
  left_join(end_station_duration, by= "station_id")

# Convert cleaned Jan dataset into sf object
station_jan_sf <- station_jan %>%
  st_as_sf(coords = c("station_longitude", "station_latitude"), crs=4326, remove=FALSE)

# Plot map of points
tm_shape(station_jan_sf) +
  tm_bubbles(size=0.1, col="ride_activity", palette="YlOrRd") +
  tm_layout(main.title = "Citibike Jan 2019",
            main.title.size=1.5, frame=FALSE, legend.position = c("left", "top"))

# Convert Census polygon crs4269 to crs4329
census_joined <- st_transform(census_joined, crs=st_crs(station_jan_sf))

# Plot interim data
tm_shape(census) +
  tm_borders() +
  tm_shape(station_jan_sf) +
  tm_bubbles(size=0.1, col="ride_activity", palette="YlOrRd") + 
  tm_layout(main.title = "Citibike Jan 2019 and Census Polygons",
            main.title.size=1, frame=FALSE, legend.position= c("left", "top"))


# Creating an aggregated station dataset at census tract level
station_jan_nyc <- station_jan_sf %>%
  # Left join to remove stations outside NYC (do not have census tract)
  st_join(census_joined, join= st_within, left=FALSE)
ct_station_jan <- station_jan_nyc %>%
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
ct_station_jan_df <- as.data.frame(ct_station_jan)
agg_citibike_jan <- census_joined %>%
  inner_join(ct_station_jan_df, by= "BoroCT2020")

# Replace NaN with 0 in all columns of the dataset
agg_citibike_jan[is.na(agg_citibike_jan)] <- 0

# Plotting the map with context
tm_shape(census_joined)+
  tm_borders(col= "white")+
  tm_fill(col= "lightgrey")+
  tm_shape(agg_citibike_jan)+
  tm_polygons(col= "total_ride_start_count",
              style= "quantile",
              palette= "YlOrRd",
              title= "Median Trip Duration")+
  tm_layout(title= "Citibike Ride Activity by Census Tract (Jan 2019)",
            title.position= c("left", "top"),
            legend.position= c("left", "top"),
            legend.outside= FALSE,
            frame= FALSE)

# Plot histogram
ggplot(data=agg_citibike_jan, aes(total_ride_activity)) + geom_histogram()

# Map of point dataset
tm_shape(station_jan_nyc) +
  tm_bubbles(size=0.1, col="ride_activity", palette="YlOrRd") +
  tm_layout(main.title = "Citibike Jan 2019",
            main.title.size=1.5, frame=FALSE, legend.position = c("left", "top"))

# # TEST: Semivariogram (so far no best fit line yet....)
# station_jan_nyc_sp <- as(station_jan_nyc, "Spatial")
# citibike_semivar <- variogram(ride_activity~1, station_jan_nyc_sp)
# citibike_semivar_fit <- fit.variogram(citibike_semivar, model= vgm("Sph"))
# 
# plot(citibike_semivar, main= "Semivariogram of Ride Activity", xlab= "Distance", ylab= "Semivariance")

# EXPLORATORY SPATIAL ANALYSIS

# Creating spatial weight matrices
library(spdep)
library(knitr)
nb <- poly2nb(agg_citibike_jan)
W <- nb2mat(nb, style='W', zero.policy = TRUE)
colnames(W) <- rownames(W)

# Global Moran's I
# Creating a weights list
Wl <- nb2listw(nb, zero.policy = TRUE)
moran(agg_citibike_jan$total_ride_start_count, Wl, n=length(Wl$neighbours), S0=Szero(Wl))

# $I
# [1] 0.4743091
# 
# $K
# [1] 11.69548

# Test under randomisation
moran.test(agg_citibike_jan$total_ride_start_count, Wl)

# Moran I test under randomisation
# 
# data:  agg_citibike_jan$total_ride_start_count  
# weights: Wl  
# n reduced by no-neighbour observations  
# 
# Moran I statistic standard deviate = 14.844, p-value < 2.2e-16
# alternative hypothesis: greater
# sample estimates:
#   Moran I statistic       Expectation          Variance 
# 0.469748398      -0.002433090       0.001011878 

# Test using Monte-Carlo simulation
moran.mc(agg_citibike_jan$total_ride_start_count, Wl, nsim=999)
# Monte-Carlo simulation of Moran I
# 
# data:  agg_citibike_jan$total_ride_start_count 
# weights: Wl  
# number of simulations + 1: 1000 
# 
# statistic = 0.46975, observed rank = 1000, p-value = 0.001
# alternative hypothesis: greater

# Moran Scatterplot
moran.plot(agg_citibike_jan$total_ride_start_count, Wl, xlab='Total Ride Start Count', ylab='Spatially Lagged Ride Start Count', labels=agg_citibike_jan$NTAName)

# Gi and Gi*
# Based 800m off the Manhattan block - also a general 20 min walk which roughly translates to a region
NbrL <- dnearneigh(st_centroid(agg_citibike_jan), 0, 1200)
# Creating a list of neighbours not including self
D <- nb2listw(NbrL, style='B')
# Creating a list of neighbours, but include self
D_star <- nb2listw(include.self(NbrL), style='B')

G <- globalG.test(agg_citibike_jan$total_ride_start_count, D)
G <- globalG.test(agg_citibike_jan$total_ride_start_count, D_star)

Gi <- localG(agg_citibike_jan$total_ride_start_count, D)
agg_citibike_jan$Gi <- Gi
Gi_star <- localG(agg_citibike_jan$total_ride_start_count, D_star)
agg_citibike_jan$Gi_star <- Gi_star

tm_shape(agg_citibike_jan) + tm_polygons(col='Gi_star', palette='-RdBu', style='quantile')


# Local Moran's I
Ii <- localmoran(agg_citibike_jan$total_ride_start_count, Wl)
agg_citibike_jan$Ii <- Ii[,'Ii']
tm_shape(agg_citibike_jan) + tm_polygons(col='Ii', palette='-RdBu', style='quantile')
# Adjusting the p-value
agg_citibike_jan$Iip_unadjusted <- Ii[,'Pr(z != E(Ii))']
agg_citibike_jan$Ii_un_sig <- 'nonsignificant'
agg_citibike_jan$Ii_un_sig[which(agg_citibike_jan$Iip_unadjusted < 0.05)] <- 'significant'
tm_shape(agg_citibike_jan) + tm_polygons(col='Ii_un_sig', palette='-RdBu')
# Adjusting with the Bonferroni method
agg_citibike_jan$Iip_adjusted <- p.adjust(agg_citibike_jan$Iip_unadjusted, method='bonferroni')
agg_citibike_jan$Ii_ad_sig <- 'nonsignificant'
agg_citibike_jan$Ii_ad_sig[which(agg_citibike_jan$Iip_adjusted < 0.05)] <- 'significant'
tm_shape(agg_citibike_jan) + tm_polygons(col='Ii_ad_sig', palette='-RdBu')

# Looking for clusters from local Moran
moranCluster <- function(shape, W, var, alpha=0.05, p.adjust.method='bonferroni')
{
  # Code adapted from https://rpubs.com/Hailstone/346625
  Ii <- localmoran(shape[[var]], W)
  shape$Ii <- Ii[,"Ii"]
  Iip <- p.adjust(Ii[,"Pr(z != E(Ii))"], method=p.adjust.method)
  shape$Iip <- Iip
  shape$sig <- shape$Iip<alpha
  # Scale the data to obtain low and high values
  shape$scaled <- scale(shape[[var]]) # high low values at location i
  shape$lag_scaled <- lag.listw(Wl, shape$scaled) # high low values at neighbours j
  shape$lag_cat <- factor(ifelse(shape$scaled>0 & shape$lag_scaled>0, "HH",
                                 ifelse(shape$scaled>0 & shape$lag_scaled<0, "HL",
                                        ifelse(shape$scaled<0 & shape$lag_scaled<0, "LL",
                                               ifelse(shape$scaled<0 & shape$lag_scaled<0, "LH", "Equivalent")))))
  shape$sig_cluster <- as.character(shape$lag_cat)
  shape$sig_cluster[!shape$sig] <- "Non-sig"
  shape$sig_cluster <- as.factor(shape$sig_cluster)
  results <- data.frame(Ii=shape$Ii, pvalue=shape$Iip, type=shape$lag_cat, sig=shape$sig_cluster)
  
  return(list(results=results))
}

clusters <- moranCluster(agg_citibike_jan, W=Wl, var='total_ride_start_count')$results
agg_citibike_jan$Ii_cluster <- clusters$sig

tm_shape(agg_citibike_jan) + tm_polygons(col='Ii_cluster')


# Testing for spatial autocorrelation in regression errors
# Can run against area of census tract
janbike.lm <- lm(total_ride_start_count ~ Pop1 + PopU18 + Pop65pl + GQClgHsg + Fam + HUnits + AvgHHSz + num_stations + total_ride_end_count, data=agg_citibike_jan)
agg_citibike_jan$lm.res <- residuals(janbike.lm)
tm_shape(agg_citibike_jan)+tm_polygons('lm.res', palette='-RdBu', style='quantile')

