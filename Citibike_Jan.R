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

# colnames(agg_citibike_jan)
# agg_citibike_jan <- agg_citibike_jan_copy

# List of columns to convert from character to integer
columns_to_convert <- c("Pop1", "PopU5", "Pop5t9", "Pop10t14", "Pop15t19", "Pop20t24", "Pop25t29", "Pop30t34", "Pop35t39", "PopU18", "Pop65pl", "GQClgHsg", "Fam", "HUnits")
# Note: chr values with comma (e.g. "3,512") automatically converts to NA -- therefore need to remove the comma
agg_citibike_jan[columns_to_convert] <- lapply(agg_citibike_jan[columns_to_convert], function(x) {
  x <- gsub(",", "", x)             # Remove commas
  x <- as.numeric(x)                # Convert to numeric
  x[is.na(x)] <- 0                  # Replace NAs with 0
  return(x)
})

# Replace NA with 0 for all numeric and integer columns
agg_citibike_jan[sapply(agg_citibike_jan, is.numeric)] <- 
  lapply(agg_citibike_jan[sapply(agg_citibike_jan, is.numeric)], function(x) {
    x[is.na(x)] <- 0
    return(x)
  })

# Verify if there are any remaining NAs
sum(is.na(agg_citibike_jan))

# Create a new column 'Pop19t64' as Pop1 - PopU18 - Pop65pl
agg_citibike_jan$Pop19t64 <- agg_citibike_jan$Pop1 - agg_citibike_jan$PopU18 - agg_citibike_jan$Pop65pl

# Verify the result
head(agg_citibike_jan[, c("Pop1", "PopU18", "Pop65pl", "Pop19t64")])


# # Verify if there are any remaining NAs and list the columns with NAs
# remaining_na_cols <- sapply(agg_citibike_jan, function(x) any(is.na(x)))
# 
# # Print out the columns with remaining NAs
# if (any(remaining_na_cols)) {
#   cat("Columns with remaining NAs:\n")
#   print(names(remaining_na_cols[remaining_na_cols == TRUE]))
# } else {
#   cat("No remaining NAs in the numeric columns.\n")
# }

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
# Based 4km roughly off the width Manhattan block - also see comparison of 4km vs 2km in commented block below

NbrL <- dnearneigh(st_centroid(agg_citibike_jan), 0, 1.6)

# Creating a list of neighbours not including self
D <- nb2listw(NbrL, style='B')
# Creating a list of neighbours, but include self
D_star <- nb2listw(include.self(NbrL), style='B')

G <- globalG.test(agg_citibike_jan$total_ride_start_count, D)
G <- globalG.test(agg_citibike_jan$total_ride_start_count, D_star)

# Check if there are any isolated observations (no neighbors) by returning the length (number of neighbours) for each polygon
sapply(NbrL, length)

# > sapply(NbrL, length) when NbrL=4
# [1] 163 166 155 170 167 174 124 166 116 170 153 154 111 105 167 161 139 143 133 154 146 136 144 151 137 133
# [27] 120 174 131 167 122 151 123 170 166 137 161 126 154 142 127 142 169 130 163 145 159 131 115 167 145 161
# [53] 156 170 144 154 163 165 148 166 167 149 163 136 130 160 150 165 133 161 148 133 132 147 119 128 154 150
# [79] 147 142 139 132 131 122 127 130 125 129 115 115 109 123 107 112 118 104 110  89  91  94  88  79  78  66
# [105]  68  70  66  57  57  87  80  71  62  15  15  15  15  15  15 155 107 116 118 111 106 101  95  90  85  84
# [131]  91  93  96 105 106 129  89  83  79  74  65  56  64  67  59  80  98  98  15 159 122 163 169 164  98  90
# [157] 164 158 151 171  78  15  15  15 113  15  76 162 174  96 148 133 130 177 169  67 174 168 114 161 156 153
# [183]  78  84 115 110 161 143 147 157 168 166 126 132 166 166 164 164 162 160 152 163 167 160  95  94 107  99
# [209]  69  76  77  15 168 155 159 110 136 113 134 137 161 135 114 143 119 130 119 115 151 138 140 146 127 162
# [235] 163 163 153 140 150 137 140 139 125 107  66  93 106 128 118 125  77 103  88 128 128 129 117 105  98  92
# [261]  88  84  87  96 100 104 111 124 121 139 149 158 170 166 149 145 131 129 120 117 112 174  97 104  97 105
# [287] 121 127 134 132 124 115 111 118 124 130 117 112 111 112 111 107  98  79  83  89  93  97  96 104  87  68
# [313] 115  86  91  98  95  96 105 112 119 112 119 127 125 139 138 142 159 160 142 144 173 173 157 155 158 114
# [339] 147  99 141 144 109  96 122 113 178 177  94  95  84 130  97 162 116 115 178  83 177 172  81 125 149 172
# [365] 164 142 166 160 125 138 133  86 173  58  69 115 130 134 138  84 112 124 123  15  15 128 120 116 107  83
# [391] 173 176  15  15  96 175  86  73 168 166 118 120 105 111  86  83 143 151  92  40 155 146 172 174 144 153

# > sapply(NbrL, length) when NbrL=2
# [1] 46 49 51 50 51 51 41 51 39 53 55 57 44 33 56 61 53 58 52 63 60 60 62 61 60 56 49 48 55 56 45 61 45 52 57
# [36] 54 60 42 60 57 46 59 55 47 57 59 58 45 34 51 58 53 56 47 57 56 52 49 59 50 45 57 49 55 46 52 57 45 51 53
# [71] 58 50 46 46 34 40 38 38 42 42 39 40 38 38 40 38 43 37 39 25 42 35 47 24 38 34 39 39 47 28 38 32 27 23 28
# [106] 29 29 23 25 46 41 34 29  8 10 11 13 15 14 30 31 32 39 39 36 33 28 31 33 29 29 34 35 40 38 41 25 21 19 21
# [141] 21 17 27 28 22 28 21 20  7 58 29 60 51 59 28 36 47 42 50 46 27 12 10 12 26 11 33 36 32 40 55 46 45 46 53
# [176] 24 46 32 34 42 41 41 34 41 27 23 51 61 60 62 45 49 53 53 43 47 41 43 44 44 42 49 50 53 37 31 26 29 33 34
# [211] 39 14 54 62 62 39 43 27 22 34 28 24 37 32 27 51 30 27 41 39 41 45 42 50 50 54 56 58 54 59 59 51 44 34 12
# [246] 37 42 52 53 62 24 51 36 61 61 58 54 50 43 36 31 18 26 33 40 47 53 57 59 59 57 53 51 50 57 55 55 53 54 50
# [281] 46 52 29 35 31 40 50 51 53 50 49 45 42 44 45 47 39 41 38 38 38 38 34 23 25 28 29 32 34 35 31 17 43 18 23
# [316] 29 32 34 33 30 34 38 40 45 38 34 34 33 39 42 38 42 42 41 40 37 29 27 28 27 49 47 53 43 34 29 47 51 35 29
# [351] 22 42 23 45 37 38 43 30 44 46 15 26 47 50 32 22 47 50 35 46 45 21 48 22 17 34 46 48 45 24 31 32 32 15 14
# [386] 49 51 35 36 25 51 48 14 14 15 44 28 25 37 42 46 42 21 30 29 29 33 31 22  5 61 65 44 46 45 45

Gi <- localG(agg_citibike_jan$total_ride_start_count, D)
agg_citibike_jan$Gi <- Gi
Gi_star <- localG(agg_citibike_jan$total_ride_start_count, D_star)
agg_citibike_jan$Gi_star <- Gi_star

tm_shape(agg_citibike_jan) + tm_polygons(col='Gi', palette='-RdBu', style='quantile')
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
janbike.lm <- lm(total_ride_start_count ~ PopU18 + Pop19t64 + Pop65pl + HUnits + Fam + AvgHHSz + GQClgHsg + Shape_Area + num_stations, data=agg_citibike_jan)
agg_citibike_jan$lm.res <- residuals(janbike.lm)
tm_shape(agg_citibike_jan)+tm_polygons('lm.res', palette='-RdBu', style='quantile')

