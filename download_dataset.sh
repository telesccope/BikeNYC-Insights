#!/bin/bash

# Define the URL of the dataset
URL="https://s3.amazonaws.com/tripdata/202409-citibike-tripdata.zip"

# Create the data directory if it doesn't exist
mkdir -p ./data

# Use curl to download the dataset into the data directory
curl -o ./data/202409-citibike-tripdata.zip $URL

# Unzip the downloaded file into the data directory
unzip ./data/202409-citibike-tripdata.zip -d ./data

unzip ./data/202409-citibike-tripdata/202409-citibike-tripdata_1.csv.zip -d ./data/202409-citibike-tripdata
# Remove the zip file after extraction (optional)
# rm ./data/202409-citibike-tripdata.zip

echo "Download and extraction completed."
