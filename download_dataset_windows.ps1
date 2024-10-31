# Define the URL of the dataset
$url = "https://s3.amazonaws.com/tripdata/202409-citibike-tripdata.zip"

# Create the data directory if it doesn't exist
$dataDir = "./data"
if (-Not (Test-Path $dataDir)) {
    New-Item -ItemType Directory -Path $dataDir
}

# Use Invoke-WebRequest to download the dataset into the data directory
$outputFile = "$dataDir/202409-citibike-tripdata.zip"
Invoke-WebRequest -Uri $url -OutFile $outputFile

# Unzip the downloaded file into the data directory
Add-Type -AssemblyName System.IO.Compression.FileSystem
[System.IO.Compression.ZipFile]::ExtractToDirectory($outputFile, $dataDir)

# If there are nested zip files, you can unzip them similarly
$nestedZip = "$dataDir/202409-citibike-tripdata/202409-citibike-tripdata_1.csv.zip"
if (Test-Path $nestedZip) {
    [System.IO.Compression.ZipFile]::ExtractToDirectory($nestedZip, "$dataDir/202409-citibike-tripdata")
}

# Remove the zip file after extraction (optional)
# Remove-Item $outputFile

Write-Host "Download and extraction completed."
