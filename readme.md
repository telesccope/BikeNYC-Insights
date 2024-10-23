# CitiBike Trip Data Analysis



## Project Structure

```
.
├── MoranI.ipynb
├── data
│   ├── 202409-citibike-tripdata
│   │   ├── 202409-citibike-tripdata_1.csv
│   │   ├── 202409-citibike-tripdata_1.csv.zip
│   │   ├── 202409-citibike-tripdata_2.csv.zip
│   │   ├── 202409-citibike-tripdata_3.csv.zip
│   │   ├── 202409-citibike-tripdata_4.csv.zip
│   │   └── 202409-citibike-tripdata_5.csv.zip
│   └── 202409-citibike-tripdata.zip
├── download_dataset.sh
├── map
│   ├── stations_local_moran_map.html
│   └── stations_map.html
├── paper
│   ├── 
├── preview.ipynb
├── readme.md
└── requirements.txt
```

## Files and Directories

- **MoranI.ipynb**: Jupyter Notebook for spatial autocorrelation analysis using Moran's I.
- **data/**: Contains the CitiBike trip datasets in CSV and zipped formats.
- **download_dataset.sh**: Shell script to download and extract the dataset.
- **map/**: Contains HTML files for visualizing station data on maps.
  - `stations_local_moran_map.html`: Map visualizing local Moran's I analysis.
  - `stations_map.html`: Map showing bike start stations.
- **paper/**: Research papers related to bike-sharing systems and their analysis.
- **preview.ipynb**: Jupyter Notebook for preliminary data exploration and visualization.
- **requirements.txt**: Python dependencies required to run the analysis.

## Scripts

### `download_dataset.sh`

This script is used to automate the download and extraction of CitiBike trip data. It handles the retrieval of data files and organizes them into the appropriate directory for analysis.

## Getting Started

1. **Install Dependencies**: Run `pip install -r requirements.txt` to install the necessary Python packages.
2. **Download Data**: Execute `bash download_dataset.sh` to download and prepare the data for analysis.
3. **Run Analysis**: Use the Jupyter Notebooks (`MoranI.ipynb` and `preview.ipynb`) to perform data analysis and visualization.

## Data Resource

- https://s3.amazonaws.com/tripdata/index.html
