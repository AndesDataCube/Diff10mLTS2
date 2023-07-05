# 💻 Repository for Spatial Data Processing and Metadata Retrieval 🌍

Welcome to the Repository for Spatial Data Processing and Metadata Retrieval! Here you will find a variety of functionalities for spatial data processing and analysis, as well as metadata retrieval for satellite imagery. Discover how you can use these tools for your geospatial analysis projects! 🛰️📊🌄

## 🌟 Key Features 🌟
1. Empty DataFrame Check ✅
   - Function `check_01` (utils.R): Checks if a DataFrame is empty by evaluating the number of rows.
2. Metadata Retrieval 📄
   - Function `get_metadata` (main.R): Retrieves metadata from different image collections, including MSI image IDs, OLI image IDs, ROI IDs, and time differences.
3. Earth Engine Image Download 📥
   - Function `download` (main.R): Downloads Earth Engine images based on the specified parameters. Adjusts the geotransform of Sentinel-2 images to ensure proper alignment and prevent errors in the downloaded images.
4. ImagesDownload 🖼️
   - Downloads satellite images, retrieves metadata, and saves them as files. Assigns the index to the 'index' column in the metadata and merges or saves the metadata based on the index value.

## 📂 Repository Contents 📂
The repository is structured as follows:

- `main.R`: The main script that contains the core functions and their implementation, including `get_metadata`, `download`, and `ImagesDownload`.
- `utils.R`: Utility functions used in the main script, including `check_01`.
- `PointGeneration.R`: Script for generating points and performing spatial analysis.

## 🚀 Start Exploring and Processing Spatial Data! 🌍
Turn your geospatial projects into something extraordinary with this repository! Download satellite images, retrieve metadata, and analyze spatial data efficiently. Feel free to explore and utilize the functionalities to enhance your geospatial analysis and visualizations.

Have fun and make the most out of this repository! May your projects reach new heights! 🛰️📊🌄
