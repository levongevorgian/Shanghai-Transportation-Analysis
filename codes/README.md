# DataVisualization_Project: "Shanghai Transportation Analysis"

## Overview
This repository contains a comprehensive data visualization project analyzing Shanghai's transportation system, created for the Data Visualization course (DS 116 / CS 343) at American University of Armenia. The project examines mobility patterns in one of the world's largest urban centers through data-driven analysis and interactive visualizations.

## Authors
- Levon Gevorgyan
- Albert Hakobyan

## Project Description
This project explores Shanghai's transportation ecosystem by analyzing three key components:

- **Bike-sharing system**: Usage patterns, trip durations, spatial distribution, and peak hours
- **Taxi services**: Speed distributions, pickup/dropoff concentrations, and temporal patterns
- **Bus operations**: Route analysis, travel time estimation, vehicle tracking, and service reliability

Through these analyses, the project reveals urban mobility patterns, infrastructure utilization, congestion points, and operational characteristics that can inform urban planning decisions.

## Data Sources and Structure
- Data obtained from the Shanghai International Open Data Platform
- Datasets include:
  - GPS records for buses (route 71)
  - Taxi GPS tracking with passenger status indicators
  - Bike sharing station activity (unlock/lock events)
- Geospatial coordinates, timestamps, vehicle IDs, and operational attributes


## Repository Contents
- `Transit_app.R`: R Shiny application for interactive visualization
- Data preprocessing and cleaning scripts
- Static and interactive visualization components
- Analysis documentation with detailed explanations
- Supplementary CSV and JSON files for bus routes and GPS data

## Visualization Techniques
The project implements various visualization techniques including:
- Heatmaps for spatial density analysis
- Temporal distribution histograms
- Interactive route maps with time markers
- Boxplots for comparative speed analysis
- Violin plots for statistical distributions
- Origin-destination flow visualizations
- Stacked bar charts for categorical comparisons

## Technical Requirements
- **R**: 4.0.0 or higher
- **Python**: 3.7 or higher
- **R Packages**: shiny, leaflet, dplyr, ggplot2, sf, raster, plotly, DT, and more
- **Python Packages**: pandas, matplotlib, seaborn, numpy, geopandas, folium, transbigdata

## Interactive Dashboard Features
Our Shiny application offers:
- Filter controls for time periods, vehicle IDs, and geographic regions
- Dynamic route visualization with adjustable parameters
- Travel time estimation based on traffic conditions
- Comparative analysis between transportation modes
- Statistical summaries and data tables
- Report generation functionality

## Running the Application
1. Clone this repository
2. Install required R and Python dependencies
3. Launch R and set the working directory to the project folder
4. Run the command `shiny::runApp("Transit_app.R")`
5. Alternatively, open `Transit_app.R` in RStudio and click "Run App"

## Practical Applications
This analysis can support:
- Urban transportation policy development
- Infrastructure investment planning
- Service optimization for public transit
- Traffic congestion reduction strategies
- Environmental impact assessment of transportation modes

## Acknowledgments
Special thanks to the Shanghai International Open Data Platform for providing access to transportation data for research purposes.
