The GitHub repository for this research contains four essential R code files, each developed to handle data preparation, analysis, and visualisation tasks. The first file, Water Quality Data Preparation, focuses on cleaning and transforming the water quality data using R libraries such as dplyr, tidyr, and imputeTS. Missing values are addressed using Kalman Smoothing, and the data is aggregated into daily and monthly formats, making it ready for visualisation and further analysis. The second file, Rainfall Data Preparation, performs similar cleaning and aggregation processes but does not utilise the imputeTS library, as the rainfall dataset is complete and requires no imputation.

The third file, Merged Data Exploratory Data Analysis, combines the processed water quality and rainfall datasets. This R script summarises and explores the merged data, facilitating comparisons and correlation analyses. It plays a critical role in identifying seasonal patterns, spatial variability, and relationships between rainfall, temperature, and water quality parameters, forming the basis for addressing the study’s research questions.

The final file, Data Visualisation, generates all the visualisations used in the report, including boxplots, smoothed time-series line plots, and correlation heatmaps. These R-based visualisations depict seasonal trends, spatial differences, and variable relationships, enhancing the interpretation of the findings. Together, these four R code files provide a cohesive framework for data preparation, analysis, and presentation, ensuring a transparent, reproducible, and insightful research process.

How to Use the Code:
	1.	Data Preparation Codes: The Water Quality Data Preparation and Rainfall Data Preparation scripts are used to read and prepare the raw data from the DEFRA webpage, transforming it into ready-to-process formats. These scripts produce daily, monthly, and time-series datasets, which are then merged for correlation analysis. To use these scripts, update the file paths to point to the raw data downloaded from DEFRA.
	
 2.	Merged Data Exploratory Data Analysis: This script merges the time-series datasets produced by the data preparation scripts. It identifies any remaining missing data and generates a summary table. The script outputs the final merged dataset and a summary table for further analysis.

 3.	Data Visualisation Code: This script creates a range of visualisations to analyse the water quality and rainfall data, including boxplots, smoothed time-series line graphs, and correlation heatmaps. These visualisations provide insights into seasonal trends, spatial patterns, and relationships between variables.
