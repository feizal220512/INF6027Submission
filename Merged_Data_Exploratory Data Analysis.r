library(dplyr)
library(tidyr)


# Load the data
rainfall_data <- read.csv("Final_To Merged_Rainfall_Data.csv")
water_quality_data <- read.csv("Final_To Merged_Daily_WQ_Data.csv")


# Merge the datasets based on the same timestamp
merged_data <- merge(rainfall_data, water_quality_data, by = "date")


## EXPLORATORY DATA ANALYSIS ##
# Summary Statistics for merged data
summary(merged_data)

# Ensure date is in Date format
merged_data$date <- as.Date(merged_data$date)

# Create a table for the summary of missing values and its percentage to the total number of rows in merged_data
missing_data <- colSums(is.na(merged_data))
missing_summary <- data.frame(
    column_name = names(missing_data),
    missing_count = missing_data,
    missing_percentage = missing_data / nrow(merged_data) * 100)

# Ensure date is in Date format
merged_data$date <- as.Date(merged_data$date)

# Export missing summary to CSV
write.csv(
    missing_summary,
    "./Final_Missing_Summary_MergedData.csv",
    row.names = FALSE
)

# Export merged data to CSV
write.csv(
    merged_data,
    "./Final_Merged_Data.csv",
    row.names = FALSE
)
