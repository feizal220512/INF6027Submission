# Load necessary libraries
library(dplyr)
library(tidyr)
library(imputeTS)


# Declare global variables to avoid binding errors
globalVariables(c("measure", "location_name", "year", "month", "day", "value", "daily_avg_value"))

# Define the list of files to process
files <- list(
  "./THAMES_HAMMERSMITH_E_200707_all-measures_2022-01-01_to_2024-12-31.csv",
  "./THAMES_CADOGAN-PIER_E_200707_all-measures_2022-01-01_to_2024-12-31.csv",
  "./THAMES_PUTNEY_E_201710_all-measures_2022-01-01_to_2024-12-31.csv"
)

# Function to process each file
process_file <- function(file_path) {
  data <- read.csv(file_path)
  # Extract the measure name from the file path
  data <- data %>%
    mutate(measure = sub(".*-(.*?)-i-subdaily.*", "\\1", measure)) %>%
    mutate(location_name = sub(".*THAMES_(.*?)_.*", "\\1", file_path)) %>%
    
    # Filter out unused columns, dateTime is unused as we have date column
    select(-dateTime, -completeness, -quality, -qcode) %>%
    
    # Parse date to year, month, and day columns
    mutate(year = as.numeric(format(as.Date(date, format="%Y-%m-%d"), "%Y")),
           month = as.numeric(format(as.Date(date, format="%Y-%m-%d"), "%m")),
           day = as.numeric(format(as.Date(date, format="%Y-%m-%d"), "%d")))
  
  # Calculate the daily average value for each measure
  daily_avg <- data %>%
    group_by(date, measure) %>%
    summarise(daily_avg_value = mean(value, na.rm = TRUE)) %>%
    ungroup()
  
  # Join the daily average value back to the original data
  data <- data %>%
    left_join(daily_avg, by = c("date", "measure")) %>%
    group_by(date, measure) %>%
    
    # Add a column to indicate if the row is the first row for each date
    mutate(is_first_row = row_number() == 1) %>%
    ungroup() %>%
    
    # Fill in the daily average value only for the first row of each date
    mutate(daily_avg_value = ifelse(is_first_row, daily_avg_value, NA)) %>%
    select(-is_first_row) %>%
    
    # Remove rows with NA daily average value
    filter(!is.na(daily_avg_value))
  
  # Group by measure and sort by date, rearrange columns for consistency
  data <- data %>%
    group_by(measure) %>%
    arrange(date) %>%
    select(location_name, measure, date, year, month, day, value, daily_avg_value)
  
  return(data)
}

# Process and combine each file and store the results in a list
processed_data <- lapply(files, process_file)
combined_data <- bind_rows(processed_data)

# Create a descriptive statistic table, with Max, min, mean, median, standard deviation
# For each location and each measure
summary_table <- combined_data %>%
  select(-year, -month, -day, -value) %>%
  group_by(location_name, measure) %>%
  summarise(across(where(is.numeric), 
  list(max = max, min = min, mean = mean, median = median, sd = sd)))

#Export summary table to CSV
write.csv(
  summary_table,
  "./Initial_WaterQuality_Summary_Table.csv",
  row.names = FALSE
)

# Remove outliers from each measure in combined data with IQR method
remove_outliers <- function(data) {
  data %>%
    group_by(location_name, measure) %>%
    summarise(across(where(is.numeric), ~{
      q1 <- quantile(., 0.25, na.rm = TRUE)
      q3 <- quantile(., 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      ifelse(. >= lower_bound & . <= upper_bound, ., NA)
    })) %>%
    ungroup()
}

# Remove outliers from the combined data
cleaned_data <- remove_outliers(combined_data)

# Remove all rows with "turb" in measure column as it has the most missing values
# Based on descriptive statistics, change accordingly
cleaned_data <- cleaned_data%>%
  filter(!grepl("turb", measure))

# Remove value column as it is no longer needed
# The daily_avg_value column will be used for further analysis
cleaned_data <- cleaned_data %>%
  select(-value)

# Impute missing values for each column with the Kalman imputation method
cleaned_data_imputed <- na_kalman(cleaned_data)

# Pivot the data into monthly average values for each location and each measure
monthly_avg_data <- cleaned_data_imputed %>%
  group_by(location_name, measure, year, month) %>%
  summarise(monthly_avg_value = mean(daily_avg_value, na.rm = TRUE)) %>%
  ungroup()

# Export monthly average data to CSV
write.csv(
  monthly_avg_data,
  "./Final_Monthly_Avg_WQ_Data.csv",
  row.names = FALSE
)

######################################################################
## CREATE FILE TO MERGE WITH RAINFALL DATA FOR CORRELATION ANALYSIS ##
######################################################################

# Pivot the cleaned data to have measures as columns with daily_avg_value, with location_name.measure as the column name
pivoted_data <- combined_data %>%
  select(location_name, date, measure, daily_avg_value) %>%
  unite("location_measure", location_name, measure, sep = ".") %>%
  pivot_wider(names_from = location_measure, values_from = daily_avg_value) %>%
  arrange(date)

View(pivoted_data) 

# Remove outliers from the pivoted data with IQR method
remove_outliers_pivoted <- function(data) {
  data %>%
    select(-date) %>%
    summarise(across(everything(), ~{
      q1 <- quantile(., 0.25, na.rm = TRUE)
      q3 <- quantile(., 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - 1.5 * iqr
      upper_bound <- q3 + 1.5 * iqr
      ifelse(. >= lower_bound & . <= upper_bound, ., NA)
    })) %>%
    bind_cols(data %>% select(date), .)
}

# Remove outliers from the pivoted data
pivoted_data_no_outliers <- remove_outliers_pivoted(pivoted_data)

# create a missing value table showing the count of missing values and 
# "NA" for each measure and its percentage to total data
missing_value_table_pivoted <- pivoted_data_no_outliers %>%
  summarise(across(everything(), ~sum(is.na(.), na.rm = TRUE))) %>%
  mutate(percentage = round(rowSums(is.na(.)) / nrow(pivoted_data) * 100, 2))

# Impute missing values for each column with the Kalman imputation method
wqdata_imputed <- na_kalman(pivoted_data_no_outliers)


# Remove "turb" columns as it has the most missing values
wqdata_imputed <- wqdata_imputed %>%
  select(-contains("turb"))

# Create a descriptive statistic table, with Max, min, mean, median, standard deviation
# For each location and each measure
summary_table <- wqdata_imputed %>%
  select(-year, -month, -day, -value) %>%
  group_by(location_name, measure) %>%
  summarise(across(where(is.numeric), 
  list(max = max, min = min, mean = mean, median = median, sd = sd)))

#Export summary table to CSV
write.csv(
  summary_table,
  "./Final_WaterQuality_Summary_Table.csv",
  row.names = FALSE
)

# Export Final Daily WQ Data to CSV
write.csv(
  wqdata_imputed,
  "./Final_To Merged_Daily_WQ_Data.csv",
  row.names = FALSE
)