# Load required libraries
library(dplyr)
library(tidyr)

# Declare global variables to avoid binding issues
globalVariables(c("location_name", "year", "month", "day", "value", "daily_total_rainfall"))

# Define consistent column order
column_order <- c("location_name", "measure", "date", "year", "month", "day", "value", "daily_rainfall")

# Define the list of files to process
files <- list(
  "./Holland-Park-rainfall-15min-Qualified-2.csv",
  "./Putney-Heath-rainfall-15min-Qualified.csv"
)

# Custom function to process each file
process_file <- function(file_path) {
    data <- read.csv(file_path)
    # Use regex to extract the location name from the file path
    data <- data %>%
        mutate(location_name = sub(".*/(.*?)-rainfall.*", "\\1", file_path)) %>%
        
        # Filter out unused columns, dateTime is unused as we have date column
        select(-dateTime, -completeness, -quality, -qcode) %>%
        
        # Parse date to year, month, and day columns
        mutate(year = as.numeric(format(as.Date(date, format="%Y-%m-%d"), "%Y")),
                     month = as.numeric(format(as.Date(date, format="%Y-%m-%d"), "%m")),
                     day = as.numeric(format(as.Date(date, format="%Y-%m-%d"), "%d")))
    
    # Calculate the daily total for each measure
    daily_total <- data %>%
        group_by(date) %>%
        summarise(daily_rainfall = sum(value, na.rm = TRUE)) %>%
        ungroup()
    
    # Join the daily total value back to the original data
    data <- data %>%
        left_join(daily_total, by = c("date")) %>%
        group_by(date) %>%
        
        # Add a column to indicate if the row is the first row for each date
        mutate(is_first_row = row_number() == 1) %>%
        ungroup() %>%
        
        # Fill in the daily total value only for the first row of each date
        mutate(daily_rainfall = ifelse(is_first_row, daily_rainfall, NA)) %>%
        select(-is_first_row) %>%
        
        # Remove rows with NA daily total value
        filter(!is.na(daily_rainfall))
    
    # Sort by date, rearrange columns for consistency
    data <- data %>%
        arrange(date) %>%
        select(all_of(column_order))

    #Delete measure column, as all the data are rainfall in this file
    #Delete value column, as we have daily_total_rainfall
    data <- data %>%
        select(-measure, -value)
    
    return(data)
}

# Process each file and combine the results
processed_data <- lapply(files, process_file)
combined_data <- bind_rows(processed_data)

#Export the combined data to a CSV file
write.csv(
  combined_data,
  "./Final_Daily_Rainfall_Data.csv",
  row.names = FALSE
)

# Create a descriptive statistic table, with Max, min, mean, median, standard deviation, or each location and each measure
summary_table <- combined_data %>%
  select(-year, -month, -day) %>%
  group_by(location_name) %>%
  summarise(across(where(is.numeric), list(max = max, min = min, mean = mean, median = median, sd = sd)))

View(summary_table)
#Export summary table to CSV
write.csv(
  summary_table,
  "./Final_Rainfall_Summary_Table.csv",
  row.names = FALSE
)

#Pivot the combined data to have daily_rainfall for each location
pivoted_data <- combined_data %>%
  select(location_name, date, daily_rainfall) %>%
  pivot_wider(names_from = location_name, values_from = daily_rainfall) %>%
  rename_with(~ ifelse(.x == "date", .x, paste0(.x, ".rainfall")))

View(pivoted_data)

# Pivot the combined data to monthly total rainfall for each location
monthly_rainfall<- combined_data %>%
  group_by(location_name, year, month) %>%
  summarise(monthly_total_rainfall = sum(daily_rainfall, na.rm = TRUE)) %>%
  ungroup()

View(monthly_rainfall)

# Export the monthly rainfall to a CSV file
write.csv(
  monthly_rainfall_summary,
  "./Final_Monthly_Rainfall_Data.csv",
  row.names = FALSE
)

#Export the pivoted data to a CSV file
write.csv(
  pivoted_data,
  "./Final_To Merged_Rainfall_Data.csv",
  row.names = FALSE
)