library(dplyr)
library(ggplot2)
library(corrplot)
library(tibble)
library(tidyr)
library(imputeTS)

# Load the data
rainfall_data <- read.csv("Final_Monthly_Rainfall_Data.csv")
water_quality_data <- read.csv("Final_Monthly_Avg_WQ_Data.csv")
merged_data <- read.csv("Final_Merged_Data.csv")
daily_rainfall_data <- read.csv("Final_Daily_Rainfall_Data.csv")

str(merged_data)

# Convert date columns to Date type
merged_data$date <- as.Date(merged_data$date, format = "%Y-%m-%d")
rainfall_data$date <- as.Date(rainfall_data$date, format = "%Y-%m-%d")
water_quality_data$date <- as.Date(water_quality_data$date, format = "%Y-%m-%d")
daily_rainfall_data$date <- as.Date(daily_rainfall_data$date, format = "%Y-%m-%d")

## RAINFALL DATA VISUALISATION ##
# Create Monthly Rainfall Heatmap summary graph for each location
ggplot(rainfall_data, aes(x = factor(month, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")), y = location_name, fill = monthly_total_rainfall)) +
  geom_tile() +
  scale_fill_gradient(low = "#f5bcbc", high = "#f05252") +
  labs(title = "Monthly Rainfall Heatmap", x = "Month", y = "Location", fill = "Monthly Total Rainfall (mm)") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Create a 3-year time series line graph for each location_name from monthly_rainfall_summary
ggplot(rainfall_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = monthly_total_rainfall, color = location_name, group = location_name)) +
  geom_line() +
  labs(title = "3-Year Monthly Rainfall Time Series", x = "Date", y = "Monthly Total Rainfall (mm)") +
  theme_minimal() +
  theme(legend.title = element_blank(),aspect.ratio = 9/16) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"))

# USED IN THE REPORT #
# Create a smoothed line time-series (3 years) graph for each location_name from monthly_rainfall_summary 
ggplot(rainfall_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = monthly_total_rainfall, color = location_name, group = location_name)) +
  geom_smooth(method = "loess", span = 0.3) +
  labs(title = "Monthly Rainfall Smoothed Line Plot 2022-2024", x = "Date", y = "Monthly Total Rainfall (mm)") +
  theme_minimal() +
  theme(legend.title = element_blank(), aspect.ratio = 9/16) +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"))



## WATER QUALITY DATA VISUALISATION ##
# Create a 3-year time series line graph for combined water quality data
ggplot(water_quality_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = monthly_avg_value, color = location_name, group = location_name)) +
  geom_line() +
  facet_wrap(~ measure, scales = "free_y") +
  labs(title = "3-Year Time Series Plot for Each Location and Measure", x = "Date", y = "Monthly Average Value") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")

## USED IN THE REPORT ##
# Create a smoothed line time-series (3 years) graph for each location_name from water_quality_data
ggplot(water_quality_data, aes(x = as.Date(paste(year, month, "01", sep = "-")), y = monthly_avg_value, color = location_name, group = location_name)) +
    geom_smooth(method = "loess", span = 0.3) +
    facet_wrap(~ measure, scales = "free_y") +
    labs(title = "Monthly Water Quality Smoothed Line Plot 2022-2024", x = "Date", y = "Monthly Average Value") +
    theme_minimal() +
    theme(legend.title = element_blank(), aspect.ratio = 9/16, panel.background = element_rect(fill = "lightblue")) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
    theme(panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"))

# Export the smoothed line time-series (3 years) graph for each location_name from water_quality_data in 600 dpi
ggsave("Monthly_Water_Quality_Smoothed_Line_Plot_2022-2024.png", width = 16, height = 9, units = "in", dpi = 600)

# USED IN THE REPORT #
# Boxplot for each location for each measure for water quality data
ggplot(water_quality_data, aes(x = location_name, y = monthly_avg_value, fill = location_name)) +
    geom_boxplot() +
    facet_wrap(~ measure, scales = "free_y") +
    labs(title = "Monthly Average Water Quality Boxplot by Location", x = "Location", y = "Monthly Average Value") +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = 24), axis.text.x = element_text(size = 16, angle = 45, hjust = 1))
 
## CORRELATION HEATMAPS ANALYSIS ##
# Create a correlation heatmap between water quality measures and rainfall with merged_data for all location and measures
# Filter out date columns
correlation_data <- merged_data %>%
  select(-date)

# Calculate the correlation matrix
correlation_matrix_corrplot <- cor(correlation_data, use = "pairwise.complete.obs")
# Create a correlation heatmap using corrplot for all columns exclude date
corrplot(correlation_matrix_corrplot, method = "color", type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)


## USED IN THE REPORT ##
# Filter out date columns
correlation_data <- merged_data %>%
  select(-date)

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data, method = "spearman", use = "pairwise.complete.obs")

# Correlation matrix, excluding the 'date' column
correlation_matrix <- cor(data[ , -which(names(data) == "date")], method = "spearman")

# Melt the correlation matrix into a long format
install.packages("reshape2")
library(reshape2)
correlation_long <- melt(correlation_matrix)

# Create the heatmap with ggplot2
ggplot(correlation_long, aes(Var1, Var2, fill = value, label = round(value, 2))) +
    geom_tile() +
    geom_text(color = "black", size = 3) +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                             midpoint = 0, limit = c(-1, 1), space = "Lab", 
                                             name = "Correlation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                axis.text.y = element_text(size = 12), 
                legend.position = "right", 
                aspect.ratio = 1) +
    labs(title = "Spearman Correlation Heatmap between Water Quality Measures and Rainfall", 
             x = "Variables", y = "Variables")

# Save the plot with 600 dpi
ggsave("Spearman_Correlation_Heatmap.png", width = 10, height = 10, units = "in", dpi = 600)

