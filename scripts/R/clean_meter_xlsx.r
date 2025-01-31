# Load required libraries
library(readxl)
library(dplyr)
library(lubridate)
library(janitor)

# Define file path (Update this path as needed)
file_path <- "C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/TileDrainage_2024.xlsx"

# Read data from Sheet1 and clean column names
data <- read_excel(file_path, sheet = "Sheet1") %>% 
  clean_names()

# Convert 'date' column from YYYYMMDD to Date format
data <- data %>% 
  mutate(date = ymd(as.character(date)))

# Ensure 'meter_reading' is numeric
data <- data %>% 
  mutate(meter_reading = as.numeric(meter_reading))

# Convert 'sample_y_n' to logical (TRUE/FALSE)
data <- data %>% 
  mutate(sample_y_n = ifelse(sample_y_n == "Y", TRUE, FALSE))

# Remove unnecessary columns or rows with all NA values
data <- data %>% 
  dplyr::filter(!is.na(meter_reading))

# Sort data by plot and date
data <- data %>% 
  arrange(plot, date)

# Calculate flow between readings
data <- data %>% 
  group_by(plot) %>% 
  mutate(flow = c(0, diff(meter_reading))) %>%  # First reading of each plot is set to 0
  ungroup()

# Save cleaned dataset with flow calculations to CSV
write.csv(data, "data/processed/Cleaned_Water_Meter_Data_2024.csv", row.names = FALSE)

# Print summary of cleaned data
print(summary(data))


