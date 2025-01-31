# Load required libraries
library(readr)
library(lubridate)
library(purrr)
library(dplyr)

# Define directory path containing .dat files
data_dir <- "C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/LoggerAutoCollect/SABR_SmallDrainage_PitFlow_2025_01_23_0105"

# Get list of all .dat files in the directory
dat_files <- list.files(path = data_dir, pattern = "*.dat", full.names = TRUE)

#dat_files <- dat_files[1:2]  # For testing purposes, only read first two files

# Function to read and process a single .dat file
read_logger_data <- function(file_path) {
  data <- read_csv(file_path, skip = 3)  # Adjust skip based on header structure
  
  # Clean column names
  colnames(data) <- c("timestamp", "record_number", paste0("plot_", sprintf("%02d", 1:15)))
  
  return(data)
}

# Read and combine all .dat files
data_combined <- map_dfr(dat_files, read_logger_data)

# Save combined dataset to CSV
write_csv(data_combined, "data/processed/Logger_Combined_Data23to24.csv")

# Print summary
print(summary(data_combined))
