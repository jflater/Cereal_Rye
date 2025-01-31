library(lubridate)
library(tidyverse)

# Define path containing meter data .csv file
data_dir <- "data/processed/Logger_Combined_Data23to24.csv"

# Read data from CSV file
water_data <- read_csv(data_dir)

test <- head(water_data, 300)

water_data_long <- water_data %>% 
  pivot_longer(
    cols = starts_with("plot_"),
    names_to = "plot",
    values_to = "flow"
  ) %>%
  mutate(
    plot = str_remove(plot, "plot_"),
    plot = as.factor(plot),
    date = as.Date(timestamp)
  ) 

df <- water_data_long %>% 
  group_by(date, plot) %>%
  summarise(flow = sum(flow)) 

ggplot(df, aes(x = date, y = flow, color = plot)) +
  geom_line() +
  labs(title = "Flow over time",
       x = "Date",
       y = "Flow",
       color = "Plot") +
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

df_may <- df %>% 
  dplyr::filter(year(date) == 2024, month(date) == 5)

ggplot(df_may, aes(x = date, y = flow, color = plot)) +
  geom_line() +
  labs(title = "Flow over time",
       x = "Date",
       y = "Flow",
       color = "Plot") +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# calculate cumulative flow for df_may using zoo package
library(zoo)
df_may_cum <- df_may %>% 
  group_by(plot) %>%
  mutate(cum_flow = cumsum(flow))

df_may_cum <- df_may_cum %>%
  mutate(date = as.Date(date))  # Convert if necessary

df_may_final <- df_may_cum %>%
  group_by(plot) %>%
  dplyr::filter(date == max(date, na.rm = TRUE))  # Ensure NA values don’t cause issues

# Create the base plot
plot <- ggplot(df_may_cum, aes(x = date, y = cum_flow, color = plot)) +
  geom_line() +
  labs(title = "Cumulative Flow Over Time",
       x = "Date",
       y = "Cumulative Flow",
       color = "Plot") +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Add text labels for total cumulative flow at the last date for each plot
plot + geom_text(data = df_may_final, aes(label = round(cum_flow, 1)), 
                 hjust = -0.2, vjust = -0.5, size = 4)

################################################################################
########################################
# Create a data frame with weekly flow for each plot, ensuring year separation
weekly_flow <- df %>% 
  mutate(
    year = year(date),
    week = isoweek(date)  # ISO week number ensures correct week assignment
  ) %>%
  group_by(year, week, plot) %>%
  summarise(weekly_flow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, week, plot)  # Ensure sorted order

# Print the first few rows of the weekly flow dataset
print(head(weekly_flow))

