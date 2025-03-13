library(readxl)
library(janitor)
library(dplyr)
# Meter Data
# Path to Box folder
path <- c("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/")

# List only the TileDrainage files for years 2023, 2024, 2025 with full paths, these
# are the hand recordings from when samples were collected. The alternative would be the
# data logger files. 

tile_files <- list.files(path, pattern = "^TileDrainage_(2023|2024|2025)\\.xlsx$", full.names = TRUE)

tile_data <- lapply(tile_files, read_excel)

names(tile_data) <- c("d_23", "d_24", "d_25")

compare_df_cols(tile_data$d_23, tile_data$d_24, tile_data$d_25)

# flow in 23
flow_23 <- tile_data$d_23 %>% 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"),
         meter_reading = as.numeric(`Meter Reading`)) %>% 
  group_by(Plot) %>% 
  arrange(Plot, Date) %>% 
  mutate(flow = meter_reading - dplyr::lag(meter_reading)) %>%
  ungroup() %>% 
  clean_names() %>% 
  dplyr::filter(plot %in% c("NE", "SE", "NW", "SW"))

glimpse(flow_23)

# flow in 24
flow_24 <- tile_data$d_24 %>% 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"),
         meter_reading = as.numeric(`Meter Reading`)) %>% 
  group_by(Plot) %>% 
  arrange(Plot, Date) %>% 
  mutate(flow = meter_reading - dplyr::lag(meter_reading)) %>%
  ungroup() %>% 
  clean_names() %>% 
  dplyr::filter(plot %in% c("NE", "SE", "NW", "SW"))

glimpse(flow_24)

# bar plot with flow by date 
library(ggplot2)
flow_23 %>% 
  dplyr::filter(plot %in% c("NW")) %>% 
  ggplot(aes(x = date, y = flow, color = plot)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Flow in 2023", x = "Date", y = "Flow (meter reading)") + 
  facet_wrap(~plot)

  

