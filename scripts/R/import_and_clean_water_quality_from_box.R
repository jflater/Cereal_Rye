library(readxl)
library(janitor)
library(lubridate)
library(tidyverse)

# From my PC:
#raw_X2024_SABR_MASTER_water <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_TileWater_Studt/2024/2024_SABR_MASTER_water.xlsm")
#raw_SABR_tile_MASTER_pre2024 <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_TileWater_Studt/2019-2023/SABR_tile_MASTER_pre2024.xlsx")
# From my Mac:
# SABR_tile_MASTER_pre2024 <- read_excel("../../Box-Box/McDaniel Lab 2.0/Projects/SABR_TileWater_Studt/2019-2023/SABR_tile_MASTER_pre2024.xlsx")
# X2024_SABR_MASTER_water <- read_excel("../../Box-Box/McDaniel Lab 2.0/Projects/SABR_TileWater_Studt/2024/2024_SABR_MASTER_water.xlsm")
# 
# colnames(X2024_SABR_MASTER_water)
# 
# colnames(SABR_tile_MASTER_pre2024)
# 
# # save both as .csv in data/raw
# 
# write_csv(X2024_SABR_MASTER_water, "data/raw/X2024_SABR_MASTER_water.csv")
# write_csv(SABR_tile_MASTER_pre2024, "data/raw/SABR_tile_MASTER_pre2024.csv")

SABR_tile_MASTER_pre2024 <- read_csv("data/raw/SABR_tile_MASTER_pre2024.csv")
X2024_SABR_MASTER_water <- read_csv("data/raw/X2024_SABR_MASTER_water.csv")

# 2023 data
df_2023 <- SABR_tile_MASTER_pre2024 %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  select(sample_id, date, plot, nitrate_ppm = no3_mg_l, ammonia_ppm = nh4_mg_l) 

# 2024 data
df_2024 <- X2024_SABR_MASTER_water %>% 
  separate(lab_id, into = c("date", "plot"), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y%m%d"),
         sample_number = as.integer(sample_number)) %>% 
  select(sample_id = sample_number, everything())

# Combine 2023 and 2024 data
df <- bind_rows(df_2023, df_2024)

nrow(df_2023) + nrow(df_2024) == nrow(df)

# Many NAs in 2023 data, we are missing the plot and date, but we have sample_id
# Check this file for date and plot

read_xlsx("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/TileDrainage_2023.xlsx") %>% 
  anyNA() 

studt_2023 <- read_xlsx("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/TileDrainage_2023.xlsx") %>% 
  clean_names() %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
         sample_id = as.double(sample_id))

glimpse(studt_2023)
glimpse(df_2023)


# join studt_2023 to df by date and plot, keep all rows

temp <- df_2023 %>% 
  left_join(studt_2023, by = c("sample_id"))  


water_n_2023 <- temp %>% 
  dplyr::filter(year(date.y) == 2023 & !plot.y %in% c("NE", "SE", "SW", "NW")) %>% 
  select(plot = plot.y, date = date.y, nitrate_mg_ml = nitrate_ppm, ammonia_mg_l = ammonia_ppm, sample_y_n) 

write_csv(water_n_2023, "data/clean/water_n_2023.csv")


# 2024 data
studt_2024 <- read_xlsx("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/TileDrainage_2024.xlsx") %>% 
  clean_names() %>% 
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
         sample_id = as.integer(sample_id))

compare_df_cols(studt_2024, df_2024)

# join studt_2024 to df_2024 by sample_id, keep all rows
temp_2024 <- df_2024 %>% 
  dplyr::filter(!plot %in% c("NE", "SE", "SW", "NW")) %>%
  select(-matches("^\\.\\.\\.[0-9]+$")) %>% 
  left_join(studt_2024, by = c("sample_id"))

# Do all rows have values in nitrate_ppm and ammonia_ppm?
anyNA(temp_2024$nitrate_ppm) # FALSE
anyNA(temp_2024$ammonia_ppm) # FALSE

# What row?
temp_2024 %>% 
  dplyr::filter(is.na(nitrate_ppm) | is.na(ammonia_ppm))

# According to comments ok to remove, remove that row
temp_2024 <- temp_2024 %>% 
  dplyr::filter(!is.na(nitrate_ppm) & !is.na(ammonia_ppm))

# Any rows with N in sample_y_n?
temp_2024 <- temp_2024  %>% 
  mutate(plot.y = case_when(
    sample_y_n == "N" & plot.y == "4" ~ "09",
    TRUE ~ plot.y
  ))

water_n_2024 <- temp_2024 %>%
  select(plot = plot.y, date = date.y, nitrate_ppm, ammonia_ppm, sample_y_n) 

# Save 
write_csv(water_n_2024, "data/clean/water_n_2024.csv")
