library(readxl)
library(janitor)

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
library(tidyverse)

SABR_tile_MASTER_pre2024 <- read_csv("data/raw/SABR_tile_MASTER_pre2024.csv")
X2024_SABR_MASTER_water <- read_csv("data/raw/X2024_SABR_MASTER_water.csv")

# 2023 data
df_2023 <- SABR_tile_MASTER_pre2024 %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  select(date, plot, nitrate_ppm = no3_mg_l, ammonia_ppm = nh4_mg_l) 

# 2024 data
df_2024 <- X2024_SABR_MASTER_water %>% 
  separate(lab_id, into = c("date", "plot"), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) %>% 
  select(date, plot, nitrate_ppm, ammonia_ppm)

# Combine 2023 and 2024 data
df <- bind_rows(df_2023, df_2024)

nrow(df_2023) + nrow(df_2024) == nrow(df)

# Many NAs in 2023 data, we are missing the plot and date, but we have sample_id
# Check this file for date and plot

read_xlsx("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/TileDrainage_2023.xlsx") %>% 
  anyNA() 

studt_2023 <- read_xlsx("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Drainage/meter data/TileDrainage_2023.xlsx") %>% 
  drop_na() %>% 
  clean_names()

colnames(studt_2023)
# join studt_2023 to df by sample_id, keep all rows
