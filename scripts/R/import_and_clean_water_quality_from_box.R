library(readxl)
# From my PC:
# X2024_SABR_MASTER_water <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_TileWater_Studt/2024/2024_SABR_MASTER_water.xlsm")
# SABR_tile_MASTER_pre2024 <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_TileWater_Studt/2019-2023/SABR_tile_MASTER_pre2024.xlsx")
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
  mutate(date = as.Date(date, format = "%Y%m%d")) 

df_2024 <- X2024_SABR_MASTER_water %>% 
  separate(lab_id, into = c("date", "plot"), sep = "_", remove = FALSE) %>% 
  mutate(date = as.Date(date, format = "%Y%m%d")) 
