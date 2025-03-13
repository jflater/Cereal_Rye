# Calculate N-loss in tile drainage from SABR small plots
library(tidyverse)
library(janitor)
library(zoo)
library(ggplot2)
# 2023
flow <- read_csv("data/clean/2023_2024_SABR_tile_flow.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.factor(str_remove(plot, "^0+")))

n_conc_23 <- read_csv("data/clean/water_n_2023.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.factor(plot)) 

n_conc_24 <- read_csv("data/clean/water_n_2024.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.factor(plot)) %>% 
  select(date, plot, nitrate_mg_ml = nitrate_ppm, ammonia_mg_l = ammonia_ppm, sample_y_n)

compare_df_cols(n_conc_23, n_conc_24)

n_conc <- rbind(n_conc_23, n_conc_24)

compare_df_cols(flow, n_conc)

flow_n <- flow %>% 
  left_join(n_conc, by = c("plot", "date"))


# convert flow to liters from gallons
flow_n <- flow_n %>% 
  mutate(flow_l = flow_gallons * 3.78541)

# linear interpolation of concentrations, group by year and plot
df_interpolated <- flow_n %>% 
  group_by(plot, year(date)) %>% 
  arrange(date) %>%
  mutate(approx_nitrate_mg_l = na.approx(nitrate_mg_ml, x = date, na.rm = FALSE),
         approx_ammonia_mg_l = na.approx(ammonia_mg_l, x = date, na.rm = FALSE)) %>% 
  ungroup()

# Save a .csv
write_csv(df_interpolated, "data/clean/2023_2024_SABR_tile_flow_n.csv")

