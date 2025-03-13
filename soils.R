library(readxl)
library(tidyverse)
library(janitor)

soils_23 <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_Soils_Flater/2023/2023_SABR_MASTER_soil.xlsx") 
soils_24 <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_Soils_Flater/2024/2024_SABR_MASTER_soil.xlsx")

colnames(soils_23)

df <- soils_23 %>% 
  select(-starts_with("...")) %>% 
  dplyr::filter(!`Project/sampling` == "N2O-methods") %>% 
  clean_names() %>%
  mutate(
    plot_clean = if_else(
      str_detect(plot_id, "(p|r)\\d+"),
      str_extract(plot_id, "(?<=p|r)\\d+"),
      plot_id
    )
  ) %>%
  separate(plot_id, into = c("sampling_number", "plot", "row_location", "rep"), sep = "-") %>%
  mutate(nitrate = as.numeric(nitrate_ppm),
         ammonia = as.numeric(ammonia_ppm)) %>% 
  pivot_longer(
    cols = c(nitrate, ammonia),
    names_to = "inorganic_n",
    values_to = "ppm"
  ) 

# dotplot of inorganic_n by plot
df %>% 
  dplyr::filter(!plot_clean %in% c("NE", "NW", "SE", "SW")) %>% 
  ggplot(aes(x = plot_clean, y = ppm, color = inorganic_n)) +
  geom_point() +
  facet_wrap(~ project_sampling) +
  theme_bw() +
  labs(
    x = "Plot",
    y = "ppm",
    title = "Inorganic N by Plot"
  )

soils_24 <- soils_24 %>% 
  dplyr::filter(str_sub(lab_id, 1, 4) == "2024") %>% 
  separate(lab_id, into = c("date", "plot"), sep = "_") %>% 
  mutate(as.Date(as.character(date), format = "%Y%m%d"),
         plot = as.numeric(plot))

