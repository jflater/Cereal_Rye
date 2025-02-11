# Make a data frame with growing season and treatment for each plot. 
library(tidyverse)
library(lubridate)

plot_ids <- factor(str_pad(1:15, width = 2, pad = "0"))

date_range <- seq(ymd("2023-01-01"), ymd("2025-01-01"), by = "day")

df <- expand_grid(date = date_range, plot = plot_ids) %>% 
  mutate(growing_season = case_when(
    date >= ymd("2023-04-01") & date <= ymd("2024-01-01") ~ "2023",
    date >= ymd("2024-04-01") & date <= ymd("2025-04-01") ~ "2024",
    TRUE ~ "2022"
  ))

df <- df %>%
  mutate(
    treatment = case_when(
      growing_season == "2023" ~ case_when(
        plot %in% c(01, 08, 11, 14) ~ "Corn",
        plot %in% c(02, 03, 05, 15) ~ "Sorghum",
        plot %in% c(07, 09, 12, 13) ~ "Sorghum + Rye",
        TRUE ~ "Soy"
      ),
      growing_season == "2024" ~ case_when(
        plot %in% c(01, 08, 11, 14) ~ "Soy",
        plot %in% c(02, 03, 05, 15) ~ "Sorghum",
        plot %in% c(07, 09, 12, 13) ~ "Sorghum + Rye",
        TRUE ~ "Corn"
      ),
      TRUE ~ "Soy"
    )
  )

# Now save the df

write_csv(df, "data/plot_treatments.csv")


