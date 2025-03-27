library(readxl)
library(tidyverse)
library(janitor)
conflicted::conflicts_prefer(dplyr::filter())
soils_23 <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_Soils_Flater/2023/2023_SABR_MASTER_soil.xlsx") 
soils_24 <- read_excel("C:/Users/jflater/Box/McDaniel Lab 2.0/Projects/SABR_Soils_Flater/2024/2024_SABR_MASTER_soil.xlsx")

df <- soils_23 %>% 
  # Remove extra columns that start with "..."
  select(-starts_with("...")) %>% 
  # Clean column names (this converts "Project/sampling" to "project_sampling")
  clean_names() %>%
  # Filter out rows with N2O-methods sampling
  dplyr::filter(project_sampling != "N2O-methods") %>% 
  # Create a cleaned plot id by extracting digits after "p" or "r"
  mutate(
    plot_clean = if_else(
      str_detect(plot_id, "(p|r)\\d+"),
      str_extract(plot_id, "(?<=p|r)\\d+"),
      plot_id
    )
  ) %>%
  # Convert nitrate and ammonia from character to numeric
  mutate(
    nitrate = as.numeric(nitrate_ppm),
    ammonia = as.numeric(ammonia_ppm)
  ) %>% 
  # Reshape the data so that nitrate and ammonia values are in one column
  pivot_longer(
    cols = c(nitrate, ammonia),
    names_to = "inorganic_n",
    values_to = "ppm"
  ) %>% 
  separate(plot_id, into = c("collection_number", "plot", "row_location", "rep"), sep = "-") %>% 
  # Try to confirm these dates are correct
  mutate(
    date = case_when(
      # For project_sampling values that are actual dates in our shorthand, convert them.
      # For example, if project_sampling is "12-2023", we assume December 1, 2023;
      # if "Spring 2023", we assume April 1, 2023.
      project_sampling == "12-2023" ~ as.Date("2023-12-01"),
      project_sampling == "Spring 2023" ~ as.Date("2023-04-01"),
      # For SABR-chem rows, use the first digit from collection_number as the day
      # and paste it into a reference date (here "2023-06-" is used; adjust as needed).
      project_sampling == "SABR-chem" ~ as.Date(
        paste0("2023-06-", str_pad(substr(collection_number, 1, 1), width = 2, pad = "0")),
        format = "%Y-%m-%d"
      ),
      project_sampling == "Summer 2023" ~ as.Date(
        paste0("2023-06-", str_pad(substr(collection_number, 1, 1), width = 2, pad = "0")
      )),
      # Otherwise, try converting project_sampling directly (if it already is in an ISO format)
      TRUE ~ as.Date(project_sampling, format = "%Y-%m-%d")
    )
  )

# Plot: side-by-side bars for inorganic N by plot (excluding specific plot_clean values)
df %>% 
  dplyr::filter(!plot_clean %in% c("NE", "NW", "SE", "SW")) %>% 
  ggplot(aes(x = date, y = ppm, fill = inorganic_n)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_bw() +
  labs(
    x = "Plot",
    y = "ppm",
    title = "Inorganic N by Plot"
  )
