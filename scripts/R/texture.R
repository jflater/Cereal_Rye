library(readxl)

texture <- read_excel("data/texture/PSA Report - AGRON-McDaniel-Potter-2024.xlsm", 
              sheet = "Main Data File format", n_max = 22) 

texture <- texture %>% 
  select(Sample, Sand = S, Silt = T, Clay = C) %>% 
  separate(Sample, into = c("ID", "plot"), sep = "_") %>% 
  dplyr::filter(!plot %in% c("SE", "NE", "SW", "NW")) %>% 
  mutate(plot = as.numeric(plot)) %>% 
  drop_na() 

library(tidyverse)

texture_long <- texture %>%
  pivot_longer(
    cols = c("Sand", "Silt", "Clay"),
    names_to = "fraction",
    values_to = "percentage"
  ) %>%
  mutate(percentage = as.numeric(percentage))

ggplot(texture_long, aes(x = factor(plot), y = percentage, fill = fraction)) +
  geom_col(position = "fill") +
  facet_wrap(~ plot,ncol = 5, scales = "free_x") +
  labs(
    x = "Plot",
    y = "Proportion of Soil Texture",
    fill = "Texture Fraction",
    title = "Soil Texture Composition by Plot"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +  # Show y-axis as %
  theme_minimal()
library(tidyverse)

texture_long <- texture %>%
  pivot_longer(
    cols = c("Sand", "Silt", "Clay"),
    names_to = "fraction",
    values_to = "percentage"
  ) %>%
  mutate(percentage = as.numeric(percentage))

ggplot(texture_long, aes(x = 1, y = percentage, fill = fraction)) +
  geom_col(position = "fill") +
  facet_wrap(~ plot) +
  labs(
    x = NULL,
    y = "Proportion of Soil Texture",
    fill = "Texture Fraction",
    title = "Soil Texture Composition by Plot"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis labels (since x=1)
    axis.ticks.x = element_blank()
  )

ggplot(texture_long, aes(x = factor(plot), y = percentage, fill = fraction)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3) +
  labs(
    x = "Plot",
    y = "Proportion of Soil Texture",
    fill = "Texture Fraction",
    title = "Soil Texture Composition by Plot"
  ) +
  scale_y_continuous(labels = percent_format()) +
  theme_minimal()

ggplot(texture_long, aes(x = 1, y = percentage, fill = fraction)) +
  geom_col(position = "fill") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_fill(vjust = 0.5),
            color = "white", size = 3) +
  scale_fill_manual(
    values = c("Sand" = "#F4A460",   # SandyBrown
               "Silt" = "black",
               "Clay" = "#DEB887")     # Burlywood (light brown)
  ) +
  facet_wrap(~ plot, ncol = 5) +
  labs(
    x = NULL,
    y = "Proportion of Soil Texture",
    fill = "Texture Fraction"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Hide x-axis labels (since x=1)
    axis.ticks.x = element_blank()
  )

ggsave("figures/texture.png", width = 8, height = 6, dpi = 300)
