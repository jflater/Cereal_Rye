---
title: "Sorghum and Cereal Rye table and figure generation"
author: "Jared Flater"
format:
  html:
    theme: flatly
---
```{r}
library(tidyr)
library(readr)
library(ggplot2)
library(zoo)
library(dplyr)
```
```{r}
treatment_colors <- c(
  "Corn" = "#fda500",
  "Soy" = "#E9967A",
  "Sorghum" = "#D2B48C",
  "Sorghum + Rye" = "#8B4513"
)

theme_sabr <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.title = element_text(size = base_size + 2),
      axis.text = element_text(size = base_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size + 2),  # For facet labels
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_blank()  # No titles on plots
    )
}

```
# 📈 Figures 

## Figure 1. 
Title: Total Nitrogen Losses by Treatment (All Years) Content:

Bar chart or boxplot showing nitrate-N and N₂O emissions across all treatments and both years

Visual summary referenced in Nitrogen Losses Overview


We will need the nitrous oxide from the soil and inorganic N from drainage water data to create this figure.


```{r}
nitrous_oxide <- read_csv("data/clean/seasonal_flux_combined.csv")
glimpse(nitrous_oxide)
```
```{r}
colors <- create_color_theme(nitrous_oxide, "Treatment")
# Plotting cumulative flux with error bars
seasonal_flux <- nitrous_oxide

# Convert date strings to Date objects
seasonal_flux$year_month_day <- as.Date(seasonal_flux$year_month_day)


# Interpolate daily flux values for each plot
df <- seasonal_flux %>%
  group_by(growing_season, Treatment, RowvsInterrow, plot) %>%
  complete(year_month_day = seq(min(year_month_day), max(year_month_day), by = "day")) %>%
  arrange(year_month_day) %>%
  mutate(gnha_day_interpolated = zoo::na.approx(gnha_day_no_negative, na.rm = FALSE)) %>%
  fill(gnha_day, .direction = "downup") %>%
  dplyr::filter(RowvsInterrow != "Fertilizer_band")

# Calculate cumulative flux for each plot
df <- df %>%
  group_by(growing_season, Treatment, RowvsInterrow, plot) %>%
  mutate(cumulative_flux = cumsum(gnha_day_interpolated))

# Calculate cumulative flux for the final day and summarize by treatment
test <- df %>%
  group_by(growing_season, Treatment, plot) %>%
  dplyr::filter(year_month_day == max(year_month_day)) %>%
  ungroup() %>%
  group_by(Treatment, growing_season) %>%
  summarize(
    mean = mean(cumulative_flux, na.rm = TRUE),
    sd   = sd(cumulative_flux, na.rm = TRUE),
    n    = n(),
    se   = sd / sqrt(n),
    
    # Use t-distribution for 95% CI
    t_crit = qt(0.975, df = n - 1),  # 0.975 for a 95% CI on both sides
    
    ci_lower = mean - t_crit * se,
    ci_upper = mean + t_crit * se,
    
    .groups = "drop"
  )
  
# Set factor levels for Treatment
test$Treatment <- factor(test$Treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))

# Plotting cumulative flux with error bars
p <- ggplot(test, aes(x = Treatment, y = mean, fill = Treatment, group = Treatment)) +
  geom_col(position = position_dodge(0.8), color = "black") +  # Using geom_col for bar plots
  geom_errorbar(aes(ymin = mean - se , ymax = mean + se),
                width = 0.2, position = position_dodge(0.8)) +
  scale_fill_manual(values = colors) +  # Apply color palette
  facet_wrap( ~ growing_season, scales = "free_y") +  
  labs(
    y = expression("Cumulative N"[2]*"O Flux (g N ha"^"-1"*")"),
    x = "Treatment"
  ) +
  theme_sabr() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)

# Print the plot
print(p)
```


```{r}
cumulative_flow_data <- read_csv("data/clean/2023_2024_SABR_tile_cumulative_n.csv")
glimpse(drainage)
```
```{r}
gs_23 <- 
  cumulative_flow_data %>% 
  dplyr::filter(growing_season == 2023 & !year(date) == 2024)

nitrogen_summary <- gs_23 %>%
  group_by(date, treatment) %>%
  mutate(cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 10^6) / 0.145) %>% 
  summarise(Cumulative_Nitrogen_mean = mean(cumulative_n_loss_kg_ha),
            Cumulative_Nitrogen_se = sd(cumulative_n_loss_kg_ha) / sqrt(n())) %>%
  ungroup()

# Plotting cumulative nitrogen lost per hectare over time, facet by treatment
colors <- create_color_theme(nitrogen_summary, "treatment")  # This should match your actual dataset, not the example 'data'

library(scales)
# Plotting cumulative nitrogen lost per hectare over time, facet by treatment
z <- ggplot(nitrogen_summary, aes(x = date, y = Cumulative_Nitrogen_mean, group = treatment, color = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = Cumulative_Nitrogen_mean - Cumulative_Nitrogen_se, 
                  ymax = Cumulative_Nitrogen_mean + Cumulative_Nitrogen_se, fill = treatment), alpha = 0.2) +
  labs(x = "Date", 
       y = expression("Cumulative Inorganic N Leached (kg N ha"^"-1"*")")) +
  facet_wrap(~ treatment) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),  # Increase text size; adjust as needed
        axis.title = element_text(size = 16),  # Specific size for axis titles
        axis.text = element_text(size = 14),  # Specific size for axis text
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
        strip.text = element_text(size = 16),  # Specific size for facet labels
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", color = "white")) + # Set overall plot background to white
  scale_x_date(labels = date_format("%b %y"))  # Format dates to show abbreviated month and the last two digits of the year

z

ggsave("figures/Cumulative_Nitrogen_23.png", plot = z, width = 12, height = 8, dpi = 300)
gs_24 <- cumulative_flow_data %>% 
  dplyr::filter(growing_season == 2024 & !year(date) %in% c(2023,2025))

nitrogen_summary <- gs_24 %>%
  group_by(date, treatment) %>%
  mutate(cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 10^6) / 0.145) %>% 
  summarise(Cumulative_Nitrogen_mean = mean(cumulative_n_loss_kg_ha),
            Cumulative_Nitrogen_se = sd(cumulative_n_loss_kg_ha) / sqrt(n())) %>%
  ungroup()

# Plotting cumulative nitrogen lost per hectare over time, facet by treatment
colors <- create_color_theme(nitrogen_summary, "treatment")  # This should match your actual dataset, not the example 'data'

library(scales)
# Plotting cumulative nitrogen lost per hectare over time, facet by treatment
zz <- ggplot(nitrogen_summary, aes(x = date, y = Cumulative_Nitrogen_mean, group = treatment, color = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = Cumulative_Nitrogen_mean - Cumulative_Nitrogen_se, 
                  ymax = Cumulative_Nitrogen_mean + Cumulative_Nitrogen_se, fill = treatment), alpha = 0.2) +
  labs(x = "Date", 
       y = expression("Cumulative Inorganic N Leached (kg N ha"^"-1"*")")) +
  facet_wrap(~ treatment) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),  # Increase text size; adjust as needed
        axis.title = element_text(size = 16),  # Specific size for axis titles
        axis.text = element_text(size = 14),  # Specific size for axis text
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
        strip.text = element_text(size = 16),  # Specific size for facet labels
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", color = "white")) + # Set overall plot background to white
  scale_x_date(labels = date_format("%b %y"))  # Format dates to show abbreviated month and the last two digits of the year

zz
ggsave("figures/Cumulative_Nitrogen_24.png", plot = zz, width = 12, height = 8, dpi = 300)
```

```{r}
# For 2023
drainage_summary_23 <- gs_23 %>%
  group_by(treatment, plot) %>%
  slice_max(order_by = cumulative_n_loss_mg, with_ties = FALSE) %>%
  mutate(growing_season = 2023)


# For 2024
drainage_summary_24 <- gs_24 %>%
  group_by(treatment, plot) %>%
  slice_max(order_by = cumulative_n_loss_mg, with_ties = FALSE) %>%
  mutate(growing_season = 2024)

# Combine and format for final bar plot
drainage_summary <- bind_rows(drainage_summary_23, drainage_summary_24) %>%
  mutate(
    cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145,
    N_Type = "Nitrogen Leaching",
    Treatment = factor(treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))
  ) %>%
  group_by(growing_season, Treatment, N_Type) %>%
  summarise(
    mean = mean(cumulative_n_loss_kg_ha, na.rm = TRUE),
    sd = sd(cumulative_n_loss_kg_ha, na.rm = TRUE),
    n = n(),
    se = sd / sqrt(n),
    t_crit = qt(0.975, df = n - 1),
    ci_lower = mean - t_crit * se,
    ci_upper = mean + t_crit * se,
    .groups = "drop"
  )
```

```{r}
n2o_summary <- test %>%
  mutate(
    mean = mean / 1000,
    se = se / 1000,
    ci_lower = ci_lower / 1000,
    ci_upper = ci_upper / 1000,
    N_Type = "N₂O Emissions"
  )

```
```{r}
print(drainage_summary)
all_n_data <- bind_rows(drainage_summary, n2o_summary)
```

```{r}
ggplot(all_n_data, aes(x = Treatment, y = mean, fill = N_Type)) +
  geom_col(position = position_dodge(0.8), color = "black") +
  geom_errorbar(
    aes(ymin = mean - se, ymax = mean + se),
    position = position_dodge(0.8),
    width = 0.2
  ) +
  facet_wrap(~ growing_season) +
  scale_fill_manual(
  values = c("N₂O Emissions" = "#d73027", "Nitrogen Leaching" = "#4575b4"),  # Red = N2O, Blue = Drainage
  labels = c("N₂O Emissions" = expression("N"[2]*"O Emissions"), "Nitrate Leaching" = "Nitrate Leaching"),
  name = "N Loss Pathway"
) +
  labs(
  y = bquote("Cumulative N Loss (kg N ha"^{-1}*")"),
  x = "Treatment",
  fill = "N Loss Pathway"
) +
  theme_sabr() 

ggsave("figures/Figure_1.tiff",
       width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_1.pdf", width = 7.1, height = 5.5)  # double-column width

```


## Figure 2. 
Title: Nitrate Concentrations in Tile Drainage – Sorghum vs. Sorghum + Rye
Content:

Time-series line graph of nitrate concentrations over the growing season

Separate lines for each treatment

Spring drainage period clearly marked

```{r}
library(ggplot2)
library(dplyr)
library(lubridate)

# Create a year column
nitrate_data <- cumulative_flow_data %>%
  dplyr::filter(
    treatment %in% c("Sorghum", "Sorghum + Rye"),
    !is.na(approx_nitrate_mg_l)
  ) %>%
  mutate(year = year(date)) %>%
  group_by(date, treatment, year) %>%
  summarise(
    mean_nitrate = mean(approx_nitrate_mg_l, na.rm = TRUE),
    .groups = "drop"
  )

# Plot without SE ribbon
ggplot(nitrate_data, aes(x = date, y = mean_nitrate, color = treatment)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Sorghum" = "#D2B48C", "Sorghum + Rye" = "#8B4513")) +
  facet_wrap(~ year, scales = "free_x") +
  labs(
    x = "Date",
    y = expression("Inorganic-N Concentration (mg N L"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_sabr() +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors)
ggsave("figures/Figure_2.tiff",
       width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_2.pdf", width = 7.1, height = 5.5)
```



## Figure 3.
Title: Boxplots of Annual Nitrate-N Losses – Sorghum Treatments
Content:

Year-by-year boxplots comparing sorghum vs. sorghum + rye

Possibly combine both years with facet panels
```{r}
# Filter data for Sorghum treatments
sorghum_data <- cumulative_flow_data %>% 
  dplyr::filter(treatment %in% c("Sorghum", "Sorghum + Rye"))

# For each plot in each growing season, get the maximum cumulative_n_loss_mg value
sorghum_max <- sorghum_data %>% 
  group_by(growing_season, plot) %>% 
  summarize(
    max_loss = max(cumulative_n_loss_mg, na.rm = TRUE),
    treatment = first(treatment),
    .groups = "drop"
  )

# Convert cumulative nitrate-N loss from mg to kg/ha if needed (using your conversion factor)
sorghum_max <- sorghum_max %>% 
  mutate(max_loss_kg_ha = (max_loss / 1e6) / 0.145)

# Create boxplots by growing season and treatment
p3 <- ggplot(sorghum_max, aes(x = factor(growing_season), y = max_loss_kg_ha, fill = treatment)) +
  geom_boxplot() +
  labs(
    x = "Growing Season",
    y = expression("Cumulative Nitrate-N Loss (kg N ha"^{-1}*")"),
    title = "Annual Nitrate-N Losses – Sorghum Treatments"
  ) +
  theme_sabr()

# Display the plot
print(p3)

# Save the plots
ggsave("figures/Figure_3.tiff", plot = p3, width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_3.pdf", plot = p3, width = 7.1, height = 5.5)
```

## Figure 4.
Title: Time-Series of Daily N₂O Fluxes – Sorghum Treatments
Content:

Daily fluxes with rainfall and fertilization markers

Emphasize post-fertilizer emission peaks

## Figure 5.
Title: Row vs. Interrow N₂O Emissions Over Time
Content:

Line graph or grouped bar plot showing row/interrow flux differences

Highlight hot moments post-fertilizer and rainfall

## Figure 6.
Title: Heatmaps of Spatial N₂O Emissions by Treatment
Content:

Plot-level spatial data (e.g., grid of flux measurements)

One map per treatment or combined year

## Figure 7.
Title: Nitrate Leaching and N₂O Emissions – Corn and Soybean Systems
Content:

Bar chart or boxplot by crop and year

Separate panels or color coding for clarity


```{r}
nitrous_oxide

```

