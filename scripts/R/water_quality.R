
library(tidyverse)
library(janitor)

flow_data <- read_csv("data/clean/2023_2024_SABR_tile_flow_n.csv")
meta <- read_csv("data/meta/plot_treatments.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.numeric(plot))
compare_df_cols(flow_data, meta)
glimpse(flow_data)
glimpse(meta)

# join the data by plot and date
flow_data <- flow_data %>% 
  left_join(meta, by = c("plot", "date"))

#Plot flow
ggplot(flow_data, aes(x = date, y = flow_l)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ plot, ncol = 3, scales = "free_y") +
  labs(x = "Date",
       y = "Flow (l)") +
  theme_minimal()

# Pivot the nutrient columns into a long format.
# We're assuming the following columns:
# - nitrate_mg_ml, approx_nitrate_mg_l, ammonia_mg_l, approx_ammonia_mg_l
nutrient_data_long <- flow_data %>%
  pivot_longer(
    cols = c(nitrate_mg_ml, approx_nitrate_mg_l, ammonia_mg_l, approx_ammonia_mg_l),
    names_to = "measurement",
    values_to = "concentration"
  ) %>%
  mutate(
    # Identify nutrient type based on column names
    nutrient = if_else(grepl("nitrate", measurement), "Nitrate", "Ammonia"),
    # Determine the method: "Observed" for mg/l and "Approx" for approx values.
    method = if_else(grepl("approx", measurement), "Approx", "Observed")
  )

# Plot: Use geom_point for Observed and geom_line for Approx.
ggplot(nutrient_data_long, aes(x = date, y = concentration, color = nutrient)) +
  facet_wrap(~ plot, scales = "free_y", ncol = 3) +
  geom_line(data = dplyr::filter(nutrient_data_long, method == "Approx"), size = 1) +
  geom_point(data = dplyr::filter(nutrient_data_long, method == "Observed"), size = 2) +
  labs(
    x = "Date",
    y = "Concentration (mg/L)",
    color = "Nutrient"
  ) +
  theme_minimal()

# Ensure year column is available
nutrient_data_long <- nutrient_data_long %>%
  mutate(year = year(date)) %>% 
  dplyr::filter(!year %in% c(2025))

# Jittered point plot grouped by year, treatment, and measurement
ggplot(nutrient_data_long, aes(x = treatment, y = concentration, color = treatment)) +
  geom_jitter(width = 0.2, height = 0, alpha = 0.7) +
  facet_grid(year ~ measurement, scales = "free_y") +
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
        plot.background = element_rect(fill = "white", color = "white")) +
  labs(
    x = "Treatment",
    y = "Concentration (mg/L)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "none")

ggsave("figures/2023_2024_SABR_tile_nutrients.png", width = 12, height = 8, dpi = 300)
#########################################################
# Calculate N-loss
flow_data <- flow_data %>% 
  mutate(
    nitrate_loss_mg = approx_nitrate_mg_l * flow_l,
    ammonia_loss_mg = approx_ammonia_mg_l * flow_l, 
    total_n_loss_mg = nitrate_loss_mg + ammonia_loss_mg
  )

# Plot N-loss
ggplot(flow_data, aes(x = date, y = total_n_loss_mg)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ plot, ncol = 3) +
  labs(x = "Date",
       y = "Total N Loss (mg)") +
  theme_minimal()

# Calculate cumulative total_n_loss_mg, by plot and year
# Replace NA with 0 in total_n_loss_mg before calculating cumulative sum
cumulative_flow_data <- flow_data %>% 
  group_by(plot, year(date)) %>% 
  arrange(date) %>%
  mutate(
    total_n_loss_mg = if_else(is.na(total_n_loss_mg), 0, total_n_loss_mg),
    cumulative_n_loss_mg = cumsum(total_n_loss_mg)
  )

write_csv(cumulative_flow_data, "data/clean/2023_2024_SABR_tile_cumulative_n.csv")
# Plot cumulative N-loss

ggplot(cumulative_flow_data, aes(x = date, y = cumulative_n_loss_mg)) +
  geom_line(color = "steelblue", linewidth = 1) +
  facet_wrap(~ plot, ncol = 5) +
  labs(x = "Date",
       y = "Cumulative Total N Loss (mg)") +
  theme_minimal()

summary_data <- cumulative_flow_data %>%
  group_by(treatment, year = `year(date)`, date) %>% 
  summarize(mean_loss = mean(cumulative_n_loss_mg, na.rm = TRUE),
            sd_loss = sd(cumulative_n_loss_mg, na.rm = TRUE),
            n = n(),
            .groups = "drop") %>%
  mutate(se_loss = sd_loss / sqrt(n),
         ci_lower = mean_loss - qt(0.975, df = n - 1) * se_loss,
         ci_upper = mean_loss + qt(0.975, df = n - 1) * se_loss)

# Plot the mean cumulative N loss with CI ribbons, faceted by year:
ggplot(summary_data, aes(x = date, y = mean_loss, color = treatment, fill = treatment)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  facet_wrap(~ year, scales = "free_y") +
  labs(title = "Mean Cumulative Total N Loss by Treatment and Year",
       x = "Date",
       y = "Cumulative Total N Loss (mg)",
       color = "Treatment",
       fill = "Treatment") +
  theme_minimal()

library(dplyr)
library(ggplot2)

# If you want to count plots that have a non-NA cumulative_n_loss_mg
bar_data <- cumulative_flow_data %>%
  dplyr::filter(!is.na(cumulative_n_loss_mg)) %>%
  group_by(date, treatment) %>%
  summarize(n_plots = n_distinct(plot), .groups = "drop")

# Create a side-by-side bar chart (dodge position)
ggplot(bar_data, aes(x = date, y = n_plots, fill = treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Number of Plots Contributing Data per Day by Treatment",
       x = "Date",
       y = "Number of Plots") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

library(dplyr)
library(ggplot2)

# For each treatment and growing season, select the final day (maximum date)
summary_year <- cumulative_flow_data %>% 
  group_by(treatment, year(date)) %>% 
  dplyr::filter(date == max(date) & `year(date)` %in% c(2023,2024)) %>%  # choose the final date as representative of cumulative loss
  summarize(
    mean_loss = mean(cumulative_n_loss_mg, na.rm = TRUE),
    sd_loss   = sd(cumulative_n_loss_mg, na.rm = TRUE),
    n         = n(),
    .groups = "drop"
  ) %>% 
  mutate(
    se_loss   = sd_loss / sqrt(n),
    # Calculate the 95% CI. If n == 1, use df = 1 to avoid NaN.
    ci_lower  = mean_loss - qt(0.975, df = if_else(n > 1, n - 1, 1)) * se_loss,
    ci_upper  = mean_loss + qt(0.975, df = if_else(n > 1, n - 1, 1)) * se_loss,
    # Ensure lower CI is not below 0
    ci_lower  = if_else(ci_lower < 0, 0, ci_lower)
  )

ggplot(summary_year, aes(x = treatment, y = mean_loss, fill = treatment)) +
  geom_bar(stat = "identity", color = "black", position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, position = position_dodge(width = 0.9)) +
  facet_wrap(~ `year(date)`, scales = "free_y") +
  labs(title = "Cumulative Total N Loss by Treatment and Growing Season",
       x = "Treatment",
       y = "Cumulative Total N Loss (mg)") +
  theme_minimal() +
  theme(legend.position = "none")

# Create treatment intervals per plot
treatment_intervals <- cumulative_flow_data %>%
  dplyr::filter(growing_season %in% c(2023, 2024)) %>% 
  group_by(plot, treatment) %>%
  summarize(treatment_start = min(date, na.rm = TRUE),
            treatment_end = max(date, na.rm = TRUE),
            .groups = "drop")

# Plot with background shading for treatments and the cumulative line overlaid.
ggplot() +
  # Shaded background for each treatment interval
  geom_rect(data = treatment_intervals, 
            aes(xmin = treatment_start, xmax = treatment_end, 
                ymin = -Inf, ymax = Inf, fill = treatment),
            alpha = 0.2, inherit.aes = FALSE) +
  # Cumulative loss line
  geom_line(data = cumulative_flow_data %>% 
              dplyr::filter(lubridate::year(date) %in% c(2023, 2024)),
            aes(x = date, y = cumulative_n_loss_mg/10^6), 
            color = "steelblue", linewidth = 1) +
  facet_wrap(~ plot, ncol = 3) +
  labs(x = "Date",
       y = "Cumulative Total N Loss (kg)",
       fill = "Treatment") +
  theme_minimal()
#Cumulative_N_Loss#########################################################
create_color_theme <- function(data, column) {
  # Define colors for crops
  colors <- c("Corn" = "#fda500", "Miscanthus" = "#006501",
              "Switchgrass" = "#fe00fb", "Prairie" = "#0000fe",
              "Soy" = "#E9967A", "Sorghum" = "#D2B48C", "Sorghum + Rye" = "#8B4513")
  
  # Ensure factor levels match data and colors defined
  data[[column]] <- factor(data[[column]], levels = names(colors))
  
  # Return colors matched to factor levels in the data
  return(colors)
}


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

##Cumulative_Flow##########################################
# Calculate cumulative flow for each plot by growing season
# Replace NA with 0 if necessary for flow_l before cumsum() if needed.
cumulative_flow_data_flow <- flow_data %>% 
  mutate(flow_mm = flow_l / 1450) %>% 
  group_by(plot, growing_season) %>% 
  arrange(date) %>%
  mutate(cumulative_flow_l = cumsum(flow_l),
         cumulative_flow_mm = cumsum(flow_mm)) %>% 
  ungroup()

# For example, let's focus on growing season 2023:
gs_23_flow <- cumulative_flow_data_flow %>% 
  dplyr::filter(growing_season == 2023 & !year(date) == 2024)

# Summarize cumulative flow by date and treatment
flow_summary <- gs_23_flow %>%
  group_by(date, treatment) %>%
  summarize(cumulative_flow_mean = mean(cumulative_flow_mm, na.rm = TRUE),
            cumulative_flow_se = sd(cumulative_flow_mm, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

# Plot cumulative flow (in liters) with error ribbons, faceted by treatment
flow_plot_23 <- ggplot(flow_summary, aes(x = date, y = cumulative_flow_mean, group = treatment, color = treatment)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = cumulative_flow_mean - cumulative_flow_se, 
                  ymax = cumulative_flow_mean + cumulative_flow_se, fill = treatment), 
              alpha = 0.2, color = NA) +
  labs(x = "Date", 
       y = "Cumulative Drainage (mm)") +
  facet_wrap(~ treatment) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_date(labels = date_format("%b %y")) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"))

# Display the plot
flow_plot_23

# Save the plot to a file
ggsave("figures/Cumulative_Flow_2023.png", plot = flow_plot_23, width = 12, height = 8, dpi = 300)

gs_24_flow <- cumulative_flow_data_flow %>% 
  dplyr::filter(growing_season == 2024 & !year(date) %in% c(2023,2025))

flow_summary <- gs_24_flow %>%
  group_by(date, treatment) %>%
  summarize(cumulative_flow_mean = mean(cumulative_flow_mm, na.rm = TRUE),
            cumulative_flow_se = sd(cumulative_flow_mm, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

flow_plot_24 <- ggplot(flow_summary, aes(x = date, y = cumulative_flow_mean, group = treatment, color = treatment)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = cumulative_flow_mean - cumulative_flow_se, 
                  ymax = cumulative_flow_mean + cumulative_flow_se, fill = treatment), 
              alpha = 0.2, color = NA) +
  labs(x = "Date", 
       y = "Cumulative Drainage (mm)") +
  facet_wrap(~ treatment) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  scale_x_date(labels = date_format("%b %y")) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white"))

# Display the plot
flow_plot_24

# Save the plot to a file
ggsave("figures/Cumulative_Flow_2024.png", plot = flow_plot_24, width = 12, height = 8, dpi = 300)

###Cumulative_Plot_Flow#####################################
# Compute treatment intervals per plot
treatment_intervals_flow <- gs_24_flow %>% 
  group_by(plot, treatment) %>% 
  summarize(treatment_start = min(date, na.rm = TRUE),
            treatment_end = max(date, na.rm = TRUE),
            .groups = "drop")

# Create the faceted cumulative flow plot with treatment shading
cumulative_flow_plot <- ggplot() +
  # Background shading for treatment intervals
  geom_rect(data = treatment_intervals_flow,
            aes(xmin = treatment_start, xmax = treatment_end, 
                ymin = -Inf, ymax = Inf, fill = treatment),
            alpha = 0.2, inherit.aes = FALSE) +
  # Cumulative flow line
  geom_line(data = gs_24_flow, 
            aes(x = date, y = cumulative_flow_mm, color = treatment), 
            linewidth = 1) +
  facet_wrap(~ plot, ncol = 5) +
  labs(title = "Cumulative Drainage by Plot (Growing Season 2024)",
       x = "Date",
       y = "Cumulative Flow (mm)",
       color = "Treatment",
       fill = "Treatment") +
  scale_x_date(labels = date_format("%b %d")) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 14),
        text = element_text(size = 12))

# Display the plot
cumulative_flow_plot

# Save the figure (adjust path, width, and height as needed)
ggsave("figures/Cumulative_Plot_Flow_2024.png", plot = cumulative_flow_plot, width = 12, height = 8, dpi = 300)

treatment_intervals_flow <- gs_23_flow %>% 
  group_by(plot, treatment) %>% 
  summarize(treatment_start = min(date, na.rm = TRUE),
            treatment_end = max(date, na.rm = TRUE),
            .groups = "drop")

# Create the faceted cumulative flow plot with treatment shading
cumulative_flow_plot <- ggplot() +
  # Background shading for treatment intervals
  geom_rect(data = treatment_intervals_flow,
            aes(xmin = treatment_start, xmax = treatment_end, 
                ymin = -Inf, ymax = Inf, fill = treatment),
            alpha = 0.2, inherit.aes = FALSE) +
  # Cumulative flow line
  geom_line(data = gs_23_flow, 
            aes(x = date, y = cumulative_flow_mm, color = treatment), 
            linewidth = 1) +
  facet_wrap(~ plot, ncol = 5) +
  labs(title = "Cumulative Flow by Plot (Growing Season 2023)",
       x = "Date",
       y = "Cumulative Drainage (mm)",
       color = "Treatment",
       fill = "Treatment") +
  scale_x_date(labels = date_format("%b %d")) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 14),
        text = element_text(size = 12))

# Display the plot
cumulative_flow_plot

# Save the figure (adjust path, width, and height as needed)
ggsave("figures/Cumulative_Plot_Flow_2023.png", plot = cumulative_flow_plot, width = 12, height = 8, dpi = 300)
