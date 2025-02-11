# Load necessary libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(ggrepel)
library(ggpubr)

# Read and reshape the data
df <- read_csv("data/processed/Logger_Combined_Data23to24.csv") %>% 
  pivot_longer(cols = starts_with("plot_"),
               names_to = "plot",
               values_to = "flow") %>%
  mutate(plot = as.factor(str_remove(plot, "plot_")),
         date = as.Date(timestamp)) %>% 
  group_by(date, plot) %>% 
  summarise(flow = sum(flow, na.rm = TRUE), .groups = "drop")

# Plot overall flow (1-week breaks)
p1 <- ggplot(df, aes(x = date, y = flow, color = plot)) +
  geom_line() +
  labs(title = "Flow over time", x = "Date", y = "Flow", color = "Plot") +
  scale_x_date(date_breaks = "1 week", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p1)

# Filter data for May 2024 and plot (1-day breaks)
df_may <- df %>% filter(year(date) == 2024, month(date) == 5)
p2 <- ggplot(df_may, aes(x = date, y = flow, color = plot)) +
  geom_line() +
  labs(title = "Flow over time (May 2024)", x = "Date", y = "Flow", color = "Plot") +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p2)

# Calculate cumulative flow for May 2024 by plot
df_may_cum <- df_may %>% 
  group_by(plot) %>% 
  arrange(date) %>% 
  mutate(cum_flow = cumsum(flow)) %>% 
  ungroup()
df_may_final <- df_may_cum %>% 
  group_by(plot) %>% 
  filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup()

# Base cumulative flow plot with dodged text labels using ggrepel
p3 <- ggplot(df_may_cum, aes(x = date, y = cum_flow, group = plot)) +
  geom_line(aes(color = plot)) +
  labs(title = "Cumulative Flow Over Time (May 2024)", x = "Date", y = "Cumulative Flow") +
  scale_x_date(date_breaks = "1 day", date_labels = "%Y-%m-%d") +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  geom_text_repel(data = df_may_final, 
                  aes(label = paste("Plot", plot, ":", round(cum_flow, 1))),
                  nudge_x = 0.1, nudge_y = 0.1, size = 4)
print(p3)

# Create a data frame with weekly flow (grouped by ISO week and year)
weekly_flow <- df %>% 
  mutate(year = year(date),
         week = isoweek(date)) %>%
  group_by(year, week, plot) %>%
  summarise(weekly_flow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, week, plot)
print(head(weekly_flow))
