# Load required libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(ggrepel)
library(viridis)
library(patchwork)
library(ggpubr)

# Read and reshape the data
df <- read_csv("data/processed/Logger_Combined_Data23to24.csv") %>%
  pivot_longer(cols = starts_with("plot_"),
               names_to = "plot",
               values_to = "flow") %>%
  mutate(
    plot = as.factor(str_remove(plot, "plot_")),
    date = as.Date(timestamp, format = "%Y-%m-%d %H:%M:%S")
  ) %>%
  group_by(date, plot) %>%
  summarise(flow = sum(flow, na.rm = TRUE), .groups = "drop")

plot_treatments <- read_csv("data/meta/plot_treatments.csv")

  # Filter May 2024 data and compute cumulative flow per plot
df_may <- df %>% dplyr::filter(year(date) == 2024, month(date) == 5)
df_may_cum <- df_may %>% 
  group_by(plot) %>% 
  arrange(date) %>% 
  mutate(cum_flow = cumsum(flow)) %>% 
  ungroup()
df_may_final <- df_may_cum %>% 
  group_by(plot) %>% 
  dplyr::filter(date == max(date, na.rm = TRUE)) %>% 
  ungroup()

# Panel 1: Heatmap of daily flow in May 2024
p_heat <- ggplot(df_may, aes(x = date, y = plot, fill = flow)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "viridis", name = "Daily Flow") +
  labs(title = "Daily Flow Heatmap (May 2024)",
       x = "Date") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Panel 2: Cumulative flow line plot with repelled plot labels
p_line <- ggplot(df_may_cum, aes(x = date, y = cum_flow, color = plot)) +
  geom_line(size = 1) +
  geom_point(data = df_may_final, size = 3) +
  geom_text_repel(data = df_may_final, aes(label = plot),
                  nudge_x = 0.2, nudge_y = 0, size = 4, show.legend = FALSE) +
  labs(title = "Cumulative Flow Over Time (May 2024)",
       x = "Date", y = "Cumulative Flow") +
  scale_x_date(date_breaks = "1 day", date_labels = "%b %d") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")

# Combine panels vertically
combined_plot <- p_heat / p_line +
  plot_layout(heights = c(1, 2)) 

# Display the composite plot
print(combined_plot)

# Create a data frame with weekly flow for each plot
weekly_flow <- df %>% 
  mutate(year = year(date),
         week = isoweek(date)) %>%
  group_by(year, week, plot) %>%
  summarise(weekly_flow = sum(flow, na.rm = TRUE), .groups = "drop") %>%
  arrange(year, week, plot)
print(head(weekly_flow))

######
# Join df to plot_treatments by date and plot
df_treatments <- df %>%
  left_join(plot_treatments, by = c("plot", "date")) %>%
  drop_na()

# select only days where flow is > 0 in at least one plot

filtered_data <- df_treatments %>%
  dplyr::filter(growing_season %in% c(2023, 2024)) %>%
  group_by(date) %>%
  dplyr::filter(any(flow > 0)) %>%
  ungroup()

#ggpubr boxplot of flow by treatment
p_box_a <- ggboxplot(filtered_data, x = "treatment", y = "flow",
                   color = "treatment", palette = "jco",
                   add = "jitter",
                   xlab = "Treatment", ylab = "Flow",
                   title = "Flow by Treatment and Plot",
                   ggtheme = theme_minimal())
print(p_box)

#########
# Load required packages
library(car)         # For leveneTest()
library(ggpubr)      # For stat_compare_means()
library(kableExtra)  # For neat HTML tables
library(ggsci)
# 1) Check normality (Shapiro-Wilk)
model <- aov(flow ~ treatment, data = filtered_data)
shapiro.test(residuals(model))  # If p < 0.05, data deviate from normality

# 2) Check homogeneity of variance (Levene's test)
leveneTest(flow ~ treatment, data = filtered_data) 
# If p < 0.05, variances are not equal

# 3) Kruskal-Wallis test (non-parametric alternative to ANOVA)
kruskal_result <- kruskal.test(flow ~ treatment, data = filtered_data)
print(kruskal_result)

# 4) Post-hoc pairwise comparisons (Wilcoxon) with multiple-testing correction
pairwise_result <- pairwise.wilcox.test(
  x = filtered_data$flow, 
  g = filtered_data$treatment, 
  p.adjust.method = "BH",
)
# Display in a nice table
kable(pairwise_result$p.value, format = "html") %>%
  kable_styling(full_width = FALSE)

# 5) Prepare data for stat_pvalue_manual (optional, if you want to add p-values onto a plot)
pairwise_pvals <- as.data.frame(as.table(pairwise_result$p.value))
colnames(pairwise_pvals) <- c("group1", "group2", "p_value")

# 6) Visualize with boxplots and add p-values
p_box <- ggboxplot(filtered_data, x = "treatment", y = "flow",
                   color = "treatment",
                   add = "jitter",
                   xlab = "Treatment", ylab = "Flow",
                   legend.title = "Treatment",
                   ggtheme = theme_minimal())

p_box + 
  stat_compare_means(
    method = "kruskal.test", 
    label = "p.format",
    label.x = 1.5,  # Adjust horizontally as needed
    label.y = 1.1 * max(filtered_data$flow)
  ) +
  stat_pvalue_manual(
    pairwise_pvals,
    label = "p_value",
    y.position = seq(
      from = 1.05 * max(filtered_data$flow),
      by   = 0.05 * max(filtered_data$flow),
      length.out = nrow(pairwise_pvals)   # Ensure length matches number of comparisons
    ),
    step.increase = 0.1
  ) + 
  scale_color_frontiers() 
