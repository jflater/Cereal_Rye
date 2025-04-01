library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(conflicted)

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

########
conflict_prefer("filter", "dplyr")

nitrous_oxide <- read_csv("data/clean/seasonal_flux_combined.csv")

# Convert date strings to Date objects if not already done
nitrous_oxide <- nitrous_oxide %>%
  mutate(year_month_day = as.Date(year_month_day))

# Define the season boundaries for each observation
# Assuming 'growing_season' is numeric (e.g., 2023 meaning the season runs from 2023-04-01 to 2024-03-31)
nitrous_oxide <- nitrous_oxide %>%
  mutate(
    season_start = as.Date(paste0(growing_season, "-04-01")),
    season_end   = as.Date(paste0(growing_season + 1, "-03-31"))
  )

# Filter the data so that only dates within the defined growing season are used
df <- nitrous_oxide %>%
  dplyr::filter(year_month_day >= season_start & year_month_day <= season_end) %>%
  group_by(growing_season, Treatment, RowvsInterrow, plot) %>%
  # Fill in missing days within the season
  complete(year_month_day = seq(min(year_month_day), max(year_month_day), by = "day")) %>%
  arrange(year_month_day) %>%
  mutate(gnha_day_interpolated = zoo::na.approx(gnha_day_no_negative, na.rm = FALSE)) %>%
  fill(gnha_day, .direction = "downup") %>%
  dplyr::filter(RowvsInterrow != "Fertilizer_band")

# Calculate the cumulative flux within each plot for the growing season
df <- df %>%
  group_by(growing_season, Treatment, RowvsInterrow, plot) %>%
  mutate(cumulative_flux = cumsum(gnha_day_interpolated))

# For each plot, use the last day within the season (which is now guaranteed to be within the April–March window)
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
    t_crit = qt(0.975, df = n - 1),
    ci_lower = mean - t_crit * se,
    ci_upper = mean + t_crit * se,
    .groups = "drop"
  )

# Set factor levels if desired
test$Treatment <- factor(test$Treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))

# Plot cumulative flux with error bars
p <- ggplot(test, aes(x = Treatment, y = mean, fill = Treatment, group = Treatment)) +
  geom_col(position = position_dodge(0.8), color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.8)) +
  scale_fill_manual(values = treatment_colors) +
  facet_wrap(~ growing_season, scales = "free_y") +
  labs(
    y = expression("Cumulative N"[2]*"O Flux (g N ha"^"-1"*")"),
    x = "Treatment"
  ) +
  theme_sabr()

print(p)

# At the very start of your script:
library(conflicted)
conflict_prefer("filter", "dplyr")

# Read in the cumulative flow data and check its structure
cumulative_flow_data <- read_csv("data/clean/2023_2024_SABR_tile_cumulative_n.csv")
glimpse(cumulative_flow_data)  # Fixed from glimpse(drainage)

# Filter for growing_season 2023, excluding dates where the year is 2024
gs_23 <- cumulative_flow_data %>% 
  dplyr::filter(growing_season == 2023, `year(date)` != 2024)

nitrogen_summary <- gs_23 %>%
  group_by(date, treatment) %>%
  mutate(cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145) %>% 
  summarise(Cumulative_Nitrogen_mean = mean(cumulative_n_loss_kg_ha),
            Cumulative_Nitrogen_se = sd(cumulative_n_loss_kg_ha) / sqrt(n())) %>%
  ungroup()

# Generate color palette for plotting (ensure your create_color_theme() works as expected)
colors <- create_color_theme(nitrogen_summary, "treatment")

library(scales)
# Plot cumulative nitrogen lost per hectare over time for 2023
z <- ggplot(nitrogen_summary, aes(x = date, y = Cumulative_Nitrogen_mean, 
                                  group = treatment, color = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = Cumulative_Nitrogen_mean - Cumulative_Nitrogen_se, 
                  ymax = Cumulative_Nitrogen_mean + Cumulative_Nitrogen_se, fill = treatment), 
              alpha = 0.2) +
  labs(x = "Date", 
       y = expression("Cumulative Inorganic N Leached (kg N ha"^"-1"*")")) +
  facet_wrap(~ treatment) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_x_date(labels = date_format("%b %y"))

z

ggsave("figures/Cumulative_Nitrogen_23.png", plot = z, width = 12, height = 8, dpi = 300)

# Filter for growing_season 2024, excluding dates where the year is 2023 or 2025
gs_24 <- cumulative_flow_data %>% 
  dplyr::filter(growing_season == 2024, !(`year(date)` %in% c(2023,2025)))

nitrogen_summary <- gs_24 %>%
  group_by(date, treatment) %>%
  mutate(cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145) %>% 
  summarise(Cumulative_Nitrogen_mean = mean(cumulative_n_loss_kg_ha),
            Cumulative_Nitrogen_se = sd(cumulative_n_loss_kg_ha) / sqrt(n())) %>%
  ungroup()

colors <- create_color_theme(nitrogen_summary, "treatment")

zz <- ggplot(nitrogen_summary, aes(x = date, y = Cumulative_Nitrogen_mean, 
                                   group = treatment, color = treatment)) +
  geom_line() +
  geom_ribbon(aes(ymin = Cumulative_Nitrogen_mean - Cumulative_Nitrogen_se, 
                  ymax = Cumulative_Nitrogen_mean + Cumulative_Nitrogen_se, fill = treatment), 
              alpha = 0.2) +
  labs(x = "Date", 
       y = expression("Cumulative Inorganic N Leached (kg N ha"^"-1"*")")) +
  facet_wrap(~ treatment) +
  scale_color_manual(values = colors) +
  scale_fill_manual(values = colors) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_x_date(labels = date_format("%b %y"))

zz

ggsave("figures/Cumulative_Nitrogen_24.png", plot = zz, width = 12, height = 8, dpi = 300)

# For drainage summary:
drainage_summary_23 <- gs_23 %>%
  group_by(treatment, plot) %>%
  slice_max(order_by = cumulative_n_loss_mg, with_ties = FALSE) %>%
  mutate(growing_season = 2023)

drainage_summary_24 <- gs_24 %>%
  group_by(treatment, plot) %>%
  slice_max(order_by = cumulative_n_loss_mg, with_ties = FALSE) %>%
  mutate(growing_season = 2024)

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

# Assuming 'test' from your earlier code (Figure 1 cumulative flux summary) is available:
n2o_summary <- test %>%
  mutate(
    mean = mean / 1000,
    se = se / 1000,
    ci_lower = ci_lower / 1000,
    ci_upper = ci_upper / 1000,
    N_Type = "N₂O Emissions"
  )

print(drainage_summary)
all_n_data <- bind_rows(drainage_summary, n2o_summary)

ggplot(all_n_data, aes(x = Treatment, y = mean, fill = N_Type)) +
  geom_col(position = position_dodge(0.8), color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                position = position_dodge(0.8), width = 0.2) +
  facet_wrap(~ growing_season) +
  scale_fill_manual(
    values = c("N₂O Emissions" = "#d73027", "Nitrogen Leaching" = "#4575b4"),
    labels = c("N₂O Emissions" = expression("N"[2]*"O Emissions"), 
               "Nitrate Leaching" = "Nitrate Leaching"),
    name = "N Loss Pathway"
  ) +
  labs(
    y = bquote("Cumulative N Loss (kg N ha"^{-1}*")"),
    x = "Treatment",
    fill = "N Loss Pathway"
  ) +
  theme_sabr()

ggsave("figures/Figure_1.tiff", width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_1.pdf", width = 7.1, height = 5.5)

library(knitr)
library(dplyr)

# Create a table for the data in Figure 1 by selecting the relevant columns
figure1_table <- all_n_data %>%
  select(growing_season, Treatment, N_Type, mean, sd, n, se, ci_lower, ci_upper)

# Display the table with a caption; adjust digits as needed
kable(figure1_table, digits = 2, caption = "Summary of Cumulative N Losses (Figure 1)")

########
library(ggplot2)
library(dplyr)
library(lubridate)

# Filter and summarize nitrate concentration data
nitrate_data <- cumulative_flow_data %>%
  dplyr::filter(treatment %in% c("Sorghum", "Sorghum + Rye"),
         !is.na(approx_nitrate_mg_l)) %>%
  mutate(year = year(date)) %>%
  group_by(date, treatment, year) %>%
  summarise(mean_nitrate = mean(approx_nitrate_mg_l, na.rm = TRUE), 
            .groups = "drop")

# Create a marker for the spring drainage period (assumed to start on April 1)
spring_marker <- nitrate_data %>%
  distinct(year) %>%
  mutate(spring_date = as.Date(paste0(year, "-04-01")))

# Plot nitrate concentration over time, with a spring marker in each facet
p2 <- ggplot(nitrate_data, aes(x = date, y = mean_nitrate, color = treatment)) +
  geom_line(size = 1) +
  geom_vline(data = spring_marker, aes(xintercept = spring_date), 
             linetype = "dashed", color = "blue") +
  scale_color_manual(values = c("Sorghum" = "#D2B48C", "Sorghum + Rye" = "#8B4513")) +
  facet_wrap(~ year, scales = "free_x") +
  labs(
    x = "Date",
    y = expression("Inorganic-N Concentration (mg N L"^{-1}*")"),
    color = "Treatment"
  ) +
  theme_sabr()

p2

ggsave("figures/Figure_2.tiff", plot = p2, width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_2.pdf", plot = p2, width = 7.1, height = 5.5)


########
# Filter for Sorghum treatments
sorghum_data <- cumulative_flow_data %>% 
  dplyr::filter(treatment %in% c("Sorghum", "Sorghum + Rye"))

# For each plot in each growing season, extract the maximum cumulative_n_loss_mg value
sorghum_max <- sorghum_data %>% 
  group_by(growing_season, plot) %>% 
  summarize(
    max_loss = max(cumulative_n_loss_mg, na.rm = TRUE),
    treatment = first(treatment),
    .groups = "drop"
  )

# Convert mg to kg/ha using your conversion factor
sorghum_max <- sorghum_max %>% 
  mutate(max_loss_kg_ha = (max_loss / 1e6) / 0.145)

# Create boxplots by growing season (x-axis) and treatment (fill)
p3 <- ggplot(sorghum_max, aes(x = factor(growing_season), y = max_loss_kg_ha, fill = treatment)) +
  geom_boxplot() +
  #stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "black") +
  labs(
    x = "Growing Season",
    y = expression("Cumulative Inorganic-N Loss (kg N ha "^{-1}*")"),
    fill = "Treatment"
  ) +
  theme_sabr() +
  scale_fill_manual(values = treatment_colors)


p3

# Save the figure as TIFF and PDF
ggsave("figures/Figure_3.tiff", plot = p3, width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_3.pdf", plot = p3, width = 7.1, height = 5.5)
# Create a summary table for Figure 3 from the sorghum_max data
figure3_table <- sorghum_max %>% 
  group_by(growing_season, treatment) %>% 
  summarise(
    mean_loss = mean(max_loss_kg_ha, na.rm = TRUE),
    sd_loss   = sd(max_loss_kg_ha, na.rm = TRUE),
    n         = n(),
    se_loss   = sd_loss / sqrt(n),
    .groups   = "drop"
  ) %>% 
  arrange(growing_season, treatment)

library(knitr)
kable(figure3_table, digits = 2, 
      caption = "Summary of Annual Nitrate-N Losses for Sorghum Treatments (Figure 3)")

########
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Summarize daily N₂O fluxes for Sorghum treatments
summarize_flux <- nitrous_oxide %>%
  dplyr::filter(Treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  group_by(growing_season, year_month_day, Treatment) %>%
  summarize(mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
            se_flux   = sd(gnha_day_no_negative, na.rm = TRUE) / sqrt(n()),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(Treatment = factor(Treatment, levels = c("Sorghum", "Sorghum + Rye")))

# Manually assign fertilizer events
fert_events <- data.frame(
  growing_season = c(2023, 2024),
  fertilizer_date = as.Date(c("2023-05-05", "2024-07-17"))
)

# Create the time-series plot with fertilizer markers
p4 <- ggplot(summarize_flux, aes(x = year_month_day, y = mean_flux, color = Treatment)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_flux - se_flux, ymax = mean_flux + se_flux),
                width = 0.2, alpha = 0.5, position = position_dodge(width = 0.5)) +
  # Add vertical dashed lines for fertilizer events
  geom_vline(data = fert_events, aes(xintercept = as.numeric(fertilizer_date)),
             linetype = "dashed", color = "red", alpha = 0.7) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d-%b-%Y") +
  labs(
    x = "Date",
    y = expression("Daily N"[2]*"O Flux (g N ha"^"-1"*")"),
    color = "Treatment"
  ) +
  theme_sabr() +
  scale_color_manual(values = treatment_colors)

# Display the plot
print(p4)

# Save the plot
ggsave("figures/Figure_4.tiff", plot = p4, width = 12, height = 8, dpi = 600, compression = "lzw")
ggsave("figures/Figure_4.pdf", plot = p4, width = 7.1, height = 5.5)

########
# Filter out fertilizer band if necessary
flux_comparison <- df %>%
  dplyr::filter(RowvsInterrow != "Fertilizer_band")

# Create the line graph for Row vs. Interrow N₂O Emissions Over Time
p5 <- ggplot(flux_comparison, aes(x = year_month_day, y = gnha_day_no_negative, color = RowvsInterrow)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(
    x = "Date",
    y = expression("Daily N"[2]*"O Flux (g N ha"^{-1}*")"),
    title = "Row vs. Interrow N₂O Emissions Over Time"
  ) +
  facet_wrap(growing_season ~ Treatment, scales = "free_x", nrow = 2) +
  theme_sabr()

print(p5)

# Save the plot as TIFF and PDF
ggsave("figures/Figure_5.tiff", plot = p5, width = 12, height = 8, dpi = 600, compression = "lzw")
ggsave("figures/Figure_5.pdf", plot = p5, width = 7.1, height = 5.5)

########

# Convert 'plot' to numeric if not already, then manually assign coordinates
library(dplyr)
library(ggplot2)
library(viridis)
library(scales)

# Create a spatial snapshot from your daily data (df)
# Assuming df has columns: growing_season, Treatment, plot, year_month_day, and cumulative_flux
final_day_data <- df %>%
  group_by(growing_season, Treatment, plot) %>%
  dplyr::filter(year_month_day == max(year_month_day)) %>%  # extract final day for each plot
  ungroup() %>%
  mutate(plot = as.numeric(plot),
         # assign x as column number (1 to 5) and y as row (3 = north, 2 = middle, 1 = south)
         x = ((plot - 1) %% 5) + 1,
         y = 3 - floor((plot - 1) / 5),
         label = Treatment)

# Create the heatmap without axis tick labels
heatmap_plot <- ggplot(final_day_data, aes(x = x, y = y, fill = cumulative_flux)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = label), size = 5, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Cumulative Flux") +
  facet_wrap(~ growing_season, ncol = 1) +
  labs(
    x = "Plot Column",
    y = "Plot Row (from south to north)"
  ) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))

print(heatmap_plot)

ggsave("figures/Figure_6.tiff", plot = heatmap_plot, width = 12, height = 8, dpi = 600, compression = "lzw")
ggsave("figures/Figure_6.pdf", plot = heatmap_plot, width = 7.1, height = 5.5)
#######

publish_table <- function(data, caption, file_name, digits = 2, engine = "xelatex") {
  # Load required packages
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Please install 'dplyr'")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Please install 'stringr'")
  if (!requireNamespace("knitr", quietly = TRUE)) stop("Please install 'knitr'")
  if (!requireNamespace("kableExtra", quietly = TRUE)) stop("Please install 'kableExtra'")
  if (!requireNamespace("tinytex", quietly = TRUE)) stop("Please install 'tinytex'")
  
  library(dplyr)
  library(stringr)
  library(knitr)
  library(kableExtra)
  library(tinytex)
  
  # Process the data:
  # 1) Ensure data is ungrouped
  # 2) Replace underscores in column names with a space for a cleaner, publication-ready look
  table_processed <- data %>% ungroup()
  colnames(table_processed) <- gsub("_", " ", colnames(table_processed))
  
  # Create the LaTeX table using kable and kableExtra.
  # We set escape = FALSE and remove scale_down (which inserts \resizebox and extra addlinespace)
  table_latex <- kable(
    table_processed,
    format   = "latex",
    booktabs = TRUE,
    caption  = caption,
    digits   = digits,
    escape   = FALSE
  ) %>%
    kable_styling(
      latex_options = c("hold_position"),
      full_width    = FALSE
    )
  
  # Remove any occurrences of \addlinespace to eliminate extra gaps.
  table_latex <- gsub("\\\\addlinespace", "", table_latex)
  
  # Build a minimal LaTeX document.
  if (engine == "xelatex") {
    latex_doc <- paste0(
      "\\documentclass{article}\n",
      "\\usepackage{fontspec}\n",         
      "\\usepackage{graphicx}\n",
      "\\usepackage{booktabs}\n",
      "\\usepackage{geometry}\n",
      "\\geometry{margin=1in}\n",
      "\\begin{document}\n",
      table_latex,
      "\n\\end{document}"
    )
  } else {
    latex_doc <- paste0(
      "\\documentclass{article}\n",
      "\\usepackage[utf8]{inputenc}\n",
      "\\usepackage{graphicx}\n",
      "\\usepackage{booktabs}\n",
      "\\usepackage{geometry}\n",
      "\\geometry{margin=1in}\n",
      "\\begin{document}\n",
      table_latex,
      "\n\\end{document}"
    )
  }
  
  # Write the LaTeX document to a temporary .tex file.
  tex_file <- tempfile(fileext = ".tex")
  writeLines(latex_doc, tex_file)
  
  # Compile the .tex file to PDF using the specified engine.
  if (engine == "xelatex") {
    tinytex::xelatex(tex_file)
  } else {
    tinytex::pdflatex(tex_file)
  }
  
  # The generated PDF file will have the same base name as the .tex file, but with a .pdf extension.
  pdf_file <- sub("\\.tex$", ".pdf", tex_file)
  
  # Create the target directory if needed and copy the PDF to file_name.
  dir.create(dirname(file_name), showWarnings = FALSE, recursive = TRUE)
  file.copy(pdf_file, file_name, overwrite = TRUE)
  
  message("PDF table saved to: ", file_name)
}



# -------------------------------

# Display the table inline
kable(
  figure1_table,
  digits = 2,
  caption = "Summary of Cumulative N Losses (Figure 1)"
)

# Generate the publication-ready PDF table
publish_table(
  figure1_table,
  caption   = "Summary of Cumulative N Losses (Figure 1)",
  file_name = "tables/figure1_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)
