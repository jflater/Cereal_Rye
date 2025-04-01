# ==============================================================================
# SCRIPT: Sorghum + Cereal Rye Analysis, Figure Generation, and Table Production
# ==============================================================================
# This script processes seasonal N₂O and nitrogen leaching data, generates 
# publication‐ready figures (with consistent themes and colors), and then creates
# corresponding tables that are exported as PDFs.

# Load necessary packages
library(lubridate)
library(ggplot2)
library(zoo)
library(conflicted)
library(lme4)
library(emmeans)
library(broom)
library(knitr)
library(kableExtra)
library(stringr)
library(tidyr)
library(scales)
library(dplyr)
conflict_prefer("filter", "dplyr")

# --- Theme & Colors (same as figure script) ---
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
      strip.text = element_text(size = base_size + 2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_blank()
    )
}

# --- Data Processing ---
# Read in nitrous oxide flux data and convert date strings
nitrous_oxide <- read_csv("data/clean/seasonal_flux_combined.csv") %>%
  mutate(year_month_day = as.Date(year_month_day),
         season_start = as.Date(paste0(growing_season, "-04-01")),
         season_end   = as.Date(paste0(growing_season + 1, "-03-31")))

# Filter to the growing season, fill missing days, and calculate cumulative flux
df <- nitrous_oxide %>%
  filter(year_month_day >= season_start & year_month_day <= season_end) %>%
  group_by(growing_season, Treatment, RowvsInterrow, plot) %>%
  complete(year_month_day = seq(min(year_month_day), max(year_month_day), by = "day")) %>%
  arrange(year_month_day) %>%
  mutate(gnha_day_interpolated = zoo::na.approx(gnha_day_no_negative, na.rm = FALSE)) %>%
  fill(gnha_day, .direction = "downup") %>%
  filter(RowvsInterrow != "Fertilizer_band") %>%
  group_by(growing_season, Treatment, RowvsInterrow, plot) %>%
  mutate(cumulative_flux = cumsum(gnha_day_interpolated))

# --- Figure 1: Cumulative N₂O Flux Summary ---
# For each plot, extract the final day and summarize by Treatment & Season
test <- df %>%
  group_by(growing_season, Treatment, plot) %>%
  filter(year_month_day == max(year_month_day)) %>%
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
test$Treatment <- factor(test$Treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))

p1 <- ggplot(test, aes(x = Treatment, y = mean, fill = Treatment)) +
  geom_col(position = position_dodge(0.8), color = "black") +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                width = 0.2, position = position_dodge(0.8)) +
  scale_fill_manual(values = treatment_colors) +
  facet_wrap(~ growing_season, scales = "free_y") +
  labs(y = expression("Cumulative N"[2]*"O Flux (g N ha"^"-1"*")"),
       x = "Treatment") +
  theme_sabr()
print(p1)
ggsave("figures/Figure_1.tiff", plot = p1, width = 180, height = 130, units = "mm", dpi = 600, compression = "lzw")
ggsave("figures/Figure_1.pdf", plot = p1, width = 7.1, height = 5.5)

# --- Figure 2: Inorganic N Leached Over Time ---
cumulative_flow_data <- read_csv("data/clean/2023_2024_SABR_tile_cumulative_n.csv")
# Filter for 2023 and 2024 separately
gs_23 <- cumulative_flow_data %>% filter(growing_season == 2023, `year(date)` != 2024)
nitrogen_summary_23 <- gs_23 %>%
  group_by(date, treatment) %>%
  mutate(cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145) %>% 
  summarise(Cumulative_Nitrogen_mean = mean(cumulative_n_loss_kg_ha),
            Cumulative_Nitrogen_se = sd(cumulative_n_loss_kg_ha) / sqrt(n())) %>%
  ungroup()

colors <- create_color_theme(nitrogen_summary_23, "treatment")  # ensure this function is defined

p2 <- ggplot(nitrogen_summary_23, aes(x = date, y = Cumulative_Nitrogen_mean, 
                                      group = treatment, color = treatment)) +
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
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_x_date(labels = date_format("%b %y"))
print(p2)
ggsave("figures/Cumulative_Nitrogen_23.png", plot = p2, width = 12, height = 8, dpi = 300)

gs_24 <- cumulative_flow_data %>% filter(growing_season == 2024, !(`year(date)` %in% c(2023,2025)))
nitrogen_summary_24 <- gs_24 %>%
  group_by(date, treatment) %>%
  mutate(cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145) %>% 
  summarise(Cumulative_Nitrogen_mean = mean(cumulative_n_loss_kg_ha),
            Cumulative_Nitrogen_se = sd(cumulative_n_loss_kg_ha) / sqrt(n())) %>%
  ungroup()
colors <- create_color_theme(nitrogen_summary_24, "treatment")
p2b <- ggplot(nitrogen_summary_24, aes(x = date, y = Cumulative_Nitrogen_mean, 
                                       group = treatment, color = treatment)) +
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
        text = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 16),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white")) +
  scale_x_date(labels = date_format("%b %y"))
print(p2b)
ggsave("figures/Cumulative_Nitrogen_24.png", plot = p2b, width = 12, height = 8, dpi = 300)

# --- Figure 3: Annual Nitrate-N Losses (Boxplots) ---
sorghum_data <- cumulative_flow_data %>% filter(treatment %in% c("Sorghum", "Sorghum + Rye"))
sorghum_max <- sorghum_data %>% 
  group_by(growing_season, plot) %>% 
  summarize(
    max_loss = max(cumulative_n_loss_mg, na.rm = TRUE),
    treatment = first(treatment),
    .groups = "drop"
  ) %>%
  mutate(max_loss_kg_ha = (max_loss / 1e6) / 0.145)
p3 <- ggplot(sorghum_max, aes(x = factor(growing_season), y = max_loss_kg_ha, fill = treatment)) +
  geom_boxplot() +
  labs(x = "Growing Season",
       y = expression("Cumulative Inorganic-N Loss (kg N ha "^{-1}*")"),
       fill = "Treatment") +
  theme_sabr() +
  scale_fill_manual(values = treatment_colors)
print(p3)
ggsave("figures/Figure_3.tiff", plot = p3, width = 180, height = 130, units = "mm", dpi = 600)
ggsave("figures/Figure_3.pdf", plot = p3, width = 7.1, height = 5.5)

# --- Figure 4: Daily N₂O Flux with Fertilizer Events ---
summarize_flux <- nitrous_oxide %>%
  filter(Treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  group_by(growing_season, year_month_day, Treatment) %>%
  summarize(mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
            se_flux   = sd(gnha_day_no_negative, na.rm = TRUE) / sqrt(n()),
            .groups = "drop") %>%
  ungroup() %>%
  mutate(Treatment = factor(Treatment, levels = c("Sorghum", "Sorghum + Rye")))
fert_events <- data.frame(
  growing_season = c(2023, 2024),
  fertilizer_date = as.Date(c("2023-05-05", "2024-07-17"))
)
p4 <- ggplot(summarize_flux, aes(x = year_month_day, y = mean_flux, color = Treatment)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = mean_flux - se_flux, ymax = mean_flux + se_flux),
                width = 0.2, alpha = 0.5, position = position_dodge(width = 0.5)) +
  geom_vline(data = fert_events, aes(xintercept = as.numeric(fertilizer_date)),
             linetype = "dashed", color = "red", alpha = 0.7) +
  scale_x_date(date_breaks = "2 week", date_labels = "%d-%b-%Y") +
  labs(x = "Date",
       y = expression("Daily N"[2]*"O Flux (g N ha"^"-1"*")"),
       color = "Treatment") +
  theme_sabr() +
  scale_color_manual(values = treatment_colors)
print(p4)
ggsave("figures/Figure_4.tiff", plot = p4, width = 12, height = 8, dpi = 600)
ggsave("figures/Figure_4.pdf", plot = p4, width = 7.1, height = 5.5)

# --- Figure 5: Row vs. Interrow N₂O Emissions ---
flux_comparison <- df %>% filter(RowvsInterrow != "Fertilizer_band")
p5 <- ggplot(flux_comparison, aes(x = year_month_day, y = gnha_day_no_negative, color = RowvsInterrow)) +
  geom_line(size = 1, alpha = 0.8) +
  labs(x = "Date",
       y = expression("Daily N"[2]*"O Flux (g N ha"^{-1}*")"),
       title = "Row vs. Interrow N₂O Emissions Over Time") +
  facet_wrap(growing_season ~ Treatment, scales = "free_x", nrow = 2) +
  theme_sabr()
print(p5)
ggsave("figures/Figure_5.tiff", plot = p5, width = 12, height = 8, dpi = 600)
ggsave("figures/Figure_5.pdf", plot = p5, width = 7.1, height = 5.5)

# --- Figure 6: Spatial Snapshot (Heatmap) ---
final_day_data <- df %>%
  group_by(growing_season, Treatment, plot) %>%
  filter(year_month_day == max(year_month_day)) %>%
  ungroup() %>%
  mutate(plot = as.numeric(plot),
         x = ((plot - 1) %% 5) + 1,
         y = 3 - floor((plot - 1) / 5),
         label = Treatment)
heatmap_plot <- ggplot(final_day_data, aes(x = x, y = y, fill = cumulative_flux)) +
  geom_tile(color = "white", width = 0.9, height = 0.9) +
  geom_text(aes(label = label), size = 5, color = "black") +
  scale_fill_viridis_c(option = "plasma", name = "Cumulative Flux") +
  facet_wrap(~ growing_season, ncol = 1) +
  labs(x = "Plot Column", y = "Plot Row (from south to north)") +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_text(size = 14),
        strip.text = element_text(size = 14))
print(heatmap_plot)
ggsave("figures/Figure_6.tiff", plot = heatmap_plot, width = 12, height = 8, dpi = 600)
ggsave("figures/Figure_6.pdf", plot = heatmap_plot, width = 7.1, height = 5.5)

# ==============================================================================
# Table Generation and Export as Publication-Ready PDFs
# ==============================================================================
# Define a function to generate and export a LaTeX table as a PDF.
# This function un-groups the data, replaces underscores in column names with spaces,
# and removes extra line spacing.
publish_table <- function(data, caption, file_name, digits = 2, engine = "xelatex", landscape = FALSE) {
  # Load required packages (if not already loaded)
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
  
  # Process the data: ungroup and replace underscores in column names with spaces.
  table_processed <- data %>% ungroup()
  colnames(table_processed) <- gsub("_", " ", colnames(table_processed))
  
  # Create the LaTeX table using kable and kableExtra.
  table_latex <- kable(
    table_processed,
    format   = "latex",
    booktabs = TRUE,
    caption  = caption,
    digits   = digits,
    escape   = FALSE
  ) %>%
    kable_styling(latex_options = c("hold_position"), full_width = FALSE)
  
  # Remove any extra \addlinespace commands.
  table_latex <- gsub("\\\\addlinespace", "", table_latex)
  
  # Optionally wrap in a landscape environment if needed.
  if (landscape) {
    table_latex <- paste0("\\begin{landscape}\n", table_latex, "\n\\end{landscape}")
  }
  
  # Build a minimal LaTeX document.
  if (engine == "xelatex") {
    latex_doc <- paste0(
      "\\documentclass{article}\n",
      "\\usepackage{fontspec}\n",          
      "\\usepackage{graphicx}\n",
      "\\usepackage{booktabs}\n",
      if (landscape) "\\usepackage{pdflscape}\n" else "",
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
      if (landscape) "\\usepackage{pdflscape}\n" else "",
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


# Generate and export tables for each figure:

## Figure 1 Table: Cumulative N Losses Summary
figure1_table_summary <- all_n_data %>%
  select(growing_season, Treatment, N_Type, mean, sd, n, se, ci_lower, ci_upper)
kable(figure1_table_summary, digits = 2, caption = "Summary of Cumulative N Losses (Figure 1)")
publish_table(
  figure1_table_summary,
  caption   = "Summary of Cumulative N Losses (Figure 1)",
  file_name = "tables/figure1_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)

## Figure 2 Table: Inorganic N Leached Data (2023)
nitrogen_table_23 <- nitrogen_summary_23
kable(nitrogen_table_23, digits = 2, caption = "Inorganic N Leached Data (Figure 2, 2023)")
publish_table(
  nitrogen_table_23,
  caption   = "Inorganic N Leached Data (Figure 2, 2023)",
  file_name = "tables/figure2_23_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)

## Figure 3 Table: Annual Nitrate-N Losses for Sorghum Treatments
kable(figure3_table, digits = 2, caption = "Summary of Annual Nitrate-N Losses for Sorghum Treatments (Figure 3)")
publish_table(
  figure3_table,
  caption   = "Summary of Annual Nitrate-N Losses for Sorghum Treatments (Figure 3)",
  file_name = "tables/figure3_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)

## Figure 4 Table: Daily N₂O Flux Data for Sorghum Treatments
daily_flux_table <- summarize_flux %>% arrange(growing_season, year_month_day, Treatment)
kable(daily_flux_table, digits = 2, caption = "Daily N₂O Flux Data for Sorghum Treatments (Figure 4)")
publish_table(
  daily_flux_table,
  caption   = "Daily N₂O Flux Data for Sorghum Treatments (Figure 4)",
  file_name = "tables/figure4_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)

## Figure 5 Table: Row vs. Interrow N₂O Emissions Data
flux_table_fig5 <- flux_comparison %>%
  select(growing_season, Treatment, RowvsInterrow, year_month_day, gnha_day_no_negative) %>%
  arrange(growing_season, Treatment, RowvsInterrow, year_month_day)
kable(flux_table_fig5, digits = 2, caption = "Row vs. Interrow N₂O Emissions Data (Figure 5)")
publish_table(
  flux_table_fig5,
  caption   = "Row vs. Interrow N₂O Emissions Data (Figure 5)",
  file_name = "tables/figure5_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)

## Figure 6 Table: Spatial Snapshot of Cumulative N₂O Flux
spatial_table_fig6 <- final_day_data %>%
  select(growing_season, plot, Treatment, cumulative_flux, x, y) %>%
  arrange(growing_season, plot)
kable(spatial_table_fig6, digits = 2, caption = "Spatial Snapshot of Cumulative N₂O Flux (Figure 6)")
publish_table(
  spatial_table_fig6,
  caption   = "Spatial Snapshot of Cumulative N₂O Flux (Figure 6)",
  file_name = "tables/figure6_table.pdf",
  digits    = 2,
  engine    = "xelatex"
)

# Statistics ==============================================================================
# ==============================================================================
# CASE 1: Cumulative N₂O Flux (Figure 1)
# ==============================================================================
# ------------------------------------------------------------------------------
# Subset Data to Focus on Sorghum and Sorghum + Rye for Final Cumulative Flux
# ------------------------------------------------------------------------------
final_sorghum <- df %>% 
  group_by(plot, growing_season, Treatment) %>%
  dplyr::filter(year_month_day == max(year_month_day)) %>%  # One value per plot
  ungroup()

# ------------------------------------------------------------------------------
# Mixed-Effects Model: Final Cumulative Flux as a Function of Treatment
# ------------------------------------------------------------------------------
library(lme4)
model_flux <- lmer(cumulative_flux ~ Treatment * factor(growing_season) + (1 | plot), data = final_sorghum)
summary(model_flux)

# ------------------------------------------------------------------------------
# Pairwise Comparisons Using emmeans
# ------------------------------------------------------------------------------
library(emmeans)
emm_flux <- emmeans(model_flux, ~ Treatment | factor(growing_season))
contrast_flux <- pairs(emm_flux)

# For a neat summary table, we can use broom.mixed to tidy the output:
library(broom.mixed)
contrast_flux_summary <- tidy(contrast_flux)
print(contrast_flux_summary)

# ------------------------------------------------------------------------------
# Generate a Publication-Ready Table of the Pairwise Comparisons
# ------------------------------------------------------------------------------
kable(contrast_flux_summary, digits = 2, 
      caption = "Cumulative N₂O Flux")

publish_table(
  contrast_flux_summary,
  caption   = "Cumulative N₂O Flux",
  file_name = "tables/figure1n2o_table.pdf",
  digits    = 2,
  engine    = "xelatex",
  landscape = TRUE
)
# ------------------------------------------------------------------------------
# Mixed-Effects Model: Final Cumulative Drainage N as a Function of Treatment
# ------------------------------------------------------------------------------
drainage_summary_stats <- bind_rows(drainage_summary_23, drainage_summary_24) %>%
  mutate(
    cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145,
    N_Type = "Nitrogen Leaching",
    Treatment = factor(treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))
  )
library(lme4)
model_drainage <- lm(cumulative_n_loss_kg_ha ~ Treatment * factor(growing_season), data = drainage_summary_stats)
summary(model_drainage)

# ------------------------------------------------------------------------------
# Pairwise Comparisons Using emmeans
# ------------------------------------------------------------------------------
library(emmeans)
emm_drainage <- emmeans(model_drainage, ~ Treatment | factor(growing_season))
contrast_drainage <- pairs(emm_drainage)

# For a neat summary table, we can use broom.mixed to tidy the output:
library(broom.mixed)
contrast_drainage_summary <- tidy(contrast_drainage)
print(contrast_drainage_summary)

# ------------------------------------------------------------------------------
# Generate a Publication-Ready Table of the Pairwise Comparisons
# ------------------------------------------------------------------------------
kable(contrast_drainage_summary, digits = 2, 
      caption = "Cumulative inorganic N drainage")

publish_table(
  contrast_drainage_summary,
  caption   = "Cumulative inorganic N drainage",
  file_name = "tables/figure1drainage_table.pdf",
  digits    = 2,
  engine    = "xelatex",
  landscape = TRUE
)
########
# Create drainage summary stats (if not already done)
drainage_summary_stats <- bind_rows(drainage_summary_23, drainage_summary_24) %>%
  mutate(
    cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145,
    N_Type = "Nitrogen Leaching",
    Treatment = factor(treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))
  )

# Fit the mixed-effects model (using plot as random effect)
model_drainage <- lmer(cumulative_n_loss_kg_ha ~ Treatment * factor(growing_season) + (1 | plot),
                       data = drainage_summary_stats)
summary(model_drainage)

# Obtain estimated marginal means, conditioning on growing_season as a factor
library(emmeans)
emm_drainage <- emmeans(model_drainage, ~ Treatment | factor(growing_season))
# Compute pairwise contrasts for Treatment within each growing season
contrast_drainage <- pairs(emm_drainage)

library(broom.mixed)
library(dplyr)
library(stringr)
library(knitr)
# Convert the emmeans object to a data frame.
emm_drainage_df <- as.data.frame(emm_drainage)
# Rename the growing season column for easier matching
names(emm_drainage_df)[names(emm_drainage_df) == "factor(growing_season)"] <- "growing_season"

# Tidy the contrast table
contrast_drainage_summary <- tidy(contrast_drainage)

# Augment the contrast table with the estimated means for each group.
# Here we assume the contrast string is of the form "Group1 - Group2"
contrast_drainage_summary_aug <- contrast_drainage_summary %>%
  rowwise() %>%
  mutate(
    group1 = str_split(contrast, " - ", simplify = TRUE)[1],
    group2 = str_split(contrast, " - ", simplify = TRUE)[2],
    # Ensure growing_season is a character for matching
    growing_season = as.character(growing_season),
    mean1 = emm_drainage_df$emmean[emm_drainage_df$treatment == group1 & 
                                     as.character(emm_drainage_df$growing_season) == growing_season],
    mean2 = emm_drainage_df$emmean[emm_drainage_df$treatment == group2 & 
                                     as.character(emm_drainage_df$growing_season) == growing_season],
    mean_difference = mean1 - mean2
  ) %>%
  ungroup()

# Display the augmented contrast table
kable(contrast_drainage_summary_aug, digits = 2,
      caption = "Cumulative Inorganic N Drainage: Pairwise Comparisons with Group Means")
# ==============================================================================
# Drainage Data Analysis: ANOVA and Post-hoc Comparisons
# ==============================================================================

# We assume drainage_summary_stats has been created with one final cumulative
# inorganic nitrogen loss value per plot, for each growing season.
drainage_summary_stats <- bind_rows(drainage_summary_23, drainage_summary_24) %>%
  mutate(
    cumulative_n_loss_kg_ha = (cumulative_n_loss_mg / 1e6) / 0.145,
    N_Type = "Nitrogen Leaching",
    Treatment = factor(treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))
  )

# ------------------------------------------------------------------------------
# Fit an ANOVA model (two-way: Treatment and Growing Season)
# ------------------------------------------------------------------------------
model_drainage_aov <- aov(cumulative_n_loss_kg_ha ~ Treatment * factor(growing_season),
                          data = drainage_summary_stats)
# Display the ANOVA table:
anova_table <- summary(model_drainage_aov)
kable(anova_table[[1]], digits = 2,
      caption = "ANOVA Table for Cumulative Inorganic N Drainage")

# ------------------------------------------------------------------------------
# Post-hoc Comparisons Using Tukey's HSD Test
# ------------------------------------------------------------------------------
# Tukey's HSD test on the Treatment factor:
tukey_results <- TukeyHSD(model_drainage_aov, "Treatment")
# Convert the Tukey output to a data frame:
tukey_df <- as.data.frame(tukey_results$Treatment)
tukey_df$Comparison <- rownames(tukey_df)
kable(tukey_df, digits = 2,
      caption = "Tukey HSD Pairwise Comparisons for Treatment (Cumulative Inorganic N Drainage)")

# ------------------------------------------------------------------------------
# Option: Pairwise t-tests for each Growing Season (if you want to see season-specific comparisons)
# ------------------------------------------------------------------------------
t_test_2023 <- pairwise.t.test(
  drainage_summary_stats$cumulative_n_loss_kg_ha[drainage_summary_stats$growing_season == 2023],
  drainage_summary_stats$Treatment[drainage_summary_stats$growing_season == 2023],
  p.adjust.method = "holm"
)
t_test_2024 <- pairwise.t.test(
  drainage_summary_stats$cumulative_n_loss_kg_ha[drainage_summary_stats$growing_season == 2024],
  drainage_summary_stats$Treatment[drainage_summary_stats$growing_season == 2024],
  p.adjust.method = "holm"
)

# Print pairwise t-test results:
print(t_test_2023)
print(t_test_2024)
