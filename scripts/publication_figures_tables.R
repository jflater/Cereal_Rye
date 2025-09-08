# =============================================================================
# SCRIPT: Publication-ready Figure and Table Example
# =============================================================================
# Demonstrates generation of publication-quality figures and tables using
# custom theme similar to other project scripts.

library(ggplot2)
library(dplyr)
library(knitr)
library(kableExtra)

# -----------------------------------------------------------------------------
# Theme definition (adapted from existing project scripts)
# -----------------------------------------------------------------------------

theme_sabr <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.title = element_text(size = base_size + 2),
      axis.text  = element_text(size = base_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = base_size),
      legend.text  = element_text(size = base_size - 1),
      strip.text   = element_text(size = base_size + 2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      plot.title = element_blank()
    )
}

# -----------------------------------------------------------------------------
# Example dataset and summary table
# -----------------------------------------------------------------------------

mtcars_summary <- mtcars %>%
  group_by(cyl) %>%
  summarise(
    mpg = mean(mpg),
    wt  = mean(wt),
    .groups = "drop"
  )

# Save formatted table as PDF
if (!dir.exists("tables")) dir.create("tables")

mtcars_table <- mtcars_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) %>%
  kable(format = "latex", booktabs = TRUE,
        col.names = c("Cylinders", "Mean MPG", "Mean Weight"),
        caption = "Example summary of mtcars dataset.") %>%
  kable_styling(latex_options = c("hold_position"), full_width = FALSE)

save_kable(mtcars_table, "tables/example_table.pdf")

# -----------------------------------------------------------------------------
# Example figure using custom theme
# -----------------------------------------------------------------------------

p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(x = "Weight", y = "Miles per Gallon", color = "Cylinders") +
  theme_sabr() +
  scale_color_brewer(palette = "Dark2")

if (!dir.exists("figures")) dir.create("figures")

ggsave("figures/example_figure.tiff", plot = p,
       width = 180, height = 130, units = "mm", dpi = 600,
       compression = "lzw")

ggsave("figures/example_figure.pdf", plot = p,
       width = 7.1, height = 5.5)

