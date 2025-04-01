library(readxl)
sorg_biomass_23 <- read_excel("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Biomass/2023GrowingSeason/DerivedDatasets/chopper_load_weights_yields.xlsx")
#######
sorg_biomass_23 <- sorg_biomass_23 %>% 
  group_by(Plot) %>% 
  mutate(total_biomass = sum(`Net Weight`)) %>% 
  select(`Harvest Date`, Plot, Treatment, total_biomass) %>% 
  distinct()

sorg_biomass_23

# Bar plot with total_biomass by treatment and with error bars showing sd
library(ggplot2)
library(dplyr)
plot_size_ha = 0.145 

sorg_biomass_23 %>% 
  group_by(Treatment) %>% 
  summarise(mean_biomass = mean(total_biomass / plot_size_ha), sd_biomass = sd(total_biomass / plot_size_ha)) %>% 
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass), width = 0.2) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha)") +
  theme_minimal()

sorg_biomass_23 %>% 
  group_by(Treatment) %>% 
  summarise(
    mean_biomass = mean(total_biomass / plot_size_ha),
    sd_biomass = sd(total_biomass / plot_size_ha),
    se_biomass = sd_biomass / sqrt(n())
  ) %>% 
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
                width = 0.2) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha) (SE)") +
  theme_minimal()

sorg_biomass_23 %>% 
  group_by(Treatment) %>% 
  summarise(
    mean_biomass = mean(total_biomass / plot_size_ha),
    sd_biomass = sd(total_biomass / plot_size_ha),
    n = n(),
    se_biomass = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass
  ) %>% 
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
                width = 0.2) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha) (95% CI)") +
  theme_minimal()

plot_size_ha <- 0.145 
conversion_factor <- 0.907185  # 1 US ton = 0.907185 metric tons (or megagrams)

biomass_summary <- sorg_biomass_23 %>% 
  group_by(Treatment) %>% 
  summarise(
    # Convert US tons to metric tons and divide by plot size (ha)
    mean_biomass = mean((total_biomass * conversion_factor) / plot_size_ha),
    sd_biomass   = sd((total_biomass * conversion_factor) / plot_size_ha),
    n = n(),
    se_biomass   = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass,
    .groups = "drop"
  )
# Custom color palette (matching your function earlier)
treatment_colors <- c(
  "No cover" = "#D2B48C", 
  "Cover" = "#8B4513"
)

# Plot with 95% CI error bars, using custom colors
ggplot(biomass_summary, aes(x = Treatment, y = mean_biomass, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
                width = 0.2, color = "black") +
  labs(x = "Treatment", y = expression("Average Biomass 2023 (Mg ha"^-1*")")) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("figures/sorghum_biomass_2023.png", width = 8, height = 6, dpi = 300)

########### 
#####2024
library(readxl)
sorg_biomass_24 <- read_excel("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Biomass/2024GrowingSeason/RecordingDataSheets/sorg_chopper_harvest_load_weights.xlsx")
sorg_biomass_24 <- sorg_biomass_24 %>% 
  dplyr::filter(Study == "Drainage") %>% 
  group_by(Plot) %>%
  mutate(total_biomass = sum(`Net Weight (t)`)) %>% 
  select(Date, Plot, total_biomass) %>% 
  distinct() %>% 
  mutate(treatment = case_when(
    Plot %in% c("2", "3", "5", "15") ~ "No cover",
    TRUE ~ "Cover",
  ))

# Figure of total_biomass by Plot
sorg_biomass_24 %>% 
  ggplot(aes(x = Plot, y = total_biomass, fill = treatment)) +
  geom_bar(stat = "identity") +
  labs(x = "Plot", y = "Total Biomass (tons)") 

plot_size_ha <- 0.145 
conversion_factor <- 0.907185  # 1 US ton = 0.907185 metric tons (or megagrams)

biomass_summary <- sorg_biomass_24 %>% 
  group_by(treatment) %>% 
  summarise(
    # Convert US tons to metric tons and divide by plot size (ha)
    mean_biomass = mean((total_biomass * conversion_factor) / plot_size_ha),
    sd_biomass   = sd((total_biomass * conversion_factor) / plot_size_ha),
    n = n(),
    se_biomass   = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass,
    .groups = "drop"
  )
# Custom color palette (matching your function earlier)
treatment_colors <- c(
  "No cover" = "#D2B48C", 
  "Cover" = "#8B4513"
)

# Plot with 95% CI error bars, using custom colors
ggplot(biomass_summary, aes(x = treatment, y = mean_biomass, fill = treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
                width = 0.2, color = "black") +
  labs(x = "Treatment", y = expression("Average Biomass 2024 (Mg ha"^-1*")")) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(legend.position = "none",
        text = element_text(size = 14),
        axis.title = element_text(size = 16))

ggsave("figures/sorghum_biomass_2024.png", width = 8, height = 6, dpi = 300)
