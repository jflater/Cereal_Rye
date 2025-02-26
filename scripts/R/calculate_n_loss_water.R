# Calculate N-loss in tile drainage from SABR small plots
library(tidyverse)
library(janitor)
library(zoo)
library(ggplot2)
# 2023
flow <- read_csv("data/clean/2023_2024_SABR_tile_flow.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.factor(str_remove(plot, "^0+")))

n_conc_23 <- read_csv("data/clean/water_n_2023.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.factor(plot)) 

n_conc_24 <- read_csv("data/clean/water_n_2024.csv") %>% 
  clean_names() %>% 
  mutate(plot = as.factor(plot)) %>% 
  select(date, plot, nitrate_mg_ml = nitrate_ppm, ammonia_mg_l = ammonia_ppm, sample_y_n)

compare_df_cols(n_conc_23, n_conc_24)

n_conc <- rbind(n_conc_23, n_conc_24)

compare_df_cols(flow, n_conc)

flow_n <- flow %>% 
  left_join(n_conc, by = c("plot", "date"))


# convert flow to liters from gallons
flow_n <- flow_n %>% 
  mutate(flow_l = flow_gallons * 3.78541)

# linear interpolation of concentrations 
df_interpolated <- flow_n %>% 
  group_by(plot) %>% 
  arrange(date) %>%
  mutate(approx_nitrate_mg_l = na.approx(nitrate_mg_ml, x = date, na.rm = FALSE),
         approx_ammonia_mg_l = na.approx(ammonia_mg_l, x = date, na.rm = FALSE)) %>% 
  ungroup()

# Save a .csv
write_csv(df_interpolated, "data/clean/2023_2024_SABR_tile_flow_n.csv")



n_loss_2023 <- df_interpolated %>% 
  dplyr::filter(year(date) == 2023)


# Plotting, move this to a new script soon

ggplot(data = n_loss_2023, aes(x = date)) +
  # Points for measured nitrate (only on rows where sample_y_n equals "Y")
  geom_point(data = n_loss_2023 %>% dplyr::filter(.data$sample_y_n == "Y"),
             aes(y = nitrate_mg_ml, color = "Measured Nitrate"),
             shape = 16, size = 2) +
  # Points for measured ammonia
  geom_point(data = n_loss_2023 %>% dplyr::filter(.data$sample_y_n == "Y"),
             aes(y = ammonia_mg_l, color = "Measured Ammonia"),
             shape = 17, size = 2) +
  # Line for interpolated nitrate
  geom_line(aes(y = approx_nitrate_mg_l, color = "Interpolated Nitrate"), size = 1) +
  # Line for interpolated ammonia (using a dashed line)
  geom_line(aes(y = approx_ammonia_mg_l, color = "Interpolated Ammonia"), size = 1, linetype = "dashed") +
  # Facet by plot
  facet_wrap(~ plot, scales = "free_y") +
  labs(x = "Date",
       y = "Value (Flow or Concentration)",
       title = "Daily Flow and Nutrient Concentrations by Plot",
       color = "Legend") +
  theme_bw() +
  theme(legend.position = "bottom")

# Determine a scaling factor. This example takes the ratio of the max flow to the max concentration.
max_flow <- max(n_loss_2023$flow_l, na.rm = TRUE)
max_conc <- max(c(n_loss_2023$nitrate_mg_ml, n_loss_2023$ammonia_mg_l), na.rm = TRUE)
scale_factor <- max_flow / max_conc


ggplot(data = n_loss_2023, aes(x = date)) +
  # Bar plot for daily flow
  geom_bar(aes(y = flow_l), stat = "identity", fill = "grey80", alpha = 0.5) +
  # Plot measured values (optional: these would also need to be scaled if you want them on the same secondary axis)
  geom_point(data = n_loss_2023 %>% dplyr::filter(.data$sample_y_n == "Y"),
             aes(y = nitrate_mg_ml * scale_factor, color = "Measured Nitrate"),
             shape = 16, size = 2) +
  geom_point(data = n_loss_2023 %>% dplyr::filter(.data$sample_y_n == "Y"),
             aes(y = ammonia_mg_l * scale_factor, color = "Measured Ammonia"),
             shape = 17, size = 2) +
  # Line for interpolated concentrations (scaled)
  geom_line(aes(y = approx_nitrate_mg_l * scale_factor, color = "Interpolated Nitrate"), size = 1) +
  geom_line(aes(y = approx_ammonia_mg_l * scale_factor, color = "Interpolated Ammonia"), size = 1, linetype = "dashed") +
  facet_wrap(~ plot, scales = "free_y") +
  scale_y_continuous(
    name = "Flow (l)",
    sec.axis = sec_axis(~ . / scale_factor, name = "Concentration (mg/ml)")
  ) +
  labs(x = "Date", color = "Legend", title = "Daily Flow and Nutrient Concentrations by Plot") +
  theme_bw() +
  theme(legend.position = "bottom")

ggplot(n_loss_2023, aes(x = date, y = flow_l)) +
  geom_col(fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ plot, scales = "free_y") +
  labs(x = "Date",
       y = "Flow (L)",
       title = "Daily Flow by Plot") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
####
flow_plot <- ggplot(n_loss_2023, aes(x = date, y = flow_l)) +
  geom_col(fill = "skyblue", color = "black", alpha = 0.7) +
  facet_wrap(~ plot, scales = "free_y") +
  labs(x = NULL,  # Remove x-axis label since it will be shared
       y = "Flow (L)",
       title = "Daily Flow") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

sample_plot <- ggplot(n_loss_2023 %>% dplyr::filter(sample_y_n == "Y"), aes(x = date)) +
  geom_point(aes(y = 0), color = "darkred", size = 2) +
  facet_wrap(~ plot, scales = "free_y") +
  labs(x = NULL,
       y = "",
       title = "Sampling Events") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

conc_plot <- ggplot(n_loss_2023, aes(x = date)) +
  # Interpolated concentrations
  geom_line(aes(y = approx_nitrate_mg_l, color = "Interpolated Nitrate"), size = 1) +
  geom_line(aes(y = approx_ammonia_mg_l, color = "Interpolated Ammonia"), 
            size = 1, linetype = "dashed") +
  # Measured concentrations
  geom_point(data = n_loss_2023 %>% dplyr::filter(sample_y_n == "Y"),
             aes(y = nitrate_mg_ml, color = "Measured Nitrate"),
             shape = 16, size = 2) +
  geom_point(data = n_loss_2023 %>% dplyr::filter(sample_y_n == "Y"),
             aes(y = ammonia_mg_l, color = "Measured Ammonia"),
             shape = 17, size = 2) +
  # Vertical lines for sample events
  geom_vline(data = n_loss_2023 %>% dplyr::filter(sample_y_n == "Y"),
             aes(xintercept = as.numeric(date)),
             color = "red", linetype = "dotted", alpha = 0.5) +
  facet_wrap(~ plot, scales = "free_y") +
  labs(x = "Date", y = "Concentration",
       title = "Nutrient Concentrations with Sampling Events",
       color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(cowplot)

combined_plot <- plot_grid(
  flow_plot,
  conc_plot,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 0.3, 1)  # adjust relative heights as needed
)

# Display the combined plot
print(combined_plot)

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)

ui <- fluidPage(
  titlePanel("Interactive Plot Selector"),
  sidebarLayout(
    sidebarPanel(
      selectInput("plot_select", "Select Plot:",
                  choices = sort(unique(n_loss_2023$plot)))
    ),
    mainPanel(
      plotlyOutput("combinedPlot")
    )
  )
)

server <- function(input, output, session) {
  
  output$combinedPlot <- renderPlotly({
    
    # Filter data for the selected plot
    filtered_data <- n_loss_2023 %>% filter(plot == input$plot_select)
    
    # Create flow plot
    flow_plot <- ggplot(filtered_data, aes(x = date, y = flow_l)) +
      geom_col(fill = "skyblue", color = "black", alpha = 0.7) +
      labs(x = NULL,
           y = "Flow (L)",
           title = "Daily Flow") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
    # Create concentration plot with sampling events overlay
    conc_plot <- ggplot(filtered_data, aes(x = date)) +
      geom_line(aes(y = approx_nitrate_mg_l, color = "Interpolated Nitrate"), size = 1) +
      geom_line(aes(y = approx_ammonia_mg_l, color = "Interpolated Ammonia"), 
                size = 1, linetype = "dashed") +
      geom_point(data = filtered_data %>% dplyr::filter(sample_y_n == "Y"),
                 aes(y = nitrate_mg_ml, color = "Measured Nitrate"),
                 shape = 16, size = 2) +
      geom_point(data = filtered_data %>% dplyr::filter(sample_y_n == "Y"),
                 aes(y = ammonia_mg_l, color = "Measured Ammonia"),
                 shape = 17, size = 2) +
      geom_vline(data = filtered_data %>% dplyr::filter(sample_y_n == "Y"),
                 aes(xintercept = as.numeric(date)),
                 color = "red", linetype = "dotted", alpha = 0.5) +
      labs(x = "Date", y = "Concentration",
           title = "Nutrient Concentrations with Sampling Events",
           color = "Legend") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convert each ggplot object to an interactive Plotly object
    flow_plotly <- ggplotly(flow_plot)
    conc_plotly <- ggplotly(conc_plot)
    
    # Combine using subplot, sharing the x-axis
    subplot(flow_plotly, conc_plotly, nrows = 2, shareX = TRUE, titleY = TRUE)
  })
}

shinyApp(ui, server)
