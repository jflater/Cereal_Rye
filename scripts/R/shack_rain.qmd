---
title: "Process shack rainfall .dat files"
format: html
---
Read from logger box files, not zipped
```{r}
path <- "C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/Rain_CABBI_Shack/Shack_Rain/"
```

```{r}
# Function to read rain data files based on a date range
read_all_rain_files <- function(path) {
  # List all files in the folder matching the pattern (if desired)
  full_paths <- list.files(path, pattern = "SABR_Precip_.*\\.dat$", full.names = TRUE)
  
  # If no files are found, return NULL
  if (length(full_paths) == 0) return(NULL)
  
  # Extract column names from the second row of the first valid file
  col_names <- read_csv(full_paths[1], n_max = 1, skip = 1, col_names = FALSE, show_col_types = FALSE) %>%
    unlist() %>%
    as.character()
  
  # Create an empty list to store each data frame
  data_list <- list()
  
  # Loop over each file path
  for (file_path in full_paths) {
    # Read the data from the file, skipping the first 4 lines
    data <- read_lines(file_path) %>% 
      .[-c(1:4)] %>%
      paste(collapse = "\n") %>%
      I() %>% 
      read_csv(col_names = col_names, show_col_types = FALSE)
    
    # Add the data frame to the list
    data_list[[file_path]] <- data
  }
  
  # Combine all data frames into one
  combined_data <- bind_rows(data_list)
  
  return(combined_data)
}

```

```{r}
new_rain <- read_all_rain_files(path)
```
```{r}
rain_23 <- new_rain %>% 
  dplyr::filter(year(TIMESTAMP) == 2023)
  
rain_24 <- new_rain %>% 
  dplyr::filter(year(TIMESTAMP) == 2024)
```

```{r}
process_and_plot_rainfall <- function(data) {
  # Calculate the row means for specified rain columns
  data <- data %>%
    mutate(Mean_Rain = rowMeans(select(., starts_with("Rain_mm"))))

  # Calculate cumulative sum of the row means
  data <- data %>%
    mutate(Cumulative_Rain = cumsum(Mean_Rain))

  # Calculate total rainfall and create a label for it
  total_rainfall <- max(data$Cumulative_Rain)
  label_text <- paste("Total:", total_rainfall, "mm")

  # Plotting the cumulative sum
    p <- ggplot(data, aes(x = TIMESTAMP)) +
    geom_col(aes(y = Mean_Rain), fill = "skyblue", alpha = 0.5) +
    geom_line(aes(y = Cumulative_Rain), color = "blue") +
    geom_text(aes(x = max(TIMESTAMP), y = total_rainfall, label = label_text), hjust = 1.1, vjust = 2.1, check_overlap = TRUE) +
    labs(title = "Cumulative and Mean Rainfall at SABR",
         x = "Time",
         y = "Rainfall (mm)") +
    theme_minimal()
  
  return(list(Data = data, Plot = p))
}
```
```{r}
df_rain_23 <- process_and_plot_rainfall(rain_23)
df_rain_24 <- process_and_plot_rainfall(rain_24)
print(df_rain_23$Plot)
print(df_rain_24$Plot)

```

```{r}
recent_rain <- new_rain |> 
  select(!Rain_mm_2_Tot) |> 
  process_and_plot_rainfall()

# To view the plot
print(recent_rain$Plot)

# To access the data
recent_rain_data <- recent_rain$Data
tail(recent_rain_data)
```

```{r}
# Function to process rainfall data and plot individual gauges side by side
process_and_plot_rainfall_gauges <- function(data) {
  # Reshape data from wide to long format, where each gauge becomes a separate observation
  data_long <- data %>%
    pivot_longer(
      cols = starts_with("Rain_mm"),
      names_to = "Gauge",
      values_to = "Rainfall_mm"
    ) 

  # Plotting individual gauges with different colors
  p <- ggplot(data_long, aes(x = TIMESTAMP, y = Rainfall_mm, fill = Gauge)) +
    geom_bar(stat = "identity", position = "dodge") +  
    scale_y_continuous(breaks = seq(0, max(data_long$Rainfall_mm, na.rm = TRUE) + 0.5, by = 0.5)) +  # Custom y-axis breaks
    labs(title = "Rainfall Measurements by Gauge",
         x = "Time",
         y = "Rainfall (mm)") +
    scale_fill_manual(values = c("skyblue", "orange", "green", "red")) +  # Customize colors for each gauge
    theme_minimal() +
    theme(legend.title = element_blank())  # Hide the legend title

  # Return the plot
  return(p)
}

result_plot <- process_and_plot_rainfall_gauges(new_rain)
print(result_plot)

```

<!-- Save data, this is raw so we will need to remove bad gauges when plotting -->
<!-- We need to append new data to combined data -->
<!-- ```{r} -->
<!-- max(combined_data$TIMESTAMP) -->
<!-- ``` -->
<!-- We need to get the files from `r max(combined_data$TIMESTAMP)` to now -->
<!-- ```{r} -->
<!-- new_rain <- read_rain_files(path, "2024-06-14", "2024-06-24") -->
<!-- ``` -->

<!-- ```{r} -->
<!-- head(new_rain) -->
<!-- head(combined_data) -->
<!-- ``` -->
<!-- ```{r} -->
<!-- rain <- bind_rows(combined_data, new_rain) -->
<!-- ``` -->
<!-- ```{r} -->
<!-- head(rain) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- tail(rain) -->
<!-- ``` -->

<!-- ```{r} -->
<!-- # Count duplicates in the column -->
<!-- duplicate_counts <- table(rain$TIMESTAMP) -->

<!-- # Filter counts to find duplicates -->
<!-- duplicate_counts[duplicate_counts > 1] -->
<!-- ``` -->

<!-- ```{r} -->
<!-- any(duplicated(rain$RECORD)) -->
<!-- any(duplicated(rain$TIMESTAMP)) -->
<!-- ``` -->

Let's drop the record column

```{r}
rain_data <- new_rain |>
  select(!RECORD)
```

```{r}
combined_data_cumulative_23 <- df_rain_23$Data %>%
  mutate(
    Cum_Rain_mm_1_Tot = cumsum(Rain_mm_1_Tot),
    Cum_Rain_mm_2_Tot = cumsum(Rain_mm_2_Tot),
    Cum_Rain_mm_3_Tot = cumsum(Rain_mm_3_Tot),
    Cum_Rain_mm_4_Tot = cumsum(Rain_mm_4_Tot)
  )

rain_data_long_23 <- combined_data_cumulative_23 %>%  
  pivot_longer(cols = starts_with("Cum_Rain_mm"),
               names_to = "Guage",
               values_to = "Cum_Rain_mm")

ggplot(rain_data_long_23, aes(x = TIMESTAMP, y = Cum_Rain_mm, color = Guage, group = Guage)) +
  geom_line() +
  labs(x = "Time", y = "Rainfall (mm)") +
  theme_minimal()

combined_data_cumulative_24 <- df_rain_24$Data %>%
  mutate(
    Cum_Rain_mm_1_Tot = cumsum(Rain_mm_1_Tot),
    Cum_Rain_mm_2_Tot = cumsum(Rain_mm_2_Tot),
    Cum_Rain_mm_3_Tot = cumsum(Rain_mm_3_Tot),
    Cum_Rain_mm_4_Tot = cumsum(Rain_mm_4_Tot)
  )

rain_data_long_24 <- combined_data_cumulative_24 |> 
  pivot_longer(cols = starts_with("Cum_Rain_mm"),
               names_to = "Guage",
               values_to = "Cum_Rain_mm")

ggplot(rain_data_long_24, aes(x = TIMESTAMP, y = Cum_Rain_mm, color = Guage, group = Guage)) +
  geom_line() +
  labs(x = "Time", y = "Rainfall (mm)") +
  theme_minimal()
```

We should check against the small drainage plots, but for now let's just use gauges 1 and 4

```{r}
rain_mean_23 <- df_rain_23$Data %>% 
  select(-Rain_mm_3_Tot) %>%
  mutate(Mean_Rain = rowMeans(across(starts_with("Rain_mm")))) %>% 
  select(-starts_with("Rain"), -Cumulative_Rain)

head(rain_mean_23)


rain_mean_24 <- df_rain_24$Data %>% 
  select(-Rain_mm_2_Tot) %>%
  mutate(Mean_Rain = rowMeans(across(starts_with("Rain_mm")))) %>% 
  select(-starts_with("Rain"), -Cumulative_Rain)

head(rain_mean_24)
all_mean <- rbind(rain_mean_23, rain_mean_24)
```
```{r}
cumulative <- all_mean %>% 
  group_by(year(TIMESTAMP)) %>% 
  arrange(TIMESTAMP) %>% 
  mutate(cumulative_rain = cumsum(Mean_Rain))
```

```{r}
library(ggplot2)
library(dplyr)
library(lubridate)

# Create a new variable for day of year
cumulative <- cumulative %>%
  mutate(doy = yday(TIMESTAMP),
         year = factor(year(TIMESTAMP)))  # factor for color/legend

# Plot, using day of year on the x-axis
ggplot(cumulative, aes(x = doy, y = cumulative_rain, color = year, group = year)) +
  geom_line(linewidth = 2) +
  # Set x-axis breaks for the start of each month (approx.)
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  ) + 
  theme(text = element_text(size = 14),  # Increase text size; adjust as needed
        axis.title = element_text(size = 16),  # Specific size for axis titles
        axis.text = element_text(size = 14),  # Specific size for axis text
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels
        strip.text = element_text(size = 16),  # Specific size for facet labels
        panel.background = element_rect(fill = "white"), # Set panel background to white
        plot.background = element_rect(fill = "white", color = "white")) +
  # Assign specific colors to each factor level
  scale_color_manual(
    values = c("2023" = "red", "2024" = "orange"),
    name = "Year"
  ) +
  labs(
    x = "Month",
    y = "Cumulative Rain (mm)",   # or whichever units apply
    color = "Year"
  ) +
  theme_minimal()

ggsave("../../figures/cumulative_rain.png", width = 12, height = 8, dpi = 300)
```

