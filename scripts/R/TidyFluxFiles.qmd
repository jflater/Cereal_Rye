---
title: "Untitled"
format: html
---

Path to files
```{r}
path_to_files <-  "../../data/processed/"
```
```{r}
files <- list.files(path_to_files, pattern = "*.csv", full.names = F)
files
```

Read in the first .csv file
```{r}
first_file <- file.path(path_to_files, files[1])

# Read the file, skipping the first row and appending the second row to headers
raw_data <- read.csv(first_file, skip = 1, header = FALSE)
headers <- paste0(raw_data[1, ], "_", raw_data[2, ])

# Assign new headers and remove the first two rows
colnames(raw_data) <- headers
data <- raw_data[-c(1, 2), ]

head(data)
```
```{r}
colnames(data)
```
```{r}
library(janitor)
library(lubridate)
library(tidyverse)

df <- data %>% separate(`LABEL_[#]`, into = c("plot", "location", "element3", "element4"), sep = "[_-]", fill = "right") %>% 
  clean_names() %>% 
  mutate(ymdhms = ymd_hms(date_time_initial_value_yyyy_mm_dd_hh_mm_ss),
         date = ymd(as.Date(ymdhms))) 

head(df)
```
```{r}
df %>% 
  group_by(date, plot, location) %>% 
  mutate(count = n()) %>% 
  select(date, plot, location, count) %>%
  ungroup() %>% 
  dplyr::filter(count > 1)
```
```{r}
# Step 1: Check unique values in the comments column
unique_comments <- df %>% 
  select(comment_number) %>% 
  distinct() %>% 
  pull(comment_number)

print(unique_comments)  # Inspect unique comments
```


```{r}
# Step 2: Filter and clean rows based on the comments column
df_cleaned <- df %>%
  mutate(
    # Standardize comments to lowercase for consistent handling
    comment_number = tolower(comment_number),
    # Add a flag for rows requiring attention based on comments
    needs_review = if_else(str_detect(comment_number, "review|issue|error"), TRUE, FALSE)
  ) %>%
  dplyr::filter(!needs_review)  # Optionally filter out rows needing review for separate handling
```


```{r}
# Step 3: Address where plot numbers are > 15
df_cleaned <- df_cleaned %>%
  mutate(
    # Extract numeric part of plot for processing
    plot_num = as.numeric(plot),
    # Create a label for plots > 15
    large_plot = if_else(plot_num > 15, "Large", "Small")
  ) %>%
  select(-plot_num)  # Remove intermediate column if unnecessary
```


```{r}
# Glimpse at the cleaned data
glimpse(df_cleaned)

```



