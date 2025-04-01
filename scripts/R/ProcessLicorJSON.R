---
title: "Licor .json flux file processing"
format: html
---

```{r}
# Path to raw files 
path_to_files <- "../../data/raw/"
duplicates_folder <- "../../data/duplicates/"

# Create the duplicates folder if it doesn't exist
if (!dir.exists(duplicates_folder)) {
  dir.create(duplicates_folder, recursive = TRUE)
}

# Save files to a list
files <- list.files(path_to_files, pattern = "*.json", full.names = F)
files
```
```{r}
library(stringr)

# Clean base names by removing spaces and numeric suffixes
cleaned_base_names <- str_remove(basename(files), " \\d+")

cleaned_base_names
```
```{r}
# Keep only unique cleaned file names
files_to_process <- unique(cleaned_base_names)
files_to_process
```
```{r}
# Check which files were removed
duplicates_removed <- setdiff(files, files_to_process)
duplicates_removed

# Move duplicate files to the duplicates folder
file.rename(file.path(path_to_files, duplicates_removed), 
            file.path(duplicates_folder, duplicates_removed))
```

