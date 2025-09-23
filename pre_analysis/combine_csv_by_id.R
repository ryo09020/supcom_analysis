# -------------------------------------------------------------------------
# Combine multiple CSVs by common ID column and output merged CSV
# -------------------------------------------------------------------------
# This script merges multiple CSV files by the common column "ID".
# The output CSV will have columns in the order: ID, columns from A, B, C, ...
# Specify input/output file names and variables at the top.
# -------------------------------------------------------------------------

# ------------------- User settings -------------------
# List of input CSV file paths (in desired order)
input_files <- c("raw_data/dummy_data.csv", "raw_data/dummy_data_with_clusters_sorted.csv")  # <-- Edit here

# Output CSV file name
output_file <- "merged_output.csv"            # <-- Edit here

# Common ID column name
id_column <- "ID"                            # <-- Edit here

# ----------------------------------------------------

library(dplyr)
library(readr)

# Read all CSVs into a list of data.frames
data_list <- lapply(input_files, function(f) read_csv(f, show_col_types = FALSE))

# Remove duplicate columns except ID (to avoid column name collision)
for (i in seq_along(data_list)) {
  cols <- colnames(data_list[[i]])
  # Remove ID from the set to check
  other_cols <- setdiff(cols, id_column)
  # If not the first file, remove columns that already appeared
  if (i > 1) {
    prev_cols <- unlist(lapply(data_list[1:(i-1)], colnames))
    dup_cols <- intersect(other_cols, prev_cols)
    data_list[[i]] <- data_list[[i]][, !(colnames(data_list[[i]]) %in% dup_cols)]
  }
}

# Merge all data.frames by ID (full join, input order)
merged_data <- Reduce(function(x, y) full_join(x, y, by = id_column), data_list)

# Arrange by ID (optional)
merged_data <- merged_data %>% arrange(.data[[id_column]])

# Write to output CSV
write_csv(merged_data, output_file)

cat(sprintf("Merged CSV saved as '%s'.\n", output_file))