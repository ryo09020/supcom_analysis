# ------------------------------------------------------------------
# ID Overlap Checker
# ------------------------------------------------------------------

library(tidyverse)

# Configuration
# Change these paths to your actual files
file1 <- "time1.csv"
file2 <- "time2_with_class.csv"
id_column <- "ID"

# Output settings
output_dir <- "."  # same folder as inputs by default
output_prefix_1 <- "time1_common"
output_prefix_2 <- "time2_common"

# Check files
if (!file.exists(file1)) stop(paste("File not found:", file1))
if (!file.exists(file2)) stop(paste("File not found:", file2))

# Read Data
message(sprintf("Reading %s...", file1))
df1 <- read_csv(file1, show_col_types = FALSE)

message(sprintf("Reading %s...", file2))
df2 <- read_csv(file2, show_col_types = FALSE)

# Check ID column
if (!id_column %in% names(df1)) stop(paste("ID column not found in", file1))
if (!id_column %in% names(df2)) stop(paste("ID column not found in", file2))

normalize_id <- function(x) {
    x |>
        as.character() |>
        trimws()
}

# Extract IDs (handle potential whitespace)
ids1 <- df1[[id_column]] |>
    normalize_id() |>
    na.omit() |>
    unique()
ids2 <- df2[[id_column]] |>
    normalize_id() |>
    na.omit() |>
    unique()

# Calculate Overlap
common_ids <- intersect(ids1, ids2)
only_in_1 <- setdiff(ids1, ids2)
only_in_2 <- setdiff(ids2, ids1)

# Filter rows to common IDs (keep all matching records)
df1_clean <- df1 |>
    mutate(.ID_norm = normalize_id(.data[[id_column]])) |>
    filter(!is.na(.ID_norm), .ID_norm %in% common_ids) |>
    select(-.ID_norm)

df2_clean <- df2 |>
    mutate(.ID_norm = normalize_id(.data[[id_column]])) |>
    filter(!is.na(.ID_norm), .ID_norm %in% common_ids) |>
    select(-.ID_norm)

# Write outputs (record count included in filename)
out1 <- file.path(output_dir, sprintf("%s_n%d.csv", output_prefix_1, nrow(df1_clean)))
out2 <- file.path(output_dir, sprintf("%s_n%d.csv", output_prefix_2, nrow(df2_clean)))

write_csv(df1_clean, out1)
write_csv(df2_clean, out2)

# Output Results
cat("\n========================================\n")
cat(" ID Overlap Analysis Summary\n")
cat("========================================\n")
cat(sprintf("File 1: %s\n", file1))
cat(sprintf("File 2: %s\n", file2))
cat("----------------------------------------\n")
cat(sprintf("Total IDs in File 1 : %d\n", length(ids1)))
cat(sprintf("Total IDs in File 2 : %d\n", length(ids2)))
cat("----------------------------------------\n")
cat(sprintf("Common IDs (Both)   : %d\n", length(common_ids)))
cat(sprintf("Unique to File 1    : %d\n", length(only_in_1)))
cat(sprintf("Unique to File 2    : %d\n", length(only_in_2)))
cat("----------------------------------------\n")
cat(sprintf("Rows kept in File 1 : %d\n", nrow(df1_clean)))
cat(sprintf("Rows kept in File 2 : %d\n", nrow(df2_clean)))
cat("----------------------------------------\n")
cat(sprintf("Wrote File 1 clean  : %s\n", out1))
cat(sprintf("Wrote File 2 clean  : %s\n", out2))
cat("========================================\n")

# Optional: Save unique IDs to file if needed
# writeLines(only_in_1, "ids_only_in_time1.txt")
# writeLines(only_in_2, "ids_only_in_time2.txt")
