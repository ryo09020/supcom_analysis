# ------------------------------------------------------------------
# Longitudinal spaghetti plot (only IDs present at both timepoints)
# ------------------------------------------------------------------

# 1. Load required packages
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------------
# Configure file names and column names
# ------------------------------------------------------------------
# Update the following paths and columns as needed.

# 2-1. Input files
file_time1 <- "time1.csv"
file_time2 <- "time2_with_class.csv"  # second timepoint with class column

# 2-2. ID and class columns
id_column <- "ID"
class_column <- "class"

# 2-3. Target item columns (plot order follows this vector)
target_items <- c("subscale_A", "subscale_B", "total_score")

# 2-4. Item labels for display
item_labels <- c(
  subscale_A = "Subscale A",
  subscale_B = "Subscale B",
  total_score = "Total Score"
)

# 2-5. Output settings
output_plot_file <- "spaghetti_transition_plot.png"

time_labels <- c("Time 1", "Time 2")

# ------------------------------------------------------------------
# Helper utilities
# ------------------------------------------------------------------

normalize_id <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    dplyr::na_if("")
}

check_required_columns <- function(df, required_cols, dataset_label) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste0(dataset_label, ": missing columns -> ", paste(missing_cols, collapse = ", ")),
      call. = FALSE
    )
  }
}

# ------------------------------------------------------------------
# 3. Data import and preparation
# ------------------------------------------------------------------

cat("Reading data...\n")
df_t1_raw <- readr::read_csv(file_time1, show_col_types = FALSE)
df_t2_raw <- readr::read_csv(file_time2, show_col_types = FALSE)

check_required_columns(df_t1_raw, c(id_column, class_column, target_items), "time1")
check_required_columns(df_t2_raw, c(id_column, target_items), "time2")

# Normalize IDs
df_t1 <- df_t1_raw |>
  dplyr::mutate(
    !!id_column := normalize_id(.data[[id_column]]),
    !!class_column := .data[[class_column]]
  ) |>
  dplyr::filter(!is.na(.data[[id_column]]), !is.na(.data[[class_column]]))

df_t2 <- df_t2_raw |>
  dplyr::mutate(!!id_column := normalize_id(.data[[id_column]])) |>
  dplyr::filter(!is.na(.data[[id_column]]))

# Retain only IDs present at both timepoints
common_ids <- intersect(df_t1[[id_column]], df_t2[[id_column]])
if (length(common_ids) == 0) {
  stop("No overlapping IDs between time1 and time2.", call. = FALSE)
}

cat(paste0("Overlapping IDs: ", length(common_ids), "\n"))

df_t1_common <- df_t1 |>
  dplyr::filter(.data[[id_column]] %in% common_ids)

df_t2_common <- df_t2 |>
  dplyr::filter(.data[[id_column]] %in% common_ids)

# Use the class assignments from time1
class_lookup <- df_t1_common |>
  dplyr::select(dplyr::all_of(c(id_column, class_column))) |>
  dplyr::distinct()

# 4. Build long-format dataset -------------------------------------------------

prepare_long <- function(df, time_label) {
  df |>
    dplyr::select(dplyr::all_of(c(id_column, target_items))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(target_items),
      names_to = "item_key",
      values_to = "value"
    ) |>
    dplyr::mutate(
      time = time_label
    )
}

df_time1_long <- prepare_long(df_t1_common, time_labels[1])
df_time2_long <- prepare_long(df_t2_common, time_labels[2])

combined_long <- dplyr::bind_rows(df_time1_long, df_time2_long) |>
  dplyr::left_join(class_lookup, by = id_column)

combined_long <- combined_long |>
  dplyr::mutate(
    item_key = factor(item_key, levels = target_items),
    item_label = item_labels[as.character(item_key)],
    time = factor(time, levels = time_labels),
    class = factor(.data[[class_column]])
  ) |>
  dplyr::filter(!is.na(value), !is.na(class))

if (nrow(combined_long) == 0) {
  stop("No valid (non-missing) values for overlapping IDs.", call. = FALSE)
}

# ------------------------------------------------------------------
# 5. Create spaghetti plot
# ------------------------------------------------------------------

cat("Generating spaghetti plot...\n")

spaghetti_plot <- ggplot(
  combined_long,
  aes(
    x = time,
    y = value,
    group = .data[[id_column]]
  )
) +
  geom_line(alpha = 0.25, color = "#1f77b4") +
  geom_point(aes(shape = time), size = 1.6, color = "#1f77b4") +
  facet_grid(rows = vars(class), cols = vars(item_label), scales = "free_y") +
  labs(
    title = "Individual Trajectories by Class",
    subtitle = "Only IDs present in both Time 1 and Time 2",
    x = "Timepoint",
    y = "Value"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

print(spaghetti_plot)

# ------------------------------------------------------------------
# 6. Save to file
# ------------------------------------------------------------------

ggsave(
  filename = output_plot_file,
  plot = spaghetti_plot,
  width = 12,
  height = 8,
  dpi = 300,
  bg = "white"
)

cat(paste0("Saved plot to '", output_plot_file, "'.\n"))
