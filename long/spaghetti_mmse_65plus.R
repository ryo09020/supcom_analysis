# ------------------------------------------------------------------
# MMSE spaghetti plot (65+ only, overlapping IDs)
# ------------------------------------------------------------------

# 1. Load required packages
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------------
# Configuration (adapt paths/columns to your data)
# ------------------------------------------------------------------

file_time1 <- "time1.csv"
file_time2 <- "time2_with_class.csv"

id_column <- "ID"
class_column <- "class"
age_column <- "age"
age_threshold <- 65

mmse_items <- c("mmse_total")

item_labels_map <- c(
  mmse_total = "MMSE Total"
)

time1_item_map <- c(
  mmse_total = "mmse_total"
)

time2_item_map <- c(
  mmse_total = "mmse_total"
)

output_plot_file <- "mmse_spaghetti_65plus.png"

time1_label <- "Time 1"
time2_label <- "Time 2"
time_labels <- c(time1_label, time2_label)

id_sym <- rlang::sym(id_column)
class_sym <- rlang::sym(class_column)

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

validate_item_map <- function(item_map, target_items, dataset_label) {
  if (length(item_map) == 0) {
    stop(paste0(dataset_label, ": item_map is empty."), call. = FALSE)
  }
  if (is.null(names(item_map)) || any(names(item_map) == "")) {
    stop(paste0(dataset_label, ": item_map must be a named vector keyed by target items."), call. = FALSE)
  }
  missing_keys <- setdiff(target_items, names(item_map))
  if (length(missing_keys) > 0) {
    stop(
      paste0(dataset_label, ": item_map missing keys -> ", paste(missing_keys, collapse = ", ")),
      call. = FALSE
    )
  }
  selected_map <- item_map[target_items]
  if (any(is.na(selected_map))) {
    stop(paste0(dataset_label, ": item_map contains NA values."), call. = FALSE)
  }
  duplicate_sources <- selected_map[duplicated(selected_map)]
  if (length(duplicate_sources) > 0) {
    stop(
      paste0(dataset_label, ": item_map reuses source columns -> ", paste(unique(duplicate_sources), collapse = ", ")),
      call. = FALSE
    )
  }
  selected_map
}

rename_columns_with_map <- function(df, selected_map) {
  renamed_df <- df
  for (idx in seq_along(selected_map)) {
    new_name <- names(selected_map)[idx]
    old_name <- selected_map[[idx]]
    if (identical(new_name, old_name)) {
      next
    }
    matching_cols <- names(renamed_df) == old_name
    if (!any(matching_cols)) {
      stop(paste0("Column '", old_name, "' not found in data."), call. = FALSE)
    }
    names(renamed_df)[matching_cols] <- new_name
  }
  renamed_df
}

coerce_numeric_columns <- function(df, cols, dataset_label) {
  df[cols] <- lapply(df[cols], function(col) {
    if (is.list(col)) {
      stop(paste0(dataset_label, ": column is list-typed; convert it before plotting."), call. = FALSE)
    }
    if (is.factor(col)) {
      col <- as.character(col)
    }
    suppressWarnings(as.numeric(col))
  })
  df
}

prepare_timepoint_data <- function(file_path, time_label, item_map, target_items, id_column, class_column, age_column) {
  message(sprintf("Reading %s (%s)...", time_label, file_path))
  df_raw <- readr::read_csv(file_path, show_col_types = FALSE)

  check_required_columns(df_raw, c(id_column, class_column, age_column), time_label)

  df_norm <- df_raw
  df_norm[[id_column]] <- normalize_id(df_norm[[id_column]])
  df_norm[[class_column]] <- as.character(df_norm[[class_column]])
  df_norm[[age_column]] <- suppressWarnings(as.numeric(df_norm[[age_column]]))

  keep_rows <- !is.na(df_norm[[id_column]]) &
    !is.na(df_norm[[class_column]]) &
    !is.na(df_norm[[age_column]])
  df_norm <- df_norm[keep_rows, , drop = FALSE]

  selected_map <- validate_item_map(item_map, target_items, time_label)
  required_columns <- unique(c(selected_map))
  check_required_columns(df_norm, required_columns, time_label)

  df_standardized <- rename_columns_with_map(df_norm, selected_map)
  df_standardized <- coerce_numeric_columns(df_standardized, target_items, time_label)

  df_standardized <- df_standardized[df_standardized[[age_column]] >= age_threshold, , drop = FALSE]

  if (nrow(df_standardized) == 0) {
    stop(paste0(time_label, ": no records meet the age threshold."), call. = FALSE)
  }

  df_standardized <- df_standardized[, c(id_column, class_column, age_column, target_items), drop = FALSE]
  df_standardized$time <- time_label
  df_standardized
}

# ------------------------------------------------------------------
# Data preparation
# ------------------------------------------------------------------

if (length(mmse_items) == 0) {
  stop("mmse_items is empty; specify at least one MMSE column.", call. = FALSE)
}

missing_label_keys <- setdiff(mmse_items, names(item_labels_map))
if (length(missing_label_keys) > 0) {
  stop(
    paste0(
      "item_labels_map missing labels for -> ",
      paste(missing_label_keys, collapse = ", ")
    ),
    call. = FALSE
  )
}

item_labels_map <- item_labels_map[mmse_items]
item_display_labels <- unname(item_labels_map)

if (is.null(age_threshold) || !is.numeric(age_threshold) || length(age_threshold) != 1L) {
  stop("age_threshold must be a single numeric value.", call. = FALSE)
}

selected_time1_map <- validate_item_map(time1_item_map, mmse_items, "time1")
selected_time2_map <- validate_item_map(time2_item_map, mmse_items, "time2")

# Read and standardize each timepoint
df_t1 <- prepare_timepoint_data(
  file_path = file_time1,
  time_label = time1_label,
  item_map = selected_time1_map,
  target_items = mmse_items,
  id_column = id_column,
  class_column = class_column,
  age_column = age_column
)

df_t2 <- prepare_timepoint_data(
  file_path = file_time2,
  time_label = time2_label,
  item_map = selected_time2_map,
  target_items = mmse_items,
  id_column = id_column,
  class_column = class_column,
  age_column = age_column
)

# Keep IDs present at both waves after age filtering
common_ids <- intersect(df_t1[[id_column]], df_t2[[id_column]])
if (length(common_ids) == 0) {
  stop("No overlapping IDs meet the age threshold at both timepoints.", call. = FALSE)
}

message(sprintf("Overlapping IDs (65+): %d", length(common_ids)))

df_t1_common <- df_t1[df_t1[[id_column]] %in% common_ids, , drop = FALSE]
df_t2_common <- df_t2[df_t2[[id_column]] %in% common_ids, , drop = FALSE]

# Combine and reshape to long format
combined_long <- dplyr::bind_rows(df_t1_common, df_t2_common)

mmse_long <- combined_long |>
  dplyr::select(dplyr::all_of(c(id_column, class_column, "time", mmse_items))) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(mmse_items),
    names_to = "item_name",
    values_to = "value"
  ) |>
  dplyr::mutate(
    item_name = factor(
      item_name,
      levels = mmse_items,
      labels = item_display_labels
    ),
    time = factor(time, levels = time_labels)
  )

mmse_long <- mmse_long[!is.na(mmse_long$value) & !is.na(mmse_long[[class_column]]), , drop = FALSE]
mmse_long$class_plot <- factor(mmse_long[[class_column]])

if (nrow(mmse_long) == 0) {
  stop("No valid MMSE values remain after filtering.", call. = FALSE)
}

# ------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------

message("Generating MMSE spaghetti plot (65+)...")

mmse_spaghetti_plot <- ggplot(
  mmse_long,
  aes(
    x = time,
    y = value,
    group = !!id_sym
  )
) +
  geom_line(alpha = 0.3, color = "#2ca02c") +
  geom_point(size = 1.6, color = "#2ca02c") +
  facet_grid(rows = vars(class_plot), cols = vars(item_name), scales = "free_y") +
  labs(
    title = "MMSE Trajectories by Class (65+)",
    subtitle = paste0(time1_label, " vs ", time2_label),
    x = "Timepoint",
    y = "MMSE Score"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

print(mmse_spaghetti_plot)

# ------------------------------------------------------------------
# Save
# ------------------------------------------------------------------

ggsave(
  filename = output_plot_file,
  plot = mmse_spaghetti_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

message(sprintf("Saved plot to '%s'.", output_plot_file))
