# ------------------------------------------------------------------
# Longitudinal violin plot for MMSE (65+ only)
# ------------------------------------------------------------------

# 1. Load required packages
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------------
# Configuration (edit these paths and column names for your data)
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

output_dir <- "longitudinal_outputs"
output_violin_file <- "mmse_violin_65plus.png"
output_boxplot_file <- "mmse_boxplot_65plus.png"
output_stats_file <- "mmse_summary_stats.csv"

time1_label <- "Time 1"
time2_label <- "Time 2"
time_levels <- c(time1_label, time2_label)

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

prepare_timepoint_data <- function(file_path, time_label, item_map, target_items, id_column, class_column, age_column) {
  message(sprintf("Reading %s (%s)...", time_label, file_path))
  df_raw <- readr::read_csv(file_path, show_col_types = FALSE)

  check_required_columns(df_raw, c(id_column, class_column, age_column), time_label)

  df_norm <- df_raw |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(id_column), normalize_id),
      dplyr::across(dplyr::all_of(class_column), ~ as.character(.x)),
      dplyr::across(dplyr::all_of(age_column), ~ suppressWarnings(as.numeric(.x)))
    )

  keep_rows <- !is.na(df_norm[[id_column]]) &
    !is.na(df_norm[[class_column]]) &
    !is.na(df_norm[[age_column]])
  df_norm <- df_norm[keep_rows, , drop = FALSE]

  selected_map <- validate_item_map(item_map, target_items, time_label)
  required_columns <- unique(c(selected_map))
  check_required_columns(df_norm, required_columns, time_label)

  df_standardized <- rename_columns_with_map(df_norm, selected_map)

  df_standardized[target_items] <- lapply(df_standardized[target_items], function(col) {
    if (is.list(col)) {
      stop(paste0(time_label, ": column is list-typed; convert to atomic vector first."), call. = FALSE)
    }
    if (is.factor(col)) {
      col <- as.character(col)
    }
    suppressWarnings(as.numeric(col))
  })

  df_standardized <- df_standardized[which(df_standardized[[age_column]] >= age_threshold), , drop = FALSE]

  if (nrow(df_standardized) == 0) {
    stop(paste0(time_label, ": no records meet the age threshold."), call. = FALSE)
  }

  df_standardized |>
    dplyr::select(
      dplyr::all_of(c(id_column, class_column, age_column, target_items))
    ) |>
    dplyr::mutate(time = time_label)
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

# Prepare each timepoint
df_t1 <- prepare_timepoint_data(
  file_path = file_time1,
  time_label = time1_label,
  item_map = time1_item_map,
  target_items = mmse_items,
  id_column = id_column,
  class_column = class_column,
  age_column = age_column
)

df_t2 <- prepare_timepoint_data(
  file_path = file_time2,
  time_label = time2_label,
  item_map = time2_item_map,
  target_items = mmse_items,
  id_column = id_column,
  class_column = class_column,
  age_column = age_column
)

# Retain overlapping IDs post age-filtering
common_ids <- intersect(df_t1[[id_column]], df_t2[[id_column]])
if (length(common_ids) == 0) {
  stop("No overlapping IDs meet the age threshold at both timepoints.", call. = FALSE)
}

message(sprintf("Overlapping IDs (65+): %d", length(common_ids)))

df_t1_common <- df_t1[df_t1[[id_column]] %in% common_ids, , drop = FALSE]
df_t2_common <- df_t2[df_t2[[id_column]] %in% common_ids, , drop = FALSE]

# Bind long format
df_combined <- dplyr::bind_rows(df_t1_common, df_t2_common)

mmse_long <- df_combined |>
  dplyr::select(dplyr::all_of(class_column), time, dplyr::all_of(mmse_items)) |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(mmse_items),
    names_to = "item_name",
    values_to = "value"
  ) |>
  dplyr::mutate(
    item_key = item_name,
    time = factor(time, levels = time_levels)
  )

mmse_long$item_name <- factor(
  mmse_long$item_key,
  levels = mmse_items,
  labels = item_display_labels
)

mmse_long <- mmse_long[!is.na(mmse_long$value) & !is.na(mmse_long[[class_column]]), , drop = FALSE]
mmse_long$class_plot <- factor(mmse_long[[class_column]])
mmse_long$class_value <- mmse_long[[class_column]]

if (nrow(mmse_long) == 0) {
  stop("No valid MMSE values remain after filtering.", call. = FALSE)
}

mmse_summary <- mmse_long |>
  dplyr::group_by(class = class_value, time, item_key, item_name) |>
  dplyr::summarise(
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    variance = var(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(item_key, time, class)

# ------------------------------------------------------------------
# Plot
# ------------------------------------------------------------------

message("Generating MMSE violin plot (65+)...")

# クラスラベルに "Profile " を追加
levels(mmse_long$class_plot) <- paste0("Profile ", levels(mmse_long$class_plot))

# mmse_summaryのクラスラベルも合わせる
mmse_summary <- mmse_summary %>%
  mutate(class = factor(paste0("Profile ", class), levels = levels(mmse_long$class_plot)))

mmse_violin_plot <- ggplot(mmse_long, aes(x = class_plot, y = value, fill = time)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.5, trim = FALSE) +

  # 平均値とSDを表示（点の追加）
  geom_pointrange(
    data = mmse_summary,
    aes(x = class, y = mean, ymin = mean - sd, ymax = mean + sd, group = time),
    position = position_dodge(width = 0.9),
    color = "black",
    size = 0.6,
    shape = 18,
    show.legend = FALSE
  ) +
  facet_wrap(~item_name, scales = "free_y") +
  labs(
    title = "MMSE Longitudinal Comparison (65+)",
    subtitle = paste0(time1_label, " vs ", time2_label, " (Mean ± SD)"),
    x = "Psychological profile", # Changed from "Class"
    y = "MMSE Score",
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 18, face = "bold")
  ) +
  scale_y_continuous(breaks = function(x) {
    vals <- pretty(x)
    vals[vals %% 1 == 0]
  })

print(mmse_violin_plot)

# ------------------------------------------------------------------
# Boxplot
# ------------------------------------------------------------------

mmse_box_plot <- ggplot(mmse_long, aes(x = class_plot, y = value, fill = time)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.size = 0.6) +
  facet_wrap(~item_name, scales = "free_y") +
  labs(
    title = "MMSE Longitudinal Boxplot (65+)",
    subtitle = paste0(time1_label, " vs ", time2_label),
    x = "Class",
    y = "MMSE Score",
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  ) +
  scale_y_continuous(breaks = function(x) {
    vals <- pretty(x)
    vals[vals %% 1 == 0]
  })

print(mmse_box_plot)

# ------------------------------------------------------------------
# Save plot
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# Save plot & stats
# ------------------------------------------------------------------

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

violin_path <- file.path(output_dir, output_violin_file)
boxplot_path <- file.path(output_dir, output_boxplot_file)
stats_path <- file.path(output_dir, output_stats_file)

ggsave(
  filename = violin_path,
  plot = mmse_violin_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

message(sprintf("Saved violin plot to '%s'.", normalizePath(violin_path)))

ggsave(
  filename = boxplot_path,
  plot = mmse_box_plot,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

message(sprintf("Saved box plot to '%s'.", normalizePath(boxplot_path)))

readr::write_csv(mmse_summary, stats_path)
message(sprintf("Saved summary statistics to '%s'.", normalizePath(stats_path)))
