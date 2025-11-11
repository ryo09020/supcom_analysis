# ------------------------------------------------------------------
# Linear mixed-effects analysis for longitudinal class comparisons
# ------------------------------------------------------------------

required_packages <- c("tidyverse", "nlme", "rlang", "emmeans")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    sprintf(
      "Missing required packages: %s. Install them with install.packages().",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(nlme)
  library(rlang)
  library(emmeans)
})

# ------------------------------------------------------------------
# Configuration (adjust for your data)
# ------------------------------------------------------------------

file_time1 <- "time1.csv"
file_time2 <- "time2_with_class.csv"

id_column <- "ID"
class_column <- "class"
class_filter <- NULL  # 例: 1:3 や c("1", "2", "3"); NULL の場合は全クラスを使用

# Items to analyse (order defines reporting order)
target_items <- c("subscale_A", "subscale_B", "total_score", "mmse_total")

# Optional printable labels per item (defaults to item keys when omitted)
item_labels_map <- c(
  subscale_A = "Subscale A",
  subscale_B = "Subscale B",
  total_score = "Total Score",
  mmse_total = "MMSE Total"
)

# Column name mapping for each time point (left = item key, right = column in file)
time1_item_map <- c(
  subscale_A = "subscale_A",
  subscale_B = "subscale_B",
  total_score = "total_score",
  mmse_total = "mmse_total"
)

time2_item_map <- c(
  subscale_A = "subscale_A",
  subscale_B = "subscale_B",
  total_score = "total_score",
  mmse_total = "mmse_total"
)

# Output locations
output_dir <- "longitudinal_outputs"
output_fixed_effects <- "lmm_fixed_effects_results.csv"
output_type3_tests <- "lmm_type3_tests.csv"
output_time_contrasts <- "lmm_time_differences_by_class.csv"
model_log_file <- "lmm_model_warnings.log"
alpha_level <- 0.05

# ------------------------------------------------------------------
# Validation helpers
# ------------------------------------------------------------------

stop_config <- function(msg) {
  stop(paste0("[CONFIG] ", msg), call. = FALSE)
}

normalize_id <- function(x) {
  x |> as.character() |> stringr::str_trim() |> dplyr::na_if("")
}

validate_item_map <- function(item_map, target_items, dataset_label) {
  if (length(item_map) == 0) {
    stop_config(paste0(dataset_label, " item_map が空です"))
  }
  if (is.null(names(item_map)) || any(names(item_map) == "")) {
    stop_config(paste0(dataset_label, " item_map には target_items を名前に持つ必要があります"))
  }
  missing_keys <- setdiff(target_items, names(item_map))
  if (length(missing_keys) > 0) {
    stop_config(paste0(dataset_label, " item_map に不足しているキー: ", paste(missing_keys, collapse = ", ")))
  }
  selected_map <- item_map[target_items]
  if (any(is.na(selected_map))) {
    stop_config(paste0(dataset_label, " item_map に NA が含まれています"))
  }
  duplicates <- selected_map[duplicated(selected_map)]
  if (length(duplicates) > 0) {
    stop_config(paste0(dataset_label, " item_map で同じ列が重複しています: ", paste(unique(duplicates), collapse = ", ")))
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
    matching <- names(renamed_df) == old_name
    if (!any(matching)) {
      stop_config(paste0("列 '", old_name, "' がデータ内に存在しません"))
    }
    names(renamed_df)[matching] <- new_name
  }
  renamed_df
}

prepare_timepoint_data <- function(file_path, time_label, item_map, target_items, id_column, class_column) {
  message(sprintf("Reading %s (%s)...", time_label, file_path))
  df_raw <- readr::read_csv(file_path, show_col_types = FALSE)
  required_cols <- unique(c(id_column, class_column, item_map))
  missing_cols <- setdiff(required_cols, names(df_raw))
  if (length(missing_cols) > 0) {
    stop_config(paste0(time_label, " に必要な列が不足しています: ", paste(missing_cols, collapse = ", ")))
  }
  df_norm <- df_raw |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(id_column), normalize_id),
      dplyr::across(dplyr::all_of(class_column), ~ as.character(.x))
    )
  selected_map <- validate_item_map(item_map, target_items, time_label)
  df_standardized <- rename_columns_with_map(df_norm, selected_map)
  df_standardized[target_items] <- lapply(df_standardized[target_items], function(col) {
    if (is.list(col)) {
      stop_config(paste0(time_label, " の項目列がリスト型です"))
    }
    if (is.factor(col)) {
      col <- as.character(col)
    }
    suppressWarnings(as.numeric(col))
  })
  df_standardized |>
    dplyr::select(dplyr::all_of(c(id_column, class_column, target_items))) |>
    dplyr::mutate(time = time_label)
}

if (!file.exists(file_time1)) {
  stop_config(paste0("Time 1 ファイルが見つかりません: ", file_time1))
}
if (!file.exists(file_time2)) {
  stop_config(paste0("Time 2 ファイルが見つかりません: ", file_time2))
}
if (length(target_items) == 0) {
  stop_config("target_items が空です")
}
if (any(duplicated(target_items))) {
  stop_config("target_items に重複があります")
}

item_labels_map <- if (length(item_labels_map) == 0) {
  stats::setNames(target_items, target_items)
} else {
  missing_labels <- setdiff(target_items, names(item_labels_map))
  if (length(missing_labels) > 0) {
    stop_config(paste0("item_labels_map に不足しているキー: ", paste(missing_labels, collapse = ", ")))
  }
  item_labels_map[target_items]
}

df_time1 <- prepare_timepoint_data(file_time1, "Time 1", time1_item_map, target_items, id_column, class_column)
df_time2 <- prepare_timepoint_data(file_time2, "Time 2", time2_item_map, target_items, id_column, class_column)

df_combined <- dplyr::bind_rows(df_time1, df_time2)

df_combined[[id_column]] <- normalize_id(df_combined[[id_column]])
df_combined[[class_column]] <- as.character(df_combined[[class_column]])

if (!is.null(class_filter)) {
  class_filter_chr <- as.character(class_filter)
  df_combined <- df_combined[df_combined[[class_column]] %in% class_filter_chr, , drop = FALSE]
  if (nrow(df_combined) == 0) {
    stop_config("class_filter に一致するデータが見つかりませんでした")
  }
}

df_long <- df_combined |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(target_items),
    names_to = "item_key",
    values_to = "value"
  ) |>
  dplyr::mutate(
    item_label = item_labels_map[item_key],
    time = factor(time, levels = c("Time 1", "Time 2")),
    !!class_column := factor(.data[[class_column]]),
    !!id_column := factor(.data[[id_column]])
  ) |>
  dplyr::filter(!is.na(.data[[id_column]]), !is.na(.data[[class_column]]), !is.na(value))

if (nrow(df_long) == 0) {
  stop_config("有効な観測がありませんでした")
}

class_levels <- levels(df_long[[class_column]])
df_long[[class_column]] <- factor(df_long[[class_column]], levels = class_levels)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(output_dir, model_log_file)
if (file.exists(log_path)) {
  file.remove(log_path)
}

append_log <- function(msg) {
  cat(msg, file = log_path, append = TRUE, sep = "\n")
}

fit_item_model <- function(item_key, item_label, data) {
  item_df <- dplyr::filter(data, .data$item_key == item_key)
  id_count <- dplyr::n_distinct(item_df[[id_column]])
  time_levels <- dplyr::n_distinct(item_df$time)
  if (id_count < 2 || time_levels < 2) {
    warn_msg <- sprintf("[WARN] %s: insufficient data (ids=%d, times=%d)", item_label, id_count, time_levels)
    message(warn_msg)
    append_log(warn_msg)
    return(NULL)
  }
  fixed_formula <- as.formula(sprintf("value ~ time * %s", class_column))
  random_formula <- as.formula(sprintf("~1|%s", id_column))
  model <- tryCatch({
    nlme::lme(fixed = fixed_formula, random = random_formula, data = item_df, method = "REML")
  }, error = function(e) {
    warn_msg <- sprintf("[ERROR] %s: %s", item_label, conditionMessage(e))
    message(warn_msg)
    append_log(warn_msg)
    return(NULL)
  })
  if (is.null(model)) {
    return(NULL)
  }
  resid_sd <- tryCatch({
    summary(model)$sigma
  }, error = function(e) NA_real_)
  t_table <- summary(model)$tTable
  coef_table <- as.data.frame(t_table)
  coef_table$term <- rownames(t_table)
  rownames(coef_table) <- NULL
  coef_table$item_key <- item_key
  coef_table$item_label <- item_label
  type3 <- as.data.frame(anova(model))
  type3$effect <- rownames(type3)
  rownames(type3) <- NULL
  type3$item_key <- item_key
  type3$item_label <- item_label
  time_emmeans <- tryCatch({
    emmeans::emmeans(model, specs = "time", by = class_column)
  }, error = function(e) {
    warn_msg <- sprintf("[WARN] %s: emmeans failed (%s)", item_label, conditionMessage(e))
    message(warn_msg)
    append_log(warn_msg)
    return(NULL)
  })
  time_contrasts <- NULL
  if (!is.null(time_emmeans)) {
    time_levels <- levels(item_df$time)
    if (length(time_levels) == 2) {
      contrast_vector <- stats::setNames(c(-1, 1), time_levels)
      contrast_list <- stats::setNames(list(contrast_vector), sprintf("%s - %s", time_levels[2], time_levels[1]))
      contrast_results <- tryCatch({
        emmeans::contrast(
          time_emmeans,
          method = contrast_list,
          adjust = "none"
        )
      }, error = function(e) {
        warn_msg <- sprintf("[WARN] %s: contrast calculation failed (%s)", item_label, conditionMessage(e))
        message(warn_msg)
        append_log(warn_msg)
        return(NULL)
      })
    } else {
      warn_msg <- sprintf("[WARN] %s: expected exactly 2 time levels, found %d", item_label, length(time_levels))
      message(warn_msg)
      append_log(warn_msg)
      contrast_results <- NULL
    }
    if (!is.null(contrast_results)) {
      contrast_summary <- tryCatch({
        summary(contrast_results, infer = TRUE)
      }, error = function(e) {
        warn_msg <- sprintf("[WARN] %s: failed to compute contrast summary (%s)", item_label, conditionMessage(e))
        message(warn_msg)
        append_log(warn_msg)
        return(NULL)
      })
      if (!is.null(contrast_summary)) {
        contrast_df <- as.data.frame(contrast_summary)
        contrast_df$effect_size <- if (!is.na(resid_sd) && resid_sd != 0) contrast_df$estimate / resid_sd else NA_real_
        contrast_df$effect_size_mag <- dplyr::case_when(
          is.na(contrast_df$effect_size) ~ NA_character_,
          abs(contrast_df$effect_size) < 0.2 ~ "negligible",
          abs(contrast_df$effect_size) < 0.5 ~ "small",
          abs(contrast_df$effect_size) < 0.8 ~ "medium",
          TRUE ~ "large"
        )
      } else {
        contrast_df <- NULL
      }
      if (!is.null(contrast_df)) {
        contrast_df$item_key <- item_key
        contrast_df$item_label <- item_label
        names(contrast_df)[names(contrast_df) == class_column] <- "class"
        time_contrasts <- contrast_df
      }
    }
  }
  list(fixed = coef_table, type3 = type3, time_contrasts = time_contrasts)
}

model_outputs <- purrr::map(df_long$item_key |> unique(), ~ fit_item_model(.x, item_labels_map[[.x]], df_long))
model_outputs <- purrr::compact(model_outputs)

if (length(model_outputs) == 0) {
  stop_config("全ての項目でモデル推定に失敗しました。ログを確認してください。")
}

fixed_effects <- purrr::map_dfr(model_outputs, "fixed") |>
  dplyr::select(item_key, item_label, term, Value, `Std.Error`, DF, `t-value`, `p-value`)

names(fixed_effects) <- c("item_key", "item_label", "term", "estimate", "std_error", "df", "t_value", "p_value")

# Highlight significance for convenience
fixed_effects <- fixed_effects |>
  dplyr::mutate(significant = ifelse(!is.na(p_value) & p_value < alpha_level, "yes", "no"))

type3_tests <- purrr::map_dfr(model_outputs, "type3")
colnames(type3_tests) <- c("num_df", "den_df", "f_value", "p_value", "effect", "item_key", "item_label")

type3_tests <- type3_tests |>
  dplyr::select(item_key, item_label, effect, num_df, den_df, f_value, p_value) |>
  dplyr::mutate(
    significant = ifelse(!is.na(p_value) & p_value < alpha_level, "yes", "no"),
    partial_eta_sq = ifelse(
      !is.na(f_value) & !is.na(num_df) & !is.na(den_df),
      (f_value * num_df) / (f_value * num_df + den_df),
      NA_real_
    )
  )

time_contrasts <- purrr::map(model_outputs, "time_contrasts") |> purrr::compact()
time_contrasts_df <- NULL
if (length(time_contrasts) > 0) {
  time_contrasts_df <- purrr::map_dfr(time_contrasts, dplyr::as_tibble) |>
    dplyr::rename(
      std_error = SE,
      t_value = `t.ratio`,
      p_value = `p.value`,
      lower_ci = `lower.CL`,
      upper_ci = `upper.CL`
    ) |>
    dplyr::select(item_key, item_label, class, contrast, estimate, std_error, df, t_value, p_value, lower_ci, upper_ci, effect_size, effect_size_mag) |>
    dplyr::mutate(significant = ifelse(!is.na(p_value) & p_value < alpha_level, "yes", "no"))
}

fixed_path <- file.path(output_dir, output_fixed_effects)
analysis <- fixed_effects |>
  dplyr::arrange(item_key, term)
readr::write_csv(analysis, fixed_path)

anova_path <- file.path(output_dir, output_type3_tests)
readr::write_csv(type3_tests, anova_path)

if (!is.null(time_contrasts_df)) {
  contrast_path <- file.path(output_dir, output_time_contrasts)
  readr::write_csv(time_contrasts_df, contrast_path)
  message(sprintf("Saved time contrasts to '%s'.", normalizePath(contrast_path)))
}

message(sprintf("Saved fixed-effect estimates to '%s'.", normalizePath(fixed_path)))
message(sprintf("Saved Type III tests to '%s'.", normalizePath(anova_path)))

if (file.exists(log_path)) {
  message(sprintf("Warnings/errors were logged to '%s'.", normalizePath(log_path)))
}

summary_glimpse <- type3_tests |>
  dplyr::filter(effect %in% c("time", sprintf("time:%s", class_column))) |>
  dplyr::mutate(outcome = dplyr::case_when(
    effect == "time" ~ "Time main effect",
    effect == sprintf("time:%s", class_column) ~ "Time × Class interaction",
    TRUE ~ effect
  )) |>
  dplyr::arrange(item_key, outcome)

if (nrow(summary_glimpse) > 0) {
  cat("\n=== Significance overview (alpha =", alpha_level, ") ===\n")
  summary_glimpse |>
    dplyr::select(item_label, outcome, f_value, p_value, significant) |>
    tibble::as_tibble() |>
    print(n = Inf)
}
