# ------------------------------------------------------------------
# Linear mixed-effects analysis for longitudinal class comparisons
# (Refactored for readability and specific item handling)
#
# 【結果の読み方ガイド】
#
# 1. 「2時点の変化の仕方（プロファイルの変化）にクラス間で差があるか？」を知りたい場合
#    -> 出力ファイル: lmm_summary_readable.csv
#    -> 見るべき列: **P_Interaction** (交互作用のP値)
#    -> 理由: これが 0.05 未満（Sig_Interactionに"*"がある）なら、「クラスによって変化のパターンが統計的に有意に異なる」ことを意味します。
#             （例：クラス1は悪化しているが、クラス2は変化なしか改善している、など）
#
# 2. 「特定のクラスで、時系列の変化（Time 1 -> Time 2）が有意か？」を知りたい場合
#    -> 出力ファイル: lmm_time_differences_by_class.csv
#    -> 見るべき列: **p_value** (および estimate)
#    -> 理由: ここにはクラスごとの「Time 2 - Time 1」の検定結果が出力されます。
#             P値 < 0.05 なら、そのクラスにおいて「有意な変化があった」と言えます。
#             estimate がプラスなら増加、マイナスなら減少です。
#
# 3. 「全体としてクラス間に差があるか（常にどちらかが高い/低い）？」を知りたい場合
#    -> 出力ファイル: lmm_summary_readable.csv
#    -> 見るべき列: **P_Class** (クラスの主効果)
#    -> 理由: これが有意なら、時点に関わらず「クラス間でスコアの平均的な高さに差がある」ことを示唆します。
#
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
# Configuration
# ------------------------------------------------------------------

file_time1 <- "time1.csv"
file_time2 <- "time2_with_class.csv"

id_column <- "ID"
class_column <- "Class"
age_column <- "age"

# Items to analyse
target_items <- c(
  # IES-R
  "542850_00", "542860_00", "542870_00",
  # GHQ-30
  "543010_00", "543020_00", "543030_00", "543040_00", "543050_00",
  # MMSE
  "516484_00"
)

# Item Labels
item_labels_map <- c(
  "542850_00" = "IES-R: Intrusion",
  "542860_00" = "IES-R: Avoidance",
  "542870_00" = "IES-R: Hyperarousal",
  "542880_00" = "IES-R: Total",
  "543010_00" = "GHQ: Somatic Symptoms",
  "543020_00" = "GHQ: Sleep Disturbance",
  "543030_00" = "GHQ: Social Dysfunction",
  "543040_00" = "GHQ: Anxiety/Depression",
  "543050_00" = "GHQ: Severe Depression",
  "516484_00" = "MMSE Total"
)

# Item-specific Age Filters (e.g., MMSE only for 65+)
item_age_filters <- list(
  "516484_00" = 65
)

# Column mapping for each time point (Item Key = Column Name in CSV)
# Change the values on the right if the column names in your CSVs are different.

time1_item_map <- c(
  # IES-R
  "542850_00" = "542850_00",
  "542860_00" = "542860_00",
  "542870_00" = "542870_00",
  "542880_00" = "542880_00",
  # GHQ-30
  "543010_00" = "543010_00",
  "543020_00" = "543020_00",
  "543030_00" = "543030_00",
  "543040_00" = "543040_00",
  "543050_00" = "543050_00",
  # MMSE
  "516484_00" = "516484_00"
)

time2_item_map <- c(
  # IES-R
  "542850_00" = "541152_00", # Intrusion
  "542860_00" = "541153_00", # Avoidance
  "542870_00" = "541154_00", # Hyperarousal
  "542880_00" = "541155_00", # Total
  # GHQ-30
  "543010_00" = "542003_00", # Somatic Symptoms
  "543020_00" = "542004_00", # Sleep Disturbance
  "543030_00" = "542005_00", # Social Dysfunction
  "543040_00" = "542006_00", # Anxiety/Dysphoria
  "543050_00" = "542007_00", # Severe Depression
  # MMSE
  "516484_00" = "516484_00"
)

# Output locations
output_dir <- "longitudinal_outputs"
output_summary_file <- "lmm_summary_readable.csv"
output_details_file <- "lmm_details_full.csv"
model_log_file <- "lmm_model_warnings.log"
alpha_level <- 0.05

# ------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------

normalize_id <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    dplyr::na_if("")
}

prepare_timepoint_data <- function(file_path, time_label, item_map, target_items, id_column, class_column, age_column) {
  message(sprintf("Reading %s (%s)...", time_label, file_path))
  df_raw <- readr::read_csv(file_path, show_col_types = FALSE)

  # Standardize Class column name (Handle "Class" vs "class")
  if (!class_column %in% names(df_raw) && "Class" %in% names(df_raw)) {
    df_raw <- df_raw |> dplyr::rename(!!class_column := Class)
    message(sprintf("   -> Renamed 'Class' to '%s'", class_column))
  }

  # Check columns
  required_cols <- unique(c(id_column, class_column, age_column, unlist(item_map)))
  missing_cols <- setdiff(required_cols, names(df_raw))
  if (length(missing_cols) > 0) {
    # Try to proceed if only some items are missing, but warn
    warning(paste0(time_label, " missing columns: ", paste(missing_cols, collapse = ", ")))
  }

  df_norm <- df_raw |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(id_column), normalize_id),
      dplyr::across(dplyr::all_of(class_column), ~ as.character(.x)),
      dplyr::across(dplyr::all_of(age_column), ~ suppressWarnings(as.numeric(.x)))
    )

  # Rename columns
  for (item in names(item_map)) {
    col_name <- item_map[[item]]
    if (col_name %in% names(df_norm)) {
      df_norm[[item]] <- suppressWarnings(as.numeric(df_norm[[col_name]]))
    } else {
      df_norm[[item]] <- NA_real_
    }
  }

  df_norm |>
    dplyr::select(dplyr::all_of(c(id_column, class_column, age_column, target_items))) |>
    dplyr::mutate(time = time_label)
}

# Load Data
df_time1 <- prepare_timepoint_data(file_time1, "Time 1", time1_item_map, target_items, id_column, class_column, age_column)
df_time2 <- prepare_timepoint_data(file_time2, "Time 2", time2_item_map, target_items, id_column, class_column, age_column)

# Combine
df_combined <- dplyr::bind_rows(df_time1, df_time2)
df_combined[[class_column]] <- factor(df_combined[[class_column]])
df_combined$time <- factor(df_combined$time, levels = c("Time 1", "Time 2"))

# Pivot Long
df_long <- df_combined |>
  tidyr::pivot_longer(
    cols = dplyr::all_of(target_items),
    names_to = "item_key",
    values_to = "value"
  ) |>
  dplyr::mutate(item_label = item_labels_map[item_key]) |>
  dplyr::filter(!is.na(value), !is.na(.data[[class_column]]), !is.na(.data[[id_column]]))

# ------------------------------------------------------------------
# Analysis
# ------------------------------------------------------------------

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(output_dir, model_log_file)
if (file.exists(log_path)) file.remove(log_path)

append_log <- function(msg) {
  cat(msg, file = log_path, append = TRUE, sep = "\n")
}

results_list <- list()

for (item in target_items) {
  label <- item_labels_map[[item]]
  message(sprintf("Analyzing: %s", label))

  item_data <- df_long |> dplyr::filter(item_key == item)

  # Apply Age Filter if defined
  if (item %in% names(item_age_filters)) {
    min_age <- item_age_filters[[item]]
    n_before <- nrow(item_data)
    # Filter IDs that have age >= min_age at BOTH timepoints (or at least one? usually baseline)
    # Here we filter rows where age >= min_age.
    # Note: Age might change between T1 and T2.
    # Strict approach: Filter IDs where age at T1 >= min_age.
    # Simple approach: Filter rows. But LME needs repeated measures.
    # Let's filter IDs based on T1 age.

    ids_valid <- df_time1 |>
      dplyr::filter(.data[[age_column]] >= min_age) |>
      dplyr::pull(.data[[id_column]])

    item_data <- item_data |> dplyr::filter(.data[[id_column]] %in% ids_valid)
    message(sprintf("   -> Applied age filter (>= %d). IDs: %d -> %d", min_age, dplyr::n_distinct(df_long[[id_column]]), dplyr::n_distinct(item_data[[id_column]])))
  }

  if (nrow(item_data) < 10) {
    append_log(sprintf("Skipping %s: Not enough data.", label))
    next
  }

  # Fit LME
  # Model: Value ~ Time * Class + (1|ID)
  tryCatch(
    {
      model <- nlme::lme(
        fixed = value ~ time * class,
        random = ~ 1 | ID,
        data = item_data,
        method = "REML",
        na.action = na.omit
      )

      # Type 3 ANOVA
      anova_res <- anova(model, type = "marginal") # 'marginal' approximates Type III in nlme

      # Extract P-values
      p_time <- anova_res["time", "p-value"]
      p_class <- anova_res["class", "p-value"]
      p_interaction <- anova_res["time:class", "p-value"]

      # Calculate Means by Group & Time
      emm <- emmeans::emmeans(model, ~ time | class)
      emm_df <- as.data.frame(emm)

      # Format Means for Summary
      # We want columns like: Class1_T1, Class1_T2, Class2_T1...
      means_wide <- emm_df |>
        dplyr::mutate(label = paste0("Class", class, "_", gsub(" ", "", time))) |>
        dplyr::select(label, emmean, SE) |>
        tidyr::pivot_wider(
          names_from = label,
          values_from = c(emmean, SE),
          names_glue = "{label}_{.value}"
        )

      # Combine into a single row
      res_row <- data.frame(
        Item_Key = item,
        Item_Label = label,
        P_Time = p_time,
        P_Class = p_class,
        P_Interaction = p_interaction,
        Sig_Interaction = ifelse(p_interaction < 0.05, "*", "")
      )

      res_row <- cbind(res_row, means_wide)
      results_list[[item]] <- res_row
    },
    error = function(e) {
      append_log(sprintf("Error analyzing %s: %s", label, e$message))
    }
  )
}

# ------------------------------------------------------------------
# Output
# ------------------------------------------------------------------

if (length(results_list) > 0) {
  final_summary <- dplyr::bind_rows(results_list)

  # Reorder columns for readability
  # Key, Label, P-values, then Means
  cols_start <- c("Item_Key", "Item_Label", "P_Interaction", "Sig_Interaction", "P_Time", "P_Class")
  cols_means <- setdiff(names(final_summary), cols_start)
  final_summary <- final_summary |> dplyr::select(all_of(c(cols_start, cols_means)))

  summary_path <- file.path(output_dir, output_summary_file)
  readr::write_csv(final_summary, summary_path)
  message(sprintf("\n✅ Analysis complete. Readable summary saved to:\n   %s", normalizePath(summary_path)))

  # Print preview
  print(final_summary |> dplyr::select(Item_Label, P_Interaction, Sig_Interaction))
} else {
  warning("No results generated.")
}
