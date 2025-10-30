#!/usr/bin/env Rscript

#################################################################
# LPA age-stratified workflow script
#
# Purpose:
# - Run Latent Profile Analysis (LPA) for predefined age cohorts
# - Produce model fit summaries and final data with assigned classes
# - Mirror the behaviour of lpa_flow.R while splitting the sample
#################################################################

# ================================================================
# Configuration
# ================================================================

# Input file path (relative to project root)
INPUT_FILE <- "raw_data/dummy_data.csv"

# Columns used in the LPA
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# Age configuration
AGE_COLUMN <- "age"
AGE_GROUPS <- list(
  list(name = "age_00_49", label = "Age 0-49", lower = 0, upper = 50, upper_inclusive = FALSE, final_clusters = 3),
  list(name = "age_50_90", label = "Age 50-90", lower = 50, upper = 90, upper_inclusive = TRUE, final_clusters = 4)
)

# LPA settings
SCALING_METHOD <- "minmax"          # Options: "zscore", "minmax", "minmax_trimmed"
PROFILE_RANGE <- 1:5                 # Candidate number of profiles to estimate
DEFAULT_FINAL_CLUSTERS <- 3          # Fallback if a group omits final_clusters
MIN_COMPLETE_CASES <- 5              # Minimum rows after drop_na to attempt LPA

# Output settings
LPA_OUTPUT_DIR <- "lpa"              # Base output directory
OUTPUT_PREFIX <- ""                  # Optional prefix for exported filenames
SAVE_COMPARISON_TABLE <- TRUE
COMPARISON_TABLE_FILENAME <- "lpa_comparison_table.csv"

# Logging verbosity
SHOW_DETAILED_OUTPUT <- TRUE

# ================================================================
# Package handling
# ================================================================

setup_packages <- function() {
  packages <- c("tidyverse", "tidyLPA", "knitr", "fmsb")
  lapply(packages, library, character.only = TRUE)
  cat("[info] Packages loaded.\n\n")
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "row_id", "Class", "N", "n", "Count", "Percent", "CumPercent", "Percentage", "sex",
    "LogLik", "AIC", "BIC", "SABIC", "Entropy", "BLRT_p"
  ))
}

ensure_output_directory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
}

# ================================================================
# Data utilities
# ================================================================

load_data <- function() {
  file_path <- INPUT_FILE

  if (SHOW_DETAILED_OUTPUT) {
    cat(sprintf("[info] Reading file: %s\n", file_path))
  }

  if (!file.exists(file_path)) {
    stop(sprintf("Input file not found: %s", file_path))
  }

  data <- readr::read_csv(file_path, show_col_types = FALSE)

  if (SHOW_DETAILED_OUTPUT) {
    cat(sprintf("[info] Loaded data: %d rows x %d columns\n\n", nrow(data), ncol(data)))
  }

  list(data = data, file_path = file_path)
}

select_lpa_variables <- function(data) {
  missing_cols <- TARGET_COLUMNS[!(TARGET_COLUMNS %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop(sprintf("Columns not found in data: %s", paste(missing_cols, collapse = ", ")))
  }

  if (SHOW_DETAILED_OUTPUT) {
    cat("[info] LPA variables:\n")
    cat(paste0("  - ", TARGET_COLUMNS, collapse = "\n"), "\n\n")
  }

  TARGET_COLUMNS
}

subset_by_age <- function(data, age_col, lower, upper, upper_inclusive) {
  if (!(age_col %in% names(data))) {
    stop(sprintf("Age column '%s' is missing from data.", age_col))
  }

  age_values <- data[[age_col]]
  mask <- !is.na(age_values)

  if (!is.null(lower)) {
    mask <- mask & (age_values >= lower)
  }

  if (!is.null(upper)) {
    if (isTRUE(upper_inclusive)) {
      mask <- mask & (age_values <= upper)
    } else {
      mask <- mask & (age_values < upper)
    }
  }

  data[mask, , drop = FALSE]
}

# ================================================================
# Scaling helper
# ================================================================

apply_scaling_method <- function(df, method) {
  method <- tolower(method)

  if (!nrow(df) || !ncol(df)) {
    return(df)
  }

  scale_minmax <- function(x) {
    if (all(is.na(x))) {
      return(rep(NA_real_, length(x)))
    }
    rng <- range(x, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
      return(rep(0.5, length(x)))
    }
    (x - rng[1]) / diff(rng)
  }

  scale_minmax_trimmed <- function(x) {
    if (all(is.na(x))) {
      return(rep(NA_real_, length(x)))
    }
    q05 <- stats::quantile(x, 0.05, na.rm = TRUE, names = FALSE, type = 7)
    q95 <- stats::quantile(x, 0.95, na.rm = TRUE, names = FALSE, type = 7)
    trimmed <- pmax(pmin(x, q95), q05)
    rng <- range(trimmed, na.rm = TRUE)
    if (!is.finite(rng[1]) || !is.finite(rng[2]) || diff(rng) == 0) {
      return(rep(0.5, length(x)))
    }
    (trimmed - rng[1]) / diff(rng)
  }

  scale_zscore <- function(x) {
    if (all(is.na(x))) {
      return(rep(NA_real_, length(x)))
    }
    mu <- mean(x, na.rm = TRUE)
    sigma <- stats::sd(x, na.rm = TRUE)
    if (is.na(sigma) || sigma == 0) {
      return(rep(0, length(x)))
    }
    (x - mu) / sigma
  }

  scaled_list <- switch(
    method,
    "minmax" = lapply(df, scale_minmax),
    "minmax_trimmed" = lapply(df, scale_minmax_trimmed),
    "zscore" = lapply(df, scale_zscore),
    stop(sprintf("Unsupported scaling method: %s", method))
  )

  scaled_df <- as.data.frame(scaled_list, optional = FALSE, stringsAsFactors = FALSE, check.names = FALSE)
  names(scaled_df) <- names(df)
  scaled_df
}

# ================================================================
# LPA preparation and modelling
# ================================================================

prepare_lpa_data <- function(data, selected_columns, scaling_method) {
  cat("[step] Preparing LPA dataset...\n")

  if (!("row_id" %in% names(data))) {
    data$row_id <- seq_len(nrow(data))
  }

  df_original <- data

  df_for_lpa <- df_original |>
    dplyr::select(dplyr::all_of(c("row_id", selected_columns))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(selected_columns), as.numeric)) |>
    tidyr::drop_na()

  df_to_scale <- df_for_lpa |> dplyr::select(-dplyr::all_of("row_id"))
  scaled_df <- apply_scaling_method(df_to_scale, scaling_method)

  df_analysis <- dplyr::bind_cols(df_for_lpa |> dplyr::select(dplyr::all_of("row_id")), scaled_df)

  removed_rows <- nrow(df_original) - nrow(df_analysis)
  cat(sprintf("[info] Complete cases: %d (removed %d due to missing values)\n\n", nrow(df_analysis), removed_rows))

  list(
    original = df_original,
    for_lpa = df_for_lpa,
    analysis = df_analysis,
    removed = removed_rows
  )
}

run_lpa_models <- function(df_analysis, profile_range, group_label) {
  if (nrow(df_analysis) < MIN_COMPLETE_CASES) {
    stop(sprintf("Not enough complete cases (%d) for group %s.", nrow(df_analysis), group_label))
  }

  if (SHOW_DETAILED_OUTPUT) {
    cat(sprintf("[step] Estimating profiles for %s (profiles: %s)\n", group_label, paste(profile_range, collapse = ", ")))
  }

  analysis_data <- df_analysis |> dplyr::select(-dplyr::all_of("row_id"))

  lpa_models <- tidyLPA::estimate_profiles(
    analysis_data,
    n_profiles = profile_range,
    boot_for_p = TRUE,
    models = 1
  )

  if (SHOW_DETAILED_OUTPUT) {
    cat("[info] LPA estimation complete.\n\n")
  }

  lpa_models
}

# ================================================================
# Fit diagnostics and comparison table
# ================================================================

create_comparison_table <- function(lpa_models, group_label) {
  cat(sprintf("[step] Building fit comparison table for %s...\n", group_label))

  tryCatch({
    fit_indices <- tidyLPA::get_fit(lpa_models)

    n_sample <- lpa_models[[1]]$model$n
    npar_vec <- sapply(lpa_models, function(mod) mod$model$df)
    fit_indices$Parameters <- npar_vec
    fit_indices$VLMR_p <- NA_real_

    if (nrow(fit_indices) > 1) {
      for (k in 2:nrow(fit_indices)) {
        null_model <- fit_indices[k - 1, ]
        alt_model <- fit_indices[k, ]

        lmr_result <- tidyLPA::calc_lrt(
          n = n_sample,
          null_ll = null_model$LogLik,
          null_param = null_model$Parameters,
          null_classes = null_model$Classes,
          alt_ll = alt_model$LogLik,
          alt_param = alt_model$Parameters,
          alt_classes = alt_model$Classes
        )

        fit_indices$VLMR_p[k] <- lmr_result[4]
      }
    }

    class_proportions_list <- vector("list", length(lpa_models))
    fit_classes <- fit_indices$Classes

    for (i in seq_along(lpa_models)) {
      model_data <- tidyLPA::get_data(lpa_models[[i]])

      proportions_text <- tryCatch({
        if (is.null(model_data) || !("Class" %in% colnames(model_data))) {
          stop("Class column missing from model data.")
        }

        class_stats <- model_data |>
          dplyr::count(Class = rlang::.data$Class, name = "N") |>
          dplyr::arrange(rlang::.data$Class) |>
          dplyr::mutate(
            Percentage = round(rlang::.data$N / sum(rlang::.data$N) * 100, 1),
            Class = as.integer(rlang::.data$Class)
          )

        paste(class_stats$Percentage, collapse = "/")
      }, error = function(e) {
        "N/A"
      })

      class_proportions_list[[i]] <- data.frame(
        Profiles = if (length(fit_classes) >= i) as.integer(fit_classes[i]) else NA_integer_,
        `% in each class` = proportions_text,
        stringsAsFactors = FALSE
      )
    }

    class_proportions <- do.call(rbind, class_proportions_list)
    names(class_proportions)[2] <- "% in each class"

    rename_map <- c(
      Classes = "Profiles",
      LogLik = "Log-likelihood",
      SABIC = "Sample-Size Adjusted BIC",
      BLRT_p = "BLRT p-value",
      VLMR_p = "VLMR p-value",
      prob_min = "Prob Min",
      prob_max = "Prob Max",
      n_min = "N Min",
      n_max = "N Max",
      BLRT_val = "BLRT Value"
    )

    common_cols <- intersect(names(rename_map), names(fit_indices))

    final_table <- fit_indices |>
      dplyr::rename_with(
        .cols = dplyr::all_of(common_cols),
        .fn = ~ unname(rename_map[.x])
      ) |>
      dplyr::left_join(class_proportions, by = "Profiles") |>
      dplyr::mutate(
        dplyr::across(
          tidyselect::any_of(c("Log-likelihood", "AIC", "AWE", "BIC", "CAIC", "CLC", "KIC", "Sample-Size Adjusted BIC", "ICL")),
          ~ round(.x, 2)
        ),
        dplyr::across(
          tidyselect::any_of(c("Entropy", "BLRT p-value", "VLMR p-value", "Prob Min", "Prob Max")),
          ~ round(.x, 3)
        ),
        dplyr::across(tidyselect::any_of(c("BLRT Value")), ~ round(.x, 2)),
        dplyr::across(
          tidyselect::any_of(c("Profiles", "Parameters", "N Min", "N Max")),
          ~ as.integer(.x)
        )
      )

    desired_order <- c(
      "Model", "Profiles", "Log-likelihood", "AIC", "BIC", "Sample-Size Adjusted BIC", "AWE", "CAIC", "CLC", "KIC", "ICL",
      "Entropy", "BLRT p-value", "VLMR p-value", "BLRT Value",
      "Prob Min", "Prob Max", "N Min", "N Max", "Parameters",
      "% in each class"
    )

    final_table <- final_table |>
      dplyr::select(tidyselect::any_of(desired_order))

    cat("[info] Fit table created.\n\n")
    final_table

  }, error = function(e) {
    stop(sprintf("Failed to create comparison table for %s: %s", group_label, e$message))
  })
}

display_and_save_comparison <- function(comparison_table, output_dir, filename) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("[info] Fit indices summary:\n")
    cat("--------------------------------------------------\n")
    print(comparison_table)
    cat("--------------------------------------------------\n\n")
  }

  if (SAVE_COMPARISON_TABLE) {
    ensure_output_directory(output_dir)
    output_path <- file.path(output_dir, filename)
    readr::write_csv(comparison_table, output_path)
    if (SHOW_DETAILED_OUTPUT) {
      cat(sprintf("[info] Comparison table saved to: %s\n\n", normalizePath(output_path)))
    }
  }
}

# ================================================================
# Model selection, assignment, and export
# ================================================================

select_final_clusters <- function(comparison_table, target_clusters, group_label) {
  if (!(target_clusters %in% comparison_table$Profiles)) {
    stop(sprintf("Target clusters %d not found in comparison table for %s.", target_clusters, group_label))
  }
  if (SHOW_DETAILED_OUTPUT) {
    cat(sprintf("[info] Using %d clusters for %s.\n\n", target_clusters, group_label))
  }
  target_clusters
}

get_selected_model <- function(lpa_models, n_clusters, group_label) {
  cat(sprintf("[step] Retrieving %d-cluster model for %s...\n", n_clusters, group_label))

  model_names <- names(lpa_models)
  matches <- grep(paste0("_class_", n_clusters, "$"), model_names)

  if (!length(matches)) {
    stop(sprintf("No model with %d clusters found for %s.", n_clusters, group_label))
  }

  selected_model <- lpa_models[[matches[1]]]

  if (SHOW_DETAILED_OUTPUT) {
    tryCatch({
      fit_indices <- tidyLPA::get_fit(selected_model)
      class_stats <- tidyLPA::get_data(selected_model) |>
  dplyr::count(Class = rlang::.data$Class, name = "N") |>
  dplyr::mutate(Percentage = round(rlang::.data$N / sum(rlang::.data$N) * 100, 2))

      cat("[info] Selected model fit indices:\n")
      print(fit_indices |> dplyr::select(dplyr::all_of(c("LogLik", "AIC", "BIC", "SABIC", "Entropy", "BLRT_p"))))
      cat("[info] Class distribution:\n")
      print(class_stats)
      cat("\n")
    }, error = function(e) {
      cat(sprintf("[warn] Unable to display fit summary: %s\n", e$message))
    })
  }

  selected_model
}

assign_clusters_to_data <- function(original_data, df_for_lpa, lpa_model, age_column) {
  cat("[step] Joining class assignments back to the subset...\n")

  lpa_results <- tidyLPA::get_data(lpa_model)

  results_with_id <- dplyr::bind_cols(
    df_for_lpa |> dplyr::select(dplyr::all_of("row_id")),
    lpa_results |> dplyr::select(dplyr::all_of("Class"))
  )

  df_final <- dplyr::left_join(original_data, results_with_id, by = "row_id")

  if ("性別" %in% colnames(df_final)) {
    df_final <- df_final |>
      dplyr::mutate(
        sex = dplyr::case_when(
          性別 %in% c("男性", "男", "M", "m", "Male") ~ 0,
          性別 %in% c("女性", "女", "F", "f", "Female") ~ 1,
          TRUE ~ NA_real_
        )
      )
  }

  df_final_sorted <- df_final |> dplyr::arrange(rlang::.data$Class)

  if (SHOW_DETAILED_OUTPUT) {
    cat("[info] Cluster distribution in final subset:\n")
    cluster_summary <- df_final_sorted |>
      dplyr::filter(!is.na(rlang::.data$Class)) |>
      dplyr::count(Class = rlang::.data$Class, name = "Count") |>
      dplyr::mutate(
        Percent = round(rlang::.data$Count / sum(rlang::.data$Count) * 100, 2),
        CumPercent = round(cumsum(rlang::.data$Count) / sum(rlang::.data$Count) * 100, 2)
      )
    print(cluster_summary)
    cat("\n")
  }

  df_final_sorted
}

save_final_results <- function(df_final, original_file_path, output_dir, group_name) {
  ensure_output_directory(output_dir)
  base_name <- tools::file_path_sans_ext(basename(original_file_path))

  if (nzchar(OUTPUT_PREFIX)) {
    output_filename <- sprintf("%s_%s_%s_with_clusters_sorted.csv", OUTPUT_PREFIX, base_name, group_name)
  } else {
    output_filename <- sprintf("%s_%s_with_clusters_sorted.csv", base_name, group_name)
  }

  output_path <- file.path(output_dir, output_filename)
  readr::write_csv(df_final, output_path)

  if (SHOW_DETAILED_OUTPUT) {
    cat(sprintf("[info] Final data saved to: %s\n\n", normalizePath(output_path)))
  }

  output_path
}

analyze_age_by_cluster <- function(df_final, age_column, group_label) {
  cat(sprintf("[step] Age summary by cluster for %s...\n", group_label))

  if (!(age_column %in% names(df_final))) {
    cat("[warn] Age column missing; skipping age summary.\n\n")
    return(invisible(NULL))
  }

  available_age_columns <- c(age_column, "参加時年齢", "受信時年齢", "年齢")
  available_age_columns <- unique(available_age_columns[available_age_columns %in% names(df_final)])

  if (!length(available_age_columns)) {
    cat("[warn] No age-related columns found; skipping age summary.\n\n")
    return(invisible(NULL))
  }

  for (age_col in available_age_columns) {
    age_sym <- rlang::sym(age_col)

    stats <- df_final |>
      dplyr::filter(!is.na(rlang::.data$Class), !is.na(!!age_sym)) |>
      dplyr::group_by(Class = rlang::.data$Class) |>
      dplyr::summarise(
        N = dplyr::n(),
        Mean = round(mean(!!age_sym, na.rm = TRUE), 2),
        SD = round(stats::sd(!!age_sym, na.rm = TRUE), 2),
        Min = min(!!age_sym, na.rm = TRUE),
        Max = max(!!age_sym, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(rlang::.data$Class)

    cat(sprintf("[info] Age column '%s' summary:\n", age_col))
    print(stats)
    cat("\n")
  }
}

# ================================================================
# Group orchestration
# ================================================================

process_age_group <- function(data_with_id, selected_columns, file_path, group_config) {
  group_name <- group_config$name
  group_label <- group_config$label
  lower <- group_config$lower
  upper <- group_config$upper
  upper_inclusive <- isTRUE(group_config$upper_inclusive)
  final_clusters <- if (!is.null(group_config$final_clusters)) group_config$final_clusters else DEFAULT_FINAL_CLUSTERS

  cat("==================================================\n")
  cat(sprintf("Processing group: %s (%s)\n", group_label, group_name))
  cat("==================================================\n")

  subset_data <- subset_by_age(data_with_id, AGE_COLUMN, lower, upper, upper_inclusive)

  if (!nrow(subset_data)) {
    cat("[warn] No participants in this group; skipping.\n\n")
    return(NULL)
  }

  prepared <- prepare_lpa_data(subset_data, selected_columns, SCALING_METHOD)
  df_analysis <- prepared$analysis

  lpa_models <- run_lpa_models(df_analysis, PROFILE_RANGE, group_label)
  comparison_table <- create_comparison_table(lpa_models, group_label)

  group_output_dir <- file.path(LPA_OUTPUT_DIR, group_name)
  ensure_output_directory(group_output_dir)
  display_and_save_comparison(comparison_table, group_output_dir, COMPARISON_TABLE_FILENAME)

  chosen_clusters <- select_final_clusters(comparison_table, final_clusters, group_label)
  selected_model <- get_selected_model(lpa_models, chosen_clusters, group_label)

  final_data <- assign_clusters_to_data(prepared$original, prepared$for_lpa, selected_model, AGE_COLUMN)
  output_path <- save_final_results(final_data, file_path, group_output_dir, group_name)
  analyze_age_by_cluster(final_data, AGE_COLUMN, group_label)

  list(
    group = group_name,
    label = group_label,
    comparison_table = comparison_table,
    selected_model = selected_model,
    final_data = final_data,
    output_path = output_path,
    removed = prepared$removed
  )
}

# ================================================================
# Main execution
# ================================================================

main_lpa_flow_by_age <- function() {
  if (SHOW_DETAILED_OUTPUT) {
    cat("[start] LPA age-stratified workflow\n")
    cat(paste(rep("-", 50), collapse = ""), "\n\n")
  }

  setup_packages()
  ensure_output_directory(LPA_OUTPUT_DIR)

  data_info <- load_data()
  data <- data_info$data
  file_path <- data_info$file_path

  data_with_id <- data |> dplyr::mutate(row_id = dplyr::row_number())
  selected_columns <- select_lpa_variables(data_with_id)

  results <- list()

  for (group_config in AGE_GROUPS) {
    group_result <- tryCatch({
      process_age_group(data_with_id, selected_columns, file_path, group_config)
    }, error = function(e) {
      cat(sprintf("[error] Group %s failed: %s\n\n", group_config$label, e$message))
      NULL
    })

    if (!is.null(group_result)) {
      results[[group_config$name]] <- group_result
    }
  }

  if (SHOW_DETAILED_OUTPUT) {
    cat("[done] Completed age-stratified LPA workflow.\n")
    cat(paste(rep("-", 50), collapse = ""), "\n")
  }

  results
}

# Execute workflow when running the script directly
results_by_group <- main_lpa_flow_by_age()

cat("[info] Age-stratified LPA processing finished.\n")
