#!/usr/bin/env Rscript

#################################################################
# PCA Only Script
# - Loads specified CSV and performs PCA on selected columns
# - Adjusts MMSE-style scores for participants aged 64 or younger
# - Outputs a PC1 vs PC2 scatter plot colored by detailed MMSE bins
#################################################################

# ================================================================
# Configuration
# ================================================================

INPUT_FILE <- "saigen.csv"
TARGET_COLUMNS <- c(
	"Memory", "Orientation", "Personal Care",
	"Judgement and Problem Solving", "Community Affairs", "Home and Hobbies"
)
AGE_COLUMN <- "Age"
MMSE_COLUMN <- "Logical Memory"

# Plot output
OUTPUT_PREFIX <- "pca_only"
OUTPUT_PLOT <- paste0(OUTPUT_PREFIX, "_pc1_pc2.png")
PLOT_WIDTH <- 10
PLOT_HEIGHT <- 8
PLOT_DPI <- 300

# PCA options
STANDARDIZE_DATA <- TRUE
SEED <- 42

# MMSE thresholds
AGE_THRESHOLD <- 64
MMSE_FULL_SCORE <- 30

MMSE_CATEGORY_LABELS <- c("Low (≤23)", "Medium (24-27)", "High (≥28)")

MMSE_SCORE_LEVELS <- c("≤23", "24", "25", "26", "27", "28", "29", "30")

MMSE_COLOR_MAP <- c(
	"≤23" = "#d73027",
	"24" = "#fc8d59",
	"25" = "#fee08b",
	"26" = "#d9ef8b",
	"27" = "#91cf60",
	"28" = "#1a9850",
	"29" = "#4575b4",
	"30" = "#313695"
)

# ================================================================
# Libraries
# ================================================================

suppressPackageStartupMessages({
	library(readr)
	library(dplyr)
	library(tidyr)
	library(ggplot2)
	library(glue)
})

# ================================================================
# Helpers
# ================================================================

load_data <- function(path) {
	if (!file.exists(path)) {
		stop(glue("Input file not found: {path}"))
	}
	cat(glue("📁 Reading data: {path}\n"))
	data <- read_csv(path, show_col_types = FALSE)
	cat(glue("✅ Loaded {nrow(data)} rows x {ncol(data)} columns\n"))
	data
}

ensure_columns <- function(data, required_cols) {
	missing <- setdiff(required_cols, names(data))
	if (length(missing)) {
		stop(glue("Required column(s) not found: {paste(missing, collapse = ', ')}"))
	}
}

adjust_mmse_scores <- function(data, mmse_column, age_column) {
	mms_raw <- suppressWarnings(as.numeric(data[[mmse_column]]))
	age_numeric <- suppressWarnings(as.numeric(data[[age_column]]))

	missing_mmse_young <- is.na(mms_raw) & !is.na(age_numeric) & age_numeric <= AGE_THRESHOLD
	if (any(missing_mmse_young, na.rm = TRUE)) {
		cat(glue("ℹ️ Setting MMSE=30 for {sum(missing_mmse_young, na.rm = TRUE)} cases aged ≤ {AGE_THRESHOLD}\n"))
		mms_raw[missing_mmse_young] <- MMSE_FULL_SCORE
	}

	list(
		mmse_numeric = mms_raw,
		age_numeric = age_numeric
	)
}

build_mmse_bins <- function(mmse_values) {
	basic_category <- case_when(
		mmse_values <= 23 ~ MMSE_CATEGORY_LABELS[1],
		mmse_values >= 24 & mmse_values <= 27 ~ MMSE_CATEGORY_LABELS[2],
		mmse_values >= 28 ~ MMSE_CATEGORY_LABELS[3],
		TRUE ~ NA_character_
	)

	detailed_bucket <- case_when(
		mmse_values <= 23 ~ "≤23",
		mmse_values == 24 ~ "24",
		mmse_values == 25 ~ "25",
		mmse_values == 26 ~ "26",
		mmse_values == 27 ~ "27",
		mmse_values == 28 ~ "28",
		mmse_values == 29 ~ "29",
		mmse_values >= 30 ~ "30",
		TRUE ~ NA_character_
	)

	detailed_bucket <- factor(detailed_bucket, levels = MMSE_SCORE_LEVELS)
	basic_category <- factor(basic_category, levels = MMSE_CATEGORY_LABELS)

	tibble(mmse_category = basic_category, mmse_bucket = detailed_bucket)
}

prepare_pca_data <- function(data) {
	ensure_columns(data, c(TARGET_COLUMNS, AGE_COLUMN, MMSE_COLUMN))

	mmse_info <- adjust_mmse_scores(data, MMSE_COLUMN, AGE_COLUMN)
	outcome <- build_mmse_bins(mmse_info$mmse_numeric)

	working <- data %>%
		mutate(
			mmse_numeric = mmse_info$mmse_numeric,
			mmse_category = outcome$mmse_category,
			mmse_bucket = outcome$mmse_bucket
		) %>%
		filter(!is.na(.data$mmse_numeric), !is.na(.data$mmse_bucket))

	if (!nrow(working)) {
		stop("No valid MMSE observations after adjustments.")
	}

	cp_data <- working %>%
		select(all_of(TARGET_COLUMNS)) %>%
		mutate(across(everything(), as.numeric))

	complete_rows <- stats::complete.cases(cp_data)
	if (!all(complete_rows)) {
		dropped <- sum(!complete_rows)
		cat(glue("ℹ️ Dropping {dropped} rows with missing predictor values\n"))
		cp_data <- cp_data[complete_rows, , drop = FALSE]
		working <- working[complete_rows, , drop = FALSE]
	}

	zero_var <- vapply(
		cp_data,
		function(x) {
			std <- stats::sd(x, na.rm = TRUE)
			is.na(std) || isTRUE(all.equal(std, 0))
		},
		logical(1)
	)
	if (any(zero_var)) {
		removed_cols <- names(zero_var)[zero_var]
		cat(glue("ℹ️ Removing {length(removed_cols)} zero-variance column(s): {paste(removed_cols, collapse = ', ')}\n"))
		cp_data <- cp_data[, !zero_var, drop = FALSE]
	}

	if (!nrow(cp_data) || !ncol(cp_data)) {
		stop("No data remaining after cleaning for PCA.")
	}

	if (STANDARDIZE_DATA) {
		pca_input <- scale(cp_data)
	} else {
		pca_input <- as.matrix(cp_data)
	}

	list(
		pca_input = pca_input,
		annotations = working %>% select(dplyr::all_of(c("mmse_numeric", "mmse_category", "mmse_bucket"))),
		row_count = nrow(working)
	)
}

run_pca <- function(matrix_data) {
	set.seed(SEED)
	prcomp(matrix_data, center = FALSE, scale. = FALSE)
}

create_pca_plot <- function(pca_model, annotations) {
	scores <- as.data.frame(pca_model$x[, 1:2, drop = FALSE])
	colnames(scores) <- c("PC1", "PC2")

	explained <- (pca_model$sdev^2) / sum(pca_model$sdev^2)
	axis_labels <- c(
		glue("PC1 ({scales::percent(explained[1], accuracy = 0.1)})"),
		glue("PC2 ({scales::percent(explained[2], accuracy = 0.1)})")
	)

	plot_df <- bind_cols(scores, annotations)

	plot_df <- plot_df %>%
		mutate(
			legend_label = case_when(
				mmse_bucket == "≤23" ~ "≤23 (Low)",
				mmse_bucket %in% c("24", "25", "26", "27") ~ glue("{mmse_bucket} (Medium)"),
				mmse_bucket %in% c("28", "29", "30") ~ glue("{mmse_bucket} (High)"),
				TRUE ~ as.character(mmse_bucket)
			)
		)

	legend_levels <- c(
		"≤23 (Low)", "24 (Medium)", "25 (Medium)", "26 (Medium)", "27 (Medium)",
		"28 (High)", "29 (High)", "30 (High)"
	)

	plot_df$legend_label <- factor(plot_df$legend_label, levels = legend_levels)

	legend_colors <- MMSE_COLOR_MAP[names(MMSE_COLOR_MAP) %in% MMSE_SCORE_LEVELS]
	legend_colors_named <- setNames(legend_colors, legend_levels)

	ggplot(plot_df, aes(x = .data$PC1, y = .data$PC2, color = .data$legend_label)) +
		geom_point(alpha = 0.8, size = 2.6) +
		scale_color_manual(
			values = legend_colors_named,
			name = "MMSE Band"
		) +
		labs(
			title = "PCA Projection Colored by MMSE Bands",
			subtitle = "MMSE scores adjusted to 30 for participants aged ≤ 64 when missing",
			x = axis_labels[1],
			y = axis_labels[2]
		) +
		theme_minimal(base_size = 13) +
		theme(
			plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
			plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#555555"),
			axis.title = element_text(face = "bold"),
			panel.grid.minor = element_blank(),
			legend.title = element_text(face = "bold")
		)
}

save_plot <- function(plot) {
	ggsave(OUTPUT_PLOT, plot, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
	cat(glue("📦 Plot saved to: {normalizePath(OUTPUT_PLOT)}\n"))
}

# ================================================================
# Main
# ================================================================

main <- function() {
	cat("=== PCA Only Script ===\n\n")
	data <- load_data(INPUT_FILE)
	prepared <- prepare_pca_data(data)

	cat(glue("Running PCA on {prepared$row_count} observations\n"))
	pca_model <- run_pca(prepared$pca_input)

	plot <- create_pca_plot(pca_model, prepared$annotations)
	save_plot(plot)

	cat("Done.\n")
}

main()
