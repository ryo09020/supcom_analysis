#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(readr)
	library(dplyr)
	library(ggplot2)
	library(glue)
})

# ================================================================
# Configuration
# ================================================================

INPUT_FILE <- "../lpa/dummy_data_with_clusters_sorted.csv"
CLASS_COLUMN <- "Class"
MMSE_COLUMNS <- c("age")
OUTPUT_PLOT <- "mmse_boxplot_by_class.png"

POINT_ALPHA <- 0.65
POINT_SIZE <- 2.2
POINT_WIDTH <- 0.18

# ================================================================
# Helpers
# ================================================================

load_source_data <- function(path) {
	if (!file.exists(path)) {
		stop(glue("Input file not found: {path}"))
	}

	cat(glue("📁 Reading data: {path}\n"))
	data <- read_csv(path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
	cat(glue("✅ Loaded {nrow(data)} rows x {ncol(data)} columns\n"))
	cat("Columns:\n")
	print(names(data))
	cat("\n")

	data
}

choose_mmse_column <- function(data, candidate_columns) {
	available <- intersect(candidate_columns, names(data))
	if (length(available) == 0) {
		stop(glue(
			"None of the specified MMSE columns were found. Candidates: {paste(candidate_columns, collapse = ', ')}"
		))
	}
	available[[1]]
}

prepare_mmse_dataset <- function(data, class_column, mmse_candidates) {
	if (!(class_column %in% names(data))) {
		stop(glue("Required class column not found: {class_column}"))
	}

	mmse_column <- choose_mmse_column(data, mmse_candidates)
	cat(glue("Using MMSE column: {mmse_column}\n"))

	prepared <- data %>%
		mutate(
			class_factor = as.factor(.data[[class_column]]),
			mmse_numeric = suppressWarnings(readr::parse_number(as.character(.data[[mmse_column]])))
		) %>%
		filter(!is.na(.data$mmse_numeric))

	if (!nrow(prepared)) {
		stop("No valid MMSE values found after filtering.")
	}

	cat(glue("Remaining rows with observed MMSE: {nrow(prepared)}\n"))

	counts <- prepared %>%
		count(.data$class_factor, name = "n") %>%
		arrange(.data$class_factor)

	cat("Counts per class:\n")
	print(counts)
	cat("\n")

	list(data = prepared, counts = counts)
}

build_mmse_boxplot <- function(prepared_data, counts) {
	y_range <- range(prepared_data$mmse_numeric, na.rm = TRUE)
	padding <- if (diff(y_range) == 0) 1 else diff(y_range) * 0.08
	y_upper <- y_range[2] + padding

	counts_plot <- counts %>%
		mutate(label = glue("n = {n}"))

	ggplot(prepared_data, aes(x = .data$class_factor, y = .data$mmse_numeric)) +
		geom_boxplot(fill = "#4C78A8", alpha = 0.65, color = "#2F3B52", outlier.alpha = 0.35) +
		geom_jitter(width = POINT_WIDTH, size = POINT_SIZE, alpha = POINT_ALPHA, color = "#7A5195") +
		geom_text(
			data = counts_plot,
			aes(x = .data$class_factor, y = y_upper, label = .data$label),
			inherit.aes = FALSE,
			fontface = "bold",
			vjust = -0.1,
			color = "#333333"
		) +
		scale_y_continuous(expand = expansion(mult = c(0.02, 0.12))) +
		labs(
			title = "MMSE Distribution by Class",
			subtitle = "Only participants with observed MMSE scores",
			x = "Class",
			y = "MMSE Score"
		) +
		theme_minimal(base_size = 13) +
		theme(
			plot.title = element_text(face = "bold", hjust = 0.5),
			plot.subtitle = element_text(hjust = 0.5, color = "#555555"),
			axis.title = element_text(face = "bold"),
			panel.grid.major.x = element_blank(),
			plot.margin = margin(15, 20, 15, 15)
		) +
		coord_cartesian(clip = "off")
}

save_plot <- function(plot, path) {
	ggsave(path, plot, width = 8, height = 6, dpi = 300)
	cat(glue("📦 Plot saved to: {normalizePath(path)}\n"))
}

# ================================================================
# Main
# ================================================================

cat("=== MMSE Boxplot Generator ===\n\n")

source_data <- load_source_data(INPUT_FILE)
prepared <- prepare_mmse_dataset(source_data, CLASS_COLUMN, MMSE_COLUMNS)

plot_object <- build_mmse_boxplot(prepared$data, prepared$counts)
save_plot(plot_object, OUTPUT_PLOT)

cat("Done.\n")
