#!/usr/bin/env Rscript

# Generalized Additive Model (GAM) plotting utility
# -----------------------------------------------
# 1. Update the CONFIG list below with your dataset and options.
# 2. Run this script (e.g., Rscript PCA/gan.R) to generate:
#    - A scatter plot with an overlaid GAM smooth
#    - A plot showing the smooth effect s(x) with confidence interval
#    - (Optional) A text summary of the GAM fit

# ==== Configuration ==========================================================
CONFIG <- list(
	data_path = "sample_gam.csv",
	x_col = "age",
	y_col = "extraversion",
	output_dir = "outputs",
	output_prefix = NULL,          # NULL -> derived automatically from the data file name
	smooth_k = 10L,                # Basis dimension for s(x)
	family = gaussian(),           # Can also be character string, e.g. "gaussian"
	scatter_alpha = 0.6,           # Point transparency (0-1)
	plot_width = 7,
	plot_height = 5,
	predict_points = 200L,         # Number of points used to draw the smooth effect curve
	ci_multiplier = 2,             # Width of the confidence band (± multiplier * SE)
	save_summary = TRUE            # Write GAM summary text file
)

# Optional: override configuration without editing the file by defining
# CONFIG_OVERRIDE <- list(...) *before* sourcing / running this script.
if (exists("CONFIG_OVERRIDE", inherits = FALSE)) {
	CONFIG <- utils::modifyList(CONFIG, CONFIG_OVERRIDE)
}

# ==== Dependencies ===========================================================
suppressPackageStartupMessages({
	required_packages <- c("readr", "dplyr", "ggplot2", "mgcv")
	missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
	if (length(missing_packages) > 0) {
		stop(
			sprintf(
				"Missing packages: %s. Please install them, e.g. install.packages(c(%s)).",
				paste(missing_packages, collapse = ", "),
				paste(sprintf('\"%s\"', missing_packages), collapse = ", ")
			),
			call. = FALSE
		)
	}

	lapply(required_packages, library, character.only = TRUE)
})

cfg <- CONFIG

if (!is.character(cfg$data_path) || length(cfg$data_path) != 1 || cfg$data_path == "") {
	stop("CONFIG$data_path must be a non-empty string.", call. = FALSE)
}

if (!file.exists(cfg$data_path)) {
	stop(sprintf("Data file not found: %s", cfg$data_path), call. = FALSE)
}

cfg$data_path <- normalizePath(cfg$data_path)

if (!dir.exists(cfg$output_dir)) {
	dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
}

if (is.null(cfg$output_prefix) || cfg$output_prefix == "") {
	cfg$output_prefix <- tools::file_path_sans_ext(basename(cfg$data_path))
}

if (!is.numeric(cfg$predict_points) || length(cfg$predict_points) != 1 || cfg$predict_points < 10) {
	stop("CONFIG$predict_points should be a single integer >= 10.", call. = FALSE)
}

if (!is.numeric(cfg$ci_multiplier) || length(cfg$ci_multiplier) != 1 || cfg$ci_multiplier <= 0) {
	stop("CONFIG$ci_multiplier should be a positive number.", call. = FALSE)
}

message("Reading data from ", cfg$data_path)
data <- readr::read_csv(cfg$data_path, show_col_types = FALSE)

if (!cfg$x_col %in% names(data)) {
	stop(sprintf("Column '%s' not found in the dataset.", cfg$x_col), call. = FALSE)
}
if (!cfg$y_col %in% names(data)) {
	stop(sprintf("Column '%s' not found in the dataset.", cfg$y_col), call. = FALSE)
}

selected <- data |>
	dplyr::select(dplyr::all_of(c(cfg$x_col, cfg$y_col)))

selected <- selected[stats::complete.cases(selected), , drop = FALSE]

if (nrow(selected) < 10) {
	warning("Fewer than 10 complete observations after removing NAs. GAM fit may be unreliable.")
}

message(sprintf("Fitting GAM: %s ~ s(%s, k = %d)", cfg$y_col, cfg$x_col, cfg$smooth_k))
formula_text <- sprintf("%s ~ s(%s, k = %d)", cfg$y_col, cfg$x_col, cfg$smooth_k)

family_obj <- if (is.character(cfg$family)) {
	fam_fun <- tryCatch(match.fun(cfg$family), error = function(e) NULL)
	if (is.null(fam_fun)) {
		stop(sprintf("Could not find family function named '%s'.", cfg$family), call. = FALSE)
	}
	fam <- fam_fun()
	if (!inherits(fam, "family")) {
		stop(sprintf("Function '%s' did not return a valid family object.", cfg$family), call. = FALSE)
	}
	fam
} else if (inherits(cfg$family, "family")) {
	cfg$family
} else {
	stop("CONFIG$family must be a family object or the name of a family function.", call. = FALSE)
}

gam_model <- mgcv::gam(
	formula = stats::as.formula(formula_text),
	data = selected,
	family = family_obj,
	method = "REML"
)

# Step 1: Scatter plot with optional GAM smoother overlay
scatter_plot <- ggplot2::ggplot(
 selected,
 ggplot2::aes(x = .data[[cfg$x_col]], y = .data[[cfg$y_col]])
) +
 ggplot2::geom_point(alpha = cfg$scatter_alpha, color = "#3182bd") +
	ggplot2::labs(
		title = sprintf("Scatter: %s vs %s", cfg$y_col, cfg$x_col),
		x = cfg$x_col,
		y = cfg$y_col
	) +
	ggplot2::theme_minimal(base_size = 13) +
	ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
	ggplot2::geom_smooth(
		method = "gam",
		formula = stats::as.formula(paste("y ~ s(x, k =", cfg$smooth_k, ")")),
		se = TRUE,
		color = "#08519c",
		fill = "#bdd7e7"
	)

scatter_path <- file.path(cfg$output_dir, sprintf("%s_scatter.png", cfg$output_prefix))
ggplot2::ggsave(scatter_path, scatter_plot, width = cfg$plot_width, height = cfg$plot_height)
message("Saved scatter plot to ", scatter_path)

# Step 2: Smooth effect plot (centered partial effect of the smooth term)
x_values <- seq(
	from = min(selected[[cfg$x_col]], na.rm = TRUE),
	to = max(selected[[cfg$x_col]], na.rm = TRUE),
	length.out = cfg$predict_points
)

newdata <- stats::setNames(data.frame(x_values), cfg$x_col)

pred_terms <- predict(gam_model, newdata = newdata, type = "terms", se.fit = TRUE)

term_names <- colnames(pred_terms$fit)
smooth_term <- grep(sprintf("^s\\(%s\\)$", cfg$x_col), term_names, value = TRUE)
if (length(smooth_term) == 0) {
	smooth_term <- term_names[1]
	warning("Could not match smooth term name explicitly; using the first available term.")
}

effect <- pred_terms$fit[, smooth_term]
se_effect <- pred_terms$se.fit[, smooth_term]

effect_df <- data.frame(
	value = x_values,
	effect = effect,
	lower = effect - cfg$ci_multiplier * se_effect,
	upper = effect + cfg$ci_multiplier * se_effect
)
names(effect_df)[names(effect_df) == "value"] <- cfg$x_col

effect_plot <- ggplot2::ggplot(
 effect_df,
 ggplot2::aes(x = .data[[cfg$x_col]], y = .data[["effect"]])
) +
 ggplot2::geom_ribbon(
	 ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
	 fill = "#c6dbef",
	 alpha = 0.6
 ) +
	ggplot2::geom_line(color = "#08306b", linewidth = 1) +
	ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#636363") +
	ggplot2::labs(
		title = sprintf("GAM smooth effect: s(%s)", cfg$x_col),
		x = cfg$x_col,
		y = sprintf("Effect on %s", cfg$y_col),
		caption = sprintf("Shaded area: ±%s × standard error", cfg$ci_multiplier)
	) +
	ggplot2::theme_minimal(base_size = 13) +
	ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))

effect_path <- file.path(cfg$output_dir, sprintf("%s_gam_effect.png", cfg$output_prefix))
ggplot2::ggsave(effect_path, effect_plot, width = cfg$plot_width, height = cfg$plot_height)
message("Saved GAM effect plot to ", effect_path)

if (isTRUE(cfg$save_summary)) {
	summary_path <- file.path(cfg$output_dir, sprintf("%s_gam_summary.txt", cfg$output_prefix))
	capture.output(
		{
			cat("GAM formula:\n")
			print(formula(gam_model))
			cat("\n")
			print(summary(gam_model))
		},
		file = summary_path
	)
	message("Saved GAM summary to ", summary_path)
}

message("Done.")

