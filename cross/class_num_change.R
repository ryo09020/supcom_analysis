## --- Configuration -----------------------------------------------------
input_file <- "raw_data/class_change_validation_dummy.csv"
class_column <- "class"
conversion_map <- c("1" = "3", "2" = "1", "3" = "2")  # old = new
output_dir <- "final_data"
output_filename <- "class_relabelled.csv"

## --- Validation --------------------------------------------------------
if (!file.exists(input_file)) {
	stop(sprintf("Input file not found: %s", input_file))
}

dataset <- read.csv(input_file, check.names = FALSE, stringsAsFactors = FALSE)

if (!class_column %in% names(dataset)) {
	stop(sprintf("Column '%s' not found in input data.", class_column))
}

if (length(conversion_map) == 0) {
	stop("'conversion_map' must contain at least one mapping (old = new).")
}

if (any(is.na(names(conversion_map)) | names(conversion_map) == "")) {
	stop("Each entry in 'conversion_map' must be named with the original class value.")
}

## --- Conversion --------------------------------------------------------
original_column <- dataset[[class_column]]
original_as_char <- as.character(original_column)
converted_as_char <- original_as_char

conversion_summary <- data.frame(
	from = names(conversion_map),
	to = as.character(unname(conversion_map)),
	matched_rows = 0L,
	changed_rows = 0L,
	stringsAsFactors = FALSE
)

for (idx in seq_along(conversion_map)) {
	old_value <- conversion_summary$from[idx]
	new_value <- conversion_summary$to[idx]
	matches <- !is.na(original_as_char) & original_as_char == old_value
	conversion_summary$matched_rows[idx] <- sum(matches)

	if (old_value != new_value) {
		conversion_summary$changed_rows[idx] <- sum(matches)
	}

	converted_as_char[matches] <- new_value
}

changed_rows_total <- sum(original_as_char != converted_as_char, na.rm = TRUE)

## --- Re-type column ----------------------------------------------------
retyped_column <- converted_as_char

if (is.numeric(original_column) || is.integer(original_column)) {
	suppressed <- suppressWarnings(type.convert(converted_as_char, as.is = TRUE))
	retyped_column <- suppressed
	if (is.integer(original_column) && !is.integer(retyped_column)) {
		retyped_column <- as.integer(round(retyped_column))
	}
} else if (is.factor(original_column)) {
	retyped_column <- factor(converted_as_char, levels = sort(unique(converted_as_char)))
}

dataset[[class_column]] <- retyped_column

## --- Output ------------------------------------------------------------
if (!dir.exists(output_dir)) {
	dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

output_path <- file.path(output_dir, output_filename)
write.csv(dataset, output_path, row.names = FALSE)

## --- Console report ----------------------------------------------------
cat(sprintf("Input file   : %s\n", normalizePath(input_file, mustWork = TRUE)))
cat(sprintf("Output file  : %s\n", normalizePath(output_path, mustWork = TRUE)))
cat(sprintf("Rows total   : %d\n", nrow(dataset)))
cat(sprintf("Rows changed : %d\n", changed_rows_total))

if (nrow(conversion_summary) > 0) {
	cat("\nConversion details:\n")
	print(conversion_summary, row.names = FALSE)
}

