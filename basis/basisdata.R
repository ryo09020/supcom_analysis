library(readr)
library(dplyr)

SexLabel <- MeanAge <- MedianAge <- Count <- NULL

# Quick helper that reports age stats by sex for the supplied dataset
summarize_age_by_sex <- function(csv_file_path, age_col, sex_col) {
	data <- tryCatch({
		read_csv(csv_file_path, col_types = cols(.default = "c")) %>%
			mutate(across(all_of(c(age_col, sex_col)), ~ as.numeric(.x)))
	}, error = function(e) {
		stop(paste0("Failed to load CSV: ", csv_file_path, "\nError: ", e$message))
	})

	if (!age_col %in% names(data)) {
		stop(paste0("Age column not found: ", age_col))
	}
	if (!sex_col %in% names(data)) {
		stop(paste0("Sex column not found: ", sex_col))
	}

	processed_data <- data %>%
		filter(
			!is.na(.data[[age_col]]),
			!is.na(.data[[sex_col]]),
			.data[[sex_col]] %in% c(0, 1)
		)

	if (nrow(processed_data) == 0) {
		stop("No records with valid age and sex (0 or 1) values.")
	}

	stats_by_sex <- processed_data %>%
		mutate(SexLabel = ifelse(.data[[sex_col]] == 0, "Male", "Female")) %>%
		group_by(SexLabel) %>%
		summarise(
			Count = n(),
			MeanAge = mean(.data[[age_col]]),
			MedianAge = median(.data[[age_col]]),
			.groups = "drop"
		) %>%
		mutate(
			MeanAge = round(MeanAge, 2),
			MedianAge = round(MedianAge, 2)
		)

	overall <- processed_data %>%
		summarise(
			Count = n(),
			MeanAge = round(mean(.data[[age_col]]), 2),
			MedianAge = round(median(.data[[age_col]]), 2)
		)

		cat("\n--- Age stats by sex ---\n")
		for (i in seq_len(nrow(stats_by_sex))) {
			cat(sprintf(
				"%s: n = %s, mean = %.2f, median = %.2f\n",
				stats_by_sex$SexLabel[i], stats_by_sex$Count[i], stats_by_sex$MeanAge[i], stats_by_sex$MedianAge[i]
			))
		}

	cat("\n--- Overall age stats ---\n")
	cat(sprintf(
		"n = %s, mean = %.2f, median = %.2f\n\n",
		as.integer(overall$Count), overall$MeanAge, overall$MedianAge
	))

	invisible(list(by_sex = stats_by_sex, overall = overall))
}

cat("Starting age summary...\n")
tryCatch({
	summarize_age_by_sex(
		csv_file_path = "time1_data.csv",
		age_col = "age",
		sex_col = "sex"
	)
}, error = function(e) {
	cat("Encountered an error while summarising ages:\n", e$message, "\n", sep = "")
})
