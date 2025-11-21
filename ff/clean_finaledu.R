#!/usr/bin/env Rscript

options(stringsAsFactors = FALSE)

DEFAULT_CONFIG <- list(
	input_csv = "path/to/input.csv",
	id_column = "ID列名",
	numeric_columns = c("数値にしたい列1", "数値にしたい列2"),
	output_csv = "path/to/cleaned_output.csv",
	na_strings = c("", "NA", "N/A", "NaN")
)

trim_and_parse_numeric <- function(values) {
	if (is.null(values)) {
		return(numeric(0))
	}

	if (is.numeric(values)) {
		return(values)
	}

	if (is.factor(values)) {
		values <- as.character(values)
	}

	cleaned <- trimws(as.character(values))
	cleaned[cleaned == ""] <- NA_character_
	cleaned <- gsub(",", "", cleaned, fixed = FALSE)

	suppressWarnings(as.numeric(cleaned))
}

`%||%` <- function(lhs, rhs) {
	if (!is.null(lhs) && lhs != "") {
		lhs
	} else {
		rhs
	}
}

validate_config <- function(cfg) {
	required_fields <- c("input_csv", "id_column", "numeric_columns")
	missing_fields <- required_fields[!required_fields %in% names(cfg)]

	if (length(missing_fields) > 0) {
		stop(sprintf(
			"DEFAULT_CONFIG に次のキーを追加してください: %s",
			paste(missing_fields, collapse = ", ")
		))
	}

	cfg$numeric_columns <- as.character(cfg$numeric_columns)
	cfg$numeric_columns <- cfg$numeric_columns[nchar(cfg$numeric_columns) > 0]

	if (length(cfg$numeric_columns) == 0) {
		stop("DEFAULT_CONFIG$numeric_columns に1つ以上の列名を指定してください。")
	}

	cfg$output_csv <- cfg$output_csv %||% NULL
	cfg$na_strings <- cfg$na_strings %||% c("", "NA", "N/A", "NaN")

	cfg
}

coerce_numeric_columns <- function(df, numeric_columns) {
	if (length(numeric_columns) == 0) {
		return(list(data = df, invalid_mask = rep(FALSE, nrow(df))))
	}

	invalid_mask <- rep(FALSE, nrow(df))

	for (column in numeric_columns) {
		numeric_values <- trim_and_parse_numeric(df[[column]])
		newly_invalid <- is.na(numeric_values)
		invalid_mask <- invalid_mask | newly_invalid
		df[[column]] <- numeric_values
	}

	list(data = df, invalid_mask = invalid_mask)
}

report_duplicate_ids <- function(df, id_column) {
	duplicate_counts <- sort(table(df[[id_column]]), decreasing = TRUE)
	duplicate_counts <- duplicate_counts[duplicate_counts > 1]

	if (length(duplicate_counts) == 0) {
		message(sprintf("ID列 '%s' に重複はありません。", id_column))
		return(invisible(NULL))
	}

	message("重複しているIDと件数:")
	for (id_value in names(duplicate_counts)) {
		message(sprintf("  %s: %d 件", id_value, duplicate_counts[[id_value]]))
	}
}

clean_finaledu <- function(input_csv,
													 numeric_columns,
													 id_column,
													 output_csv = NULL,
													 na_strings = c("", "NA", "N/A", "NaN")) {
	if (!file.exists(input_csv)) {
		stop(sprintf("指定されたCSVファイルが見つかりません: %s", input_csv))
	}

	raw_df <- utils::read.csv(
		input_csv,
		na.strings = na_strings,
		strip.white = TRUE,
		check.names = FALSE,
		stringsAsFactors = FALSE
	)

	missing_cols <- setdiff(c(numeric_columns, id_column), names(raw_df))
	if (length(missing_cols) > 0) {
		stop(sprintf("次の列がCSV内に見つかりません: %s", paste(missing_cols, collapse = ", ")))
	}

	conversion <- coerce_numeric_columns(raw_df, numeric_columns)
	converted_df <- conversion$data
	invalid_rows <- conversion$invalid_mask
	dropped_rows <- sum(invalid_rows)

	cleaned_df <- converted_df[!invalid_rows, , drop = FALSE]

	message(sprintf("元の行数: %d", nrow(raw_df)))
	message(sprintf("数値変換で削除された行数: %d", dropped_rows))
	message(sprintf("クリーンデータの行数: %d", nrow(cleaned_df)))

	report_duplicate_ids(cleaned_df, id_column)

	if (!is.null(output_csv)) {
		utils::write.csv(cleaned_df, output_csv, row.names = FALSE, na = "")
		message(sprintf("クリーン済みデータを出力しました: %s", output_csv))
	}

	invisible(cleaned_df)
}

split_columns <- function(column_string) {
	if (is.null(column_string) || column_string == "") {
		return(character(0))
	}

	trimmed <- trimws(unlist(strsplit(column_string, ",")))
	trimmed[nchar(trimmed) > 0]
}

parse_cli_args <- function(args) {
	result <- list()
	i <- 1

	while (i <= length(args)) {
		key <- args[[i]]
		if (!startsWith(key, "--")) {
			stop(sprintf("不正な引数です: %s", key))
		}
		if (i == length(args)) {
			stop(sprintf("引数 %s に値が指定されていません。", key))
		}

		value <- args[[i + 1]]
		result[[substring(key, 3)]] <- value
		i <- i + 2
	}

	result
}

print_usage <- function() {
	message(
		paste(
			"使用方法:",
			"  Rscript clean_finaledu.R --input 入力CSV --id id列 --columns 列1,列2[,列3...] [--output 出力CSV]",
			"",
			"例:",
			"  Rscript clean_finaledu.R --input time1_data.csv --id participant_id --columns math_score,science_score --output cleaned_time1.csv",
			sep = "\n"
		)
	)
}

run_from_config <- function(cfg) {
	validated <- validate_config(cfg)

	clean_finaledu(
		input_csv = validated$input_csv,
		numeric_columns = validated$numeric_columns,
		id_column = validated$id_column,
		output_csv = validated$output_csv,
		na_strings = validated$na_strings
	)
}

if (identical(environment(), globalenv())) {
	args <- commandArgs(trailingOnly = TRUE)

	if (length(args) == 0) {
		message("ファイル先頭の DEFAULT_CONFIG を使って処理を実行します。")
		tryCatch(
			run_from_config(DEFAULT_CONFIG),
			error = function(err) {
				message("設定を使った処理でエラーが発生しました: ", err$message)
				quit(save = "no", status = 1)
			}
		)
	} else {
		parsed <- tryCatch(
			parse_cli_args(args),
			error = function(err) {
				message("引数の解析に失敗しました: ", err$message)
				print_usage()
				quit(save = "no", status = 1)
			}
		)

		required_fields <- c("input", "id", "columns")
		missing_fields <- required_fields[!required_fields %in% names(parsed)]

		if (length(missing_fields) > 0) {
			message(sprintf("必須引数が不足しています: %s", paste(missing_fields, collapse = ", ")))
			print_usage()
			quit(save = "no", status = 1)
		}

		numeric_columns <- split_columns(parsed$columns)

		if (length(numeric_columns) == 0) {
			message("--columns に1つ以上の列名を指定してください。")
			quit(save = "no", status = 1)
		}

		tryCatch(
			clean_finaledu(
				input_csv = parsed$input,
				numeric_columns = numeric_columns,
				id_column = parsed$id,
				output_csv = parsed$output %||% NULL
			),
			error = function(err) {
				message("処理中にエラーが発生しました: ", err$message)
				quit(save = "no", status = 1)
			}
		)
	}
}
