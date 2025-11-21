#!/usr/bin/env Rscript

# ===== 設定ブロック =====
COMBINE_CONFIG <- list(
	csv1 = "path/to/first.csv",      # 1つ目のCSVファイルパス
	csv2 = "path/to/second.csv",     # 2つ目のCSVファイルパス
	id_column = "ID列名",            # 両方のCSVで共通のID列名
	columns_from_csv2 = c("付加したい列名1"), # 2つ目CSVから付加したい列名（複数可）
	output_csv = "path/to/combined.csv",      # 出力先CSVファイルパス
	na_strings = c("", "NA", "N/A", "NaN")
)

validate_combine_config <- function(cfg) {
	required <- c("csv1", "csv2", "id_column", "columns_from_csv2")
	missing <- required[!required %in% names(cfg)]
	if (length(missing) > 0) {
		stop(sprintf("COMBINE_CONFIG に次のキーを追加してください: %s", paste(missing, collapse = ", ")))
	}
	cfg$columns_from_csv2 <- as.character(cfg$columns_from_csv2)
	if (length(cfg$columns_from_csv2) == 0) {
		stop("columns_from_csv2 に1つ以上の列名を指定してください。")
	}
	cfg$output_csv <- cfg$output_csv %||% NULL
	cfg$na_strings <- cfg$na_strings %||% c("", "NA", "N/A", "NaN")
	cfg
}

`%||%` <- function(lhs, rhs) {
	if (!is.null(lhs) && lhs != "") lhs else rhs
}

combine_csv_columns <- function(csv1, csv2, id_column, columns_from_csv2, output_csv = NULL, na_strings = c("", "NA", "N/A", "NaN")) {
	if (!file.exists(csv1)) stop(sprintf("1つ目のCSVが見つかりません: %s", csv1))
	if (!file.exists(csv2)) stop(sprintf("2つ目のCSVが見つかりません: %s", csv2))

	df1 <- utils::read.csv(csv1, na.strings = na_strings, check.names = FALSE, stringsAsFactors = FALSE)
	df2 <- utils::read.csv(csv2, na.strings = na_strings, check.names = FALSE, stringsAsFactors = FALSE)

	if (!(id_column %in% names(df1))) stop(sprintf("1つ目のCSVにID列 '%s' がありません。", id_column))
	if (!(id_column %in% names(df2))) stop(sprintf("2つ目のCSVにID列 '%s' がありません。", id_column))

	missing_cols <- setdiff(columns_from_csv2, names(df2))
	if (length(missing_cols) > 0) stop(sprintf("2つ目のCSVに次の列がありません: %s", paste(missing_cols, collapse = ", ")))

	# 必要な列だけ抽出
	df2_sub <- df2[, c(id_column, columns_from_csv2), drop = FALSE]

	# IDでマージ
	merged <- merge(df1, df2_sub, by = id_column, all.x = TRUE, sort = FALSE)

	if (!is.null(output_csv)) {
		utils::write.csv(merged, output_csv, row.names = FALSE, na = "")
		message(sprintf("マージ結果を出力しました: %s", output_csv))
	}
	invisible(merged)
}

run_combine_from_config <- function(cfg) {
	validated <- validate_combine_config(cfg)
	combine_csv_columns(
		csv1 = validated$csv1,
		csv2 = validated$csv2,
		id_column = validated$id_column,
		columns_from_csv2 = validated$columns_from_csv2,
		output_csv = validated$output_csv,
		na_strings = validated$na_strings
	)
}

if (identical(environment(), globalenv())) {
	message("ファイル先頭の COMBINE_CONFIG を使って処理を実行します。")
	tryCatch(
		run_combine_from_config(COMBINE_CONFIG),
		error = function(err) {
			message("設定を使った処理でエラーが発生しました: ", err$message)
			quit(save = "no", status = 1)
		}
	)
}
