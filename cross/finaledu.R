#!/usr/bin/env Rscript

suppressPackageStartupMessages({
	library(dplyr)
	library(readr)
})

invisible(utils::globalVariables(c("final_education", "final_edu_int")))

# ================================================================
# 設定値
# ================================================================

# デフォルトの入力ファイルパス（コマンドライン引数で上書き可能）
INPUT_FILE <- "raw_data/dummy_data.csv"  # 分析したいCSVファイルのパス


# 出力ファイル名に付与するサフィックス
OUTPUT_SUFFIX <- "_intedu"

# ---------------------------------------------------------------
# final_education の値を学歴年数に変換
# ---------------------------------------------------------------
map_final_education <- function(values) {
	codes <- suppressWarnings(as.integer(values))
	result <- rep(NA_real_, length(codes))

	result[codes == 1L] <- 9
	result[codes == 2L] <- 12
	result[codes == 3L] <- 14
	result[codes == 4L] <- 14
	result[codes == 5L] <- 16
	result[codes == 6L] <- 18
	result[codes == 7L] <- NA_real_

	result
}

# ---------------------------------------------------------------
# メイン処理
# ---------------------------------------------------------------
process_final_education <- function(input_path) {
	if (!file.exists(input_path)) {
		stop(sprintf("指定したファイルが見つかりません: %s", input_path))
	}

	data <- read_csv(input_path, show_col_types = FALSE)

	if (!"final_education" %in% colnames(data)) {
		stop("入力データに 'final_education' 列が見つかりません。")
	}

	transformed <- data %>%
		mutate(final_edu_int = map_final_education(.data$final_education))

	output_path <- file.path(
		dirname(input_path),
		paste0(tools::file_path_sans_ext(basename(input_path)), OUTPUT_SUFFIX, ".csv")
	)

	write_csv(transformed, output_path)

	cat("✅ final_education の変換が完了しました。\n")
	cat(sprintf("   入力:  %s\n", normalizePath(input_path)))
	cat(sprintf("   出力:  %s\n", normalizePath(output_path)))

		summary_table <- transformed %>%
			group_by(.data$final_education, .data$final_edu_int) %>%
			summarise(count = dplyr::n(), .groups = "drop") %>%
			arrange(.data$final_education)

	cat("\n--- final_education と final_edu_int の対応分布 ---\n")
	print(summary_table, n = nrow(summary_table))

	invisible(output_path)
}

# ---------------------------------------------------------------
# 実行フロー
# ---------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)

input_file <- if (length(args) >= 1) args[[1]] else INPUT_FILE

process_final_education(input_file)
