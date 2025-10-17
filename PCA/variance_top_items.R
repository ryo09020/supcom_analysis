#################################################################
# 分散上位項目抽出スクリプト
# 指定したファイルと列リストを基に、分散が大きい上位項目を出力
#################################################################

# ================================================================
# 設定変数（必要に応じて書き換えてください）
# ================================================================

# 入力ファイル（例: "raw_data/dummy_data.csv"）
INPUT_FILE <- "raw_data/dummy_data.csv"

# 分析対象となる列名ベクトル（例: c("item1", "item2", ... )）
TARGET_COLUMNS <- c(
  # "542690_00",
  # "542700_00"
)

# 出力する上位件数
TOP_N <- 10

# 結果を書き出すCSVファイル名（NULLの場合は保存しない）
OUTPUT_FILE <- NULL  # 例: "variance_top10.csv"

# ================================================================
# パッケージ読み込み
# ================================================================

suppressMessages({
  suppressWarnings({
    library(readr)
  library(dplyr)
  library(tibble)
  })
})

# ================================================================
# 関数定義
# ================================================================

# データ読み込み -------------------------------------------------
load_data <- function(file_path) {
  if (!file.exists(file_path)) {
    stop("指定されたファイルが見つかりません: ", file_path)
  }
  cat("データ読み込み中:", basename(file_path), "\n")
  data <- readr::read_csv(file_path, show_col_types = FALSE)
  cat("データサイズ:", nrow(data), "行", ncol(data), "列\n")
  data
}

# 列存在確認 -----------------------------------------------------
validate_columns <- function(data, columns) {
  missing_cols <- columns[!(columns %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop("指定された列が見つかりません: ", paste(missing_cols, collapse = ", "))
  }
}

# 数値への変換と警告 ---------------------------------------------
ensure_numeric <- function(data) {
  for (col_name in colnames(data)) {
    original_na <- sum(is.na(data[[col_name]]))
    numeric_col <- suppressWarnings(as.numeric(data[[col_name]]))
    coerced_na <- sum(is.na(numeric_col)) - original_na
    if (coerced_na > 0) {
      warning(col_name, ": 数値変換で", coerced_na, "件のNAが新たに発生しました。")
    }
    data[[col_name]] <- numeric_col
  }
  data
}

# 分散計算 -------------------------------------------------------
calculate_variances <- function(data, columns) {
  variance_values <- sapply(columns, function(col) stats::var(data[[col]], na.rm = TRUE))
  non_missing_counts <- sapply(columns, function(col) sum(!is.na(data[[col]])))
  missing_counts <- sapply(columns, function(col) sum(is.na(data[[col]])))

  stats <- tibble(
    column = columns,
    variance = as.numeric(variance_values),
    non_missing = as.integer(non_missing_counts),
    missing = as.integer(missing_counts)
  )

  stats <- stats |>
    arrange(desc(.data$variance))

  stats
}

# 上位N件を取得 ---------------------------------------------------
get_top_n <- function(stats_tbl, top_n) {
  usable_n <- min(top_n, nrow(stats_tbl))
  if (usable_n == 0) {
    stop("有効な列がありません (variance = NA)。")
  }
  dplyr::slice_head(stats_tbl, n = usable_n)
}

# 結果出力 -------------------------------------------------------
output_results <- function(top_stats, output_file = NULL) {
  cat("\n=== 分散上位項目 ===\n")
  print(top_stats, n = nrow(top_stats))

  if (!is.null(output_file)) {
    readr::write_csv(top_stats, output_file)
    cat("\n結果を保存しました:", output_file, "\n")
  }
}

# ================================================================
# メイン処理
# ================================================================

main <- function() {
  cat("=== 分散上位項目抽出 ===\n")

  if (length(TARGET_COLUMNS) == 0) {
    stop("TARGET_COLUMNS に1つ以上の列名を指定してください。")
  }

  data <- load_data(INPUT_FILE)
  validate_columns(data, TARGET_COLUMNS)

  analysis_data <- dplyr::select(data, dplyr::all_of(TARGET_COLUMNS))
  numeric_data <- ensure_numeric(analysis_data)

  variance_stats <- calculate_variances(numeric_data, TARGET_COLUMNS)
  variance_stats <- variance_stats |> dplyr::filter(!is.na(.data$variance))

  top_stats <- get_top_n(variance_stats, TOP_N)
  output_results(top_stats, OUTPUT_FILE)

  cat("\n=== 処理完了 ===\n")
}

# 実行 -----------------------------------------------------------
main()
