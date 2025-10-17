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

# スケーリング設定
# options: "none", "minmax", "minmax_trimmed", "minmax_metadata", "zscore", "robust_zscore"
SCALING_METHOD <- "minmax"

# トリミング確率 (minmax_trimmed 用)
TRIM_PROBS <- c(0.05, 0.95)

# 列ごとの理論レンジを記載したCSV (列: column, min, max)
RANGE_METADATA_FILE <- NULL

# 生データの分散も併記するか
INCLUDE_RAW_STATS <- TRUE

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

# レンジメタデータ読み込み -------------------------------------
load_range_metadata <- function(file_path, columns) {
  if (is.null(file_path)) {
    return(NULL)
  }

  if (!file.exists(file_path)) {
    stop("レンジメタデータファイルが見つかりません: ", file_path)
  }

  meta <- readr::read_csv(file_path, show_col_types = FALSE)
  required_cols <- c("column", "min", "max")
  missing_meta_cols <- required_cols[!(required_cols %in% colnames(meta))]
  if (length(missing_meta_cols) > 0) {
    stop("レンジメタデータに必要な列が不足しています: ", paste(missing_meta_cols, collapse = ", "))
  }

  meta <- meta |>
    dplyr::filter(.data$column %in% columns)

  missing_columns <- columns[!(columns %in% meta$column)]
  if (length(missing_columns) > 0) {
    warning("レンジメタデータに存在しない列があります: ", paste(missing_columns, collapse = ", "))
  }

  meta
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

# スケーリング処理 ---------------------------------------------
apply_scaling <- function(data, method, range_meta = NULL, trim_probs = c(0.05, 0.95)) {
  method <- tolower(method)

  if (method %in% c("none", "raw", "")) {
    return(data)
  }

  scaled <- data

  for (col_name in colnames(data)) {
    values <- data[[col_name]]

    if (all(is.na(values))) {
      scaled[[col_name]] <- NA_real_
      next
    }

    if (method == "minmax") {
      min_val <- min(values, na.rm = TRUE)
      max_val <- max(values, na.rm = TRUE)

      if (isTRUE(all.equal(min_val, max_val))) {
        scaled[[col_name]] <- 0
      } else {
        scaled[[col_name]] <- (values - min_val) / (max_val - min_val)
      }

    } else if (method == "minmax_trimmed") {
      lower <- stats::quantile(values, probs = trim_probs[1], na.rm = TRUE, type = 7)
      upper <- stats::quantile(values, probs = trim_probs[2], na.rm = TRUE, type = 7)

      if (!is.finite(lower) || !is.finite(upper) || isTRUE(all.equal(lower, upper))) {
        scaled[[col_name]] <- 0
      } else {
        trimmed <- pmax(pmin(values, upper), lower)
        scaled[[col_name]] <- (trimmed - lower) / (upper - lower)
      }

    } else if (method == "minmax_metadata") {
      if (is.null(range_meta)) {
        stop("SCALING_METHOD = 'minmax_metadata' には RANGE_METADATA_FILE が必要です。")
      }

      row_meta <- dplyr::filter(range_meta, .data$column == col_name)
      if (nrow(row_meta) == 0) {
        warning(col_name, ": レンジメタデータが存在しません。NAを返します。")
        scaled[[col_name]] <- NA_real_
        next
      }

      min_val <- row_meta$min[[1]]
      max_val <- row_meta$max[[1]]

      if (!is.finite(min_val) || !is.finite(max_val) || isTRUE(all.equal(min_val, max_val))) {
        warning(col_name, ": レンジメタデータの値が不適切です。NAを返します。")
        scaled[[col_name]] <- NA_real_
      } else {
        scaled[[col_name]] <- (values - min_val) / (max_val - min_val)
      }

    } else if (method == "zscore") {
      mu <- mean(values, na.rm = TRUE)
      sigma <- stats::sd(values, na.rm = TRUE)

      if (!is.finite(sigma) || isTRUE(all.equal(sigma, 0))) {
        scaled[[col_name]] <- 0
      } else {
        scaled[[col_name]] <- (values - mu) / sigma
      }

    } else if (method == "robust_zscore") {
      med <- stats::median(values, na.rm = TRUE)
      mad_val <- stats::mad(values, center = med, constant = 1.4826, na.rm = TRUE)

      if (!is.finite(mad_val) || isTRUE(all.equal(mad_val, 0))) {
        scaled[[col_name]] <- 0
      } else {
        scaled[[col_name]] <- (values - med) / mad_val
      }

    } else {
      stop("未対応のスケーリング方法です: ", method)
    }
  }

  scaled
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
  range_meta <- load_range_metadata(RANGE_METADATA_FILE, TARGET_COLUMNS)

  analysis_data <- dplyr::select(data, dplyr::all_of(TARGET_COLUMNS))
  numeric_data <- ensure_numeric(analysis_data)

  scaled_data <- apply_scaling(numeric_data, SCALING_METHOD, range_meta, TRIM_PROBS)

  variance_stats <- calculate_variances(scaled_data, TARGET_COLUMNS)
  variance_stats <- variance_stats |>
    dplyr::filter(!is.na(.data$variance)) |>
    dplyr::rename(scaled_variance = .data$variance)

  if (INCLUDE_RAW_STATS && tolower(SCALING_METHOD) != "none") {
    raw_stats <- calculate_variances(numeric_data, TARGET_COLUMNS) |>
      dplyr::filter(!is.na(.data$variance)) |>
      dplyr::rename(raw_variance = .data$variance)

    variance_stats <- dplyr::left_join(variance_stats, raw_stats, by = "column")
  }

  top_stats <- get_top_n(variance_stats, TOP_N)
  output_results(top_stats, OUTPUT_FILE)

  cat("\n=== 処理完了 ===\n")
  cat("スケーリング方法:", SCALING_METHOD, "\n")
}

# 実行 -----------------------------------------------------------
main()
