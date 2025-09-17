# ID重複チェックスクリプト
# 指定したカラムが重複していないかを確認し、重複レコードがあれば詳細情報を出力する

# ============================================================================
# 設定セクション - ここを編集してください
# ============================================================================

# 入力ファイルの設定
INPUT_FILE <- "dummy_data.csv"  # チェックしたいCSVファイルのパスを指定

# チェック対象カラムの設定
TARGET_COLUMN <- "id"  # 重複をチェックしたいカラム名を指定

# 出力設定
SHOW_DUPLICATE_RECORDS <- TRUE  # 重複レコードの詳細を表示するか
EXPORT_DUPLICATES <- FALSE  # 重複レコードを別ファイルに出力するか
OUTPUT_PREFIX <- "duplicate_records"  # 重複レコード出力ファイルのプレフィックス

# エンコーディング設定
FILE_ENCODING <- "UTF-8"  # ファイルの文字エンコーディング

# ============================================================================
# 以下は関数定義 - 通常は変更不要
# ============================================================================

# 必要なライブラリを読み込み
library(dplyr)
library(readr)

# ID重複チェック関数
check_id_duplicates <- function(input_file, target_column, encoding = "UTF-8") {
  # ファイル読み込み
  cat("ファイル読み込み中:", input_file, "\n")
  
  if (!file.exists(input_file)) {
    cat("エラー: ファイルが見つかりません:", input_file, "\n")
    return(NULL)
  }
  
  data <- read_csv(input_file, locale = locale(encoding = encoding))
  
  # カラムの存在確認
  if (!target_column %in% colnames(data)) {
    cat("エラー: カラム '", target_column, "' がデータに存在しません。\n")
    cat("利用可能なカラム:", paste(colnames(data), collapse = ", "), "\n")
    return(NULL)
  }
  
  total_rows <- nrow(data)
  cat("総レコード数:", total_rows, "\n")
  cat("チェック対象カラム:", target_column, "\n")
  
  # NA値の確認
  na_count <- sum(is.na(data[[target_column]]))
  if (na_count > 0) {
    cat("警告: NA値が", na_count, "個見つかりました\n")
  }
  
  # 空文字の確認
  empty_count <- sum(data[[target_column]] == "", na.rm = TRUE)
  if (empty_count > 0) {
    cat("警告: 空文字が", empty_count, "個見つかりました\n")
  }
  
  # 重複チェック
  duplicate_values <- data[[target_column]][duplicated(data[[target_column]]) | duplicated(data[[target_column]], fromLast = TRUE)]
  unique_duplicates <- unique(duplicate_values)
  unique_duplicates <- unique_duplicates[!is.na(unique_duplicates)]  # NAを除く
  
  duplicate_count <- length(unique_duplicates)
  duplicate_records <- sum(duplicated(data[[target_column]]) | duplicated(data[[target_column]], fromLast = TRUE))
  
  # 結果の表示
  cat("\n=== 重複チェック結果 ===\n")
  if (duplicate_count == 0) {
    cat("✅ 重複なし: すべての値がユニークです！\n")
    cat("ユニーク値数:", length(unique(data[[target_column]], na.rm = TRUE)), "\n")
  } else {
    cat("❌ 重複あり:", duplicate_count, "個の値に重複があります\n")
    cat("重複している値の数:", duplicate_count, "\n")
    cat("重複レコードの総数:", duplicate_records, "\n")
    cat("重複率:", round((duplicate_records / total_rows) * 100, 2), "%\n")
    
    # 重複値の詳細表示
    cat("\n重複している値:\n")
    for (dup_val in unique_duplicates) {
      count <- sum(data[[target_column]] == dup_val, na.rm = TRUE)
      cat("  - '", dup_val, "' : ", count, "回出現\n")
    }
  }
  
  # 重複レコードの詳細情報を取得
  duplicate_rows <- NULL
  if (duplicate_count > 0) {
    duplicate_rows <- data[data[[target_column]] %in% unique_duplicates, ]
    duplicate_rows <- duplicate_rows[order(duplicate_rows[[target_column]]), ]
  }
  
  # 結果を返す
  return(list(
    total_rows = total_rows,
    unique_count = length(unique(data[[target_column]], na.rm = TRUE)),
    duplicate_count = duplicate_count,
    duplicate_records = duplicate_records,
    duplicate_rate = (duplicate_records / total_rows) * 100,
    na_count = na_count,
    empty_count = empty_count,
    duplicate_values = unique_duplicates,
    duplicate_rows_data = duplicate_rows,
    is_unique = duplicate_count == 0
  ))
}

# 重複レコード表示関数
show_duplicate_details <- function(result) {
  if (!result$is_unique && SHOW_DUPLICATE_RECORDS) {
    cat("\n=== 重複レコードの詳細 ===\n")
    if (!is.null(result$duplicate_rows_data) && nrow(result$duplicate_rows_data) > 0) {
      print(result$duplicate_rows_data)
    }
  }
}

# 重複レコード出力関数
export_duplicate_records <- function(result, input_file) {
  if (!result$is_unique && EXPORT_DUPLICATES && !is.null(result$duplicate_rows_data)) {
    file_base <- tools::file_path_sans_ext(basename(input_file))
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- paste0(file_base, "_", OUTPUT_PREFIX, "_", timestamp, ".csv")
    
    write_csv(result$duplicate_rows_data, output_file, na = "")
    cat("重複レコードを出力しました:", output_file, "\n")
  }
}

# メイン処理関数（設定セクションの変数を使用）
main_check_process <- function() {
  cat("=== ID重複チェック処理開始 ===\n")
  cat("入力ファイル:", INPUT_FILE, "\n")
  cat("対象カラム:", TARGET_COLUMN, "\n")
  cat("==============================\n")
  
  # 重複チェック実行
  result <- check_id_duplicates(INPUT_FILE, TARGET_COLUMN, FILE_ENCODING)
  
  if (is.null(result)) {
    return(NULL)
  }
  
  # 重複レコードの詳細表示
  show_duplicate_details(result)
  
  # 重複レコードの出力
  export_duplicate_records(result, INPUT_FILE)
  
  cat("\n=== 処理完了 ===\n")
  return(result)
}

# ============================================================================
# 実行セクション
# ============================================================================

# 基本的な使用方法:
# 1. 上記の設定セクションで INPUT_FILE と TARGET_COLUMN を設定
# 2. 以下のコメントを外して実行
# result <- main_check_process()

# 個別に設定を指定したい場合の使用例:
# result <- check_id_duplicates(
#   input_file = "your_data.csv",
#   target_column = "user_id",
#   encoding = "UTF-8"
# )

# インタラクティブ実行用の関数（対話形式で設定）
interactive_check <- function() {
  cat("=== インタラクティブID重複チェック ===\n")
  
  # 入力ファイルの選択
  input_file <- readline(prompt = "入力ファイル名を入力してください: ")
  
  if (!file.exists(input_file)) {
    cat("エラー: ファイルが見つかりません:", input_file, "\n")
    return(NULL)
  }
  
  # データの列名を表示
  sample_data <- read_csv(input_file, n_max = 1, locale = locale(encoding = FILE_ENCODING))
  cat("利用可能なカラム:", paste(colnames(sample_data), collapse = ", "), "\n")
  
  # 対象カラムの入力
  target_column <- readline(prompt = "チェックするカラム名を入力してください: ")
  target_column <- trimws(target_column)
  
  # 確認
  cat("\n設定確認:\n")
  cat("入力ファイル:", input_file, "\n")
  cat("対象カラム:", target_column, "\n")
  
  proceed <- readline(prompt = "処理を実行しますか？ (y/n): ")
  
  if (tolower(proceed) %in% c("y", "yes")) {
    result <- check_id_duplicates(input_file, target_column, FILE_ENCODING)
    
    if (!is.null(result)) {
      # 重複レコードの詳細表示（インタラクティブでは常に表示）
      if (!result$is_unique && !is.null(result$duplicate_rows_data) && nrow(result$duplicate_rows_data) > 0) {
        cat("\n=== 重複レコードの詳細 ===\n")
        print(result$duplicate_rows_data)
      }
    }
    
    return(result)
  } else {
    cat("処理をキャンセルしました。\n")
    return(NULL)
  }
}

# 複数カラムを一括チェックする関数
check_multiple_columns <- function(input_file, target_columns, encoding = "UTF-8") {
  cat("=== 複数カラム重複チェック ===\n")
  results <- list()
  
  for (col in target_columns) {
    cat("\n--- カラム '", col, "' のチェック ---\n")
    result <- check_id_duplicates(input_file, col, encoding)
    results[[col]] <- result
  }
  
  # 全体サマリー
  cat("\n=== 全体サマリー ===\n")
  for (col in names(results)) {
    if (!is.null(results[[col]])) {
      status <- if (results[[col]]$is_unique) "✅ ユニーク" else "❌ 重複あり"
      cat(col, ":", status)
      if (!results[[col]]$is_unique) {
        cat(" (", results[[col]]$duplicate_count, "個の重複値)")
      }
      cat("\n")
    }
  }
  
  return(results)
}
