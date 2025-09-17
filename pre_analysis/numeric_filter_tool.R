# 数値変換フィルタリングスクリプト
# 指定されたカラムが数値に変換できないレコードを削除し、新しいファイルに出力する

# ============================================================================
# 設定セクション - ここを編集してください
# ============================================================================

# 入力ファイルの設定
INPUT_FILE <- "dummy_data.csv"  # 処理したいCSVファイルのパスを指定

# 数値変換をチェックするカラムの設定
TARGET_COLUMNS <- c("column1", "column2")  # チェックしたいカラム名を指定

# 出力設定
OUTPUT_PREFIX <- "numeric_filtered"  # 出力ファイル名のプレフィックス
ADD_TIMESTAMP <- TRUE  # 出力ファイル名にタイムスタンプを追加するか

# エンコーディング設定
FILE_ENCODING <- "UTF-8"  # ファイルの文字エンコーディング

# ============================================================================
# 以下は関数定義 - 通常は変更不要
# ============================================================================

# 必要なライブラリを読み込み
library(dplyr)
library(readr)

# 数値変換フィルタリング関数
filter_numeric_convertible <- function(input_file, output_file, target_columns, encoding = "UTF-8") {
  # ファイル読み込み
  cat("ファイル読み込み中:", input_file, "\n")
  data <- read_csv(input_file, locale = locale(encoding = encoding))
  
  original_rows <- nrow(data)
  cat("元のデータ行数:", original_rows, "\n")
  
  # 各対象カラムについて数値変換をチェック
  for (col in target_columns) {
    if (!col %in% colnames(data)) {
      warning("カラム '", col, "' がデータに存在しません。スキップします。")
      next
    }
    
    cat("カラム '", col, "' を処理中...\n")
    
    # 数値変換可能かチェック（NAは残す）
    numeric_check <- suppressWarnings(as.numeric(data[[col]]))
    
    # 変換できない行（NAになったが元々NAでなかった行）を特定
    invalid_rows <- is.na(numeric_check) & !is.na(data[[col]])
    
    if (sum(invalid_rows) > 0) {
      cat("  - カラム '", col, "' で", sum(invalid_rows), "行を削除\n")
      # 無効な行を削除
      data <- data[!invalid_rows, ]
    } else {
      cat("  - カラム '", col, "' で削除された行はありません\n")
    }
  }
  
  final_rows <- nrow(data)
  removed_rows <- original_rows - final_rows
  
  cat("処理後のデータ行数:", final_rows, "\n")
  cat("削除された行数:", removed_rows, "\n")
  cat("削除率:", round((removed_rows / original_rows) * 100, 2), "%\n")
  
  # 結果をファイルに出力
  write_csv(data, output_file, na = "")
  cat("出力完了:", output_file, "\n")
  
  # サマリーを返す
  return(list(
    original_rows = original_rows,
    final_rows = final_rows,
    removed_rows = removed_rows,
    removal_rate = (removed_rows / original_rows) * 100
  ))
}

# メイン処理関数（設定セクションの変数を使用）
main_filter_process <- function() {
  # 出力ファイル名の生成
  file_base <- tools::file_path_sans_ext(basename(INPUT_FILE))
  
  if (ADD_TIMESTAMP) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- paste0(file_base, "_", OUTPUT_PREFIX, "_", timestamp, ".csv")
  } else {
    output_file <- paste0(file_base, "_", OUTPUT_PREFIX, ".csv")
  }
  
  cat("=== 数値変換フィルタリング処理開始 ===\n")
  cat("入力ファイル:", INPUT_FILE, "\n")
  cat("対象カラム:", paste(TARGET_COLUMNS, collapse = ", "), "\n")
  cat("出力ファイル:", output_file, "\n")
  cat("=====================================\n")
  
  # フィルタリング実行
  result <- filter_numeric_convertible(INPUT_FILE, output_file, TARGET_COLUMNS, FILE_ENCODING)
  
  cat("\n=== 処理完了 ===\n")
  cat("サマリー:\n")
  cat("- 元の行数:", result$original_rows, "\n")
  cat("- 最終行数:", result$final_rows, "\n")
  cat("- 削除行数:", result$removed_rows, "\n")
  cat("- 削除率:", round(result$removal_rate, 2), "%\n")
  
  return(result)
}

# ============================================================================
# 実行セクション
# ============================================================================

# 基本的な使用方法:
# 1. 上記の設定セクションで INPUT_FILE と TARGET_COLUMNS を設定
# 2. 以下のコメントを外して実行
# result <- main_filter_process()

# 個別にカラムや設定を指定したい場合の使用例:
# result <- filter_numeric_convertible(
#   input_file = "your_data.csv",
#   output_file = "filtered_data.csv", 
#   target_columns = c("age", "score"),
#   encoding = "UTF-8"
# )

# インタラクティブ実行用の関数（対話形式で設定）
interactive_filter <- function() {
  cat("=== インタラクティブ数値フィルタリング ===\n")
  
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
  target_cols_input <- readline(prompt = "チェックするカラム名をカンマ区切りで入力してください: ")
  target_columns <- trimws(strsplit(target_cols_input, ",")[[1]])
  
  # 出力ファイル名の生成
  file_base <- tools::file_path_sans_ext(basename(input_file))
  if (ADD_TIMESTAMP) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    output_file <- paste0(file_base, "_", OUTPUT_PREFIX, "_", timestamp, ".csv")
  } else {
    output_file <- paste0(file_base, "_", OUTPUT_PREFIX, ".csv")
  }
  
  # 確認
  cat("\n設定確認:\n")
  cat("入力ファイル:", input_file, "\n")
  cat("対象カラム:", paste(target_columns, collapse = ", "), "\n")
  cat("出力ファイル:", output_file, "\n")
  
  proceed <- readline(prompt = "処理を実行しますか？ (y/n): ")
  
  if (tolower(proceed) %in% c("y", "yes")) {
    result <- filter_numeric_convertible(input_file, output_file, target_columns, FILE_ENCODING)
    return(result)
  } else {
    cat("処理をキャンセルしました。\n")
    return(NULL)
  }
}