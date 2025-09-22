# 入力ファイルの設定
INPUT_FILE <- "dummy_data.csv"  # 処理したいCSVファイルのパスを指定

# IDカラムの設定
ID_COLUMN <- "ID"  # ID列の名前を指定

# 重複チェック対象カラムの設定（参考用、実際は使用しない）
TARGET_COLUMNS <- c("final_education")  # 参考：以前はこのカラムで値一致をチェックしていた

# 出力設定
OUTPUT_PREFIX <- "duplicate_filtered"  # 出力ファイル名のプレフィックス
ADD_TIMESTAMP <- TRUE  # 出力ファイル名にタイムスタンプを追加するか

# エンコーディング設定
FILE_ENCODING <- "UTF-8"  # ファイルの文字エンコーディング

# 必要なライブラリの読み込み
library(dplyr)
library(readr)

# メイン処理関数
remove_duplicate_by_id <- function() {
  cat("重複ID削除処理を開始します...\n")
  
  # ファイルの存在チェック
  if (!file.exists(INPUT_FILE)) {
    stop(paste("入力ファイルが見つかりません:", INPUT_FILE))
  }
  
  # データの読み込み
  cat("データを読み込んでいます...\n")
  data <- read_csv(INPUT_FILE, show_col_types = FALSE)
  cat(paste("読み込み完了:", nrow(data), "行,", ncol(data), "列\n"))
  
  # IDカラムの存在チェック
  if (!ID_COLUMN %in% names(data)) {
    stop(paste("IDカラムがデータに存在しません:", ID_COLUMN))
  }
  
  # 元のデータ行数
  original_rows <- nrow(data)
  
  # IDの重複をチェック（削除される行数を確認用）
  duplicate_count <- data %>%
    group_by(!!sym(ID_COLUMN)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(count > 1) %>%
    summarise(total_duplicates = sum(count - 1)) %>%
    pull(total_duplicates)
  
  if (is.na(duplicate_count) || duplicate_count == 0) {
    duplicate_count <- 0
  }
  
  cat(paste("重複により削除される行数:", duplicate_count, "\n"))
  
  # 重複IDがある場合は最初のレコードのみ残す（問答無用で削除）
  cat("重複ID削除処理中...\n")
  filtered_data <- data %>%
    group_by(!!sym(ID_COLUMN)) %>%
    slice(1) %>%  # 各IDグループの最初の行のみ残す
    ungroup()
  
  # 削除されたIDの詳細表示
  if (duplicate_count > 0) {
    duplicate_ids <- data %>%
      group_by(!!sym(ID_COLUMN)) %>%
      filter(n() > 1) %>%
      distinct(!!sym(ID_COLUMN)) %>%
      pull(!!sym(ID_COLUMN))
    
    cat(paste("重複していたID:", paste(head(duplicate_ids, 10), collapse = ", ")))
    if (length(duplicate_ids) > 10) {
      cat(paste(" ...他", length(duplicate_ids) - 10, "件"))
    }
    cat("\n")
    cat("-> すべて最初のレコードを保持、残りを削除しました。\n")
  } else {
    cat("重複IDが見つかりませんでした。\n")
  }
  
  # 結果の表示
  removed_rows <- original_rows - nrow(filtered_data)
  cat(paste("処理結果: 元の行数", original_rows, "-> 処理後", nrow(filtered_data), "行\n"))
  cat(paste("削除された行数:", removed_rows, "\n"))
  
  # 出力ファイル名の生成
  output_filename <- if (ADD_TIMESTAMP) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    paste0(OUTPUT_PREFIX, "_", timestamp, ".csv")
  } else {
    paste0(OUTPUT_PREFIX, ".csv")
  }
  
  # データの出力
  write_csv(filtered_data, output_filename)
  cat(paste("結果を保存しました:", output_filename, "\n"))
  
  # 処理統計の表示
  cat("\n=== 処理統計 ===\n")
  cat(paste("入力ファイル:", INPUT_FILE, "\n"))
  cat(paste("出力ファイル:", output_filename, "\n"))
  cat(paste("IDカラム:", ID_COLUMN, "\n"))
  cat(paste("削除行数:", removed_rows, "\n"))
}

# 処理実行
tryCatch({
  remove_duplicate_by_id()
  cat("\n処理が正常に完了しました。\n")
}, error = function(e) {
  cat(paste("エラーが発生しました:", e$message, "\n"))
})