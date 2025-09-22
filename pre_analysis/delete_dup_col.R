# 入力ファイルの設定
INPUT_FILE <- "dummy_data.csv"  # 処理したいCSVファイルのパスを指定

# IDカラムの設定
ID_COLUMN <- "ID"  # ID列の名前を指定

# 重複チェック対象カラムの設定
TARGET_COLUMNS <- c("final_education")  # 重複IDで値が一致している場合に削除対象とするカラム名を指定

# 出力設定
OUTPUT_PREFIX <- "duplicate_filtered"  # 出力ファイル名のプレフィックス
ADD_TIMESTAMP <- TRUE  # 出力ファイル名にタイムスタンプを追加するか

# エンコーディング設定
FILE_ENCODING <- "UTF-8"  # ファイルの文字エンコーディング

# 必要なライブラリの読み込み
library(dplyr)
library(readr)

# メイン処理関数
remove_duplicate_by_id_and_columns <- function() {
  cat("重複ID・指定カラム値チェック処理を開始します...\n")
  
  # ファイルの存在チェック
  if (!file.exists(INPUT_FILE)) {
    stop(paste("入力ファイルが見つかりません:", INPUT_FILE))
  }
  
  # データの読み込み
  cat("データを読み込んでいます...\n")
  data <- read_csv(INPUT_FILE, locale = locale(encoding = FILE_ENCODING))
  cat(paste("読み込み完了:", nrow(data), "行,", ncol(data), "列\n"))
  
  # 必要なカラムの存在チェック
  missing_columns <- c(ID_COLUMN, TARGET_COLUMNS)[!c(ID_COLUMN, TARGET_COLUMNS) %in% names(data)]
  if (length(missing_columns) > 0) {
    stop(paste("以下のカラムがデータに存在しません:", paste(missing_columns, collapse = ", ")))
  }
  
  # 元のデータ行数
  original_rows <- nrow(data)
  
  # IDの重複をチェック
  duplicate_ids <- data %>%
    group_by(!!sym(ID_COLUMN)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    filter(count > 1) %>%
    pull(!!sym(ID_COLUMN))
  
  cat(paste("重複IDの数:", length(duplicate_ids), "\n"))
  
  if (length(duplicate_ids) == 0) {
    cat("重複IDが見つかりませんでした。元のデータをそのまま出力します。\n")
    filtered_data <- data
  } else {
    # 重複IDのデータを分析
    duplicate_data <- data %>% filter(!!sym(ID_COLUMN) %in% duplicate_ids)
    non_duplicate_data <- data %>% filter(!!!sym(ID_COLUMN) %in% duplicate_ids)
    
    # 各重複IDグループで指定カラムの値が同じかチェック
    processed_duplicate_data <- duplicate_data %>%
      group_by(!!sym(ID_COLUMN)) %>%
      group_modify(~ {
        # 指定カラムの値がすべて同じかチェック
        values_identical <- all(sapply(TARGET_COLUMNS, function(col) {
          values <- .x[[col]]
          # NAを除いて値が同じかチェック
          unique_values <- unique(values[!is.na(values)])
          length(unique_values) <= 1
        }))
        
        if (values_identical) {
          # 値が同じ場合は最初の行のみ残す
          cat(paste("ID", .y[[ID_COLUMN]], ": 指定カラムの値が一致 -> 重複削除\n"))
          return(.x[1, , drop = FALSE])
        } else {
          # 値が異なる場合はすべて残す
          cat(paste("ID", .y[[ID_COLUMN]], ": 指定カラムの値が不一致 -> すべて保持\n"))
          return(.x)
        }
      })
    
    # データを結合
    filtered_data <- bind_rows(non_duplicate_data, processed_duplicate_data)
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
  write_csv(filtered_data, output_filename, locale = locale(encoding = FILE_ENCODING))
  cat(paste("結果を保存しました:", output_filename, "\n"))
  
  # 処理統計の表示
  cat("\n=== 処理統計 ===\n")
  cat(paste("入力ファイル:", INPUT_FILE, "\n"))
  cat(paste("出力ファイル:", output_filename, "\n"))
  cat(paste("IDカラム:", ID_COLUMN, "\n"))
  cat(paste("チェック対象カラム:", paste(TARGET_COLUMNS, collapse = ", "), "\n"))
  cat(paste("重複ID数:", length(duplicate_ids), "\n"))
  cat(paste("削除行数:", removed_rows, "\n"))
}

# 処理実行
tryCatch({
  remove_duplicate_by_id_and_columns()
  cat("\n処理が正常に完了しました。\n")
}, error = function(e) {
  cat(paste("エラーが発生しました:", e$message, "\n"))
})