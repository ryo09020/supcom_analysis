# ---
# CSV内の日付カラムから最小・最大日付を取得するRスクリプト
# ---

# 0. 必要なライブラリのインストール（初回のみ）
# 以下の行のコメントを解除して実行してください。
# install.packages("readr")

# 1. ライブラリの読み込み
library(readr)

#' CSVを読み込み、指定された日付カラムの最小・最大日付をコンソールに出力する関数
#'
#' @param csv_file_path 読み込むCSVファイルのパス
#' @param date_col 日付データ（"yyyy-mm-dd"形式）が入っているカラム名（文字列）
#'
find_date_range <- function(csv_file_path, date_col) {
  
  # 2. データの読み込み
  tryCatch({
    # col_types = cols(.default = "c") で一度すべてを文字列として読み込みます
    data <- read_csv(csv_file_path, col_types = cols(.default = "c"))
  }, error = function(e) {
    stop(paste0("CSVファイルの読み込みに失敗しました: ", csv_file_path, "\nエラー: ", e$message))
  })
  
  # 3. 指定されたカラムの存在チェック
  if (!date_col %in% names(data)) {
    stop(paste0("指定された日付カラム '", date_col, "' がCSV内に見つかりません。"))
  }
  
  # 4. 指定されたカラムを日付型に変換
  # "yyyy-mm-dd" 形式は as.Date() のデフォルト形式です
  dates <- as.Date(data[[date_col]])
  
  # 5. 欠損値のチェック (元のNA + 形式不正でNAになったもの)
  na_count <- sum(is.na(dates))
  cat("\n--- 日付データチェック ---\n")
  cat(paste0("カラム '", date_col, "' を処理中...\n"))
  cat(paste0("有効な日付エントリ数: ", sum(!is.na(dates)), "\n"))
  cat(paste0("欠損 (NA) または不正な形式の数: ", na_count, "\n"))
  
  if (sum(!is.na(dates)) == 0) {
    cat("有効な日付データが0件のため、最小・最大日付は計算できませんでした。\n")
    cat("---------------------------\n")
    return(invisible(NULL))
  }
  
  # 6. 最小・最大日付の取得と出力
  # na.rm = TRUE で欠損値を無視して計算します
  oldest_date <- min(dates, na.rm = TRUE)
  newest_date <- max(dates, na.rm = TRUE)
  
  cat("\n--- 結果 ---\n")
  cat(paste0("最も古い日付: ", oldest_date, "\n"))
  cat(paste0("最も新しい日付: ", newest_date, "\n"))
  cat("----------------\n")
}



# ---
# (B) 関数の実行 (ここが本番です)
# ---

# 1. "my_dates.csv" を実際のファイルパスに書き換えてください。
# 2. "start_date" を実際の「日付」カラム名に書き換えてください。

print("日付の最小・最大値の取得を開始します...")
tryCatch({
  
  find_date_range(
    csv_file_path = "dummy_data.csv", # <- 実際のファイルパスに変更
    date_col = "date"         # <- 実際の年齢カラム名に変更
  )
  
}, error = function(e) {
  cat("\nエラーが発生しました: ", e$message, "\n")
})