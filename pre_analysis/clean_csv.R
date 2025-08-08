# --- 0. 準備 ---
# 最初に必要なパッケージをインストールします
# もし一度もインストールしたことがなければ、以下の行の先頭の # を消して実行してください
# install.packages("tidyverse") 

# パッケージを読み込みます
library(dplyr)
library(readr)


# --- 1. 設定 ---
# ユーザーが自身の環境に合わせて変更する箇所です

# 入力するCSVファイルのパスを指定
input_file <- "time2_data_d.csv" 

# 出力する新しいCSVファイルのパスを指定
output_file <- "time2_data.csv" 

# 欠損値をチェックしたい列の名前を複数指定
# 例: c("列A", "列B", "列C") のように指定します
target_columns <- c("sex", "final_education") 


# --- 2. 処理 ---
# スクリプトの本体です。通常は変更する必要はありません

# Step 1: CSVファイルを読み込み
cat("Step 1: CSVファイルを読み込んでいます...\n")
tryCatch({
  data <- read_csv(input_file)
}, error = function(e) {
  stop("エラー: ファイルが読み込めませんでした。パスやファイル名を確認してください。\n", e)
})

# 対象の列が存在するかチェック
missing_cols <- setdiff(target_columns, colnames(data))
if (length(missing_cols) > 0) {
  stop(paste("エラー: 指定された列 '", paste(missing_cols, collapse=", "), "' がCSVファイルに存在しません。", sep=""))
}

original_rows <- nrow(data)
cat(paste("元のデータには", original_rows, "行あります。\n"))

# Step 2: 指定された全ての列で数値に変換できない行を削除
cat(paste("Step 2: 指定列をチェックし、数値でない行を削除しています...\n"))

cleaned_data <- data %>%
  filter(
    # if_all: 指定した全ての列が...
    # all_of(target_columns): target_columnsの列に対して...
    # ~ !is.na(as.numeric(.)): 値を数値に変換したときにNAではない、という条件を満たす
    if_all(all_of(target_columns), ~ !is.na(as.numeric(.)))
  )

cleaned_rows <- nrow(cleaned_data)


# --- 3. 結果の出力 ---

# Step 3: クリーンなデータを新しいCSVファイルに書き出し (readr::write_csv を使用)
cat("Step 3: クリーンなデータを新しいCSVファイルに書き出しています...\n")
write_csv(cleaned_data, output_file)

# 処理結果のサマリーを表示
cat("\n--- 処理完了 ---\n")
cat(paste("クリーンなデータを '", output_file, "' に保存しました。\n", sep=""))
cat(paste("元の行数: ", original_rows, "\n"))
cat(paste("処理後の行数: ", cleaned_rows, "\n"))
cat(paste("削除された行数: ", original_rows - cleaned_rows, "\n"))