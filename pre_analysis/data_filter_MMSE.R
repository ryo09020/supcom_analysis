# -------------------------------------------------------------------------
# データフィルタリングスクリプト（条件付き）
# -------------------------------------------------------------------------
# このスクリプトは、指定した条件に基づいてデータをフィルタリングします。
# - NULL値の除外
# - 数値条件によるフィルタリング（以上・以下・範囲指定）
# -------------------------------------------------------------------------

# --- 0. 準備 ---
# 必要なパッケージをインストール・読み込み
# install.packages(c("dplyr", "readr"))
library(dplyr)
library(readr)

# --- 1. 設定 ---
# ユーザーが環境に合わせて変更する箇所

# 入力・出力ファイル設定
input_file <- "time1_data.csv"
output_file <- "time1_data_filtered.csv"

# NULL値チェック対象列（従来機能）
null_check_columns <- c("sex", "final_education")

# 数値条件フィルタリング設定
# 複数の条件を設定可能
filter_conditions <- list(
  # 使用例1: 65歳以上でMMSEが24以上のレコードのみ残す（65歳未満は除外対象外）
  list(
    column = "MMSE",
    condition = "conditional_age_mmse",
    mmse_threshold = 24,
    age_threshold = 65,
    age_column = "age"
  )
  
  # 使用例2: 年齢が18以上65以下のレコードのみ残す
  # list(
  #   column = "age",
  #   condition = "between",
  #   min_value = 18,
  #   max_value = 65
  # ),
  
  # 使用例3: 特定の項目が10以下のレコードのみ残す
  # list(
  #   column = "depression_score",
  #   condition = "<=",
  #   value = 10
  # )
)

# フィルタリング条件を適用するかどうか（TRUE/FALSE）
apply_numeric_filters <- TRUE

# --- 2. 関数定義 ---

# 数値条件チェック関数
apply_filter_condition <- function(data, condition_list) {
  column <- condition_list$column
  condition <- condition_list$condition
  
  # 列が存在するかチェック
  if (!column %in% colnames(data)) {
    warning(paste("警告: 列 '", column, "' が存在しません。スキップします。", sep=""))
    return(data)
  }
  
  cat(paste("条件適用中: ", column, " ", condition, "\n"))
  
  # 条件に応じてフィルタリング
  if (condition == ">=") {
    value <- condition_list$value
    data <- data %>% filter(!!sym(column) >= value)
    cat(paste("  → ", column, " >= ", value, "\n"))
    
  } else if (condition == "<=") {
    value <- condition_list$value
    data <- data %>% filter(!!sym(column) <= value)
    cat(paste("  → ", column, " <= ", value, "\n"))
    
  } else if (condition == ">") {
    value <- condition_list$value
    data <- data %>% filter(!!sym(column) > value)
    cat(paste("  → ", column, " > ", value, "\n"))
    
  } else if (condition == "<") {
    value <- condition_list$value
    data <- data %>% filter(!!sym(column) < value)
    cat(paste("  → ", column, " < ", value, "\n"))
    
  } else if (condition == "==") {
    value <- condition_list$value
    data <- data %>% filter(!!sym(column) == value)
    cat(paste("  → ", column, " == ", value, "\n"))
    
  } else if (condition == "between") {
    min_val <- condition_list$min_value
    max_val <- condition_list$max_value
    data <- data %>% filter(!!sym(column) >= min_val & !!sym(column) <= max_val)
    cat(paste("  → ", min_val, " <= ", column, " <= ", max_val, "\n"))
    
  } else if (condition == "conditional_age_mmse") {
    # 年齢条件付きMMSEフィルタリング
    mmse_threshold <- condition_list$mmse_threshold
    age_threshold <- condition_list$age_threshold
    age_column <- condition_list$age_column
    
    # 年齢列が存在するかチェック
    if (!age_column %in% colnames(data)) {
      warning(paste("警告: 年齢列 '", age_column, "' が存在しません。スキップします。", sep=""))
      return(data)
    }
    
    # 65歳以上でMMSE < threshold のレコードを除外
    # （65歳未満は除外対象外、65歳以上でMMSE >= threshold は保持）
    excluded_data <- data %>% 
      filter(!!sym(age_column) >= age_threshold & !!sym(column) < mmse_threshold)
    excluded_count <- nrow(excluded_data)
    
    data <- data %>% 
      filter(!(!!sym(age_column) >= age_threshold & !!sym(column) < mmse_threshold))
    
    cat(paste("  → ", age_threshold, "歳以上で", column, " < ", mmse_threshold, " のレコードを除外\n"))
    cat(paste("  → ", age_threshold, "歳未満のレコードは", column, "の値に関係なく保持\n"))
    cat(paste("  → 除外されたレコード数: ", excluded_count, "\n"))
    
  } else {
    warning(paste("警告: 不明な条件 '", condition, "' です。スキップします。", sep=""))
  }
  
  return(data)
}

# --- 3. メイン処理 ---

cat("=== データフィルタリング開始 ===\n\n")

# Step 1: データ読み込み
cat("Step 1: CSVファイルを読み込んでいます...\n")
tryCatch({
  data <- read_csv(input_file)
  cat(paste("ファイル '", input_file, "' を読み込みました。\n", sep=""))
}, error = function(e) {
  stop("エラー: ファイルが読み込めませんでした。パスやファイル名を確認してください。\n", e)
})

original_rows <- nrow(data)
cat(paste("元のデータ: ", original_rows, " 行\n\n"))

# Step 2: NULL値チェック（従来機能）
if (length(null_check_columns) > 0) {
  cat("Step 2: NULL値チェックを実行中...\n")
  
  # 対象列が存在するかチェック
  missing_cols <- setdiff(null_check_columns, colnames(data))
  if (length(missing_cols) > 0) {
    warning(paste("警告: 指定された列 '", paste(missing_cols, collapse=", "), "' が存在しません。", sep=""))
    null_check_columns <- intersect(null_check_columns, colnames(data))
  }
  
  if (length(null_check_columns) > 0) {
    # NULL値を持つ行を除外
    before_null_filter <- nrow(data)
    data <- data %>%
      filter(if_all(all_of(null_check_columns), ~ !is.na(.) & . != ""))
    after_null_filter <- nrow(data)
    
    cat(paste("NULL値フィルタリング: ", before_null_filter, " → ", after_null_filter, " 行\n"))
    cat(paste("除外された行数: ", before_null_filter - after_null_filter, "\n\n"))
  }
} else {
  cat("Step 2: NULL値チェックをスキップします。\n\n")
}

# Step 3: 数値条件フィルタリング
if (apply_numeric_filters && length(filter_conditions) > 0) {
  cat("Step 3: 数値条件フィルタリングを実行中...\n")
  
  for (i in seq_along(filter_conditions)) {
    before_filter <- nrow(data)
    data <- apply_filter_condition(data, filter_conditions[[i]])
    after_filter <- nrow(data)
    
    cat(paste("フィルタリング後: ", before_filter, " → ", after_filter, " 行\n"))
    cat(paste("除外された行数: ", before_filter - after_filter, "\n\n"))
  }
} else {
  cat("Step 3: 数値条件フィルタリングをスキップします。\n\n")
}

final_rows <- nrow(data)

# Step 4: 結果保存
cat("Step 4: フィルタリング済みデータを保存中...\n")
write_csv(data, output_file)

# --- 4. 結果サマリー ---
cat("\n=== 処理完了 ===\n")
cat(paste("フィルタリング済みデータを '", output_file, "' に保存しました。\n", sep=""))
cat("\n--- 処理結果サマリー ---\n")
cat(paste("元の行数: ", original_rows, "\n"))
cat(paste("最終行数: ", final_rows, "\n"))
cat(paste("合計除外行数: ", original_rows - final_rows, "\n"))
cat(paste("データ保持率: ", round((final_rows / original_rows) * 100, 1), "%\n"))

# 適用された条件の確認
cat("\n--- 適用された条件 ---\n")
if (length(null_check_columns) > 0) {
  cat("NULL値チェック対象列:", paste(null_check_columns, collapse=", "), "\n")
}
if (apply_numeric_filters && length(filter_conditions) > 0) {
  cat("数値条件:\n")
  for (condition in filter_conditions) {
    if (condition$condition == "between") {
      cat(paste("  - ", condition$column, ": ", condition$min_value, " ～ ", condition$max_value, "\n", sep=""))
    } else if (condition$condition == "conditional_age_mmse") {
      cat(paste("  - ", condition$age_threshold, "歳以上で", condition$column, " >= ", condition$mmse_threshold, " (", condition$age_threshold, "歳未満は除外対象外)\n", sep=""))
    } else {
      cat(paste("  - ", condition$column, " ", condition$condition, " ", condition$value, "\n", sep=""))
    }
  }
}

cat("\n=== スクリプト実行完了 ===\n")
