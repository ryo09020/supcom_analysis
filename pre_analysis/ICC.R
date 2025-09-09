# 必要なパッケージをインストールして読み込む
# install.packages("psych")
# install.packages("knitr")
library(psych)
library(knitr)

# ===================================================================
# パラメータ設定 (利用者はこの部分を編集してください)
# ===================================================================

# 1. データが含まれるCSVファイルのパス
file_path <- "longitudinal_dummy_data.csv"

# 2. 分析する変数のペアを定義
# list( "分析したい変数名" = c("Time1の列名", "Time2の列名"), ... ) の形式で指定します。
# 下位尺度得点や合計点など、比較したい変数のペアを必要なだけ追加できます。
variables_to_analyze <- list(
  "N_Score"     = c("N_Score_T1", "N_Score_T2"),
  "E_Score"     = c("E_Score_T1", "E_Score_T2"),
  "O_Score"     = c("O_Score_T1", "O_Score_T2"),
  "A_Score"     = c("A_Score_T1", "A_Score_T2"),
  "C_Score"     = c("C_Score_T1", "C_Score_T2")
)

# 3. 出力設定
output_log_file <- "ICC_analysis_log.txt"  # ログファイル名（NULLで出力しない）

# ===================================================================
# 分析関数 (この部分は編集不要です)
# ===================================================================

#' 2時点間のデータから級内相関係数(ICC)を計算する関数
#'
#' @param file_path character. データファイルのパス。
#' @param variables_list list. 分析対象の変数ペアのリスト。
#' @param output_log_file character. ログファイル名（NULLの場合は保存しない）。
#'
calculate_icc_for_variables <- function(file_path, variables_list, output_log_file = NULL) {

  # ログ出力の開始
  if (!is.null(output_log_file)) {
    sink(output_log_file, split = TRUE)  # split=TRUEでコンソールにも表示
    cat("==================================================================\n")
    cat("ICC分析ログ - 開始時刻: ", as.character(Sys.time()), "\n")
    cat("データファイル: ", file_path, "\n")
    cat("==================================================================\n\n")
  }
  
  # --- 1. データの読み込み ---
  if (!file.exists(file_path)) {
    stop("指定されたファイルが見つかりません: ", file_path)
  }
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  cat("CSVファイルの読み込みが完了しました。\n\n")
  
  # 結果を保存するための空のリストを作成
  all_results <- list()
  
  # --- 2. 各変数ペアについてループ処理 ---
  for (variable_name in names(variables_list)) {
    
    time1_col <- variables_list[[variable_name]][1]
    time2_col <- variables_list[[variable_name]][2]
    
    # データに必要な列が存在するかチェック
    if (!all(c(time1_col, time2_col) %in% names(data))) {
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      cat("警告: 変数 '", variable_name, "' の列が見つかりません。スキップします。\n")
      cat("必要な列: ", time1_col, ", ", time2_col, "\n")
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
      next
    }
    
    cat("--------------------------------------------------------\n")
    cat("変数:", variable_name, "のICCを計算します。\n")
    cat("--------------------------------------------------------\n")
    
    # 対象の2列を抽出
    icc_data <- data[, c(time1_col, time2_col)]
    
    # 数値でないデータを除外（NAや文字列など）
    icc_data[[time1_col]] <- as.numeric(as.character(icc_data[[time1_col]]))
    icc_data[[time2_col]] <- as.numeric(as.character(icc_data[[time2_col]]))
    
    # 欠損値（NA、数値変換できない文字列を含む）を除いた完全なケースのみを使用
    complete_cases <- complete.cases(icc_data)
    icc_data_complete <- icc_data[complete_cases, ]
    
    # 使用した行数を表示
    total_rows <- nrow(icc_data)
    used_rows <- nrow(icc_data_complete)
    missing_rows <- total_rows - used_rows
    
    cat("データ情報:\n")
    cat("  総行数: ", total_rows, "\n")
    cat("  使用行数: ", used_rows, "\n")
    if (missing_rows > 0) {
      cat("  欠損により除外された行数: ", missing_rows, "\n")
    }
    cat("\n")
    
    # ICCの計算 (psychパッケージのICC関数は引数なしで複数のタイプを計算します)
    icc_results <- ICC(icc_data_complete)
    
    # ICC計算で実際に使用されたデータの詳細情報
    cat("ICC計算の詳細情報:\n")
    cat("  ICC計算に使用されたサンプル数: ", icc_results$n.obs, "\n")
    cat("  評価者数（時点数）: ", icc_results$n.judge, "\n")
    if (!is.null(icc_results$missing)) {
      cat("  ICC計算内で検出された欠損値: ", icc_results$missing, "\n")
    }
    cat("\n")
    
    # 結果の表示
    print(icc_results)
    
    # サマリー用に結果を抽出
    # 論文で言及されている「2元混合効果モデル・絶対一致」はICC3に相当し、
    # psychパッケージの出力では "Single_fixed_raters" の行に対応します。
    icc_value <- icc_results$results["Single_fixed_raters", "ICC"]
    ci_lower <- icc_results$results["Single_fixed_raters", "lower bound"]
    ci_upper <- icc_results$results["Single_fixed_raters", "upper bound"]
    
    all_results[[variable_name]] <- data.frame(
      Variable = variable_name,
      ICC = icc_value,
      "95% CI Lower" = ci_lower,
      "95% CI Upper" = ci_upper,
      check.names = FALSE # 列名のハイフンを維持するため
    )
  }
  
  # --- 3. 最終結果のサマリー表示 ---
  if (length(all_results) > 0) {
    cat("\n\n########################################################\n")
    cat("############### 全変数のICC分析結果サマリー ###############\n")
    cat("########################################################\n\n")
    
    summary_df <- do.call(rbind, all_results)
    rownames(summary_df) <- NULL # 行名を削除
    
    # 数値を見やすくフォーマット
    numeric_cols <- c("ICC", "95% CI Lower", "95% CI Upper")
    summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], function(x) round(x, 3))
    
    print(kable(summary_df, caption = "各変数の級内相関係数（ICC）"))
  } else {
    cat("\n分析対象の変数がありませんでした。\n")
  }
  
  # ログ出力の終了
  if (!is.null(output_log_file)) {
    cat("\n==================================================================\n")
    cat("ICC分析ログ - 終了時刻: ", as.character(Sys.time()), "\n")
    cat("==================================================================\n")
    sink()  # ログファイル出力を終了
    cat("ログファイルに保存しました: ", output_log_file, "\n")
  }
}

# ===================================================================
# 関数の実行 (この部分は編集不要です)
# ===================================================================

# 上部で設定したパラメータを使って関数を実行します
calculate_icc_for_variables(
  file_path = file_path,
  variables_list = variables_to_analyze,
  output_log_file = output_log_file
)
