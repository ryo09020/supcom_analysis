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
  "神経症傾向スコア" = c("N_Score_T1", "N_Score_T2"),
  "外向性スコア"     = c("E_Score_T1", "E_Score_T2"),
  "開放性スコア"     = c("O_Score_T1", "O_Score_T2"),
  "調和性スコア"     = c("A_Score_T1", "A_Score_T2"),
  "誠実性スコア"     = c("C_Score_T1", "C_Score_T2")
)

# ===================================================================
# 分析関数 (この部分は編集不要です)
# ===================================================================

#' 2時点間のデータから級内相関係数(ICC)を計算する関数
#'
#' @param file_path character. データファイルのパス。
#' @param variables_list list. 分析対象の変数ペアのリスト。
#'
calculate_icc_for_variables <- function(file_path, variables_list) {
  
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
    
    # ICCの計算 (psychパッケージのICC関数は引数なしで複数のタイプを計算します)
    icc_results <- ICC(icc_data)
    
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
}

# ===================================================================
# 関数の実行 (この部分は編集不要です)
# ===================================================================

# 上部で設定したパラメータを使って関数を実行します
calculate_icc_for_variables(
  file_path = file_path,
  variables_list = variables_to_analyze
)
