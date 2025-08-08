# 必要なパッケージを読み込む
library(lavaan)
library(psych)
library(knitr) # 最後のサマリー表にのみ利用

# ===================================================================
# パラメータ設定 (利用者はこの部分を編集してください)
# ===================================================================
# 1. データが含まれるCSVファイルのパス
file_path <- "cfa_dummy.csv" 

# 2. 尺度の最大点（例: 5段階評価なら5）
max_score <- 5

# 3. 逆転項目リスト
reverse_items <- c("N2", "N4", "E1", "E5", "A2", "A6", "C1", "C3")

# 4. 分析する因子の定義
factors_to_analyze <- list(
  Neuroticism = c("N1", "N2", "N3", "N4", "N5"),
  Extraversion = c("E1", "E2", "E3", "E4", "E5"),
  Openness = c("O1", "O2", "O3", "O4", "O5"),
  Agreeableness = c("A1", "A2", "A3", "A4", "A5", "A6"),
  Conscientiousness = c("C1", "C2", "C3", "C4", "C5")
)

# 5. 出力するレポートファイル名の接頭辞
report_filename_prefix <- "full_analysis_report"

# ===================================================================
# 分析・出力関数 (この部分は編集不要です)
# ===================================================================

#' 質問紙の妥当性・信頼性を検証し、詳細な結果をテキストファイルに出力する関数
run_and_save_full_report <- function(file_path, factors_list, reverse_items, max_score, filename_prefix) {
  
  # --- 出力先をファイルに指定 ---
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  output_file <- paste0(filename_prefix, "_", timestamp, ".txt")
  
  # sink() で、これ以降のコンソール出力をすべてファイルに書き出す
  sink(output_file)
  
  # --- レポート内容の生成 ---
  
  # 1. データの読み込みと前処理
  if (!file.exists(file_path)) { stop("指定されたファイルが見つかりません: ", file_path) }
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  cat("########################################################\n")
  cat("############### 質問紙 妥当性・信頼性 詳細レポート ###############\n")
  cat("########################################################\n\n")
  cat("実行日時: ", as.character(Sys.time()), "\n")
  cat("データファイル: ", file_path, "\n\n")
  
  if (length(reverse_items) > 0) {
    data[, reverse_items] <- (max_score + 1) - data[, reverse_items]
    cat("逆転項目を処理しました:", paste(reverse_items, collapse = ", "), "\n")
  }
  all_results_summary <- list()
  
  # 2. 各因子についてループ処理
  for (factor_name in names(factors_list)) {
    
    # このループで使う変数を初期化
    CFI_val <- NA; TLI_val <- NA; RMSEA_val <- NA; SRMR_val <- NA
    omega_val <- NA; status_msg <- "Unknown Error"
    
    cat("\n\n========================================================\n")
    cat("■ 因子:", factor_name, "の分析結果\n")
    cat("========================================================\n")
    
    # --- 項目存在チェック ---
    item_columns <- factors_list[[factor_name]]
    if (!all(item_columns %in% names(data))) {
      cat("エラー: 指定された項目がデータに存在しないため、分析をスキップしました。\n")
      status_msg <- "Items Not Found"
    } else {
      # --- 確認的因子分析 (CFA) ---
      cat("\n--- (1) 確認的因子分析 (CFA) の詳細結果 ---\n")
      model_syntax <- paste(factor_name, "=~", paste(item_columns, collapse = " + "))
      cfa_fit <- try(cfa(model_syntax, data = data[, item_columns]), silent = TRUE)
      
      if (inherits(cfa_fit, "try-error")) {
        cat("CFAでエラーが発生しました。\n")
        status_msg <- "CFA Error"
      } else {
        # CFAが成功した場合、結果を出力・保存
        print(summary(cfa_fit, fit.measures = TRUE, standardized = TRUE))
        fit_indices <- fitMeasures(cfa_fit, c("cfi", "tli", "rmsea", "srmr"))
        CFI_val <- fit_indices["cfi"]; TLI_val <- fit_indices["tli"]
        RMSEA_val <- fit_indices["rmsea"]; SRMR_val <- fit_indices["srmr"]
        
        # --- 信頼性係数 ---
        cat("\n\n--- (2) 信頼性係数 (マクドナルドのω) の詳細結果 ---\n")
        omega_results <- try(omega(data[, item_columns], nfactors = 1, fm = "ml"), silent = TRUE)
        
        # ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
        # 【変更点】omega()の結果がNULLでないか、より厳密にチェックする
        if (inherits(omega_results, "try-error")) {
          cat("ω係数の計算でエラーが発生しました。\n")
          status_msg <- "Omega Error"
        } else if (is.null(omega_results$omega_t) || length(omega_results$omega_t) != 1) {
          # omega()は成功したが、結果が取得できない場合
          cat("ω係数の値が取得できませんでした。\n")
          print(omega_results, G = FALSE)
          status_msg <- "Omega Value Error"
        } else {
          # 正常に成功した場合
          print(omega_results, G = FALSE)
          omega_val <- omega_results$omega_t
          status_msg <- "Success"
        }
        # ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
      }
    }
    
    # --- この因子の結果をサマリーリストに格納 ---
    all_results_summary[[factor_name]] <- list(
      CFI = CFI_val, TLI = TLI_val, RMSEA = RMSEA_val, SRMR = SRMR_val,
      Omega = omega_val, Status = status_msg
    )
  }
  
  # 3. 最終結果のサマリー表示
  cat("\n\n\n########################################################\n")
  cat("############### 全因子の分析結果サマリー ###############\n")
  cat("########################################################\n\n")
  
  summary_df <- do.call(rbind, lapply(all_results_summary, function(x) as.data.frame(lapply(x, unlist))))
  numeric_cols <- c("CFI", "TLI", "RMSEA", "SRMR", "Omega")
  summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], function(x) round(x, 3))
  print(kable(summary_df, caption = "各因子の適合度指標と信頼性係数", align = 'c'))
  cat("\n--- レポート終了 ---\n")
  
  # sink()を閉じて、出力をコンソールに復元
  sink()
  
  cat("\nレポートが '", output_file, "' に正常に保存されました。\n")
}

# ===================================================================
# 関数の実行
# ===================================================================
run_and_save_full_report(
  file_path = file_path,
  factors_list = factors_to_analyze,
  reverse_items = reverse_items,
  max_score = max_score,
  filename_prefix = report_filename_prefix
)
