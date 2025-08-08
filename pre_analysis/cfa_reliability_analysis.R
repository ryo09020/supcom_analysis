# 必要なパッケージをインストールして読み込む
# install.packages("lavaan")
# install.packages("psych")
# install.packages("knitr")
# install.packages("tidySEM") # ★ これまでの代替として tidySEM をインストール
library(lavaan)
library(psych)
library(knitr)
library(tidySEM) # ★ tidySEM を読み込む
library(ggplot2) # ★ グラフの調整に使う

# ===================================================================
# パラメータ設定 (利用者はこの部分を編集してください)
# ===================================================================
# (この部分は変更ありません)
# 1. データが含まれるCSVファイルのパス
file_path <- "cfa_dummy.csv" 

# 2. 尺度の最大点（例: 5段階評価なら5）
max_score <- 5

# 3. 逆転項目リスト（尺度全体で逆転処理が必要な項目をすべて指定）
reverse_items <- c("N2", "N4", "E1", "E5", "A2", "A6", "C1", "C3")

# 4. 分析する因子の定義
factors_to_analyze <- list(
  Neuroticism = c("N1", "N2", "N3", "N4", "N5"),
  Extraversion = c("E1", "E2", "E3", "E4", "E5"),
  Openness = c("O1", "O2", "O3", "O4", "O5"),
  Agreeableness = c("A1", "A2", "A3", "A4", "A5", "A6"),
  Conscientiousness = c("C1", "C2", "C3", "C4", "C5")
)

# 5. パス図を描画するかどうか (TRUE = 描画する, FALSE = 描画しない)
create_plots <- TRUE

# ===================================================================
# 分析関数 (この部分は編集不要です)
# ===================================================================

#' 質問紙の複数因子における妥当性、信頼性、パス図を検証する関数
validate_multiple_factors <- function(file_path, factors_list, reverse_items, max_score, create_plots) {
  
  # (この部分は変更ありません)
  if (!file.exists(file_path)) { stop("指定されたファイルが見つかりません: ", file_path) }
  data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE)
  cat("CSVファイルの読み込みが完了しました。\n")
  if (length(reverse_items) > 0) {
    if (!all(reverse_items %in% names(data))) {
      missing_cols <- setdiff(reverse_items, names(data))
      stop("逆転項目として指定された列がデータ内に存在しません: ", paste(missing_cols, collapse=", "))
    }
    data[, reverse_items] <- (max_score + 1) - data[, reverse_items]
    cat("分析のため、メモリ上で逆転項目を処理しました:\n", paste(reverse_items, collapse = ", "), "\n")
  }
  all_results <- list()
  
  # --- 各因子についてループ処理 ---
  for (factor_name in names(factors_list)) {
    
    item_columns <- factors_list[[factor_name]]
    
    if (!all(item_columns %in% names(data))) {
      missing_cols <- setdiff(item_columns, names(data))
      cat("\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
      cat("警告: 因子 '", factor_name, "' の項目がデータに存在しません。スキップします。\n")
      cat("不足している項目: ", paste(missing_cols, collapse=", "), "\n")
      cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
      next
    }
    
    cat("\n========================================================\n")
    cat("因子:", factor_name, "の分析を開始します。\n")
    cat("========================================================\n")
    
    model_syntax <- paste(factor_name, "=~", paste(item_columns, collapse = " + "))
    cfa_fit <- try(cfa(model_syntax, data = data[, item_columns]), silent = TRUE)
    
    if (inherits(cfa_fit, "try-error")) {
      cat("\n--- 確認的因子分析 (CFA) でエラーが発生しました ---\n")
      cat(cfa_fit[1], "\n")
      all_results[[factor_name]] <- list(CFI=NA, TLI=NA, RMSEA=NA, SRMR=NA, Omega=NA, Status="CFA Error")
      next
    }
    
    fit_indices <- fitMeasures(cfa_fit, c("cfi", "tli", "rmsea", "srmr"))
    cat("\n--- 確認的因子分析 (CFA) の結果 ---\n")
    print(summary(cfa_fit, fit.measures = TRUE))
    
    # ▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼▼
    # 【変更点】パス図の描画を tidySEM に変更
    if (create_plots) {
      cat("\n--- パス図を描画します (tidySEMを使用) ---\n")
      try({
        # tidySEMでパス図のレイアウトを作成
        p <- graph_sem(model = cfa_fit)
        
        # 描画して表示
        print(p + ggtitle(paste("Path Diagram for", factor_name)))
        
      }, silent = TRUE)
    }
    # ▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲▲
    
    # (この部分は変更ありません)
    omega_results <- try(omega(data[, item_columns], nfactors = 1, fm = "ml"), silent = TRUE)
    if (inherits(omega_results, "try-error")) {
      cat("\n--- マクドナルドのω係数の計算でエラーが発生しました ---\n")
      cat(omega_results[1], "\n")
      omega_t_value <- NA
      status <- "Omega Error"
    } else {
      if (!is.null(omega_results$omega_t) && length(omega_results$omega_t) == 1) {
        omega_t_value <- omega_results$omega_t
        status <- "Success"
      } else {
        cat("\n--- マクドナルドのω係数の値が取得できませんでした ---\n")
        omega_t_value <- NA
        status <- "Omega Value Error"
      }
    }
    all_results[[factor_name]] <- list(
      CFI = fit_indices["cfi"], TLI = fit_indices["tli"],
      RMSEA = fit_indices["rmsea"], SRMR = fit_indices["srmr"],
      Omega = omega_t_value, Status = status
    )
  }
  
  # --- 最終結果のサマリー表示 ---
  cat("\n\n########################################################\n")
  cat("############### 全因子の分析結果サマリー ###############\n")
  cat("########################################################\n\n")
  
  summary_df <- do.call(rbind, lapply(all_results, function(x) as.data.frame(lapply(x, unlist))))
  numeric_cols <- c("CFI", "TLI", "RMSEA", "SRMR", "Omega")
  summary_df[numeric_cols] <- lapply(summary_df[numeric_cols], function(x) round(x, 3))
  print(kable(summary_df, caption = "各因子の適合度指標と信頼性係数"))
}

# ===================================================================
# 関数の実行 (この部分は編集不要です)
# ===================================================================
validate_multiple_factors(
  file_path = file_path,
  factors_list = factors_to_analyze,
  reverse_items = reverse_items,
  max_score = max_score,
  create_plots = create_plots
)