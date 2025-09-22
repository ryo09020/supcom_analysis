#################################################################
# LPAモデル比較検証スクリプト
#
# 目的：
# - モデル1、2、3、6でそれぞれ1-10プロファイルのLPAを実行
# - 全モデル・全プロファイル数の適合度指標を1つのCSVにまとめて出力
# - モデル選択のための包括的な比較表を作成
#################################################################

# ================================================================
# 🔧 設定変数
# ================================================================

# ★★★ 入力ファイルの設定 ★★★
INPUT_FILE <- "raw_data/dummy_data.csv"  # 分析したいCSVファイルのパス

# ★★★ 分析項目の設定 ★★★
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# ★★★ モデル・プロファイル数の設定 ★★★
MODELS_TO_COMPARE <- c(1, 2, 3, 6)  # 比較するモデル番号
PROFILE_RANGE <- 1:10  # 比較するプロファイル数の範囲

# ★★★ 出力設定 ★★★
OUTPUT_FILENAME <- "lpa_model_comparison_results.csv"  # 結果CSVファイル名
SHOW_DETAILED_OUTPUT <- TRUE  # 詳細な進行状況を表示するか

# ================================================================

# ---------------------------------------------------------------
# 1. パッケージ管理とセットアップ
# ---------------------------------------------------------------

#' パッケージの準備と読み込み
setup_packages <- function() {
  packages <- c("tidyverse", "tidyLPA", "knitr", "fmsb")
  lapply(packages, library, character.only = TRUE)
  cat("✅ パッケージの読み込みが完了しました。\n\n")
}

# ---------------------------------------------------------------
# 2. データ読み込みと準備
# ---------------------------------------------------------------

#' CSVファイルの読み込み
load_data <- function() {
  file_path <- INPUT_FILE
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("📁 ファイルパス:", file_path, "\n"))
  }
  
  if (!file.exists(file_path)) {
    stop(paste("❌ 指定されたファイル '", file_path, "' が見つかりません。", sep=""))
  }
  
  cat(paste("📖 ファイルを読み込み中:", basename(file_path), "\n"))
  data <- read_csv(file_path, show_col_types = FALSE)
  
  cat(paste("✅ データ読み込み完了。行数:", nrow(data), "、列数:", ncol(data), "\n\n"))
  
  return(list(data = data, file_path = file_path))
}

#' LPA分析項目の検証
select_lpa_variables <- function(data) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🔢 設定された分析項目を検証中...\n")
  }
  
  missing_cols <- TARGET_COLUMNS[!(TARGET_COLUMNS %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop(paste("❌ 指定された列が見つかりません:", paste(missing_cols, collapse = ", ")))
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("✅ 分析項目 (", length(TARGET_COLUMNS), "個):\n", sep = ""))
    for (col in TARGET_COLUMNS) {
      cat(paste("  - ", col, "\n", sep = ""))
    }
    cat("\n")
  }
  
  return(TARGET_COLUMNS)
}

#' LPA分析用データの準備
prepare_lpa_data <- function(data, selected_columns) {
  cat("🔧 LPA分析用データを準備中...\n")
  
  # 元のデータに行IDを付与
  df_original <- data %>%
    mutate(row_id = row_number())
  
  # LPA分析用のデータを作成
  df_for_lpa <- df_original %>%
    select(row_id, all_of(selected_columns)) %>%
    mutate(across(all_of(selected_columns), as.numeric)) %>%
    na.omit()
  
  # Zスコアに標準化
  df_to_scale <- df_for_lpa %>% select(-row_id)
  df_scaled <- as.data.frame(scale(df_to_scale))
  
  # row_idを再度結合
  df_analysis <- bind_cols(df_for_lpa %>% select(row_id), df_scaled)
  
  cat(paste("✅ 分析用データ準備完了。対象者:", nrow(df_analysis), "名\n"))
  cat(paste("   欠損値により除外:", nrow(data) - nrow(df_analysis), "名\n\n"))
  
  return(df_analysis %>% select(-row_id))
}

# ---------------------------------------------------------------
# 3. 複数モデルでのLPA実行
# ---------------------------------------------------------------

#' 単一モデルでの複数プロファイル数LPA実行
#' @param df_analysis 標準化された分析用データ
#' @param model_num モデル番号 (1, 2, 3, 6)
#' @param profile_range プロファイル数の範囲
#' @return LPAモデルと適合度指標
run_single_model_lpa <- function(df_analysis, model_num, profile_range = PROFILE_RANGE) {
  cat(paste("🧮 モデル", model_num, "で", min(profile_range), "から", max(profile_range), "プロファイルのLPAを実行中...\n"))
  
  tryCatch({
    lpa_models <- estimate_profiles(
      df_analysis,
      n_profiles = profile_range,
      models = model_num,
      boot_for_p = TRUE  # BLRT p-valueを計算
    )
    
    cat(paste("✅ モデル", model_num, "のLPA計算完了。\n"))
    return(lpa_models)
    
  }, error = function(e) {
    cat(paste("❌ モデル", model_num, "でエラーが発生:", e$message, "\n"))
    return(NULL)
  })
}

#' 全モデルでのLPA実行
#' @param df_analysis 標準化された分析用データ
#' @return 全モデルのLPA結果リスト
run_all_models_lpa <- function(df_analysis) {
  cat("🚀 複数モデルでのLPA実行を開始...\n")
  cat("⏳ BLRTの計算を含むため、時間がかかる場合があります。\n\n")
  
  all_results <- list()
  
  for (model_num in MODELS_TO_COMPARE) {
    cat(paste(rep("-", 40), collapse = ""), "\n")
    result <- run_single_model_lpa(df_analysis, model_num)
    if (!is.null(result)) {
      all_results[[paste0("model_", model_num)]] <- result
    }
    cat("\n")
  }
  
  cat("✅ 全モデルのLPA実行完了。\n\n")
  return(all_results)
}

# ---------------------------------------------------------------
# 4. 適合度指標の統合
# ---------------------------------------------------------------

#' 単一モデルの適合度指標とクラス所属割合を取得
#' @param lpa_models 単一モデルのLPA結果
#' @param model_num モデル番号
#' @return 適合度指標とクラス所属割合を含むデータフレーム
extract_model_results <- function(lpa_models, model_num) {
  cat(paste("📊 モデル", model_num, "の結果を抽出中...\n"))
  
  tryCatch({
    # 基本的な適合度指標を取得
    fit_indices <- get_fit(lpa_models)
    
    # モデル番号を追加
    fit_indices$Model_Number <- model_num
    
    # VLMR p値の計算
    if (nrow(fit_indices) > 1) {
      cat(paste("📊 モデル", model_num, "のVLMR p値を計算中...\n"))
      
      # サンプルサイズNを取得
      N <- lpa_models[[1]]$model$n
      
      # パラメータ数を取得
      npar_vec <- sapply(lpa_models, function(mod) {
        return(mod$model$df)
      })
      fit_indices$Parameters <- npar_vec
      
      # VLMR p値を初期化
      fit_indices$VLMR_p <- NA_real_
      
      # 2番目のモデルから最後のモデルまで比較
      for (k in 2:nrow(fit_indices)) {
        null_model <- fit_indices[k - 1, ]
        alt_model  <- fit_indices[k, ]
        
        lmr_result <- tidyLPA::calc_lrt(
          n = N,
          null_ll = null_model$LogLik,
          null_param = null_model$Parameters,
          null_classes = null_model$Classes,
          alt_ll = alt_model$LogLik,
          alt_param = alt_model$Parameters,
          alt_classes = alt_model$Classes
        )
        
        fit_indices$VLMR_p[k] <- lmr_result[4]
      }
    } else {
      fit_indices$VLMR_p <- NA_real_
      fit_indices$Parameters <- NA_integer_
    }
    
    # 各プロファイル数のクラス所属割合を計算
    class_proportions_list <- list()
    
    for(i in 1:length(lpa_models)) {
      profiles_num <- fit_indices$Classes[i]
      
      tryCatch({
        model_data <- get_data(lpa_models[[i]])
        if (!is.null(model_data) && "Class" %in% colnames(model_data)) {
          class_stats <- model_data %>%
            count(Class) %>%
            mutate(Percentage = round(n / sum(n) * 100))
          
          proportions_text <- paste(class_stats$Percentage, collapse = "/")
          
          class_proportions_list[[i]] <- data.frame(
            Classes = profiles_num,
            Class_Proportions = proportions_text,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        class_proportions_list[[i]] <- data.frame(
          Classes = profiles_num,
          Class_Proportions = "N/A",
          stringsAsFactors = FALSE
        )
      })
    }
    
    # クラス所属割合を結合
    if (length(class_proportions_list) > 0) {
      class_proportions <- do.call(rbind, class_proportions_list)
    } else {
      class_proportions <- data.frame(
        Classes = fit_indices$Classes,
        Class_Proportions = "N/A"
      )
    }
    
    # 最終結果を作成
    final_result <- fit_indices %>%
      left_join(class_proportions, by = "Classes") %>%
      select(
        Model_Number, Model, Classes, LogLik, AIC, BIC, SABIC, AWE, CAIC, CLC, KIC, ICL,
        Entropy, BLRT_p, VLMR_p, BLRT_val, prob_min, prob_max, n_min, n_max, 
        Parameters, Class_Proportions
      ) %>%
      mutate(
        across(c(LogLik, AIC, AWE, BIC, CAIC, CLC, KIC, SABIC, ICL), ~round(.x, 2)),
        across(c(Entropy, BLRT_p, VLMR_p, prob_min, prob_max), ~round(.x, 3)),
        across(c(BLRT_val), ~round(.x, 2)),
        across(c(Classes, Parameters, n_min, n_max), ~as.integer(.x))
      )
    
    cat(paste("✅ モデル", model_num, "の結果抽出完了。\n"))
    return(final_result)
    
  }, error = function(e) {
    cat(paste("❌ モデル", model_num, "の結果抽出でエラー:", e$message, "\n"))
    return(NULL)
  })
}

#' 全モデルの適合度指標を統合
#' @param all_lpa_results 全モデルのLPA結果
#' @return 統合された適合度比較表
create_comprehensive_comparison_table <- function(all_lpa_results) {
  cat("📊 全モデルの適合度指標を統合中...\n")
  
  all_model_results <- list()
  
  for (model_name in names(all_lpa_results)) {
    model_num <- as.numeric(str_extract(model_name, "\\d+"))
    lpa_models <- all_lpa_results[[model_name]]
    
    model_result <- extract_model_results(lpa_models, model_num)
    if (!is.null(model_result)) {
      all_model_results[[model_name]] <- model_result
    }
  }
  
  # 全結果を結合
  if (length(all_model_results) > 0) {
    comprehensive_table <- do.call(rbind, all_model_results)
    rownames(comprehensive_table) <- NULL
    
    # モデル番号とプロファイル数でソート
    comprehensive_table <- comprehensive_table %>%
      arrange(Model_Number, Classes)
    
    cat("✅ 包括的比較表の作成完了。\n\n")
    return(comprehensive_table)
  } else {
    cat("❌ 統合できる結果がありませんでした。\n")
    return(NULL)
  }
}

# ---------------------------------------------------------------
# 5. 結果の表示と保存
# ---------------------------------------------------------------

#' 比較表の表示と保存
#' @param comparison_table 包括的比較表
display_and_save_comprehensive_results <- function(comparison_table) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("📈 包括的モデル比較表:\n")
    cat(paste(rep("=", 80), collapse = ""), "\n")
    print(comparison_table)
    cat(paste(rep("=", 80), collapse = ""), "\n")
    
    # モデル別サマリー
    cat("\n💡 モデル別サマリー:\n")
    model_summary <- comparison_table %>%
      group_by(Model_Number) %>%
      summarise(
        Min_Profiles = min(Classes),
        Max_Profiles = max(Classes),
        Total_Models = n(),
        .groups = 'drop'
      )
    print(model_summary)
    
    cat("\n💡 'Class_Proportions' 列の見方:\n")
    cat("   各数値は「クラス1の割合/クラス2の割合/...」の形式で表示されています。\n")
    cat("   例: '30/45/25' = クラス1: 30%, クラス2: 45%, クラス3: 25%\n\n")
  }
  
  # CSVファイルとして保存
  write_csv(comparison_table, OUTPUT_FILENAME)
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("💾 包括的比較表が '", OUTPUT_FILENAME, "' として保存されました。\n", sep=""))
    cat(paste("   📊 ", nrow(comparison_table), " 行の結果が保存されました。\n"))
    cat(paste("   📋 含まれる列: ", paste(colnames(comparison_table), collapse = ", "), "\n\n"))
  }
}

# ---------------------------------------------------------------
# 6. メイン実行関数
# ---------------------------------------------------------------

#' LPAモデル比較検証のメイン実行関数
main_lpa_model_comparison <- function() {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🚀 LPAモデル比較検証開始\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  # 1. パッケージセットアップ
  setup_packages()
  
  # 2. データ読み込み
  data_info <- load_data()
  data <- data_info$data
  
  # 3. 分析項目選択
  selected_columns <- select_lpa_variables(data)
  
  # 4. LPA用データ準備
  df_analysis <- prepare_lpa_data(data, selected_columns)
  
  # 5. 全モデルでLPA実行
  all_lpa_results <- run_all_models_lpa(df_analysis)
  
  # 6. 包括的比較表作成
  comprehensive_table <- create_comprehensive_comparison_table(all_lpa_results)
  
  # 7. 結果表示・保存
  if (!is.null(comprehensive_table)) {
    display_and_save_comprehensive_results(comprehensive_table)
    
    if (SHOW_DETAILED_OUTPUT) {
      cat("🎉 LPAモデル比較検証が正常に完了しました！\n")
      cat(paste("📊 比較対象: モデル", paste(MODELS_TO_COMPARE, collapse = ", "), "\n"))
      cat(paste("📈 プロファイル数: ", min(PROFILE_RANGE), "-", max(PROFILE_RANGE), "\n"))
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
    
    return(list(
      comparison_table = comprehensive_table,
      all_results = all_lpa_results,
      selected_columns = selected_columns
    ))
  } else {
    cat("❌ 比較表の作成に失敗しました。\n")
    return(NULL)
  }
}

# ---------------------------------------------------------------
# 実行部分
# ---------------------------------------------------------------

# 🚀 メイン実行
cat("🔍 LPAモデル比較検証スクリプト\n")
cat(paste("📊 比較対象モデル: ", paste(MODELS_TO_COMPARE, collapse = ", "), "\n"))
cat(paste("📈 プロファイル数範囲: ", min(PROFILE_RANGE), "-", max(PROFILE_RANGE), "\n"))
cat(paste("💾 出力ファイル: ", OUTPUT_FILENAME, "\n\n"))

results <- main_lpa_model_comparison()