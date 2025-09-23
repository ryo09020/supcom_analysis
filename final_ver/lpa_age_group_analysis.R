#################################################################
# 年代別LPA分析スクリプト
#
# 目的：
# - 年齢層（10代、20代、30代...80代）ごとにデータを分割
# - 各年代でLPA（潜在プロファイル分析）を実行
# - 年代別のクラスター結果をCSVファイルとして保存
# - 年代間でのクラスタープロファイル比較を可能にする
#################################################################

# ================================================================
# 🔧 設定変数（ここで全ての設定を一括指定）
# ================================================================

# ★★★ 入力ファイルの設定 ★★★
INPUT_FILE <- "raw_data/dummy_data.csv"  # 分析したいCSVファイルのパス

# ★★★ 分析項目の設定 ★★★
# 分析に使用する列名を直接指定
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# ★★★ 年齢設定 ★★★
AGE_COLUMN <- "age"  # 年齢が格納されている列名
AGE_GROUPS <- list(
  "10代" = c(10, 19),
  "20代" = c(20, 29), 
  "30代" = c(30, 39),
  "40代" = c(40, 49),
  "50代" = c(50, 59),
  "60代" = c(60, 69),
  "70代" = c(70, 79),
  "80代" = c(80, 89)
)
MIN_SAMPLE_SIZE <- 30  # 年代別LPAに必要な最小サンプルサイズ

# ★★★ LPA設定 ★★★
PROFILE_RANGE <- 2:4  # 各年代で試すクラスター数の範囲
FINAL_CLUSTERS <- 3   # 最終的に使用するクラスター数

# ★★★ 出力設定 ★★★
OUTPUT_DIR <- "age_group_lpa_results"  # 結果を保存するディレクトリ
SAVE_COMPARISON_TABLES <- TRUE  # 年代別適合度比較表を保存するか
SHOW_DETAILED_OUTPUT <- TRUE    # 詳細な進行状況を表示するか

# ================================================================

# ---------------------------------------------------------------
# 1. パッケージ管理とセットアップ
# ---------------------------------------------------------------

#' パッケージの準備と読み込み
setup_packages <- function() {
  # 必要なパッケージのリスト
  packages <- c("tidyverse", "tidyLPA", "knitr", "fmsb")
  
  # パッケージの読み込み
  lapply(packages, library, character.only = TRUE)
  
  cat("✅ パッケージの読み込みが完了しました。\n\n")
}

# ---------------------------------------------------------------
# 2. 出力ディレクトリの作成
# ---------------------------------------------------------------

#' 出力ディレクトリの作成
create_output_directory <- function() {
  if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR, recursive = TRUE)
    cat(paste("📁 出力ディレクトリを作成しました:", OUTPUT_DIR, "\n"))
  } else {
    cat(paste("📁 出力ディレクトリが既に存在します:", OUTPUT_DIR, "\n"))
  }
  cat("\n")
}

# ---------------------------------------------------------------
# 3. データ読み込みと年代別分割
# ---------------------------------------------------------------

#' CSVファイルの読み込み
load_data <- function() {
  file_path <- INPUT_FILE
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("📁 ファイルパス:", file_path, "\n"))
  }
  
  # ファイルの存在確認
  if (!file.exists(file_path)) {
    stop(paste("❌ 指定されたファイル '", file_path, "' が見つかりません。", sep=""))
  }
  
  cat(paste("📖 ファイルを読み込み中:", basename(file_path), "\n"))
  data <- read_csv(file_path, show_col_types = FALSE)
  
  cat(paste("✅ データ読み込み完了。行数:", nrow(data), "、列数:", ncol(data), "\n\n"))
  
  return(data)
}

#' 分析項目の検証
validate_columns <- function(data) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🔢 設定された分析項目を検証中...\n")
  }
  
  # 年齢列の確認
  if (!AGE_COLUMN %in% colnames(data)) {
    stop(paste("❌ 年齢列 '", AGE_COLUMN, "' が見つかりません。", sep=""))
  }
  
  # 分析項目の確認
  missing_cols <- TARGET_COLUMNS[!(TARGET_COLUMNS %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop(paste("❌ 指定された列が見つかりません:", paste(missing_cols, collapse = ", ")))
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("✅ 年齢列:", AGE_COLUMN, "\n"))
    cat(paste("✅ 分析項目 (", length(TARGET_COLUMNS), "個):\n", sep = ""))
    for (col in TARGET_COLUMNS) {
      cat(paste("  - ", col, "\n", sep = ""))
    }
  }
  
  cat("\n")
  return(TRUE)
}

#' 年代別データ分割
split_data_by_age <- function(data) {
  cat("📊 年代別データ分割を実行中...\n")
  
  # 年齢データの前処理
  data_clean <- data %>%
    mutate(!!AGE_COLUMN := as.numeric(.data[[AGE_COLUMN]])) %>%
    filter(!is.na(.data[[AGE_COLUMN]]))
  
  # 年代別データのリスト
  age_group_data <- list()
  age_group_summary <- data.frame(
    年代 = character(),
    サンプル数 = integer(),
    年齢範囲_実際 = character(),
    LPA実行 = character(),
    stringsAsFactors = FALSE
  )
  
  for (group_name in names(AGE_GROUPS)) {
    min_age <- AGE_GROUPS[[group_name]][1]
    max_age <- AGE_GROUPS[[group_name]][2]
    
    # 年代に該当するデータを抽出
    group_data <- data_clean %>%
      filter(.data[[AGE_COLUMN]] >= min_age & .data[[AGE_COLUMN]] <= max_age)
    
    sample_size <- nrow(group_data)
    
    # 実際の年齢範囲
    if (sample_size > 0) {
      actual_min <- min(group_data[[AGE_COLUMN]], na.rm = TRUE)
      actual_max <- max(group_data[[AGE_COLUMN]], na.rm = TRUE)
      actual_range <- paste(actual_min, "〜", actual_max, "歳")
    } else {
      actual_range <- "データなし"
    }
    
    # サンプルサイズチェック
    will_analyze <- sample_size >= MIN_SAMPLE_SIZE
    
    age_group_data[[group_name]] <- list(
      data = group_data,
      sample_size = sample_size,
      will_analyze = will_analyze,
      min_age = min_age,
      max_age = max_age
    )
    
    # サマリーに追加
    age_group_summary <- rbind(age_group_summary, data.frame(
      年代 = group_name,
      サンプル数 = sample_size,
      年齢範囲_実際 = actual_range,
      LPA実行 = ifelse(will_analyze, "✅", "❌ (サンプル不足)"),
      stringsAsFactors = FALSE
    ))
  }
  
  # サマリー表示
  cat("\n📊 年代別データ分割結果:\n")
  cat("--------------------------------------------------\n")
  print(age_group_summary)
  cat("--------------------------------------------------\n")
  cat(paste("✅ LPA実行対象:", sum(age_group_summary$LPA実行 == "✅"), "年代\n"))
  cat(paste("❌ サンプル不足:", sum(age_group_summary$LPA実行 != "✅"), "年代\n"))
  cat(paste("   (最小サンプル数:", MIN_SAMPLE_SIZE, ")\n\n"))
  
  return(age_group_data)
}

# ---------------------------------------------------------------
# 4. 年代別LPA実行
# ---------------------------------------------------------------

#' 単一年代でのLPA分析用データ準備
prepare_age_group_lpa_data <- function(group_data, group_name) {
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("🔧 ", group_name, "のLPA分析用データを準備中...\n", sep=""))
  }
  
  # 元のデータに行IDを付与
  df_original <- group_data %>%
    mutate(row_id = row_number())
  
  # LPA分析用のデータを作成
  df_for_lpa <- df_original %>%
    select(row_id, all_of(TARGET_COLUMNS)) %>%
    mutate(across(all_of(TARGET_COLUMNS), as.numeric)) %>%
    na.omit()
  
  # Zスコアに標準化
  df_to_scale <- df_for_lpa %>% select(-row_id)
  df_scaled <- as.data.frame(scale(df_to_scale))
  
  # row_idを再度結合
  df_analysis <- bind_cols(df_for_lpa %>% select(row_id), df_scaled)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("   対象者:", nrow(df_analysis), "名\n"))
    cat(paste("   欠損値により除外:", nrow(group_data) - nrow(df_analysis), "名\n"))
  }
  
  return(list(
    original = df_original,
    for_lpa = df_for_lpa,
    analysis = df_analysis
  ))
}

#' 年代別LPA実行
run_age_group_lpa <- function(df_analysis, group_name) {
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("🧮 ", group_name, ": ", min(PROFILE_RANGE), "から", max(PROFILE_RANGE), "クラスターのLPAを実行中...\n", sep=""))
  }
  
  # LPA実行（row_idを除外）
  analysis_data <- df_analysis %>% select(-row_id)
  
  tryCatch({
    lpa_models <- estimate_profiles(
      analysis_data,
      n_profiles = PROFILE_RANGE,
      boot_for_p = TRUE,
      models = 6
    )
    
    if (SHOW_DETAILED_OUTPUT) {
      cat(paste("✅ ", group_name, "のLPA計算完了。\n", sep=""))
    }
    
    return(lpa_models)
    
  }, error = function(e) {
    cat(paste("❌ ", group_name, "のLPA計算でエラー: ", e$message, "\n", sep=""))
    return(NULL)
  })
}

#' 年代別適合度指標の比較表作成
create_age_group_comparison_table <- function(lpa_models, group_name) {
  if (is.null(lpa_models)) {
    return(NULL)
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("📊 ", group_name, "の適合度指標の比較表を作成中...\n", sep=""))
  }
  
  tryCatch({
    # 基本的な適合度指標を取得
    fit_indices <- get_fit(lpa_models)
    
    # VLMR p値の計算
    N <- lpa_models[[1]]$model$n
    npar_vec <- sapply(lpa_models, function(mod) {
      return(mod$model$df)
    })
    fit_indices$Parameters <- npar_vec
    fit_indices$VLMR_p <- NA_real_
    
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
    
    # 各クラスター数の実際の所属割合を計算
    class_proportions_list <- list()
    
    for(i in 1:length(lpa_models)) {
      model_name <- names(lpa_models)[i]
      profiles_num <- as.numeric(gsub("model_6_class_", "", model_name))
      
      tryCatch({
        model_data <- get_data(lpa_models[[i]])
        if (!is.null(model_data) && "Class" %in% colnames(model_data)) {
          class_stats <- model_data %>%
            count(Class) %>%
            mutate(Percentage = round(n / sum(n) * 100))
          
          proportions_text <- paste(class_stats$Percentage, collapse = "/")
          
          class_proportions_list[[i]] <- data.frame(
            Profiles = profiles_num,
            `% in each class` = proportions_text,
            stringsAsFactors = FALSE
          )
        }
      }, error = function(e) {
        class_proportions_list[[i]] <- data.frame(
          Profiles = profiles_num,
          `% in each class` = "N/A",
          stringsAsFactors = FALSE
        )
      })
    }
    
    # すべての所属割合を結合
    if (length(class_proportions_list) > 0) {
      class_proportions <- do.call(rbind, class_proportions_list)
      names(class_proportions)[2] <- "% in each class"
    } else {
      class_proportions <- data.frame(
        Profiles = fit_indices$Classes,
        `% in each class` = "N/A"
      )
    }
    
    # 年代情報を追加した最終的な比較表を作成
    final_comparison_table <- fit_indices %>%
      rename(
        Profiles = Classes,
        `Log-likelihood` = LogLik,
        `Sample-Size Adjusted BIC` = SABIC,
        `BLRT p-value` = BLRT_p,
        `VLMR p-value` = VLMR_p,
        `Prob Min` = prob_min,
        `Prob Max` = prob_max,
        `N Min` = n_min,
        `N Max` = n_max,
        `BLRT Value` = BLRT_val
      ) %>%
      left_join(class_proportions, by = "Profiles") %>%
      mutate(
        年代 = group_name,
        across(c(`Log-likelihood`, AIC, AWE, BIC, CAIC, CLC, KIC, `Sample-Size Adjusted BIC`, ICL), ~round(.x, 2)),
        across(c(Entropy, `BLRT p-value`, `VLMR p-value`, `Prob Min`, `Prob Max`), ~round(.x, 3)),
        across(c(`BLRT Value`), ~round(.x, 2)),
        across(c(Profiles, Parameters, `N Min`, `N Max`), ~as.integer(.x))
      ) %>%
      select(
        年代, Model, Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`, 
        AWE, CAIC, CLC, KIC, ICL, Entropy, `BLRT p-value`, `VLMR p-value`, 
        `BLRT Value`, `Prob Min`, `Prob Max`, `N Min`, `N Max`, Parameters,
        `% in each class`
      )
    
    if (SHOW_DETAILED_OUTPUT) {
      cat(paste("✅ ", group_name, "の比較表作成完了。\n", sep=""))
    }
    
    return(final_comparison_table)
    
  }, error = function(e) {
    cat(paste("❌ ", group_name, "の比較表作成エラー: ", e$message, "\n", sep=""))
    return(NULL)
  })
}

#' 年代別最終クラスターモデル取得
get_age_group_final_model <- function(lpa_models, group_name) {
  if (is.null(lpa_models)) {
    return(NULL)
  }
  
  target_pattern <- paste0("model_6_class_", FINAL_CLUSTERS)
  model_names <- names(lpa_models)
  model_index <- which(model_names == target_pattern)
  
  if (length(model_index) == 0) {
    cat(paste("❌ ", group_name, ": ", FINAL_CLUSTERS, "クラスターのモデルが見つかりません。\n", sep=""))
    return(NULL)
  }
  
  selected_model <- lpa_models[[model_index]]
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("✅ ", group_name, ": ", FINAL_CLUSTERS, "クラスターモデルを取得しました。\n", sep=""))
  }
  
  return(selected_model)
}

#' 年代別クラスター情報付与
assign_age_group_clusters <- function(original_data, df_for_lpa, lpa_model, group_name) {
  if (is.null(lpa_model)) {
    return(original_data)
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("🔗 ", group_name, ": 元データにクラスター情報を付与中...\n", sep=""))
  }
  
  # クラスター割り当て結果を取得
  lpa_results <- get_data(lpa_model)
  
  # row_idとクラスター番号を紐づけ
  results_with_id <- bind_cols(
    df_for_lpa %>% select(row_id), 
    lpa_results %>% select(Class)
  )
  
  # 元データにクラスター情報を結合
  df_final <- left_join(original_data, results_with_id, by = "row_id")
  
  # 性別列の変換（存在する場合）
  if ("性別" %in% colnames(df_final)) {
    df_final <- df_final %>%
      mutate(
        sex = case_when(
          性別 == "男性" | 性別 == "男" | 性別 == "M" | 性別 == "m" | 性別 == "Male" ~ 0,
          性別 == "女性" | 性別 == "女" | 性別 == "F" | 性別 == "f" | 性別 == "Female" ~ 1,
          TRUE ~ NA_real_
        )
      )
  }
  
  # クラスター番号でソート
  df_final_sorted <- df_final %>%
    arrange(Class)
  
  if (SHOW_DETAILED_OUTPUT) {
    # クラスター統計の表示
    cluster_summary <- df_final_sorted %>%
      filter(!is.na(Class)) %>%
      count(Class, name = "人数") %>%
      mutate(
        割合_パーセント = round(人数 / sum(人数) * 100, 2)
      )
    
    cat(paste("   ", group_name, "クラスター統計:\n", sep=""))
    print(cluster_summary)
    cat(paste("   総対象者数:", sum(cluster_summary$人数), "名\n"))
  }
  
  return(df_final_sorted)
}

# ---------------------------------------------------------------
# 5. 結果保存
# ---------------------------------------------------------------

#' 年代別結果の保存
save_age_group_results <- function(df_final, group_name, comparison_table = NULL) {
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("💾 ", group_name, "の結果を保存中...\n", sep=""))
  }
  
  # CSVファイル名を生成
  csv_filename <- paste0(group_name, "_lpa_results.csv")
  csv_path <- file.path(OUTPUT_DIR, csv_filename)
  
  # データを保存
  write_csv(df_final, csv_path)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("   ✅ ", csv_filename, " を保存しました。\n", sep=""))
  }
  
  # 適合度比較表の保存
  if (SAVE_COMPARISON_TABLES && !is.null(comparison_table)) {
    comparison_filename <- paste0(group_name, "_lpa_comparison.csv")
    comparison_path <- file.path(OUTPUT_DIR, comparison_filename)
    write_csv(comparison_table, comparison_path)
    
    if (SHOW_DETAILED_OUTPUT) {
      cat(paste("   ✅ ", comparison_filename, " を保存しました。\n", sep=""))
    }
  }
  
  return(csv_path)
}

# ---------------------------------------------------------------
# 6. 統合比較表の作成
# ---------------------------------------------------------------

#' 全年代の適合度比較表を統合
create_integrated_comparison_table <- function(all_comparison_tables) {
  if (length(all_comparison_tables) == 0) {
    return(NULL)
  }
  
  cat("📊 全年代の適合度比較表を統合中...\n")
  
  # 有効な比較表のみを結合
  valid_tables <- all_comparison_tables[!sapply(all_comparison_tables, is.null)]
  
  if (length(valid_tables) > 0) {
    integrated_table <- do.call(rbind, valid_tables)
    
    # 統合ファイルとして保存
    integrated_path <- file.path(OUTPUT_DIR, "all_age_groups_lpa_comparison.csv")
    write_csv(integrated_table, integrated_path)
    
    cat(paste("✅ 統合比較表を保存しました:", integrated_path, "\n\n"))
    return(integrated_table)
  }
  
  return(NULL)
}

# ---------------------------------------------------------------
# 7. メイン実行関数
# ---------------------------------------------------------------

#' 年代別LPA分析のメイン実行関数
main_age_group_lpa <- function() {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🚀 年代別LPA分析開始\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  # 1. パッケージセットアップ
  setup_packages()
  
  # 2. 出力ディレクトリ作成
  create_output_directory()
  
  # 3. データ読み込み
  data <- load_data()
  validate_columns(data)
  
  # 4. 年代別データ分割
  age_group_data <- split_data_by_age(data)
  
  # 結果保存用のリスト
  all_results <- list()
  all_comparison_tables <- list()
  successful_analyses <- 0
  
  # 5. 各年代でLPA実行
  for (group_name in names(age_group_data)) {
    group_info <- age_group_data[[group_name]]
    
    cat(paste("🎯 ", group_name, " の分析を開始...\n", sep=""))
    
    if (!group_info$will_analyze) {
      cat(paste("⏭️ ", group_name, ": サンプル数不足のためスキップ (n=", group_info$sample_size, ")\n\n", sep=""))
      next
    }
    
    # データ準備
    prepared_data <- prepare_age_group_lpa_data(group_info$data, group_name)
    
    # LPA実行
    lpa_models <- run_age_group_lpa(prepared_data$analysis, group_name)
    
    # 適合度比較表作成
    comparison_table <- create_age_group_comparison_table(lpa_models, group_name)
    all_comparison_tables[[group_name]] <- comparison_table
    
    # 最終モデル取得
    final_model <- get_age_group_final_model(lpa_models, group_name)
    
    # クラスター情報付与
    final_data <- assign_age_group_clusters(
      prepared_data$original,
      prepared_data$for_lpa,
      final_model,
      group_name
    )
    
    # 結果保存
    output_path <- save_age_group_results(final_data, group_name, comparison_table)
    
    # 結果をリストに保存
    all_results[[group_name]] <- list(
      data = final_data,
      model = final_model,
      comparison_table = comparison_table,
      output_path = output_path,
      sample_size = group_info$sample_size
    )
    
    successful_analyses <- successful_analyses + 1
    cat(paste("✅ ", group_name, " の分析完了\n\n", sep=""))
  }
  
  # 6. 統合比較表の作成
  integrated_comparison <- create_integrated_comparison_table(all_comparison_tables)
  
  # 7. 最終サマリー表示
  if (SHOW_DETAILED_OUTPUT) {
    cat("📊 年代別LPA分析完了サマリー\n")
    cat("--------------------------------------------------\n")
    cat(paste("✅ 分析成功:", successful_analyses, "年代\n"))
    cat(paste("📁 出力ディレクトリ:", OUTPUT_DIR, "\n"))
    cat(paste("📄 使用クラスター数:", FINAL_CLUSTERS, "\n"))
    cat(paste("📋 分析項目数:", length(TARGET_COLUMNS), "\n"))
    cat("--------------------------------------------------\n")
    
    # 成功した年代のリスト表示
    if (length(all_results) > 0) {
      cat("\n📝 分析成功年代とサンプル数:\n")
      for (group_name in names(all_results)) {
        sample_size <- all_results[[group_name]]$sample_size
        cat(paste("  - ", group_name, ": ", sample_size, "名\n", sep=""))
      }
    }
    
    cat(paste("\n🎉 年代別LPA分析が正常に完了しました！\n"))
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }
  
  return(list(
    results = all_results,
    integrated_comparison = integrated_comparison,
    successful_analyses = successful_analyses,
    output_directory = OUTPUT_DIR
  ))
}

# ---------------------------------------------------------------
# 実行部分
# ---------------------------------------------------------------

# 🚀 メイン実行
results <- main_age_group_lpa()

cat("🎯 年代別LPA分析スクリプトの実行が完了しました。\n")
cat(paste("📁 結果は '", OUTPUT_DIR, "' ディレクトリに保存されています。\n", sep=""))
