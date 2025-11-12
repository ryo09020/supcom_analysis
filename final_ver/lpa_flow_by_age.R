#################################################################
# LPA統合フロー Rスクリプト
#
# 目的：
# - 設定されたCSVファイルと分析項目でLPA（潜在プロファイル分析）を実行
# - クラスター数2-10の適合度指標比較表を作成・保存
# - 指定されたクラスター数で元データにクラスター番号を付加
# - 結果をCSVファイルとして保存
#################################################################

# ================================================================
# 🔧 設定変数（ここで全ての設定を一括指定）
# ================================================================

# ★★★ 入力ファイルの設定 ★★★
INPUT_FILE <- "raw_data/dummy_data.csv"  # 分析したいCSVファイルのパス

# ★★★ 分析項目の設定 ★★★
# 分析に使用する列名を直接指定
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# ★★★ クラスター数の設定 ★★★
PROFILE_RANGE <- 1:3  # 比較するクラスター数の範囲
FINAL_CLUSTERS <- 3  # 最終的に使用するクラスター数

# ★★★ tidyLPAのモデル設定 ★★★
# 1: 等分散・ゼロ共分散, 2: 等分散・等共分散, 3: 等分散・クラス別共分散,
# 4: クラス別分散・ゼロ共分散, 5: クラス別分散・等共分散, 6: クラス別分散・クラス別共分散
PROFILE_MODEL <- 1  # 使用するtidyLPAモデル番号（1-6など）

# ★★★ 出力設定 ★★★
OUTPUT_PREFIX <- ""  # 出力ファイル名の接頭辞（空文字の場合は元ファイル名ベース）
SAVE_COMPARISON_TABLE <- TRUE  # 適合度比較表をCSVで保存するか
COMPARISON_TABLE_FILENAME <- "lpa_comparison_table2.csv"  # 適合度比較表のファイル名

# ★★★ 年齢フィルタ設定 ★★★
AGE_COLUMN <- "age"  # 例: "age"（NULLの場合はフィルタ無効）
AGE_RANGE <- c(60, 70)  # 例: c(0, 50)（NULLの場合はフィルタ無効）

# ★★★ 出力ディレクトリ設定 ★★★
OUTPUT_DIRECTORY <- "outputs"  # 結果ファイルを保存するディレクトリ

# ★★★ 実行設定 ★★★
SHOW_DETAILED_OUTPUT <- TRUE  # 詳細な進行状況を表示するか

# ================================================================

# ---------------------------------------------------------------
# 1. パッケージ管理とセットアップ
# ---------------------------------------------------------------

#' パッケージの準備と読み込み
#' @description 必要なパッケージをインストール・読み込みする
setup_packages <- function() {
  
  # 必要なパッケージのリスト
  packages <- c("tidyverse", "tidyLPA", "knitr", "fmsb")
  
  # パッケージの読み込み
  lapply(packages, library, character.only = TRUE)
  
  cat("✅ パッケージの読み込みが完了しました。\n\n")
}

#' 出力ディレクトリの準備
#' @description 指定された保存先ディレクトリを作成する
#' @param dir_path 保存先ディレクトリ
#' @return 生成または確認されたディレクトリパス
ensure_output_directory <- function(dir_path = OUTPUT_DIRECTORY) {
  if (is.null(dir_path) || dir_path == "") {
    stop("❌ OUTPUT_DIRECTORY には保存先のパスを指定してください。")
  }
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste0("📂 出力ディレクトリを使用: ", normalizePath(dir_path, mustWork = FALSE), "\n\n"))
  }
  return(dir_path)
}

# ---------------------------------------------------------------
# 2. データ読み込みと項目選択
# ---------------------------------------------------------------

#' CSVファイルの読み込み
#' @description 設定されたファイルパスからデータを読み込む
#' @param file_path ファイルパス（NULLの場合は設定変数を使用）
#' @return 読み込まれたデータフレームとファイルパス
load_data <- function() {
  # 設定変数またはパラメータからファイルパスを決定
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
  
  return(list(data = data, file_path = file_path))
}

#' 年齢によるデータフィルタ
#' @description 指定された年齢範囲でデータを抽出する
#' @param data データフレーム
#' @return 年齢フィルタ適用後のデータフレーム
filter_data_by_age <- function(data) {
  if (is.null(AGE_COLUMN) || is.null(AGE_RANGE)) {
    if (SHOW_DETAILED_OUTPUT) {
      cat("ℹ️  年齢フィルタは設定されていません。全データを使用します。\n\n")
    }
    return(data)
  }
  if (!(AGE_COLUMN %in% colnames(data))) {
    stop(paste0("❌ 指定された年齢列 '", AGE_COLUMN, "' がデータ内に存在しません。"))
  }
  if (length(AGE_RANGE) != 2 || any(is.na(AGE_RANGE))) {
    stop("❌ AGE_RANGE には下限・上限の2要素を指定してください。")
  }
  age_limits <- sort(as.numeric(AGE_RANGE))
  if (any(is.na(age_limits))) {
    stop("❌ AGE_RANGE に数値以外が指定されています。")
  }
  age_values <- suppressWarnings(as.numeric(data[[AGE_COLUMN]]))
  if (all(is.na(age_values))) {
    stop("❌ 年齢列が数値に変換できません。フィルタを適用できません。")
  }
  available_mask <- !is.na(age_values)
  within_mask <- available_mask & age_values >= age_limits[1] & age_values <= age_limits[2]
  filtered_data <- data[within_mask, , drop = FALSE]
  excluded_by_range <- sum(available_mask & !within_mask)
  excluded_missing <- sum(!available_mask)
  if (nrow(filtered_data) == 0) {
    stop(paste0("❌ 年齢範囲 ", age_limits[1], "〜", age_limits[2], " 歳に該当するレコードがありません。"))
  }
  if (SHOW_DETAILED_OUTPUT) {
    cat("🎯 年齢フィルタを適用しました。\n")
    cat(paste0("   対象年齢: ", age_limits[1], "〜", age_limits[2], " 歳\n"))
    cat(paste0("   使用レコード: ", nrow(filtered_data), " 件\n"))
    cat(paste0("   年齢欠損で除外: ", excluded_missing, " 件\n"))
    cat(paste0("   年齢範囲外で除外: ", excluded_by_range, " 件\n\n"))
  }
  return(filtered_data)
}

#' LPA分析項目の検証
#' @description 設定された分析項目がデータに存在するかチェック
#' @param data データフレーム
#' @return 選択された列名のベクトル
select_lpa_variables <- function(data) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🔢 設定された分析項目を検証中...\n")
  }
  
  # 指定された列がデータに存在するかチェック
  missing_cols <- TARGET_COLUMNS[!(TARGET_COLUMNS %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop(paste("❌ 指定された列が見つかりません:", paste(missing_cols, collapse = ", ")))
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("✅ 分析項目 (", length(TARGET_COLUMNS), "個):\n", sep = ""))
    for (col in TARGET_COLUMNS) {
      cat(paste("  - ", col, "\t", sep = ""))
    }
    cat("\n")
  }
  
  return(TARGET_COLUMNS)
}

# ---------------------------------------------------------------
# 3. LPA実行とモデル比較
# ---------------------------------------------------------------

#' LPA分析用データの準備
#' @description 選択された項目でLPA用のデータを準備（標準化含む）
#' @param data 元データ
#' @param selected_columns 分析対象列名
#' @return 準備されたデータのリスト
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
  
  return(list(
    original = df_original,
    for_lpa = df_for_lpa,
    analysis = df_analysis
  ))
}

#' 複数クラスター数でのLPA実行
#' @description 指定範囲のクラスター数でLPAを実行し、適合度指標を計算
#' @param df_analysis 標準化された分析用データ
#' @param profile_range クラスター数の範囲（設定変数から取得）
#' @return LPAモデルと適合度指標
run_lpa_models <- function(df_analysis, profile_range = PROFILE_RANGE) {
  if (length(PROFILE_MODEL) != 1) {
    stop("PROFILE_MODEL には単一のモデル番号を指定してください（例: PROFILE_MODEL <- 1）。")
  }

  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("🧮 ", min(profile_range), "から", max(profile_range), "クラスターのLPAを実行中...\n", sep=""))
    cat("⏳ BLRTの計算を含むため、時間がかかる場合があります。\n\n")
  }
  
  # デバッグ情報を表示
  cat("🔍 分析データの確認:\n")
  cat(paste("   行数:", nrow(df_analysis), "、列数:", ncol(df_analysis), "\n"))
  cat(paste("   profile_range:", paste(profile_range, collapse = ", "), "\n"))
  cat(paste("   使用モデル番号 (tidyLPA):", paste(PROFILE_MODEL, collapse = ", "), "\n"))
  
  # LPA実行（row_idを除外）
  analysis_data <- df_analysis %>% select(-row_id)
  cat(paste("   LPA用データ列数:", ncol(analysis_data), "\n\n"))
  
  lpa_models <- estimate_profiles(
    analysis_data,
    n_profiles = profile_range,
    models = PROFILE_MODEL,
    boot_for_p = TRUE  # BLRT p-valueを計算
  )
  
  cat("✅ LPA計算完了。\n")
  cat(paste("🔍 lpa_modelsのクラス:", class(lpa_models), "\n\n"))
  
  return(lpa_models)
}

#' 適合度指標の比較表作成（実際の所属割合付き）
#' @description LPAモデルの適合度指標と各クラスの実際の所属率をまとめた表を作成
#' @param lpa_models LPAモデル
#' @return 比較表
create_comparison_table <- function(lpa_models) {
  cat("📊 適合度指標の比較表を作成中...\n")
  
  tryCatch({
  # 基本的な適合度指標を取得
  fit_indices <- get_fit(lpa_models)
    cat("✅ 適合度指標の取得完了。\n")

  # tidyLPAモデル番号を列として追加（複数指定時は先頭を使用）
  fit_indices$Model <- PROFILE_MODEL[1]


    # ===============================================================
    # ▼▼▼ ここからVLMR p値の計算ロジックを挿入 ▼▼▼
    # ===============================================================
    cat("📊 VLMR p値を計算中...\n")

    # サンプルサイズNをモデルオブジェクトから直接取得
    N <- lpa_models[[1]]$model$n

    # パラメータ数をモデルオブジェクトから直接取得
    npar_vec <- sapply(lpa_models, function(mod) {
      return(mod$model$df)
    })
    fit_indices$Parameters <- npar_vec

    # 結果を保存するための空の列（NAで埋める）を用意
    fit_indices$VLMR_p <- NA_real_

    # 2番目のモデルから最後のモデルまで、順番に比較
    for (k in 2:nrow(fit_indices)) {
      null_model <- fit_indices[k - 1, ]
      alt_model  <- fit_indices[k, ]

      # tidyLPA::calc_lrt を実行してp値を計算
      lmr_result <- tidyLPA::calc_lrt(
        n = N,
        null_ll = null_model$LogLik,
        null_param = null_model$Parameters,
        null_classes = null_model$Classes,
        alt_ll = alt_model$LogLik,
        alt_param = alt_model$Parameters,
        alt_classes = alt_model$Classes
      )
      
      # 結果（数値ベクトル）の4番目がp値
      fit_indices$VLMR_p[k] <- lmr_result[4]
      cat(names(lmr_result))
      cat(lmr_result)
      cat("ddddddddddddddddddddddd")
    }
    cat("✅ VLMR p値の計算完了。\n")
    
    # 各クラスター数の実際の所属割合を計算
    cat("📊 各クラスター数のクラス所属割合を計算中...\n")
    
    class_proportions_list <- list()
    
    # 各クラスター数について実際の所属割合を計算
    for(i in seq_along(lpa_models)) {
      # tidyLPAモデル名から正しいクラスター数を抽出
      model_name <- names(lpa_models)[i]
  profiles_num <- suppressWarnings(as.numeric(sub(".*_(\\\\d+)$", "\\1", model_name)))
      if (is.na(profiles_num)) {
        profiles_num <- fit_indices$Classes[i]
      }
      
      if (SHOW_DETAILED_OUTPUT) {
        cat(paste("   処理中:", model_name, "-> クラスター数:", profiles_num, "\n"))
      }
      
      tryCatch({
        # 各クラスター数モデルからデータを取得
        model_data <- get_data(lpa_models[[i]])
        if (!is.null(model_data) && "Class" %in% colnames(model_data)) {
          class_stats <- model_data %>%
            count(Class) %>%
            mutate(Percentage = round(n / sum(n) * 100))
          
          proportions_text <- paste(class_stats$Percentage, collapse = "/")
          
          model_number <- suppressWarnings(as.numeric(sub("model_(\\\\d+)_.*", "\\1", model_name)))
          if (is.na(model_number)) {
            model_number <- PROFILE_MODEL[1]
          }

          class_proportions_list[[i]] <- data.frame(
            Model = model_number,
            Profiles = profiles_num,
            `% in each class` = proportions_text,
            stringsAsFactors = FALSE
          )
          
          if (SHOW_DETAILED_OUTPUT) {
            cat(paste("   ", profiles_num, "クラスター: ", proportions_text, "\n"))
          }
        }
      }, error = function(e) {
        cat(paste("⚠️ ", profiles_num, "クラスターの割合計算でエラー:", e$message, "\n"))
        class_proportions_list[[i]] <- data.frame(
          Model = PROFILE_MODEL[1],
          Profiles = profiles_num,
          `% in each class` = "N/A",
          stringsAsFactors = FALSE
        )
      })
    }
    
    # すべての所属割合を結合
    if (length(class_proportions_list) > 0) {
      class_proportions <- do.call(rbind, class_proportions_list)
      colnames(class_proportions) <- c("Model", "Profiles", "% in each class")
    } else {
      class_proportions <- data.frame(
        Model = fit_indices$Model,
        Profiles = fit_indices$Classes,
        `% in each class` = "N/A"
      )
    }
    
    # 最終的な比較表を作成
    final_comparison_table <- fit_indices %>%
      rename(
        Profiles = Classes,
        `Log-likelihood` = LogLik,
        `Sample-Size Adjusted BIC` = SABIC,
        `BLRT p-value` = BLRT_p,
        `VLMR p-value` = VLMR_p
      ) %>%
      select(
        Model, Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`,
        Entropy, `BLRT p-value`, `VLMR p-value`
      ) %>%
      left_join(class_proportions, by = c("Model", "Profiles")) %>%
      mutate(
        across(c(`Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`), ~round(.x, 2)),
        across(c(Entropy, `BLRT p-value`, `VLMR p-value`), ~round(.x, 3))
      )
    
    cat("✅ 実際の所属割合を含む比較表の作成完了。\n\n")
    return(final_comparison_table)
    
  }, error = function(e) {
    cat("❌ 比較表作成エラー:", e$message, "\n")
    return(NULL)
  })
}

#' 適合度指標の表示と保存
#' @description 比較表を表示し、設定に応じてCSVファイルとして保存
#' @param comparison_table 比較表
display_and_save_comparison <- function(comparison_table, output_dir = OUTPUT_DIRECTORY) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("📈 適合度指標の比較表:\n")
    cat("--------------------------------------------------\n")
    print(comparison_table)
    cat("--------------------------------------------------\n")
    
    # クラス所属割合の詳細説明を追加
    if ("% in each class" %in% colnames(comparison_table)) {
      cat("\n💡 '% in each class' 列の見方:\n")
      cat("   各数値は「クラス1の割合/クラス2の割合/...」の形式で表示されています。\n")
      cat("   例: '30/45/25' = クラス1: 30%, クラス2: 45%, クラス3: 25%\n\n")
    }
  }
  
  # CSVファイルとして保存（設定に応じて）
  if (SAVE_COMPARISON_TABLE) {
    target_path <- COMPARISON_TABLE_FILENAME
    if (dirname(target_path) %in% c(".", "")) {
      output_path <- file.path(output_dir, target_path)
    } else {
      output_path <- target_path
      dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    }
    write_csv(comparison_table, output_path)
    if (SHOW_DETAILED_OUTPUT) {
      cat(paste0("💾 適合度比較表が '", normalizePath(output_path, mustWork = FALSE), "' として保存されました。\n"))
      cat(paste("   📊 クラス所属割合（% in each class）を含む比較表が保存されました。\n"))
      cat(paste("   📋 含まれる列: ", paste(colnames(comparison_table), collapse = ", "), "\n\n"))
    }
  }
}

# ---------------------------------------------------------------
# 4. クラスター数選択とクラスター付与
# ---------------------------------------------------------------

#' 最終クラスター数の決定
#' @description 設定されたクラスター数を検証して返す
#' @param comparison_table 適合度比較表
#' @return 選択されたクラスター数
select_final_clusters <- function(comparison_table) {
  # 設定されたクラスター数が利用可能な範囲にあるかチェック
  if (!(FINAL_CLUSTERS %in% comparison_table$Profiles)) {
    stop(paste("❌ 設定されたクラスター数", FINAL_CLUSTERS, "は利用可能な範囲にありません。"))
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("🎯 設定されたクラスター数を使用:", FINAL_CLUSTERS, "\n\n"))
  }
  
  return(FINAL_CLUSTERS)
}

#' 選択されたクラスター数モデルを既存のLPA結果から取得
#' @description 既存のLPAモデルから指定されたクラスター数の結果を抽出（再実行なし）
#' @param lpa_models 複数クラスター数のLPAモデル
#' @param n_clusters 選択されたクラスター数
#' @return 選択されたクラスター数のLPAモデル
get_selected_model <- function(lpa_models, n_clusters) {
  cat(paste("🎯 既存のLPA結果から", n_clusters, "クラスターモデルを取得中...\n", sep=""))
  
  # LPAモデルの構造を確認
  model_names <- names(lpa_models)
  cat(paste("🔍 利用可能なモデル名:", paste(model_names, collapse = ", "), "\n"))
  
  # tidyLPAの命名規則に従ってモデルを検索
  target_pattern <- paste0("model_", PROFILE_MODEL[1], "_class_", n_clusters)
  model_index <- which(model_names == target_pattern)
  
  if (length(model_index) == 0) {
    # 代替パターンを試行
    alt_patterns <- c(
      as.character(n_clusters),
      paste0("class_", n_clusters),
      paste0(n_clusters, "_class")
    )
    
    for (pattern in alt_patterns) {
      model_index <- which(model_names == pattern)
      if (length(model_index) > 0) break
    }
  }
  
  if (length(model_index) == 0) {
    stop(paste("❌ ", n_clusters, "クラスターのモデルが見つかりません。利用可能なモデル名: ", 
               paste(model_names, collapse = ", "), sep=""))
  }
  
  selected_model <- lpa_models[[model_index]]
  cat(paste("✅ モデル '", model_names[model_index], "' を取得しました。\n", sep=""))
  
  # 統計情報の表示
  tryCatch({
    fit_indices <- get_fit(selected_model)
    class_stats <- get_data(selected_model) %>%
      count(Class) %>%
      mutate(Percentage = round(n / sum(n) * 100, 2)) %>%
      rename(N = n)
    
    cat("\n--------------------------------------------------\n")
    cat(paste("--- ", n_clusters, "-Cluster Model: 既存結果からの情報 ---\n", sep=""))
    cat("\n[Fit Indices]\n")
    print(fit_indices %>% select(LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
    cat("\n[Class Membership Distribution]\n")
    print(class_stats)
    
    # 各クラスの所属確率の詳細表示
    cat("\n[Detailed Class Probability Distribution]\n")
    for(i in 1:nrow(class_stats)) {
      class_num <- class_stats$Class[i]
      class_n <- class_stats$N[i]
      class_pct <- class_stats$Percentage[i]
      cat(paste("クラス", class_num, ": ", class_n, "名 (", class_pct, "%)\n", sep=""))
    }
    cat("--------------------------------------------------\n\n")
    
  }, error = function(e) {
    cat("⚠️ 統計情報の表示中にエラーが発生しましたが、モデルは正常に取得されました。\n")
    cat("エラー詳細:", e$message, "\n\n")
  })
  
  return(selected_model)
}

#' クラスター情報の元データへの付与
#' @description LPA結果を元データに結合する
#' @param original_data 元データ
#' @param df_for_lpa LPA用データ（row_id付き）
#' @param lpa_model LPAモデル
#' @return クラスター情報付きデータ
assign_clusters_to_data <- function(original_data, df_for_lpa, lpa_model) {
  cat("🔗 元データにクラスター情報を付与中...\n")
  
  # クラスター割り当て結果を取得
  lpa_results <- get_data(lpa_model)
  
  # row_idとクラスター番号を紐づけ
  results_with_id <- bind_cols(
    df_for_lpa %>% select(row_id), 
    lpa_results %>% select(Class)
  )
  
  # 元データにクラスター情報を結合
  df_final <- left_join(original_data, results_with_id, by = "row_id")
  
  # クラスター番号でソート
  df_final_sorted <- df_final %>%
    arrange(Class)
  
  # クラスター統計の表示
  if (SHOW_DETAILED_OUTPUT) {
    cat("\n📊 最終的なクラスター統計:\n")
    cluster_summary <- df_final_sorted %>%
      filter(!is.na(Class)) %>%
      count(Class, name = "人数") %>%
      mutate(
        割合_パーセント = round(人数 / sum(人数) * 100, 2),
        累積_パーセント = round(cumsum(人数) / sum(人数) * 100, 2)
      )
    print(cluster_summary)
    cat(paste("\n   総対象者数:", sum(cluster_summary$人数), "名\n"))
    cat(paste("   欠損により除外:", nrow(df_final_sorted) - sum(cluster_summary$人数), "名\n\n"))
  }
  
  cat("✅ クラスター情報の付与完了。\n\n")
  
  return(df_final_sorted)
}

#' 結果の保存
#' @description クラスター情報付きデータをCSVファイルとして保存
#' @param df_final 最終データ
#' @param original_file_path 元ファイルのパス
save_final_results <- function(df_final, original_file_path, output_dir = OUTPUT_DIRECTORY) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("💾 結果を保存中...\n")
  }
  
  # 出力ファイル名を生成
  if (is.null(output_dir) || output_dir == "") {
    stop("❌ OUTPUT_DIRECTORY には保存先のパスを指定してください。")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  base_name <- tools::file_path_sans_ext(basename(original_file_path))
  
  if (OUTPUT_PREFIX != "") {
    output_filename <- paste0(OUTPUT_PREFIX, "_", base_name, "_with_clusters_sorted.csv")
  } else {
    output_filename <- paste0(base_name, "_with_clusters_sorted.csv")
  }
  
  output_path <- file.path(output_dir, output_filename)
  
  # CSVファイルとして保存
  write_csv(df_final, output_path)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("✅ クラスター情報付きファイルが保存されました。\n")
    cat(paste0("   📁 保存先: ", normalizePath(output_path, mustWork = FALSE), "\n\n"))
  }
  
  return(output_path)
}

# ---------------------------------------------------------------
# 5. 年齢分析（オプション）
# ---------------------------------------------------------------

#' クラスターごとの年齢分析
#' @description クラスターごとの平均年齢を算出（年齢データがある場合）
#' @param df_final 最終データ
analyze_age_by_cluster <- function(df_final) {
  cat("📊 クラスターごとの年齢分析を実行中...\n")
  
  # 年齢データの確認
  age_columns <- c("参加時年齢", "受信時年齢", "年齢")
  available_age_columns <- age_columns[age_columns %in% colnames(df_final)]
  
  if (length(available_age_columns) > 0) {
    cat("\n--------------------------------------------------\n")
    cat("--- Cluster-wise Average Age Analysis ---\n\n")
    
    for (age_col in available_age_columns) {
      cat(paste("[", age_col, "の平均値]\n", sep=""))
      
      # クラスターごとの平均年齢を計算
      age_stats <- df_final %>%
        filter(!is.na(Class) & !is.na(.data[[age_col]])) %>%
        group_by(Class) %>%
        summarise(
          N = n(),
          Mean_Age = round(mean(.data[[age_col]], na.rm = TRUE), 2),
          SD_Age = round(sd(.data[[age_col]], na.rm = TRUE), 2),
          Min_Age = min(.data[[age_col]], na.rm = TRUE),
          Max_Age = max(.data[[age_col]], na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(Class)
      
      # 結果を表示
      print(age_stats)
      cat("\n")
    }
    
    # 全体の平均年齢も表示
    for (age_col in available_age_columns) {
      overall_mean <- round(mean(df_final[[age_col]], na.rm = TRUE), 2)
      cat(paste("全体の", age_col, "平均: ", overall_mean, "歳\n", sep=""))
    }
    
    cat("--------------------------------------------------\n\n")
    
  } else {
    cat("ℹ️  年齢データが見つかりませんでした。\n\n")
  }
}

# ---------------------------------------------------------------
# 6. メイン実行関数
# ---------------------------------------------------------------

#' LPA統合フローのメイン実行関数
#' @description 全体のワークフローを統合して実行
#' @param input_file 入力ファイルパス（NULLの場合は設定変数または対話的選択）
main_lpa_flow <- function() {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🚀 LPA統合フロー開始\n")
    cat(paste(rep("=", 50), collapse = ""), "\n\n")
  }
  
  # 1. パッケージセットアップ
  setup_packages()
  output_dir <- ensure_output_directory()
  
  # 2. データ読み込み
  data_info <- load_data()
  data <- data_info$data
  file_path <- data_info$file_path
  data <- filter_data_by_age(data)
  
  # 3. 分析項目選択
  selected_columns <- select_lpa_variables(data)
  
  # 4. LPA用データ準備
  prepared_data <- prepare_lpa_data(data, selected_columns)
  
  # 5. 複数クラスター数でLPA実行
  lpa_models <- run_lpa_models(prepared_data$analysis)
  
  # 6. 適合度比較表作成・表示
  comparison_table <- create_comparison_table(lpa_models)
  if (is.null(comparison_table)) {
    stop("❌ 適合度比較表の作成に失敗しました。詳細メッセージを確認してください。")
  }
  display_and_save_comparison(comparison_table, output_dir)
  
  # 7. 最終クラスター数選択
  chosen_clusters <- select_final_clusters(comparison_table)
  
  # 8. 既存のLPA結果から選択されたクラスター数のモデルを取得（再実行なし）
  final_lpa_model <- get_selected_model(lpa_models, chosen_clusters)
  
  # 9. クラスター情報を元データに付与
  final_data <- assign_clusters_to_data(
    prepared_data$original, 
    prepared_data$for_lpa, 
    final_lpa_model
  )
  
  # 10. 結果保存
  output_path <- save_final_results(final_data, file_path, output_dir)
  
  # 11. 年齢分析（オプション）
  analyze_age_by_cluster(final_data)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("🎉 LPA統合フローが正常に完了しました！\n")
    cat(paste(rep("=", 50), collapse = ""), "\n")
  }
  
  return(list(
    final_data = final_data,
    lpa_model = final_lpa_model,
    comparison_table = comparison_table,
    output_path = output_path,
    selected_columns = selected_columns,
    output_directory = output_dir
  ))
}

# ---------------------------------------------------------------
# 実行例とメイン実行
# ---------------------------------------------------------------

# ✅ 修正により以下の関数は不要になりました
# add_class_proportions_to_csv関数は削除されました
# 現在は一つのLPA実行結果を一貫して使用するため、後処理は不要です

# 🚀 メイン実行（設定変数に基づく完全自動実行）
results <- main_lpa_flow()

# ✅ 修正完了：適合度比較表と最終CSVで同一のLPA結果を使用
cat("🎯 修正により、比較表と最終CSVで一貫した結果が保証されました。\n")