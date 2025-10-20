#################################################################
# PCA（主成分分析）統合スクリプト
#
# 目的：
# - 設定されたCSVファイルと分析項目でPCAを実行
# - 主成分の寄与率、因子負荷量を算出・表示
# - バイプロット、スクリープロット、因子負荷量ヒートマップを作成
# - 主成分得点を元データに付加してCSVファイルとして保存
#################################################################

# ================================================================
# 🔧 設定変数（ここで全ての設定を一括指定）
# ================================================================

# ★★★ 入力ファイルの設定 ★★★
INPUT_FILE <- "../raw_data/dummy_data.csv"  # 分析したいCSVファイルのパス

# ★★★ 分析項目の設定 ★★★
# PCAに使用する列名を直接指定
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# ★★★ 分析パラメータの設定 ★★★
N_COMPONENTS <- 5  # 抽出する主成分数（最大値、実際はデータに依存）
ROTATION_METHOD <- "none"  # 回転法（"none", "varimax", "promax" など）
STANDARDIZE_DATA <- TRUE  # データを標準化するか

# ★★★ 出力設定 ★★★
OUTPUT_PREFIX <- "pca"  # 出力ファイル名の接頭辞
SAVE_PLOTS <- TRUE  # プロットを保存するか
SAVE_RESULTS_CSV <- TRUE  # 結果をCSVで保存するか
PLOT_WIDTH <- 10  # プロットの幅（インチ）
PLOT_HEIGHT <- 8  # プロットの高さ（インチ）
PLOT_DPI <- 300  # プロットの解像度

# ★★★ 表示設定 ★★★
SHOW_DETAILED_OUTPUT <- TRUE  # 詳細な進行状況を表示するか
SHOW_LOADINGS_THRESHOLD <- 0.3  # 因子負荷量の表示閾値

# ★★★ MMSE 可視化設定 ★★★
MMSE_COLUMN <- "Mini-Mental State Exam"  # MMSEスコア列名
AGE_COLUMN <- "Age"  # 年齢列名
MMSE_BREAKS <- c(0, 0.5, 1, 3)  # t-SNEスクリプトと同様のカテゴリ境界
MMSE_LABELS <- c("Low (≤23)", "Medium (24-27)", "High (≥28)")
MMSE_COLOR_PALETTE <- c("#E31A1C", "#FF7F00", "#1F78B4")
MMSE_MISSING_COLOR <- "#6C757D"

invisible(utils::globalVariables(c(
  "row_id", "mmse_category", "mmse_value", "count", "PC1", "PC2"
)))

# ================================================================

# ---------------------------------------------------------------
# 1. ユーティリティ
# ---------------------------------------------------------------

#' MMSE列の数値化とカテゴリ変換を実施
prepare_mmse_info <- function(data) {
  if (!(MMSE_COLUMN %in% names(data))) {
    stop(sprintf("入力データに MMSE 列 '%s' がありません。", MMSE_COLUMN))
  }
  if (!(AGE_COLUMN %in% names(data))) {
    stop(sprintf("入力データに年齢列 '%s' がありません。", AGE_COLUMN))
  }

  mmse_raw <- data[[MMSE_COLUMN]]
  mmse_chr <- trimws(as.character(mmse_raw))
  mmse_chr[mmse_chr == ""] <- NA_character_
  mmse_numeric <- suppressWarnings(as.numeric(mmse_chr))
  invalid_numeric <- !is.na(mmse_chr) & is.na(mmse_numeric)

  if (any(invalid_numeric, na.rm = TRUE)) {
    cat(sprintf("⚠️ MMSE列で数値に変換できなかった値: %d件 → NAとして扱います。\n",
                sum(invalid_numeric, na.rm = TRUE)))
  }

  age_raw <- data[[AGE_COLUMN]]
  age_chr <- trimws(as.character(age_raw))
  age_chr[age_chr == ""] <- NA_character_
  age_numeric <- suppressWarnings(as.numeric(age_chr))
  invalid_age <- !is.na(age_chr) & is.na(age_numeric)

  if (any(invalid_age, na.rm = TRUE)) {
    cat(sprintf("⚠️ 年齢列で数値に変換できなかった値: %d件 → 年齢条件の計算から除外します。\n",
                sum(invalid_age, na.rm = TRUE)))
  }

  missing_mmse_young <- is.na(mmse_numeric) & !is.na(age_numeric) & age_numeric <= 64
  filled_young <- sum(missing_mmse_young, na.rm = TRUE)

  if (filled_young > 0) {
    cat(sprintf("ℹ️ 64歳以下の欠損MMSEを満点(30)で補完: %d件\n", filled_young))
    mmse_numeric[missing_mmse_young] <- 30
  }

  expected_breaks <- length(MMSE_LABELS) + 1
  if (length(MMSE_BREAKS) != expected_breaks) {
    stop(sprintf("MMSE_BREAKS はラベル数+1 個の値が必要です (現在: %d, 期待: %d)",
                 length(MMSE_BREAKS), expected_breaks))
  }

  adjusted_breaks <- MMSE_BREAKS
  last_break <- tail(adjusted_breaks, 1)
  if (!is.infinite(last_break) && any(mmse_numeric > last_break, na.rm = TRUE)) {
    adjusted_breaks[length(adjusted_breaks)] <- Inf
  }

  mmse_category <- cut(
    mmse_numeric,
    breaks = adjusted_breaks,
    labels = MMSE_LABELS,
    include.lowest = TRUE,
    right = TRUE
  )

  list(
    mapping = tibble::tibble(
      row_id = data$row_id,
      mmse_value = mmse_numeric,
      mmse_category = mmse_category
    ),
    invalid_numeric = sum(invalid_numeric, na.rm = TRUE),
    invalid_age = sum(invalid_age, na.rm = TRUE),
    filled_young = filled_young
  )
}

# ---------------------------------------------------------------
# 2. パッケージ管理とセットアップ
# ---------------------------------------------------------------

#' パッケージの読み込み（スーパーコンピューター用）
setup_packages <- function() {
  packages <- c("tidyverse", "psych", "corrplot", "factoextra", "FactoMineR", 
                "pheatmap", "ggplot2", "gridExtra", "RColorBrewer")
  
  cat("📦 必要なパッケージを読み込み中...\n")
  cat("必要パッケージ:", paste(packages, collapse = ", "), "\n\n")
  
  # パッケージの読み込み（警告を抑制）
  tryCatch({
    suppressMessages({
      suppressWarnings({
        lapply(packages, library, character.only = TRUE)
      })
    })
    cat("✅ パッケージの読み込みが完了しました。\n\n")
  }, error = function(e) {
    cat("❌ パッケージ読み込みエラー:", e$message, "\n")
    cat("💡 以下のパッケージが必要です（事前にインストールしてください）:\n")
    cat(paste(" -", packages, collapse = "\n"))
    cat("\n")
    stop("必要なパッケージが読み込めませんでした。")
  })
}

# ---------------------------------------------------------------
# 3. データ読み込みと準備
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

#' PCA分析項目の検証
select_pca_variables <- function(data) {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🔢 設定された分析項目を検証中...\n")
  }
  
  # 重複チェックを追加
  if (length(TARGET_COLUMNS) != length(unique(TARGET_COLUMNS))) {
    duplicated_items <- TARGET_COLUMNS[duplicated(TARGET_COLUMNS)]
    cat("⚠️  警告: 重複する項目が検出されました:\n")
    for (item in unique(duplicated_items)) {
      count <- sum(TARGET_COLUMNS == item)
      cat(paste("   '", item, "' が ", count, " 回指定されています\n", sep=""))
    }
    cat("\n")
    
    # ユーザーに確認を求める
    cat("💡 このまま続行しますか？重複項目は結果に偏りを生じさせる可能性があります。\n")
    cat("   重複を除去する場合は、スクリプトを停止してTARGET_COLUMNSを修正してください。\n\n")
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

#' PCA分析用データの準備
prepare_pca_data <- function(data, selected_columns) {
  cat("🔧 PCA分析用データを準備中...\n")
  
  # 元のデータに行IDを付与
  df_original <- data %>%
    mutate(row_id = row_number())

  # MMSE列の前処理
  mmse_info <- prepare_mmse_info(df_original)
  
  # PCA分析用のデータを作成
  df_for_pca <- df_original %>%
    select(row_id, all_of(selected_columns)) %>%
    mutate(across(all_of(selected_columns), as.numeric)) %>%
    na.omit()
  
  # 標準化の実行（設定に応じて）
  if (STANDARDIZE_DATA) {
    cat("📊 データを標準化中...\n")
    df_to_scale <- df_for_pca %>% select(-row_id)
    df_scaled <- as.data.frame(scale(df_to_scale))
    df_analysis <- bind_cols(df_for_pca %>% select(row_id), df_scaled)
    cat("✅ 標準化完了。\n")
  } else {
    df_analysis <- df_for_pca
    cat("ℹ️  標準化をスキップしました。\n")
  }
  
  # 相関行列での異常値検出（重複項目の検出）
  analysis_data <- df_analysis %>% select(-row_id)
  cor_matrix <- cor(analysis_data)
  
  # 完全相関（r=1.0）のペアを検出（対角線以外で）
  perfect_cors <- which(abs(cor_matrix) > 0.999 & cor_matrix != 1, arr.ind = TRUE)
  
  if (nrow(perfect_cors) > 0) {
    cat("⚠️  警告: ほぼ完全相関（r > 0.999）のペアが検出されました:\n")
    for (i in 1:nrow(perfect_cors)) {
      row_idx <- perfect_cors[i, "row"]
      col_idx <- perfect_cors[i, "col"]
      cor_val <- cor_matrix[row_idx, col_idx]
      cat(sprintf("   %s vs %s: r = %.4f\n",
                  colnames(cor_matrix)[row_idx],
                  colnames(cor_matrix)[col_idx],
                  cor_val))
    }
    cat("💡 これらは同一項目または極めて類似した項目の可能性があります。\n\n")
  }
  
  cat(paste("✅ 分析用データ準備完了。対象者:", nrow(df_analysis), "名\n"))
  cat(paste("   欠損値により除外:", nrow(data) - nrow(df_analysis), "名\n\n"))
  
  row_mapping <- df_analysis %>%
    select(row_id) %>%
    left_join(mmse_info$mapping, by = "row_id")

  if (SHOW_DETAILED_OUTPUT) {
    mmse_summary <- row_mapping %>%
      dplyr::mutate(mmse_category = forcats::fct_explicit_na(.data$mmse_category, na_level = "Missing")) %>%
      dplyr::count(mmse_category, name = "count") %>%
      dplyr::arrange(dplyr::desc(.data$count))

    cat("📐 MMSEカテゴリ内訳 (PCA対象データのみ):\n")
    print(mmse_summary, n = nrow(mmse_summary))
    cat("\n")
  }

  return(list(
    original = df_original,
    for_pca = df_for_pca,
    analysis = df_analysis %>% select(-row_id),
    row_mapping = row_mapping,
    mmse_details = mmse_info
  ))
}

# ---------------------------------------------------------------
# 4. PCA実行
# ---------------------------------------------------------------

#' PCAの実行
run_pca_analysis <- function(df_analysis) {
  cat("🧮 PCA（主成分分析）を実行中...\n")
  
  # 主成分数の調整（変数数を超えないように）
  max_components <- min(N_COMPONENTS, ncol(df_analysis), nrow(df_analysis) - 1)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("   設定された主成分数:", N_COMPONENTS, "\n"))
    cat(paste("   実際に抽出する主成分数:", max_components, "\n"))
    cat(paste("   標準化:", ifelse(STANDARDIZE_DATA, "実行済み", "なし"), "\n"))
    cat(paste("   回転法:", ROTATION_METHOD, "\n\n"))
  }
  
  # PCAの実行
  tryCatch({
    # FactoMineRを使用してPCAを実行
    pca_result <- PCA(df_analysis, 
                     ncp = max_components,
                     scale.unit = FALSE,  # 既に標準化済みの場合はFALSE
                     graph = FALSE)
    
    # psychパッケージでも結果を取得（比較用）
    pca_psych <- principal(df_analysis, 
                          nfactors = max_components, 
                          rotate = ROTATION_METHOD,
                          scores = TRUE)
    
    cat("✅ PCA計算完了。\n\n")
    
    return(list(
      factomine = pca_result,
      psych = pca_psych,
      n_components = max_components
    ))
    
  }, error = function(e) {
    cat("❌ PCAの実行でエラーが発生:", e$message, "\n")
    return(NULL)
  })
}

# ---------------------------------------------------------------
# 5. 結果の整理と表示
# ---------------------------------------------------------------

#' PCA結果の要約表示
display_pca_summary <- function(pca_results) {
  cat("📊 PCA結果の要約\n")
  cat("================================================\n\n")
  
  factomine_result <- pca_results$factomine
  psych_result <- pca_results$psych
  
  # 寄与率と累積寄与率
  cat("🔹 各主成分の寄与率:\n")
  eigenvalues <- factomine_result$eig
  contribution_table <- data.frame(
    Component = paste0("PC", seq_len(nrow(eigenvalues))),
    Eigenvalue = round(eigenvalues[,1], 3),
    Variance_Percent = round(eigenvalues[,2], 2),
    Cumulative_Percent = round(eigenvalues[,3], 2)
  )
  print(contribution_table)
  cat("\n")
  
  # Kaiser基準（固有値>1）による主成分数の推奨
  kaiser_components <- sum(eigenvalues[,1] > 1)
  cat(paste("💡 Kaiser基準（固有値>1）による推奨主成分数:", kaiser_components, "\n"))
  
  # 累積寄与率80%に達する主成分数
  cumvar_80_components <- which(eigenvalues[,3] >= 80)[1]
  if (!is.na(cumvar_80_components)) {
    cat(paste("💡 累積寄与率80%に達する主成分数:", cumvar_80_components, "\n"))
  }
  cat("\n")
  
  # 因子負荷量の表示（閾値以上のもの）
  cat(paste("🔹 因子負荷量 (|loading| >=", SHOW_LOADINGS_THRESHOLD, "):\n"))
  loadings_matrix <- factomine_result$var$coord
  
  for (i in 1:ncol(loadings_matrix)) {
    pc_name <- paste0("PC", i)
    cat(paste("\n[", pc_name, " - 寄与率:", round(eigenvalues[i,2], 2), "%]\n"))
    
    # 閾値以上の因子負荷量を抽出
    high_loadings <- loadings_matrix[abs(loadings_matrix[,i]) >= SHOW_LOADINGS_THRESHOLD, i, drop = FALSE]
    
    if (nrow(high_loadings) > 0) {
      # 絶対値の大きい順にソート
      sorted_loadings <- high_loadings[order(abs(high_loadings[,1]), decreasing = TRUE), , drop = FALSE]
      
      for (j in 1:nrow(sorted_loadings)) {
        var_name <- rownames(sorted_loadings)[j]
        loading_value <- sorted_loadings[j,1]
        cat(sprintf("  %s: %.3f\n", var_name, loading_value))
      }
    } else {
      cat("  (閾値以上の負荷量なし)\n")
    }
  }
  cat("\n")
}

#' 因子負荷量テーブルの作成
create_loadings_table <- function(pca_results) {
  cat("📋 因子負荷量テーブルを作成中...\n")
  
  factomine_result <- pca_results$factomine
  loadings_matrix <- factomine_result$var$coord
  
  # テーブル形式で整理
  loadings_table <- as.data.frame(loadings_matrix) %>%
    rownames_to_column("Variable") %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))
  
  # 列名を分かりやすく変更
  pc_names <- paste0("PC", seq_len(ncol(loadings_table)-1))
  colnames(loadings_table) <- c("Variable", pc_names)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("📊 因子負荷量テーブル:\n")
    print(loadings_table)
    cat("\n")
  }
  
  return(loadings_table)
}

# ---------------------------------------------------------------
# 6. 可視化
# ---------------------------------------------------------------

#' スクリープロットの作成（改良版）
create_scree_plot <- function(pca_results) {
  cat("📈 スクリープロットを作成中...\n")
  
  factomine_result <- pca_results$factomine
  eigenvalues <- factomine_result$eig
  
  # データフレームの準備
  scree_data <- data.frame(
    PC = factor(seq_len(nrow(eigenvalues)), labels = paste0("PC", seq_len(nrow(eigenvalues)))),
    PC_num = seq_len(nrow(eigenvalues)),
    Eigenvalue = eigenvalues[,1],
    Variance_Percent = eigenvalues[,2],
    Cumulative_Percent = eigenvalues[,3]
  )
  
  # 改良版スクリープロット（固有値）
  p1 <- ggplot(scree_data, aes(x = PC_num, y = Eigenvalue)) +
    geom_point(size = 4, color = "steelblue", alpha = 0.8) +
    geom_line(color = "steelblue", linewidth = 1.2, alpha = 0.7) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 1, alpha = 0.8) +
    geom_text(aes(label = round(Eigenvalue, 2)), vjust = -1, size = 3.5, fontface = "bold") +
    annotate("text", x = max(scree_data$PC_num) * 0.7, y = 1.1, 
             label = "Kaiser Criterion (Eigenvalue = 1)", color = "red", size = 3.5, fontface = "bold") +
    scale_x_continuous(breaks = scree_data$PC_num, labels = paste0("PC", scree_data$PC_num)) +
    labs(title = "📊 Scree Plot: Eigenvalues of Principal Components",
         subtitle = "Components above red line are recommended (Kaiser Criterion)",
         x = "Principal Component",
         y = "Eigenvalue",
         caption = "Higher eigenvalues indicate that the component explains more variance in the data") +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
  
  # 寄与率プロット（改良版）
  p2 <- ggplot(scree_data, aes(x = PC_num, y = Variance_Percent)) +
    geom_col(fill = "lightblue", color = "steelblue", alpha = 0.8, width = 0.7) +
    geom_text(aes(label = paste0(round(Variance_Percent, 1), "%")), 
              vjust = -0.5, size = 3.5, fontface = "bold") +
    scale_x_continuous(breaks = scree_data$PC_num, labels = paste0("PC", scree_data$PC_num)) +
    labs(title = "📈 Variance Contribution of Each Principal Component",
         subtitle = "Percentage of total variance explained by each component",
         x = "Principal Component",
         y = "Variance Contribution (%)",
         caption = "Sum of all variance contributions equals 100%") +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
  
  # 累積寄与率プロット（新規追加）
  p3 <- ggplot(scree_data, aes(x = PC_num, y = Cumulative_Percent)) +
    geom_point(size = 4, color = "darkgreen", alpha = 0.8) +
    geom_line(color = "darkgreen", linewidth = 1.2, alpha = 0.7) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "orange", linewidth = 1, alpha = 0.8) +
    geom_text(aes(label = paste0(round(Cumulative_Percent, 1), "%")), 
              vjust = -1, size = 3.5, fontface = "bold") +
    annotate("text", x = max(scree_data$PC_num) * 0.7, y = 85, 
             label = "80% Line (Common Threshold)", color = "orange", size = 3.5, fontface = "bold") +
    scale_x_continuous(breaks = scree_data$PC_num, labels = paste0("PC", scree_data$PC_num)) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    labs(title = "📋 Cumulative Variance Explained",
         subtitle = "Cumulative explanatory power when components are combined",
         x = "Principal Component",
         y = "Cumulative Variance Explained (%)",
         caption = "80% or higher is often considered sufficient explanatory power") +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank()
    )
  
  # 3つのプロットを結合
  combined_plot <- grid.arrange(p1, p2, p3, nrow = 3)
  
  return(list(eigenvalue_plot = p1, variance_plot = p2, cumulative_plot = p3, combined = combined_plot))
}

#' バイプロットの作成
create_biplot <- function(pca_results, mmse_categories, palette = MMSE_COLOR_PALETTE) {
  cat("📊 バイプロットを作成中...\n")

  factomine_result <- pca_results$factomine
  scores <- factomine_result$ind$coord

  if (nrow(scores) != length(mmse_categories)) {
    stop("MMSEカテゴリの長さがPCAの個体数と一致しません。")
  }

  mmse_factor <- factor(mmse_categories, levels = MMSE_LABELS)
  mmse_factor <- forcats::fct_explicit_na(mmse_factor, na_level = "Missing")

  palette_named <- setNames(c(palette, MMSE_MISSING_COLOR), c(MMSE_LABELS, "Missing"))
  palette_used <- unname(palette_named[levels(mmse_factor)])

  biplot <- fviz_pca_biplot(
    factomine_result,
    axes = c(1, 2),
    geom.ind = "point",
    geom.var = c("arrow", "text"),
    habillage = mmse_factor,
    palette = palette_used,
    col.var = "red",
    alpha.ind = 0.7,
    repel = TRUE,
    title = "PCA Biplot Colored by MMSE"
  ) +
    labs(color = "MMSE Category") +
    theme_classic() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white")
    )

  return(biplot)
}

#' MMSEカテゴリ別のPCAスコア散布図を作成
create_mmse_scores_plot <- function(pca_results, mmse_categories, palette = MMSE_COLOR_PALETTE) {
  cat("🌈 MMSEカテゴリ別スコア散布図を作成中...\n")

  factomine_result <- pca_results$factomine
  scores <- factomine_result$ind$coord

  if (nrow(scores) == 0) {
    stop("PCAスコアが存在しません。")
  }
  if (nrow(scores) != length(mmse_categories)) {
    stop("MMSEカテゴリの長さがPCAの個体数と一致しません。")
  }

  mmse_factor <- factor(mmse_categories, levels = MMSE_LABELS)
  mmse_factor <- forcats::fct_explicit_na(mmse_factor, na_level = "Missing")

  palette_named <- setNames(c(palette, MMSE_MISSING_COLOR), c(MMSE_LABELS, "Missing"))
  palette_used <- unname(palette_named[levels(mmse_factor)])

  scores_df <- tibble::as_tibble(scores[, 1:2, drop = FALSE], .name_repair = "minimal") %>%
    dplyr::rename(PC1 = 1, PC2 = 2) %>%
    dplyr::mutate(mmse_category = mmse_factor)

  eigenvalues <- factomine_result$eig[, 2]
  axis_x <- sprintf("PC1 (%.1f%%)", eigenvalues[1])
  axis_y <- sprintf("PC2 (%.1f%%)", eigenvalues[2])

  ggplot(scores_df, aes(x = .data$PC1, y = .data$PC2, color = .data$mmse_category)) +
    geom_point(size = 2.8, alpha = 0.8) +
    scale_color_manual(values = palette_used, drop = FALSE) +
    labs(
      title = "PCA Scores Colored by MMSE",
      x = axis_x,
      y = axis_y,
      color = "MMSE Category"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      legend.title = element_text(face = "bold"),
      panel.border = element_rect(fill = NA, color = "gray70", linewidth = 1)
    )
}

#' 因子負荷量ヒートマップの作成
create_loadings_heatmap <- function(pca_results) {
  cat("🔥 因子負荷量ヒートマップを作成中...\n")
  
  factomine_result <- pca_results$factomine
  loadings_matrix <- factomine_result$var$coord
  
  # ヒートマップの作成
  heatmap_plot <- pheatmap(
    loadings_matrix,
    main = "Factor Loadings Heatmap",
    display_numbers = TRUE,
    number_format = "%.2f",
    fontsize_number = 10,
    cluster_rows = TRUE,
    cluster_cols = FALSE,
    color = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100),
    breaks = seq(-1, 1, length.out = 101),
    border_color = "white",
    fontsize_row = 10,
    fontsize_col = 12,
    angle_col = 0,
    silent = TRUE
  )
  
  return(heatmap_plot)
}

#' PCAの軸の取り方を説明する図の作成
create_pca_interpretation_plot <- function(pca_results) {
  cat("🎯 PCA軸の解釈図を作成中...\n")
  
  factomine_result <- pca_results$factomine
  loadings_matrix <- factomine_result$var$coord
  
  # PC1とPC2の因子負荷量を取得
  if (ncol(loadings_matrix) >= 2) {
    loadings_df <- data.frame(
      Variable = rownames(loadings_matrix),
      PC1 = loadings_matrix[, 1],
      PC2 = loadings_matrix[, 2],
      PC1_abs = abs(loadings_matrix[, 1]),
      PC2_abs = abs(loadings_matrix[, 2])
    )
    
    # 因子負荷量の大きさによる色分け
    loadings_df$Magnitude <- sqrt(loadings_df$PC1^2 + loadings_df$PC2^2)
    
    # 軸の解釈プロット
    interpretation_plot <- ggplot(loadings_df, aes(x = PC1, y = PC2)) +
      geom_hline(yintercept = 0, linetype = "solid", color = "black", alpha = 0.3) +
      geom_vline(xintercept = 0, linetype = "solid", color = "black", alpha = 0.3) +
      geom_point(aes(size = Magnitude, color = Magnitude), alpha = 0.8) +
      geom_text(aes(label = Variable), 
               vjust = -0.5, hjust = 0.5, size = 3, fontface = "bold") +
      geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2, color = Magnitude),
                  arrow = arrow(length = unit(0.2, "cm")), 
                  alpha = 0.7, linewidth = 1) +
      scale_color_gradient2(low = "lightblue", mid = "steelblue", high = "darkred",
                           midpoint = median(loadings_df$Magnitude),
                           name = "Loading\nMagnitude") +
      scale_size_continuous(range = c(3, 6), name = "Loading\nMagnitude") +
      coord_equal() +
      labs(
        title = "🎯 PCA Factor Loadings: Axis Interpretation",
        subtitle = "How each variable contributes to PC1 and PC2",
        x = paste0("PC1 (", round(factomine_result$eig[1,2], 1), "% of variance)"),
        y = paste0("PC2 (", round(factomine_result$eig[2,2], 1), "% of variance)"),
        caption = "Arrow length and color indicate the strength of each variable's contribution"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
        plot.caption = element_text(size = 9, color = "gray50"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        panel.background = element_rect(fill = "white", color = "white"),
        plot.background = element_rect(fill = "white", color = "white"),
        panel.grid.major = element_line(color = "gray90", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "white", color = "gray80"),
        legend.box.background = element_rect(fill = "white", color = "white")
      )
    
    # 四象限の解釈テキストを追加
    max_range <- max(abs(c(loadings_df$PC1, loadings_df$PC2))) * 1.1
    
    interpretation_plot <- interpretation_plot +
      annotate("text", x = max_range * 0.7, y = max_range * 0.7, 
               label = "High PC1 &\nHigh PC2", 
               color = "darkgreen", size = 3, fontface = "bold", alpha = 0.7) +
      annotate("text", x = -max_range * 0.7, y = max_range * 0.7, 
               label = "Low PC1 &\nHigh PC2", 
               color = "darkblue", size = 3, fontface = "bold", alpha = 0.7) +
      annotate("text", x = max_range * 0.7, y = -max_range * 0.7, 
               label = "High PC1 &\nLow PC2", 
               color = "darkorange", size = 3, fontface = "bold", alpha = 0.7) +
      annotate("text", x = -max_range * 0.7, y = -max_range * 0.7, 
               label = "Low PC1 &\nLow PC2", 
               color = "darkred", size = 3, fontface = "bold", alpha = 0.7) +
      xlim(-max_range, max_range) +
      ylim(-max_range, max_range)
    
    return(interpretation_plot)
    
  } else {
    cat("⚠️  PC2が存在しないため、軸の解釈図をスキップします。\n")
    return(NULL)
  }
}

#' PCAの概念説明図の作成
create_pca_concept_plot <- function(pca_results) {
  cat("📚 PCA概念説明図を作成中...\n")
  
  factomine_result <- pca_results$factomine
  eigenvalues <- factomine_result$eig
  
  # 寄与率データの準備
  concept_data <- data.frame(
    Component = factor(seq_len(min(5, nrow(eigenvalues))), 
                      labels = paste0("PC", seq_len(min(5, nrow(eigenvalues))))),
    Eigenvalue = eigenvalues[seq_len(min(5, nrow(eigenvalues))), 1],
    Variance = eigenvalues[seq_len(min(5, nrow(eigenvalues))), 2],
    Cumulative = eigenvalues[seq_len(min(5, nrow(eigenvalues))), 3],
    Category = c("Most Important", rep("Important", min(4, nrow(eigenvalues)-1)))
  )
  
  # 概念説明プロット
  concept_plot <- ggplot(concept_data, aes(x = Component, y = Eigenvalue, fill = Category)) +
    geom_col(alpha = 0.8, color = "white", linewidth = 1) +
    geom_text(aes(label = paste0("λ = ", round(Eigenvalue, 2), "\n", 
                                round(Variance, 1), "%")), 
              vjust = -0.3, size = 3.5, fontface = "bold") +
    geom_hline(yintercept = 1, linetype = "dashed", color = "red", 
               linewidth = 1, alpha = 0.8) +
    scale_fill_manual(values = c("Most Important" = "steelblue", 
                                "Important" = "lightblue")) +
    labs(
      title = "📚 PCA Concept: Principal Components Extraction",
      subtitle = "Each component extracts maximum variance from remaining data",
      x = "Principal Components (ordered by importance)",
      y = "Eigenvalue (Amount of variance explained)",
      caption = "PC1 explains most variance, PC2 explains most of remaining variance, etc.\nComponents with eigenvalue > 1 (red line) are typically retained"
    ) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
      plot.caption = element_text(size = 9, color = "gray50"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      panel.background = element_rect(fill = "white", color = "white"),
      plot.background = element_rect(fill = "white", color = "white"),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = "gray80"),
      legend.box.background = element_rect(fill = "white", color = "white")
    ) +
    annotate("text", x = length(concept_data$Component) * 0.7, y = max(concept_data$Eigenvalue) * 0.8,
             label = "Kaiser Criterion\n(Eigenvalue > 1)", 
             color = "red", size = 3.5, fontface = "bold")
  
  return(concept_plot)
}

# ---------------------------------------------------------------
# 7. 結果の保存
# ---------------------------------------------------------------

#' プロットの保存
save_plots <- function(scree_plots, biplot, mmse_plot, heatmap_plot, interpretation_plot = NULL, concept_plot = NULL) {
  if (!SAVE_PLOTS) {
    cat("ℹ️  プロット保存がスキップされました。\n")
    return()
  }
  
  cat("💾 プロットを保存中...\n")
  
  # スクリープロット
  ggsave(paste0(OUTPUT_PREFIX, "_scree_plot.png"), 
         plot = scree_plots$combined, 
         width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  
  # バイプロット
  ggsave(paste0(OUTPUT_PREFIX, "_biplot.png"), 
         plot = biplot, 
         width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)

  # MMSEカテゴリ散布図
  ggsave(paste0(OUTPUT_PREFIX, "_scores_by_mmse.png"), 
    plot = mmse_plot, 
    width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  
  # 因子負荷量ヒートマップ
  png(paste0(OUTPUT_PREFIX, "_loadings_heatmap.png"), 
      width = PLOT_WIDTH * PLOT_DPI, height = PLOT_HEIGHT * PLOT_DPI, res = PLOT_DPI)
  print(heatmap_plot)
  dev.off()
  
  # 軸の解釈図
  if (!is.null(interpretation_plot)) {
    ggsave(paste0(OUTPUT_PREFIX, "_axis_interpretation.png"), 
           plot = interpretation_plot, 
           width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  }
  
  # 概念説明図
  if (!is.null(concept_plot)) {
    ggsave(paste0(OUTPUT_PREFIX, "_concept_explanation.png"), 
           plot = concept_plot, 
           width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
  }
  
  cat("✅ プロットの保存完了。\n")
  cat("  - MMSEカテゴリ散布図を保存しました。\n")
  if (!is.null(interpretation_plot)) {
    cat("  - PCA軸の解釈図も保存されました。\n")
  }
  if (!is.null(concept_plot)) {
    cat("  - PCA概念説明図も保存されました。\n")
  }
  cat("\n")
}

#' 結果CSVの保存
save_results_csv <- function(pca_results, loadings_table, original_data, row_mapping) {
  if (!SAVE_RESULTS_CSV) {
    cat("ℹ️  CSV保存がスキップされました。\n")
    return()
  }
  
  cat("💾 結果CSVを保存中...\n")
  
  factomine_result <- pca_results$factomine
  
  # 1. 寄与率テーブル
  eigenvalues <- factomine_result$eig
  contribution_table <- data.frame(
    Component = paste0("PC", seq_len(nrow(eigenvalues))),
    Eigenvalue = eigenvalues[,1],
    Variance_Percent = eigenvalues[,2],
    Cumulative_Percent = eigenvalues[,3]
  )
  write_csv(contribution_table, paste0(OUTPUT_PREFIX, "_contribution_rates.csv"))
  
  # 2. 因子負荷量テーブル
  write_csv(loadings_table, paste0(OUTPUT_PREFIX, "_factor_loadings.csv"))
  
  # 3. 主成分得点付きデータ
  pc_scores <- factomine_result$ind$coord
  
  # 主成分得点を元データに結合
  pc_scores_df <- as.data.frame(pc_scores) %>%
    rownames_to_column("temp_id") %>%
    mutate(temp_id = as.numeric(temp_id)) %>%
    bind_cols(row_mapping) %>%
    select(-temp_id)
  
  # 元データと結合
  final_data <- left_join(original_data, pc_scores_df, by = "row_id") %>%
    select(-row_id)
  
  write_csv(final_data, paste0(OUTPUT_PREFIX, "_data_with_pc_scores.csv"))
  
  cat("✅ 以下のCSVファイルを保存しました:\n")
  cat(paste("  -", paste0(OUTPUT_PREFIX, "_contribution_rates.csv"), "\n"))
  cat(paste("  -", paste0(OUTPUT_PREFIX, "_factor_loadings.csv"), "\n"))
  cat(paste("  -", paste0(OUTPUT_PREFIX, "_data_with_pc_scores.csv"), "\n\n"))
}

# ---------------------------------------------------------------
# 8. メイン実行関数
# ---------------------------------------------------------------

#' PCA統合フローのメイン実行関数
main_pca_flow <- function() {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🚀 PCA統合フロー開始\n")
    cat(paste(rep("=", 60), collapse = ""), "\n\n")
  }
  
  # 1. パッケージセットアップ
  setup_packages()
  
  # 2. データ読み込み
  data_info <- load_data()
  data <- data_info$data
  file_path <- data_info$file_path
  
  # 3. 分析項目選択
  selected_columns <- select_pca_variables(data)
  
  # 4. PCA用データ準備
  prepared_data <- prepare_pca_data(data, selected_columns)
  
  # 5. PCA実行
  pca_results <- run_pca_analysis(prepared_data$analysis)
  
  if (is.null(pca_results)) {
    cat("❌ PCA実行に失敗しました。\n")
    return(NULL)
  }
  
  # 6. 結果の表示
  display_pca_summary(pca_results)
  
  # 7. 因子負荷量テーブル作成
  loadings_table <- create_loadings_table(pca_results)
  
  # 8. 可視化
  scree_plots <- create_scree_plot(pca_results)
  mmse_categories <- prepared_data$row_mapping$mmse_category
  biplot <- create_biplot(pca_results, mmse_categories)
  mmse_scores_plot <- create_mmse_scores_plot(pca_results, mmse_categories)
  heatmap_plot <- create_loadings_heatmap(pca_results)
  interpretation_plot <- create_pca_interpretation_plot(pca_results)
  concept_plot <- create_pca_concept_plot(pca_results)
  
  # 9. 保存
  save_plots(scree_plots, biplot, mmse_scores_plot, heatmap_plot, interpretation_plot, concept_plot)
  save_results_csv(pca_results, loadings_table, prepared_data$original, prepared_data$row_mapping)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("🎉 PCA統合フローが正常に完了しました！\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }
  
  return(list(
    pca_results = pca_results,
    loadings_table = loadings_table,
    selected_columns = selected_columns,
    plots = list(
      scree = scree_plots,
      biplot = biplot,
      mmse_scores = mmse_scores_plot,
      heatmap = heatmap_plot,
      interpretation = interpretation_plot,
      concept = concept_plot
    )
  ))
}

# ---------------------------------------------------------------
# 実行部分
# ---------------------------------------------------------------

# 🚀 メイン実行
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && nzchar(args[1])) {
  INPUT_FILE <<- args[1]
  cat(sprintf("🛠️  INPUT_FILE を上書き: %s\n", INPUT_FILE))
}
if (length(args) >= 2 && nzchar(args[2])) {
  OUTPUT_PREFIX <<- args[2]
  cat(sprintf("🛠️  OUTPUT_PREFIX を上書き: %s\n", OUTPUT_PREFIX))
}

cat("🔍 PCA統合分析スクリプト\n")
cat(paste("📊 分析対象項目:", length(TARGET_COLUMNS), "個\n"))
cat(paste("📈 抽出主成分数:", N_COMPONENTS, "(最大)\n"))
cat(paste("🔄 標準化:", ifelse(STANDARDIZE_DATA, "実行", "なし"), "\n"))
cat(paste("💾 出力接頭辞:", OUTPUT_PREFIX, "\n\n"))

results <- main_pca_flow()
