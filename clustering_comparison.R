#################################################################
# ハードクラスタリング vs ソフトクラスタリング比較検証
#
# 目的：
# - 5次元データを生成・標準化
# - ハードクラスタリング（k-means）実行
# - ソフトクラスタリング（LPA Model1）実行  
# - 結果を統合してCSV出力・比較分析
#################################################################

# ================================================================
# 🔧 設定変数
# ================================================================

# 基本設定
set.seed(12345)          # 再現性のための乱数シード
N_SAMPLES <- 1000        # サンプル数
N_DIMENSIONS <- 5        # 次元数
N_CLUSTERS <- 3          # クラスター数
OUTPUT_FILE <- "personality_clustering_comparison_results.csv"

# 詳細出力設定
SHOW_DETAILED_OUTPUT <- TRUE

# パッケージの明示的な読み込み
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyLPA)
  library(cluster)
  library(stats)
  library(knitr)
})

# グローバル変数の定義（R CMD checkのため）
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "id", "soft_cluster", "max_probability", "hard_cluster", "true_cluster",
  "clustering_agreement", "confidence_level"
))

# ================================================================
# 1. パッケージ管理とセットアップ
# ================================================================

#' パッケージの準備と読み込み
setup_packages <- function() {
  packages <- c("dplyr", "readr", "tidyLPA", "cluster", "stats", "knitr")
  
  # パッケージの存在確認とインストール
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
      cat(paste("Installing package:", pkg, "\n"))
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  cat("✅ パッケージの読み込みが完了しました。\n\n")
}

# ================================================================
# 2. 5次元データ生成
# ================================================================

#' 5次元データの生成（ソフトクラスタリングデモ用）
#' @description より現実的で重複のあるクラスターを持つ5次元データを生成
generate_5d_data <- function() {
  cat("🎲 ソフトクラスタリングデモ用5次元データを生成中...\n")
  
  # より現実的なクラスター設定
  # クラスター1: 内向的・神経症的傾向（低い外向性、高い神経症）
  n1 <- round(N_SAMPLES * 0.35)  # 35%
  cluster1 <- data.frame(
    extraversion = rnorm(n1, mean = 2.5, sd = 1.2),     # 低い外向性
    neuroticism = rnorm(n1, mean = 4.2, sd = 1.1),      # 高い神経症
    openness = rnorm(n1, mean = 3.8, sd = 1.0),         # 中程度の開放性
    agreeableness = rnorm(n1, mean = 4.1, sd = 0.9),    # やや高い協調性
    conscientiousness = rnorm(n1, mean = 3.5, sd = 1.1), # 中程度の誠実性
    true_cluster = 1
  )
  
  # クラスター2: バランス型（全体的に平均的だが少し外向的）
  n2 <- round(N_SAMPLES * 0.40)  # 40%
  cluster2 <- data.frame(
    extraversion = rnorm(n2, mean = 4.0, sd = 1.0),     # やや高い外向性
    neuroticism = rnorm(n2, mean = 3.2, sd = 1.2),      # やや低い神経症
    openness = rnorm(n2, mean = 3.7, sd = 1.1),         # 中程度の開放性
    agreeableness = rnorm(n2, mean = 3.8, sd = 1.0),    # 中程度の協調性
    conscientiousness = rnorm(n2, mean = 4.2, sd = 0.9), # 高い誠実性
    true_cluster = 2
  )
  
  # クラスター3: 高外向・低神経症型（外向的で安定）
  n3 <- N_SAMPLES - n1 - n2  # 残り（約25%）
  cluster3 <- data.frame(
    extraversion = rnorm(n3, mean = 5.2, sd = 0.9),     # 高い外向性
    neuroticism = rnorm(n3, mean = 2.1, sd = 1.0),      # 低い神経症
    openness = rnorm(n3, mean = 4.5, sd = 1.2),         # 高い開放性
    agreeableness = rnorm(n3, mean = 4.3, sd = 1.1),    # 高い協調性
    conscientiousness = rnorm(n3, mean = 4.8, sd = 0.8), # 非常に高い誠実性
    true_cluster = 3
  )
  
  # データを結合してシャッフル
  data_raw <- rbind(cluster1, cluster2, cluster3)
  data_raw$id <- seq_len(nrow(data_raw))
  data_raw <- data_raw[sample(nrow(data_raw)), ]  # シャッフル
  data_raw$id <- seq_len(nrow(data_raw))  # IDを再割り当て
  
  # 値を1-7の範囲に制限（心理尺度の一般的な範囲）
  personality_vars <- c("extraversion", "neuroticism", "openness", "agreeableness", "conscientiousness")
  for(var in personality_vars) {
    data_raw[[var]] <- pmax(1, pmin(7, data_raw[[var]]))
  }
  
  if (SHOW_DETAILED_OUTPUT) {
    cat(paste("✅ データ生成完了:", nrow(data_raw), "サンプル x", N_DIMENSIONS, "次元\n"))
    cat("\n[生成データの統計（1-7スケール）]\n")
    summary_stats <- summary(data_raw[, personality_vars])
    print(summary_stats)
    
    cat("\n[真のクラスター分布]\n")
    cluster_dist <- table(data_raw$true_cluster)
    cluster_pct <- round(prop.table(cluster_dist) * 100, 1)
    for(i in seq_along(cluster_dist)) {
      cat(paste("クラスター", i, ":", cluster_dist[i], "名 (", cluster_pct[i], "%)\n"))
    }
    
    # 各クラスターの特徴を表示
    cat("\n[各クラスターの心理的特徴（平均値）]\n")
    for(i in 1:3) {
      cluster_data <- data_raw[data_raw$true_cluster == i, personality_vars]
      means <- round(sapply(cluster_data, mean), 2)
      cat(paste("クラスター", i, ":", paste(names(means), "=", means, collapse = ", "), "\n"))
    }
    cat("\n")
  }
  
  return(data_raw)
}

# ================================================================
# 3. データ標準化
# ================================================================

#' データの標準化（Z-score）
standardize_data <- function(data_raw) {
  cat("📊 心理データをZ-scoreで標準化中...\n")
  
  # 分析対象列を抽出（心理変数）
  analysis_columns <- c("extraversion", "neuroticism", "openness", "agreeableness", "conscientiousness")
  data_for_analysis <- data_raw[, analysis_columns]
  
  # Z-score標準化
  data_standardized <- as.data.frame(scale(data_for_analysis))
  colnames(data_standardized) <- analysis_columns
  
  # IDと真のクラスター情報を追加
  data_standardized$id <- data_raw$id
  data_standardized$true_cluster <- data_raw$true_cluster
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("✅ 標準化完了\n")
    cat("\n[標準化後の統計（各次元の平均は0、標準偏差は1になる）]\n")
    summary_stats <- summary(data_standardized[, analysis_columns])
    print(summary_stats)
    
    # 平均と標準偏差を確認
    cat("\n[各次元の平均値（0に近い値になるはず）]\n")
    means <- sapply(data_standardized[, analysis_columns], mean)
    print(round(means, 3))
    
    cat("\n[各次元の標準偏差（1になるはず）]\n")
    sds <- sapply(data_standardized[, analysis_columns], sd)
    print(round(sds, 3))
    cat("\n")
  }
  
  return(data_standardized)
}

# ================================================================
# 4. ハードクラスタリング（k-means）
# ================================================================

#' k-meansクラスタリング実行
perform_hard_clustering <- function(data_standardized) {
  cat("🔴 ハードクラスタリング（k-means）を実行中...\n")
  
  # k-means用のデータ準備
  analysis_columns <- c("extraversion", "neuroticism", "openness", "agreeableness", "conscientiousness")
  kmeans_data <- data_standardized[, analysis_columns]
  
  # k-meansクラスタリング実行
  kmeans_result <- kmeans(kmeans_data, centers = N_CLUSTERS, nstart = 25)
  
  # 結果をデータに追加
  data_with_hard <- data_standardized
  data_with_hard$hard_cluster <- kmeans_result$cluster
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("✅ k-meansクラスタリング完了\n")
    
    cat("\n[ハードクラスタリング結果]\n")
    hard_cluster_summary <- table(data_with_hard$hard_cluster)
    hard_pct <- round(prop.table(hard_cluster_summary) * 100, 1)
    for(i in seq_along(hard_cluster_summary)) {
      cat(paste("クラスター", i, ":", hard_cluster_summary[i], "名 (", hard_pct[i], "%)\n"))
    }
    
    cat("\n[クラスター内二乗和]\n")
    cat("Total within-cluster sum of squares:", round(kmeans_result$tot.withinss, 3), "\n")
    cat("Between-cluster sum of squares:", round(kmeans_result$betweenss, 3), "\n")
    
    cat("\n[各クラスターのセンター（標準化後）]\n")
    centers_df <- as.data.frame(kmeans_result$centers)
    rownames(centers_df) <- paste("Cluster", 1:nrow(centers_df))
    print(round(centers_df, 3))
    
    # 心理学的解釈を追加
    cat("\n[クラスターの心理学的特徴（標準化後）]\n")
    for(i in 1:nrow(centers_df)) {
      cat(paste("クラスター", i, ":\n"))
      center <- centers_df[i, ]
      traits <- c()
      if(center$extraversion > 0.5) traits <- c(traits, "高外向性")
      if(center$extraversion < -0.5) traits <- c(traits, "低外向性（内向性）")
      if(center$neuroticism > 0.5) traits <- c(traits, "高神経症性")
      if(center$neuroticism < -0.5) traits <- c(traits, "低神経症性（情緒安定）")
      if(center$openness > 0.5) traits <- c(traits, "高開放性")
      if(center$conscientiousness > 0.5) traits <- c(traits, "高誠実性")
      if(center$agreeableness > 0.5) traits <- c(traits, "高協調性")
      
      if(length(traits) > 0) {
        cat(paste("  ", paste(traits, collapse = ", "), "\n"))
      } else {
        cat("   平均的な特徴\n")
      }
    }
    cat("\n")
  }
  
  return(list(
    data = data_with_hard,
    kmeans_model = kmeans_result
  ))
}

# ================================================================
# 5. ソフトクラスタリング（LPA Model1）
# ================================================================

#' LPA（Model1）クラスタリング実行
perform_soft_clustering <- function(data_standardized) {
  cat("🔵 ソフトクラスタリング（LPA Model1）を実行中...\n")
  
  # LPA用のデータ準備
  analysis_columns <- c("extraversion", "neuroticism", "openness", "agreeableness", "conscientiousness")
  lpa_data <- data_standardized[, analysis_columns]
  
  # LPA実行（Model1を指定）
  lpa_result <- estimate_profiles(
    lpa_data,
    n_profiles = N_CLUSTERS,
    models = 1  # Model1を指定
  )
  
  # LPA結果からデータと所属確率を取得
  lpa_data_with_probs <- get_data(lpa_result)
  
  # 元データにLPA結果を結合
  data_with_soft <- data_standardized
  data_with_soft$soft_cluster <- lpa_data_with_probs$Class
  
  # 各クラスターの所属確率も追加
  prob_columns <- grep("^CPROB", colnames(lpa_data_with_probs), value = TRUE)
  for (col in prob_columns) {
    data_with_soft[[col]] <- lpa_data_with_probs[[col]]
  }
  
  # 最大所属確率も計算
  prob_matrix <- lpa_data_with_probs[, prob_columns]
  data_with_soft$max_probability <- apply(prob_matrix, 1, max)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("✅ LPA（Model1）クラスタリング完了\n")
    
    cat("\n[ソフトクラスタリング結果]\n")
    soft_cluster_summary <- table(data_with_soft$soft_cluster)
    soft_pct <- round(prop.table(soft_cluster_summary) * 100, 1)
    for(i in seq_along(soft_cluster_summary)) {
      cat(paste("クラスター", i, ":", soft_cluster_summary[i], "名 (", soft_pct[i], "%)\n"))
    }
    
    cat("\n[LPA適合度指標]\n")
    fit_indices <- get_fit(lpa_result)
    print(fit_indices[, c("LogLik", "AIC", "BIC", "Entropy")])
    
    cat("\n[平均所属確率]\n")
    avg_max_prob <- round(mean(data_with_soft$max_probability), 3)
    cat("平均最大所属確率:", avg_max_prob, "\n")
    
    cat("\n[所属確率の分布]\n")
    print(round(summary(data_with_soft$max_probability), 3))
    cat("\n")
  }
  
  return(list(
    data = data_with_soft,
    lpa_model = lpa_result,
    lpa_data_with_probs = lpa_data_with_probs
  ))
}

# ================================================================
# 6. 結果統合と比較分析
# ================================================================

#' ハード・ソフトクラスタリング結果の統合
integrate_clustering_results <- function(hard_results, soft_results) {
  cat("🔗 クラスタリング結果を統合中...\n")
  
  # データの統合（IDをキーとして結合）
  final_data <- hard_results$data %>%
    dplyr::left_join(
      soft_results$data %>% dplyr::select(id, soft_cluster, max_probability, dplyr::starts_with("CPROB")),
      by = "id"
    )
  
  # クラスター順序の調整（必要に応じて）
  final_data <- final_data %>%
    dplyr::arrange(id)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("✅ 結果統合完了\n")
    cat(paste("   統合データサイズ:", nrow(final_data), "行 x", ncol(final_data), "列\n\n"))
  }
  
  return(final_data)
}

#' クラスタリング結果の比較分析
compare_clustering_results <- function(final_data) {
  cat("📈 クラスタリング手法の比較分析中...\n")
  
  # クロス集計表作成
  cat("\n[ハード vs ソフトクラスタリングのクロス集計表]\n")
  cross_table <- table(
    Hard = final_data$hard_cluster, 
    Soft = final_data$soft_cluster
  )
  print(cross_table)
  
  # 一致率の計算
  agreement_rate <- sum(final_data$hard_cluster == final_data$soft_cluster) / nrow(final_data)
  cat(paste("\n[手法間の一致率]:", round(agreement_rate * 100, 2), "%\n"))
  
  # 真のクラスターとの比較（参考）
  if ("true_cluster" %in% colnames(final_data)) {
    cat("\n[真のクラスターとの比較]\n")
    
    # ハードクラスタリングの精度
    hard_accuracy <- sum(final_data$true_cluster == final_data$hard_cluster) / nrow(final_data)
    cat(paste("ハードクラスタリング精度:", round(hard_accuracy * 100, 2), "%\n"))
    
    # ソフトクラスタリングの精度
    soft_accuracy <- sum(final_data$true_cluster == final_data$soft_cluster) / nrow(final_data)
    cat(paste("ソフトクラスタリング精度:", round(soft_accuracy * 100, 2), "%\n"))
    
    # 真のクラスターとのクロス集計
    cat("\n[真のクラスター vs ハードクラスタリング]\n")
    print(table(True = final_data$true_cluster, Hard = final_data$hard_cluster))
    
    cat("\n[真のクラスター vs ソフトクラスタリング]\n")
    print(table(True = final_data$true_cluster, Soft = final_data$soft_cluster))
  }
  
  # 所属確率による分析
  cat("\n[ソフトクラスタリングの確信度分析]\n")
  confidence_stats <- final_data %>%
    dplyr::group_by(soft_cluster) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_confidence = round(mean(max_probability), 3),
      min_confidence = round(min(max_probability), 3),
      max_confidence = round(max(max_probability), 3),
      .groups = 'drop'
    )
  print(confidence_stats)
  
  cat("\n")
  
  return(list(
    cross_table = cross_table,
    agreement_rate = agreement_rate,
    confidence_stats = confidence_stats
  ))
}

# ================================================================
# 7. CSV出力
# ================================================================

#' 最終結果をCSVファイルに保存
save_results_to_csv <- function(final_data, comparison_results) {
  cat("💾 結果をCSVファイルに保存中...\n")
  
  # 出力用データの準備
  output_data <- final_data %>%
    dplyr::select(
      id,
      # 元の5次元データ（標準化後）
      extraversion, neuroticism, openness, agreeableness, conscientiousness,
      # 真のクラスター
      true_cluster,
      # ハードクラスタリング結果
      hard_cluster,
      # ソフトクラスタリング結果
      soft_cluster,
      # 最大所属確率
      max_probability,
      # 各クラスターへの所属確率
      dplyr::starts_with("CPROB")
    ) %>%
    # データを見やすく整理
    dplyr::mutate(
      # 手法間の一致フラグ
      clustering_agreement = ifelse(hard_cluster == soft_cluster, "一致", "不一致"),
      # 確信度レベル
      confidence_level = dplyr::case_when(
        max_probability >= 0.8 ~ "高",
        max_probability >= 0.6 ~ "中",
        max_probability >= 0.4 ~ "低",
        TRUE ~ "非常に低"
      )
    ) %>%
    # IDでソート
    dplyr::arrange(id)
  
  # CSVファイル保存
  readr::write_csv(output_data, OUTPUT_FILE)
  
  cat(paste("✅ 結果が", OUTPUT_FILE, "に保存されました\n"))
  cat(paste("   保存データサイズ:", nrow(output_data), "行 x", ncol(output_data), "列\n"))
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("\n[保存されたデータの列]\n")
    cat(paste("列名:", paste(colnames(output_data), collapse = ", "), "\n"))
    
    cat("\n[保存データの概要統計]\n")
    cat("手法間一致状況:\n")
    print(table(output_data$clustering_agreement))
    
    cat("\n確信度レベル分布:\n")
    print(table(output_data$confidence_level))
    cat("\n")
  }
  
  return(output_data)
}

# ================================================================
# 8. メイン実行関数
# ================================================================

#' メイン実行関数
main_clustering_comparison <- function() {
  if (SHOW_DETAILED_OUTPUT) {
    cat("🚀 ハード vs ソフトクラスタリング比較検証開始\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
    cat(paste("設定: サンプル数=", N_SAMPLES, ", 次元数=", N_DIMENSIONS, ", クラスター数=", N_CLUSTERS, "\n\n"))
  }
  
  # 1. パッケージセットアップ
  setup_packages()
  
  # 2. 5次元データ生成
  raw_data <- generate_5d_data()
  
  # 3. データ標準化
  standardized_data <- standardize_data(raw_data)
  
  # 4. ハードクラスタリング実行
  hard_results <- perform_hard_clustering(standardized_data)
  
  # 5. ソフトクラスタリング実行
  soft_results <- perform_soft_clustering(standardized_data)
  
  # 6. 結果統合
  final_data <- integrate_clustering_results(hard_results, soft_results)
  
  # 7. 比較分析
  comparison_results <- compare_clustering_results(final_data)
  
  # 8. CSV出力
  output_data <- save_results_to_csv(final_data, comparison_results)
  
  if (SHOW_DETAILED_OUTPUT) {
    cat("🎉 比較検証が正常に完了しました！\n")
    cat(paste(rep("=", 60), collapse = ""), "\n")
  }
  
  return(list(
    final_data = final_data,
    hard_results = hard_results,
    soft_results = soft_results,
    comparison_results = comparison_results,
    output_data = output_data
  ))
}

# ================================================================
# 9. 実行
# ================================================================

# メイン実行
cat("ハード vs ソフトクラスタリング比較検証を開始します...\n\n")
results <- main_clustering_comparison()

# 実行完了メッセージ
cat(paste("\n📁 結果ファイル:", OUTPUT_FILE, "を確認してください。\n"))
cat("🔍 このファイルには以下の情報が含まれています:\n")
cat("   - 標準化された5つのパーソナリティ特性データ\n")
cat("   - ハードクラスタリング結果\n")
cat("   - ソフトクラスタリング結果と所属確率\n")
cat("   - 手法間の一致/不一致情報\n")
cat("   - 確信度レベル分類\n")
cat("   - 曖昧なケースの特定情報\n\n")