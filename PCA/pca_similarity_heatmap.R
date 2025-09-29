#################################################################
# PCA得点を用いた人間同士の類似度分析・可視化スクリプト
#
# 目的：
# - PCA得点（5次元）を使用して人間同士の類似度を計算
# - 大規模データ（10,000人）に対応した効率的な可視化
# - クラスタリング、サンプリング、階層化による可視化手法
# - 類似度の統計的分析とパターン発見
#################################################################

# ================================================================
# 🔧 設定変数（ここで全ての設定を一括指定）
# ================================================================

# ★★★ 入力ファイルの設定 ★★★
SIMILARITY_INPUT_FILE <- "pca_data_with_pc_scores.csv"  # PCA得点付きデータファイル
PC_COLUMNS <- c("Dim.1", "Dim.2", "Dim.3", "Dim.4", "Dim.5")  # 主成分得点の列名

# ★★★ 類似度計算の設定 ★★★
SIMILARITY_METHOD <- "euclidean"  # 距離計算方法（"euclidean", "manhattan", "cosine"）
NORMALIZE_SCORES <- TRUE  # PC得点を正規化するか

# ★★★ 可視化パラメータ ★★★
MAX_DISPLAY_SIZE <- 1000  # ヒートマップで表示する最大人数
SAMPLE_METHOD <- "stratified"  # サンプリング手法（"random", "stratified", "extreme"）
N_CLUSTERS <- 10  # クラスタリング数（階層表示用）
CLUSTER_METHOD <- "kmeans"  # クラスタリング手法（"kmeans", "hierarchical"）

# ★★★ 出力設定 ★★★
SIMILARITY_OUTPUT_PREFIX <- "similarity"  # 出力ファイル名の接頭辞
SAVE_SIMILARITY_PLOTS <- TRUE  # プロットを保存するか
SAVE_SIMILARITY_CSV <- TRUE  # 結果をCSVで保存するか
HEATMAP_WIDTH <- 12  # ヒートマップの幅（インチ）
HEATMAP_HEIGHT <- 10  # ヒートマップの高さ（インチ）
HEATMAP_DPI <- 300  # 解像度

# ★★★ 表示設定 ★★★
SHOW_SIMILARITY_DETAILS <- TRUE  # 詳細な分析結果を表示するか
SIMILARITY_THRESHOLD <- 0.8  # 高類似度のペアを抽出する閾値

# ================================================================

# ---------------------------------------------------------------
# 1. パッケージの読み込み
# ---------------------------------------------------------------

#' 類似度分析用パッケージの読み込み
setup_similarity_packages <- function() {
  packages <- c("tidyverse", "pheatmap", "RColorBrewer", "cluster", 
                "ggplot2", "gridExtra")
  
  cat("📦 類似度分析用パッケージを読み込み中...\n")
  cat("必要パッケージ:", paste(packages, collapse = ", "), "\n\n")
  
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
# 2. データ読み込みと準備
# ---------------------------------------------------------------

#' PCA得点データの読み込み
load_pca_scores <- function() {
  if (!file.exists(SIMILARITY_INPUT_FILE)) {
    stop(paste("❌ ファイル '", SIMILARITY_INPUT_FILE, "' が見つかりません。", sep=""))
  }
  
  cat(paste("📖 PCA得点データを読み込み中:", SIMILARITY_INPUT_FILE, "\n"))
  data <- read_csv(SIMILARITY_INPUT_FILE, show_col_types = FALSE)
  
  # PC列の存在確認
  missing_pc_cols <- PC_COLUMNS[!(PC_COLUMNS %in% colnames(data))]
  if (length(missing_pc_cols) > 0) {
    stop(paste("❌ 主成分得点列が見つかりません:", paste(missing_pc_cols, collapse = ", ")))
  }
  
  cat(paste("✅ データ読み込み完了。対象者:", nrow(data), "名\n"))
  cat(paste("   使用する主成分:", paste(PC_COLUMNS, collapse = ", "), "\n\n"))
  
  return(data)
}

#' PCA得点の前処理
prepare_similarity_data <- function(data) {
  cat("🔧 類似度計算用データを準備中...\n")
  
  # PCA得点のみを抽出
  pc_scores <- data %>%
    select(all_of(PC_COLUMNS)) %>%
    na.omit()
  
  # 正規化（設定に応じて）
  if (NORMALIZE_SCORES) {
    cat("📊 PCA得点を正規化中...\n")
    pc_scores_normalized <- as.data.frame(scale(pc_scores))
    cat("✅ 正規化完了。\n")
  } else {
    pc_scores_normalized <- pc_scores
    cat("ℹ️  正規化をスキップしました。\n")
  }
  
  # 行IDを付与
  pc_scores_final <- pc_scores_normalized %>%
    mutate(person_id = row_number()) %>%
    select(person_id, everything())
  
  cat(paste("✅ データ準備完了。分析対象:", nrow(pc_scores_final), "名\n\n"))
  
  return(pc_scores_final)
}

# ---------------------------------------------------------------
# 3. 類似度計算
# ---------------------------------------------------------------

#' 人間同士の類似度計算
calculate_similarity_matrix <- function(pc_scores) {
  cat("🧮 類似度行列を計算中...\n")
  cat(paste("   計算方法:", SIMILARITY_METHOD, "\n"))
  cat(paste("   対象者数:", nrow(pc_scores), "名\n"))
  
  # PCA得点のみを抽出（person_idを除く）
  score_matrix <- pc_scores %>%
    select(-person_id) %>%
    as.matrix()
  
  # 距離行列の計算
  if (SIMILARITY_METHOD == "euclidean") {
    distance_matrix <- dist(score_matrix, method = "euclidean")
  } else if (SIMILARITY_METHOD == "manhattan") {
    distance_matrix <- dist(score_matrix, method = "manhattan")
  } else if (SIMILARITY_METHOD == "cosine") {
    # コサイン類似度の計算
    cosine_sim <- function(x, y) sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
    n <- nrow(score_matrix)
    distance_matrix <- matrix(0, n, n)
    for (i in 1:(n-1)) {
      for (j in (i+1):n) {
        sim <- cosine_sim(score_matrix[i,], score_matrix[j,])
        distance_matrix[i, j] <- distance_matrix[j, i] <- 1 - sim  # 距離に変換
      }
    }
    distance_matrix <- as.dist(distance_matrix)
  }
  
  # 類似度に変換（距離の逆数）
  max_distance <- max(distance_matrix)
  similarity_matrix <- 1 - (as.matrix(distance_matrix) / max_distance)
  
  # 対角成分を1に設定
  diag(similarity_matrix) <- 1
  
  cat("✅ 類似度行列の計算完了。\n\n")
  
  return(list(
    similarity = similarity_matrix,
    distance = distance_matrix,
    n_people = nrow(pc_scores)
  ))
}

# ---------------------------------------------------------------
# 4. データサンプリングとクラスタリング
# ---------------------------------------------------------------

#' 大規模データの効率的サンプリング
smart_sampling <- function(pc_scores, similarity_result, target_size = MAX_DISPLAY_SIZE) {
  n_total <- nrow(pc_scores)
  
  if (n_total <= target_size) {
    cat("ℹ️  データサイズが表示限界以下のため、全データを使用します。\n\n")
    return(list(
      sampled_indices = 1:n_total,
      sampling_method = "全データ",
      cluster_info = NULL
    ))
  }
  
  cat(paste("🎯 大規模データ（", n_total, "名）をサンプリング中...\n"))
  cat(paste("   目標サンプル数:", target_size, "名\n"))
  cat(paste("   サンプリング手法:", SAMPLE_METHOD, "\n"))
  
  if (SAMPLE_METHOD == "random") {
    # ランダムサンプリング
    sampled_indices <- sample(1:n_total, target_size)
    cluster_info <- NULL
    
  } else if (SAMPLE_METHOD == "stratified") {
    # クラスタリング後の層化サンプリング
    cat("   クラスタリングを実行中...\n")
    
    score_matrix <- pc_scores %>% select(-person_id) %>% as.matrix()
    
    if (CLUSTER_METHOD == "kmeans") {
      clusters <- kmeans(score_matrix, centers = N_CLUSTERS, nstart = 25)
      cluster_labels <- clusters$cluster
    } else {
      hc <- hclust(similarity_result$distance)
      cluster_labels <- cutree(hc, k = N_CLUSTERS)
    }
    
    # 各クラスタから均等にサンプリング
    samples_per_cluster <- floor(target_size / N_CLUSTERS)
    remaining_samples <- target_size - (samples_per_cluster * N_CLUSTERS)
    
    sampled_indices <- c()
    cluster_info <- data.frame(
      cluster = 1:N_CLUSTERS,
      total_size = as.numeric(table(cluster_labels)),
      sampled_size = samples_per_cluster
    )
    
    for (i in 1:N_CLUSTERS) {
      cluster_members <- which(cluster_labels == i)
      n_to_sample <- samples_per_cluster
      if (i <= remaining_samples) n_to_sample <- n_to_sample + 1
      
      if (length(cluster_members) >= n_to_sample) {
        sampled_from_cluster <- sample(cluster_members, n_to_sample)
      } else {
        sampled_from_cluster <- cluster_members
      }
      sampled_indices <- c(sampled_indices, sampled_from_cluster)
    }
    
  } else if (SAMPLE_METHOD == "extreme") {
    # 極端な類似度ペアを重視したサンプリング
    sim_matrix <- similarity_result$similarity
    
    # 高類似度ペアの抽出
    high_sim_pairs <- which(sim_matrix > quantile(sim_matrix[upper.tri(sim_matrix)], 0.95), arr.ind = TRUE)
    high_sim_individuals <- unique(as.vector(high_sim_pairs))
    
    # 低類似度ペアの抽出
    low_sim_pairs <- which(sim_matrix < quantile(sim_matrix[upper.tri(sim_matrix)], 0.05), arr.ind = TRUE)
    low_sim_individuals <- unique(as.vector(low_sim_pairs))
    
    # 極端なケースと中央値付近をバランスよく
    extreme_individuals <- unique(c(high_sim_individuals, low_sim_individuals))
    n_extreme <- min(length(extreme_individuals), target_size * 0.6)
    n_random <- target_size - n_extreme
    
    sampled_indices <- c(
      sample(extreme_individuals, n_extreme),
      sample(setdiff(1:n_total, extreme_individuals), n_random)
    )
    cluster_info <- NULL
  }
  
  cat(paste("✅ サンプリング完了。選択された人数:", length(sampled_indices), "名\n\n"))
  
  return(list(
    sampled_indices = sampled_indices,
    sampling_method = SAMPLE_METHOD,
    cluster_info = cluster_info
  ))
}

# ---------------------------------------------------------------
# 5. 可視化
# ---------------------------------------------------------------

#' 基本的な類似度ヒートマップ
create_basic_similarity_heatmap <- function(similarity_matrix, sample_info) {
  cat("🔥 基本類似度ヒートマップを作成中...\n")
  
  sampled_sim <- similarity_matrix[sample_info$sampled_indices, sample_info$sampled_indices]
  n_sample <- nrow(sampled_sim)
  
  # 行列のラベルを設定
  rownames(sampled_sim) <- paste0("Person_", sample_info$sampled_indices)
  colnames(sampled_sim) <- paste0("Person_", sample_info$sampled_indices)
  
  # ヒートマップ作成
  heatmap_plot <- pheatmap(
    sampled_sim,
    main = paste0("Human Similarity Heatmap (", n_sample, " people)"),
    color = colorRampPalette(c("#313695", "#74ADD1", "#FFFFFF", "#F46D43", "#A50026"))(100),
    breaks = seq(0, 1, length.out = 101),
    show_rownames = ifelse(n_sample <= 50, TRUE, FALSE),
    show_colnames = ifelse(n_sample <= 50, TRUE, FALSE),
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    fontsize_row = max(8 - n_sample/100, 4),
    fontsize_col = max(8 - n_sample/100, 4),
    border_color = "white",
    silent = TRUE
  )
  
  return(heatmap_plot)
}

#' 階層クラスタリング付きヒートマップ
create_hierarchical_heatmap <- function(similarity_matrix, sample_info) {
  cat("📊 階層クラスタリング付きヒートマップを作成中...\n")
  
  sampled_sim <- similarity_matrix[sample_info$sampled_indices, sample_info$sampled_indices]
  
  # 距離行列に変換
  distance_matrix <- 1 - sampled_sim
  distance_matrix[distance_matrix < 0] <- 0  # 負の値をゼロクリップ
  
  # 階層クラスタリング
  hc_row <- hclust(as.dist(distance_matrix), method = "ward.D2")
  hc_col <- hclust(as.dist(distance_matrix), method = "ward.D2")
  
  # デンドログラム付きヒートマップ
  hierarchical_plot <- pheatmap(
    sampled_sim,
    main = paste0("Hierarchical Clustering of Human Similarity (", nrow(sampled_sim), " people)"),
    color = colorRampPalette(c("blue", "white", "red"))(100),
    cluster_rows = hc_row,
    cluster_cols = hc_col,
    show_rownames = FALSE,
    show_colnames = FALSE,
    cutree_rows = min(10, nrow(sampled_sim)/10),
    cutree_cols = min(10, nrow(sampled_sim)/10),
    border_color = NA,
    silent = TRUE
  )
  
  return(hierarchical_plot)
}

#' 類似度分布の統計プロット
create_similarity_distribution_plot <- function(similarity_matrix) {
  cat("📈 類似度分布プロットを作成中...\n")
  
  # 上三角行列のみを取得（対角成分を除く）
  similarity_values <- similarity_matrix[upper.tri(similarity_matrix)]
  
  # 統計情報
  stats_info <- data.frame(
    Mean = mean(similarity_values),
    Median = median(similarity_values),
    SD = sd(similarity_values),
    Min = min(similarity_values),
    Max = max(similarity_values),
    Q25 = quantile(similarity_values, 0.25),
    Q75 = quantile(similarity_values, 0.75)
  )
  
  # ヒストグラム
  p1 <- ggplot(data.frame(similarity = similarity_values), aes(x = similarity)) +
    geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7, color = "white") +
    geom_vline(xintercept = stats_info$Mean, color = "red", linetype = "dashed", size = 1) +
    geom_vline(xintercept = stats_info$Median, color = "orange", linetype = "dashed", size = 1) +
    labs(title = "Distribution of Human Similarity Scores",
         subtitle = paste0("Red: Mean (", round(stats_info$Mean, 3), "), Orange: Median (", round(stats_info$Median, 3), ")"),
         x = "Similarity Score", y = "Frequency") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5))
  
  # ボックスプロット
  p2 <- ggplot(data.frame(similarity = similarity_values), aes(y = similarity)) +
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    geom_hline(yintercept = SIMILARITY_THRESHOLD, color = "red", linetype = "dashed") +
    labs(title = "Similarity Score Distribution",
         subtitle = paste0("Red line: High similarity threshold (", SIMILARITY_THRESHOLD, ")"),
         x = "", y = "Similarity Score") +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  # 結合プロット
  combined_plot <- grid.arrange(p1, p2, ncol = 2)
  
  return(list(
    histogram = p1,
    boxplot = p2,
    combined = combined_plot,
    stats = stats_info
  ))
}

#' 高類似度ペアの詳細分析
analyze_high_similarity_pairs <- function(similarity_matrix, pc_scores, threshold = SIMILARITY_THRESHOLD) {
  cat(paste("🔍 高類似度ペア（閾値:", threshold, "）を分析中...\n"))
  
  # 高類似度ペアの抽出
  high_sim_indices <- which(similarity_matrix >= threshold & 
                           similarity_matrix < 1, arr.ind = TRUE)  # 対角成分を除く
  
  if (nrow(high_sim_indices) == 0) {
    cat("⚠️  指定された閾値以上の類似度ペアが見つかりませんでした。\n")
    return(NULL)
  }
  
  # 類似度の高い順にソート
  high_sim_data <- data.frame(
    person1 = high_sim_indices[, 1],
    person2 = high_sim_indices[, 2],
    similarity = similarity_matrix[high_sim_indices]
  ) %>%
    arrange(desc(similarity)) %>%
    slice_head(n = min(100, nrow(.)))  # 上位100ペア
  
  # PCA得点の比較
  high_sim_comparison <- high_sim_data %>%
    rowwise() %>%
    mutate(
      pc1_diff = abs(pc_scores$Dim.1[person1] - pc_scores$Dim.1[person2]),
      pc2_diff = abs(pc_scores$Dim.2[person1] - pc_scores$Dim.2[person2]),
      pc3_diff = abs(pc_scores$Dim.3[person1] - pc_scores$Dim.3[person2]),
      pc4_diff = abs(pc_scores$Dim.4[person1] - pc_scores$Dim.4[person2]),
      pc5_diff = abs(pc_scores$Dim.5[person1] - pc_scores$Dim.5[person2]),
      avg_pc_diff = (pc1_diff + pc2_diff + pc3_diff + pc4_diff + pc5_diff) / 5
    ) %>%
    ungroup()
  
  cat(paste("✅ 高類似度ペア分析完了。検出されたペア数:", nrow(high_sim_data), "\n\n"))
  
  return(high_sim_comparison)
}

# ---------------------------------------------------------------
# 6. 結果の保存
# ---------------------------------------------------------------

#' プロットの保存
save_similarity_plots <- function(basic_heatmap, hierarchical_heatmap, distribution_plots) {
  if (!SAVE_SIMILARITY_PLOTS) {
    cat("ℹ️  プロット保存がスキップされました。\n")
    return()
  }
  
  cat("💾 類似度分析プロットを保存中...\n")
  
  # 基本ヒートマップ
  png(paste0(SIMILARITY_OUTPUT_PREFIX, "_basic_heatmap.png"), 
      width = HEATMAP_WIDTH * HEATMAP_DPI, height = HEATMAP_HEIGHT * HEATMAP_DPI, res = HEATMAP_DPI)
  print(basic_heatmap)
  dev.off()
  
  # 階層ヒートマップ
  png(paste0(SIMILARITY_OUTPUT_PREFIX, "_hierarchical_heatmap.png"), 
      width = HEATMAP_WIDTH * HEATMAP_DPI, height = HEATMAP_HEIGHT * HEATMAP_DPI, res = HEATMAP_DPI)
  print(hierarchical_heatmap)
  dev.off()
  
  # 分布プロット
  ggsave(paste0(SIMILARITY_OUTPUT_PREFIX, "_distribution.png"), 
         plot = distribution_plots$combined, 
         width = HEATMAP_WIDTH, height = HEATMAP_HEIGHT/2, dpi = HEATMAP_DPI)
  
  cat("✅ プロット保存完了。\n\n")
}

#' 結果CSVの保存
save_similarity_results <- function(similarity_result, sample_info, distribution_stats, high_sim_pairs) {
  if (!SAVE_SIMILARITY_CSV) {
    cat("ℹ️  CSV保存がスキップされました。\n")
    return()
  }
  
  cat("💾 類似度分析結果を保存中...\n")
  
  # 1. 類似度統計情報
  similarity_stats <- data.frame(
    total_people = similarity_result$n_people,
    sampled_people = length(sample_info$sampled_indices),
    sampling_method = sample_info$sampling_method,
    similarity_method = SIMILARITY_METHOD,
    mean_similarity = distribution_stats$Mean,
    median_similarity = distribution_stats$Median,
    sd_similarity = distribution_stats$SD
  )
  write_csv(similarity_stats, paste0(SIMILARITY_OUTPUT_PREFIX, "_summary_stats.csv"))
  
  # 2. 高類似度ペア
  if (!is.null(high_sim_pairs)) {
    write_csv(high_sim_pairs, paste0(SIMILARITY_OUTPUT_PREFIX, "_high_similarity_pairs.csv"))
  }
  
  # 3. サンプリング情報
  if (!is.null(sample_info$cluster_info)) {
    write_csv(sample_info$cluster_info, paste0(SIMILARITY_OUTPUT_PREFIX, "_cluster_info.csv"))
  }
  
  cat("✅ CSV保存完了。\n\n")
}

# ---------------------------------------------------------------
# 7. メイン実行関数
# ---------------------------------------------------------------

#' 類似度分析メイン実行関数
main_similarity_analysis <- function() {
  cat("🚀 人間類似度分析開始\n")
  cat(paste(rep("=", 60), collapse = ""), "\n\n")
  
  # 1. パッケージセットアップ
  setup_similarity_packages()
  
  # 2. データ読み込み
  pca_data <- load_pca_scores()
  prepared_data <- prepare_similarity_data(pca_data)
  
  # 3. 類似度計算
  similarity_result <- calculate_similarity_matrix(prepared_data)
  
  # 4. サンプリング
  sample_info <- smart_sampling(prepared_data, similarity_result)
  
  # 5. 可視化
  basic_heatmap <- create_basic_similarity_heatmap(similarity_result$similarity, sample_info)
  hierarchical_heatmap <- create_hierarchical_heatmap(similarity_result$similarity, sample_info)
  distribution_plots <- create_similarity_distribution_plot(similarity_result$similarity)
  
  # 6. 高類似度ペア分析
  high_sim_pairs <- analyze_high_similarity_pairs(similarity_result$similarity, prepared_data)
  
  # 7. 結果保存
  save_similarity_plots(basic_heatmap, hierarchical_heatmap, distribution_plots)
  save_similarity_results(similarity_result, sample_info, distribution_plots$stats, high_sim_pairs)
  
  # 8. 結果表示
  if (SHOW_SIMILARITY_DETAILS) {
    cat("📊 分析結果サマリー\n")
    cat("====================\n")
    cat(paste("総対象者数:", similarity_result$n_people, "名\n"))
    cat(paste("表示サンプル数:", length(sample_info$sampled_indices), "名\n"))
    cat(paste("平均類似度:", round(distribution_plots$stats$Mean, 4), "\n"))
    cat(paste("類似度標準偏差:", round(distribution_plots$stats$SD, 4), "\n"))
    if (!is.null(high_sim_pairs)) {
      cat(paste("高類似度ペア数（閾値≥", SIMILARITY_THRESHOLD, "）:", nrow(high_sim_pairs), "ペア\n"))
    }
    cat("\n")
  }
  
  cat("🎉 類似度分析が正常に完了しました！\n")
  cat(paste(rep("=", 60), collapse = ""), "\n")
  
  return(list(
    similarity_result = similarity_result,
    sample_info = sample_info,
    plots = list(
      basic_heatmap = basic_heatmap,
      hierarchical_heatmap = hierarchical_heatmap,
      distribution = distribution_plots
    ),
    high_similarity_pairs = high_sim_pairs,
    stats = distribution_plots$stats
  ))
}

# ---------------------------------------------------------------
# 実行部分
# ---------------------------------------------------------------

# 🚀 メイン実行
cat("🔍 PCA得点を用いた人間類似度分析\n")
cat(paste("📊 入力ファイル:", SIMILARITY_INPUT_FILE, "\n"))
cat(paste("📈 類似度計算方法:", SIMILARITY_METHOD, "\n"))
cat(paste("🎯 最大表示人数:", MAX_DISPLAY_SIZE, "名\n"))
cat(paste("📋 サンプリング手法:", SAMPLE_METHOD, "\n\n"))

similarity_results <- main_similarity_analysis()
