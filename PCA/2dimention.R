#################################################################
# 次元削減統合スクリプト（PCA, t-SNE, MDS, UMAP）
# MMSEスコアによる色分けプロット
#################################################################

# ================================================================
# 設定変数
# ================================================================

# 入力ファイル設定
INPUT_FILE <- "../raw_data/dummy_data.csv"
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# 色分け設定
COLOR_BY_COLUMN <- "MMSE"
MMSE_BREAKS <- c(0, 23, 27, 30)
MMSE_LABELS <- c("Low (≤23)", "Medium (24-27)", "High (≥28)")

# スケーリング設定
SCALING_METHOD <- "minmax_trimmed"  # "minmax", "minmax_trimmed", "zscore"

# パラメータ設定
TSNE_PERPLEXITY <- 30
TSNE_SEED <- 42
UMAP_N_NEIGHBORS <- 15
UMAP_MIN_DIST <- 0.1
UMAP_SEED <- 42

# 出力設定
OUTPUT_PREFIX <- "dimensionality_reduction"
PLOT_WIDTH <- 16
PLOT_HEIGHT <- 12
PLOT_DPI <- 300

# ================================================================
# パッケージ読み込み
# ================================================================

suppressMessages({
  suppressWarnings({
    library(tidyverse)
    library(ggplot2)
    library(gridExtra)
    library(grid)
    library(Rtsne)
    library(umap)
    library(MASS)
  })
})

# ================================================================
# 関数定義
# ================================================================

# データ読み込み
load_data <- function() {
  if (!file.exists(INPUT_FILE)) {
    stop("指定されたファイルが見つかりません: ", INPUT_FILE)
  }
  cat("データ読み込み中:", basename(INPUT_FILE), "\n")
  data <- readr::read_csv(INPUT_FILE, show_col_types = FALSE)
  cat("データサイズ:", nrow(data), "行", ncol(data), "列\n")
  return(data)
}

# スケーリング
apply_scaling <- function(data, method) {
  if (method == "minmax") {
    scaled_data <- as.data.frame(lapply(data, function(x) {
      (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
    }))
  } else if (method == "minmax_trimmed") {
    scaled_data <- as.data.frame(lapply(data, function(x) {
      q05 <- quantile(x, 0.05, na.rm = TRUE)
      q95 <- quantile(x, 0.95, na.rm = TRUE)
      x_trimmed <- pmax(pmin(x, q95), q05)
      (x_trimmed - min(x_trimmed, na.rm = TRUE)) / 
        (max(x_trimmed, na.rm = TRUE) - min(x_trimmed, na.rm = TRUE))
    }))
  } else if (method == "zscore") {
    scaled_data <- as.data.frame(scale(data))
  } else {
    stop("未対応のスケーリング方法: ", method)
  }
  return(scaled_data)
}

# データ準備
prepare_data <- function(data) {
  # 列の存在確認
  missing_cols <- TARGET_COLUMNS[!(TARGET_COLUMNS %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop("指定された列が見つかりません: ", paste(missing_cols, collapse = ", "))
  }
  if (!(COLOR_BY_COLUMN %in% colnames(data))) {
    stop("色分け用の列が見つかりません: ", COLOR_BY_COLUMN)
  }
  
  # データ準備
  df <- data %>%
    dplyr::mutate(row_id = dplyr::row_number()) %>%
    dplyr::select(row_id, dplyr::all_of(TARGET_COLUMNS), dplyr::all_of(COLOR_BY_COLUMN)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(TARGET_COLUMNS), as.numeric)) %>%
    dplyr::mutate(!!COLOR_BY_COLUMN := as.numeric(.data[[COLOR_BY_COLUMN]])) %>%
    na.omit()
  
  cat("分析対象:", nrow(df), "名\n")
  
  # スケーリング
  analysis_data <- df %>% dplyr::select(-row_id, -dplyr::all_of(COLOR_BY_COLUMN))
  scaled_data <- apply_scaling(analysis_data, SCALING_METHOD)
  color_data <- df %>% dplyr::select(row_id, dplyr::all_of(COLOR_BY_COLUMN))
  
  return(list(
    scaled = scaled_data,
    color_data = color_data,
    row_mapping = df %>% dplyr::select(row_id)
  ))
}

# PCA
run_pca <- function(scaled_data) {
  cat("PCA実行中...\n")
  pca_result <- prcomp(scaled_data, center = FALSE, scale. = FALSE)
  scores <- as.data.frame(pca_result$x[, 1:2])
  colnames(scores) <- c("PC1", "PC2")
  var_explained <- (pca_result$sdev^2 / sum(pca_result$sdev^2) * 100)[1:2]
  return(list(scores = scores, var_explained = var_explained))
}

# t-SNE
run_tsne <- function(scaled_data) {
  cat("t-SNE実行中...\n")
  if (TSNE_PERPLEXITY >= nrow(scaled_data)) {
    stop("perplexityが大きすぎます: ", TSNE_PERPLEXITY, " >= ", nrow(scaled_data))
  }
  set.seed(TSNE_SEED)
  tsne_result <- Rtsne(scaled_data, dims = 2, perplexity = TSNE_PERPLEXITY, 
                      max_iter = 1000, check_duplicates = FALSE)
  scores <- as.data.frame(tsne_result$Y)
  colnames(scores) <- c("tSNE1", "tSNE2")
  return(list(scores = scores))
}

# MDS
run_mds <- function(scaled_data) {
  cat("MDS実行中...\n")
  dist_matrix <- dist(scaled_data)
  mds_result <- cmdscale(dist_matrix, k = 2, eig = TRUE)
  scores <- as.data.frame(mds_result$points)
  colnames(scores) <- c("MDS1", "MDS2")
  return(list(scores = scores))
}

# UMAP
run_umap <- function(scaled_data) {
  cat("UMAP実行中...\n")
  config <- umap.defaults
  config$n_components <- 2
  config$n_neighbors <- UMAP_N_NEIGHBORS
  config$min_dist <- UMAP_MIN_DIST
  config$random_state <- UMAP_SEED
  umap_result <- umap(scaled_data, config = config)
  scores <- as.data.frame(umap_result$layout)
  colnames(scores) <- c("UMAP1", "UMAP2")
  return(list(scores = scores))
}

# プロット作成
create_plot <- function(scores, method_name, color_data, var_explained = NULL) {
  plot_data <- data.frame(
    Dim1 = scores[, 1],
    Dim2 = scores[, 2]
  )
  
  # MMSEカテゴリ作成
  color_values <- color_data[[COLOR_BY_COLUMN]]
  plot_data$category <- cut(color_values, breaks = MMSE_BREAKS, 
                           labels = MMSE_LABELS, include.lowest = TRUE)
  
  # タイトル作成
  if (!is.null(var_explained)) {
    title <- paste0(method_name, "\n(PC1: ", round(var_explained[1], 1), 
                   "%, PC2: ", round(var_explained[2], 1), "%)")
  } else {
    title <- method_name
  }
  
  # プロット
  ggplot(plot_data, aes(x = Dim1, y = Dim2, color = category)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(values = c("#E31A1C", "#FF7F00", "#1F78B4"), 
                      name = paste0(COLOR_BY_COLUMN, "\nCategory")) +
    labs(title = title, x = paste0(method_name, " Dimension 1"), 
         y = paste0(method_name, " Dimension 2")) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
          panel.grid.major = element_line(color = "gray90"),
          panel.grid.minor = element_blank(),
          panel.border = element_rect(color = "gray70", fill = NA, linewidth = 1),
          legend.position = "right")
}

# 統合プロット作成
create_combined_plot <- function(pca_result, tsne_result, mds_result, umap_result, color_data) {
  cat("統合プロット作成中...\n")
  
  p1 <- create_plot(pca_result$scores, "PCA", color_data, pca_result$var_explained)
  p2 <- create_plot(tsne_result$scores, "t-SNE", color_data)
  p3 <- create_plot(mds_result$scores, "MDS", color_data)
  p4 <- create_plot(umap_result$scores, "UMAP", color_data)
  
  title_text <- paste0("Comparison of Dimensionality Reduction Methods (Colored by ", COLOR_BY_COLUMN, ")")
  
  suppressWarnings({
    combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2,
                                   top = grid::textGrob(title_text, 
                                                       gp = grid::gpar(fontsize = 16, fontface = "bold")))
  })
  
  return(combined_plot)
}

# 結果保存
save_results <- function(plots) {
  # プロット保存
  filename <- paste0(OUTPUT_PREFIX, "_combined_", SCALING_METHOD, ".png")
  cat("プロット保存中:", filename, "\n")
  ggsave(filename, plot = plots, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
}

# ================================================================
# メイン処理
# ================================================================

main <- function() {
  cat("=== 次元削減統合スクリプト ===\n")
  
  # データ準備
  data <- load_data()
  prepared_data <- prepare_data(data)
  
  # 次元削減実行
  cat("\n--- 次元削減実行 ---\n")
  pca_result <- run_pca(prepared_data$scaled)
  tsne_result <- run_tsne(prepared_data$scaled)
  mds_result <- run_mds(prepared_data$scaled)
  umap_result <- run_umap(prepared_data$scaled)
  
  # 可視化・保存
  cat("\n--- 可視化・保存 ---\n")
  plots <- create_combined_plot(pca_result, tsne_result, mds_result, umap_result, prepared_data$color_data)
  save_results(plots)
  
  cat("\n=== 処理完了 ===\n")
  cat("設定: ", SCALING_METHOD, "スケーリング、", COLOR_BY_COLUMN, "による色分け\n")
}

# 実行
main()