#################################################################
# t-SNE 専用スクリプト
# MMSEスコアによる色分けプロット
#################################################################

# ================================================================
# 設定変数
# ================================================================

# 入力ファイル設定
INPUT_FILE <- "../raw_data/dummy_data.csv"
TARGET_COLUMNS <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")
AGE_COLUMN <- "age"

# 色分け設定
COLOR_BY_COLUMN <- "MMSE"
MMSE_BREAKS <- c(0, 23, 27, 30)
MMSE_LABELS <- c("Low (≤23)", "Medium (24-27)", "High (≥28)")

# スケーリング設定
SCALING_METHOD <- "minmax_trimmed"  # "minmax", "minmax_trimmed", "zscore"

# t-SNEパラメータ
TSNE_PERPLEXITY <- 30
TSNE_ITER <- 1000
TSNE_SEED <- 42

# 出力設定
OUTPUT_PREFIX <- "tsne_only"
PLOT_WIDTH <- 10
PLOT_HEIGHT <- 8
PLOT_DPI <- 300

# ================================================================
# パッケージ読み込み
# ================================================================

suppressMessages({
  suppressWarnings({
    library(readr)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(Rtsne)
    library(rlang)
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
  missing_cols <- TARGET_COLUMNS[!(TARGET_COLUMNS %in% colnames(data))]
  if (length(missing_cols) > 0) {
    stop("指定された列が見つかりません: ", paste(missing_cols, collapse = ", "))
  }
  if (!(COLOR_BY_COLUMN %in% colnames(data))) {
    stop("色分け用の列が見つかりません: ", COLOR_BY_COLUMN)
  }
  if (!(AGE_COLUMN %in% colnames(data))) {
    stop("年齢列が見つかりません: ", AGE_COLUMN)
  }

  df <- data |>
    dplyr::select(dplyr::all_of(TARGET_COLUMNS), dplyr::all_of(COLOR_BY_COLUMN), dplyr::all_of(AGE_COLUMN)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(TARGET_COLUMNS), as.numeric))

  df[[COLOR_BY_COLUMN]] <- as.numeric(df[[COLOR_BY_COLUMN]])
  df[[AGE_COLUMN]] <- as.numeric(df[[AGE_COLUMN]])

  missing_mmse_young <- is.na(df[[COLOR_BY_COLUMN]]) & df[[AGE_COLUMN]] <= 64
  if (any(missing_mmse_young, na.rm = TRUE)) {
    cat("64歳以下の欠損MMSEを満点(30)として補完:", sum(missing_mmse_young, na.rm = TRUE), "件\n")
    df[[COLOR_BY_COLUMN]][missing_mmse_young] <- 30
  }

  required_cols <- c(TARGET_COLUMNS, COLOR_BY_COLUMN)
  df <- df[stats::complete.cases(df[, required_cols]), , drop = FALSE]

  cat("分析対象:", nrow(df), "名\n")

  analysis_data <- dplyr::select(df, dplyr::all_of(TARGET_COLUMNS))
  scaled_data <- apply_scaling(analysis_data, SCALING_METHOD)
  color_values <- df[[COLOR_BY_COLUMN]]

  return(list(
    scaled = scaled_data,
    color_values = color_values
  ))
}

# t-SNE
run_tsne <- function(scaled_data) {
  cat("t-SNE実行中...\n")
  if (TSNE_PERPLEXITY >= nrow(scaled_data)) {
    stop("perplexityが大きすぎます: ", TSNE_PERPLEXITY, " >= ", nrow(scaled_data))
  }
  set.seed(TSNE_SEED)
  tsne_result <- Rtsne::Rtsne(
    scaled_data,
    dims = 2,
    perplexity = TSNE_PERPLEXITY,
    max_iter = TSNE_ITER,
    check_duplicates = FALSE
  )
  scores <- as.data.frame(tsne_result$Y)
  colnames(scores) <- c("tSNE1", "tSNE2")
  return(scores)
}

# プロット作成
create_tsne_plot <- function(tsne_scores, color_values) {
  if (nrow(tsne_scores) != length(color_values)) {
    stop("t-SNE結果と色分けデータの行数が一致しません。")
  }

  plot_data <- dplyr::mutate(
    tsne_scores,
    category = cut(
      color_values,
      breaks = MMSE_BREAKS,
      labels = MMSE_LABELS,
      include.lowest = TRUE
    )
  )

  ggplot(plot_data, aes(x = .data$tSNE1, y = .data$tSNE2, color = .data$category)) +
    geom_point(alpha = 0.7, size = 2) +
    scale_color_manual(
      values = c("#E31A1C", "#FF7F00", "#1F78B4"),
      name = paste0(COLOR_BY_COLUMN, "\nCategory")
    ) +
    labs(
      title = "t-SNE Projection Colored by MMSE",
      x = "t-SNE Dimension 1",
      y = "t-SNE Dimension 2"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "gray70", fill = NA, linewidth = 1),
      legend.position = "right"
    )
}

# 結果保存
save_tsne_plot <- function(plot) {
  filename <- paste0(OUTPUT_PREFIX, "_", SCALING_METHOD, ".png")
  cat("プロット保存中:", filename, "\n")
  ggsave(filename, plot = plot, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
}

# ================================================================
# メイン処理
# ================================================================

main <- function() {
  cat("=== t-SNE 専用スクリプト ===\n")

  data <- load_data()
  prepared_data <- prepare_data(data)

  tsne_scores <- run_tsne(prepared_data$scaled)

  cat("\n--- 可視化・保存 ---\n")
  tsne_plot <- create_tsne_plot(tsne_scores, prepared_data$color_values)
  save_tsne_plot(tsne_plot)

  cat("\n=== 処理完了 ===\n")
  cat("設定: ", SCALING_METHOD, "スケーリング、", COLOR_BY_COLUMN, "による色分け\n")
}

# 実行
main()
