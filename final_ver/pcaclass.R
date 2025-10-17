#!/usr/bin/env Rscript

#################################################################
# PCA クラス別可視化スクリプト
#
# 指定した特徴量で PCA を実行し、CSV のクラス列を用いて
# 1〜10 クラスを色分けした PC1 vs PC2 の散布図を作成します。
#################################################################

# ================================================================
# 設定変数
# ================================================================

INPUT_FILE <- "raw_data/dummy_data_with_clusters_sorted.csv"
FEATURE_COLUMNS <- c(
  "X542690_00", "X542700_00", "X542710_00",
  "X542720_00", "X542730_00"
)
CLASS_COLUMN <- "Class"
CLASS_COUNT <- 3L              # 可視化するクラス数（1〜10）
SCALING_METHOD <- "zscore"     # "minmax", "minmax_trimmed", "zscore"

OUTPUT_PREFIX <- "pcaclass"
OUTPUT_WIDTH <- 10
OUTPUT_HEIGHT <- 8
OUTPUT_DPI <- 300

# ================================================================
# パッケージ読み込み
# ================================================================

suppressMessages({
  suppressWarnings({
    library(readr)
    library(dplyr)
    library(ggplot2)
    library(rlang)
  })
})

# ================================================================
# ユーティリティ関数
# ================================================================

validate_settings <- function() {
  if (!is.character(INPUT_FILE) || INPUT_FILE == "") {
    stop("INPUT_FILE は非空文字列で指定してください。")
  }
  if (!is.numeric(CLASS_COUNT) || length(CLASS_COUNT) != 1) {
    stop("CLASS_COUNT は数値で指定してください。")
  }
  if (CLASS_COUNT < 1 || CLASS_COUNT > 10) {
    stop("CLASS_COUNT は 1〜10 の範囲で指定してください。")
  }
  valid_scaling <- c("minmax", "minmax_trimmed", "zscore")
  if (!(SCALING_METHOD %in% valid_scaling)) {
    stop("SCALING_METHOD は ", paste(valid_scaling, collapse = ", "), " のいずれかで指定してください。")
  }
}

load_data <- function() {
  if (!file.exists(INPUT_FILE)) {
    stop("指定されたファイルが見つかりません: ", INPUT_FILE)
  }
  cat("データ読み込み中:", basename(INPUT_FILE), "\n")
  data <- readr::read_csv(INPUT_FILE, show_col_types = FALSE)
  cat("データサイズ:", nrow(data), "行", ncol(data), "列\n")
  data
}

apply_scaling <- function(data, method) {
  if (method == "minmax") {
    scaled <- lapply(data, function(x) {
      rng <- range(x, na.rm = TRUE)
      if (diff(rng) == 0) {
        return(rep(0.5, length(x)))
      }
      (x - rng[1]) / diff(rng)
    })
  } else if (method == "minmax_trimmed") {
    scaled <- lapply(data, function(x) {
      q05 <- quantile(x, 0.05, na.rm = TRUE, names = FALSE)
      q95 <- quantile(x, 0.95, na.rm = TRUE, names = FALSE)
      trimmed <- pmax(pmin(x, q95), q05)
      rng <- range(trimmed, na.rm = TRUE)
      if (diff(rng) == 0) {
        return(rep(0.5, length(x)))
      }
      (trimmed - rng[1]) / diff(rng)
    })
  } else if (method == "zscore") {
    scaled <- lapply(data, function(x) {
      mu <- mean(x, na.rm = TRUE)
      sigma <- stats::sd(x, na.rm = TRUE)
      if (is.na(sigma) || sigma == 0) {
        return(rep(0, length(x)))
      }
      (x - mu) / sigma
    })
  } else {
    stop("未対応のスケーリング方法です: ", method)
  }
  as.data.frame(scaled)
}

coerce_class_values <- function(values) {
  if (is.factor(values)) {
    values <- as.character(values)
  }
  if (is.character(values)) {
    numeric_like <- grepl("^-?\\d+$", values)
    if (!all(numeric_like | is.na(values))) {
      stop("クラス列には整数のみを指定してください。")
    }
    values <- as.integer(values)
  }
  if (!is.numeric(values)) {
    stop("クラス列を数値に変換できませんでした。")
  }
  values
}

prepare_data <- function(data) {
  missing_features <- FEATURE_COLUMNS[!(FEATURE_COLUMNS %in% colnames(data))]
  if (length(missing_features) > 0) {
    stop("指定した特徴量列が見つかりません: ", paste(missing_features, collapse = ", "))
  }
  if (!(CLASS_COLUMN %in% colnames(data))) {
    stop("クラス列が見つかりません: ", CLASS_COLUMN)
  }

  df <- data |>
    dplyr::select(dplyr::all_of(c(FEATURE_COLUMNS, CLASS_COLUMN))) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(FEATURE_COLUMNS), as.numeric))

  class_values <- coerce_class_values(df[[CLASS_COLUMN]])

  target_classes <- seq_len(CLASS_COUNT)
  in_range <- class_values %in% target_classes
  if (!all(in_range | is.na(class_values))) {
    drop_count <- sum(!in_range & !is.na(class_values))
    cat("指定範囲外のクラスを除外:", drop_count, "件\n")
  }

  df <- df[in_range & stats::complete.cases(df[FEATURE_COLUMNS]), , drop = FALSE]
  class_values <- class_values[in_range]

  cat("分析対象:", nrow(df), "名\n")
  if (nrow(df) < 5) {
    warning("有効な行が 5 未満です。PCA の結果が不安定になる可能性があります。")
  }

  scaled_features <- apply_scaling(dplyr::select(df, dplyr::all_of(FEATURE_COLUMNS)), SCALING_METHOD)

  list(
    scaled = scaled_features,
    classes = class_values
  )
}

run_pca <- function(scaled_data) {
  cat("PCA 実行中...\n")
  pca <- stats::prcomp(scaled_data, center = FALSE, scale. = FALSE)
  scores <- as.data.frame(pca$x[, 1:2, drop = FALSE])
  colnames(scores) <- c("PC1", "PC2")
  explained <- (pca$sdev^2) / sum(pca$sdev^2)
  list(scores = scores, explained = explained)
}

create_pca_plot <- function(scores, class_values, explained) {
  if (nrow(scores) != length(class_values)) {
    stop("PCA スコアとクラス列の長さが一致しません。")
  }

  available_classes <- sort(unique(class_values))
  labels <- paste0("Class ", available_classes)

  base_colors <- c(
    "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
    "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
  )
  class_palette <- base_colors[available_classes]

  plot_data <- scores |>
    dplyr::mutate(
      class = factor(class_values, levels = available_classes, labels = labels)
    )

  pct <- round(100 * explained[1:2], 1)
  x_label <- sprintf("PC1 (%.1f%%)", pct[1])
  y_label <- sprintf("PC2 (%.1f%%)", pct[2])

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$PC1, y = .data$PC2, color = .data$class)) +
    ggplot2::geom_point(alpha = 0.75, size = 2.6) +
    ggplot2::scale_color_manual(values = class_palette) +
    ggplot2::labs(
      title = "PCA Scores Colored by Class",
      x = x_label,
      y = y_label,
      color = "Class"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
      panel.grid.major = ggplot2::element_line(color = "gray90"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "gray70", fill = NA, linewidth = 1),
      legend.position = "right"
    )
}

save_pca_plot <- function(plot) {
  filename <- sprintf("%s_class%s.png", OUTPUT_PREFIX, CLASS_COUNT)
  cat("プロット保存中:", filename, "\n")
  ggplot2::ggsave(filename, plot = plot, width = OUTPUT_WIDTH, height = OUTPUT_HEIGHT, dpi = OUTPUT_DPI)
}

# ================================================================
# メイン処理
# ================================================================

main <- function() {
  cat("=== PCA クラス可視化スクリプト ===\n")
  validate_settings()

  data <- load_data()
  prepared <- prepare_data(data)
  pca_result <- run_pca(prepared$scaled)

  cat("\n--- 可視化・保存 ---\n")
  pca_plot <- create_pca_plot(pca_result$scores, prepared$classes, pca_result$explained)
  save_pca_plot(pca_plot)

  cat("\n=== 処理完了 ===\n")
  cat(
    "設定: ", SCALING_METHOD, " スケーリング、クラス 1〜", CLASS_COUNT,
    " を色分け", sep = ""
  )
  cat("\n主成分寄与率: PC1=", round(100 * pca_result$explained[1], 1),
      "%, PC2=", round(100 * pca_result$explained[2], 1), "%\n", sep = "")
}

# 実行
main()
