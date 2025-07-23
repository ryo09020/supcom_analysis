# -------------------------------------------------------------------------
# 286×286大規模データ用ヒートマップテストスクリプト
# -------------------------------------------------------------------------

library(pheatmap)
library(tidyverse)

# ▼ 286項目のダミーデータを使用 ▼
file_path <- "raw_data/dummy_data_286items.csv" 

# データを読み込み
cat("286項目のダミーデータを読み込み中...\n")
full_data <- read.csv(file_path)

# 全ての項目を分析対象とする
all_target_columns <- colnames(full_data)
cat(sprintf("読み込まれた項目数: %d\n", length(all_target_columns)))

# 項目ラベルをそのまま使用
all_item_labels <- all_target_columns

# データの前処理
cat("データを前処理中...\n")
analysis_data <- full_data %>%
  select(all_of(all_target_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

cat(sprintf("前処理後のデータ: %d行 × %d列\n", nrow(analysis_data), ncol(analysis_data)))

# 列名をラベルに変更
colnames(analysis_data) <- all_item_labels

# 相関行列の計算
cat("286×286相関行列を計算中...\n")
cor_matrix_all <- cor(analysis_data, use = "everything")
cat(sprintf("相関行列のサイズ: %d × %d\n", nrow(cor_matrix_all), ncol(cor_matrix_all)))

# 項目数に応じてセルサイズを動的に調整
n_items <- ncol(cor_matrix_all)
cat(sprintf("\n=== 286項目用の表示設定 ===\n"))

if (n_items > 100) {
  # 286項目の場合：極小セル、数値表示なし
  cell_size <- max(2, 150 / n_items)  # 286項目の場合約0.5px、最小2px保証
  show_numbers <- FALSE
  font_size <- 4  # 極小フォント
  cat(sprintf("大規模データ（%d項目）:\n", n_items))
  cat(sprintf("- セルサイズ: %.2fpx\n", cell_size))
  cat(sprintf("- 数値表示: オフ\n"))
  cat(sprintf("- フォントサイズ: %dpt\n", font_size))
}

# メイン相関ヒートマップ（通常版）
cat("\n286×286相関ヒートマップ（通常版）を作成中...\n")
pheatmap(
  cor_matrix_all,
  main = "286×286 Correlation Heatmap (Standard Order)",
  display_numbers = show_numbers,
  number_format = "%.2f",
  fontsize_number = font_size,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  cellwidth = cell_size,
  cellheight = cell_size,
  border_color = "grey80",  # 境界線を薄くして視覚的ノイズを軽減
  fontsize_row = max(3, min(8, 150 / n_items)),
  fontsize_col = max(3, min(8, 150 / n_items)),
  show_rownames = FALSE,  # 行名を非表示（286項目では読めないため）
  show_colnames = FALSE   # 列名を非表示（286項目では読めないため）
)

cat("通常版ヒートマップの作成が完了しました。\n")

# クラスタリング版
cat("\n286×286相関ヒートマップ（クラスタリング版）を作成中...\n")
pheatmap(
  cor_matrix_all,
  main = "286×286 Correlation Heatmap (with Clustering)",
  display_numbers = show_numbers,
  number_format = "%.2f",
  fontsize_number = font_size,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  cellwidth = cell_size,
  cellheight = cell_size,
  border_color = "grey80",
  fontsize_row = max(3, min(8, 150 / n_items)),
  fontsize_col = max(3, min(8, 150 / n_items)),
  show_rownames = FALSE,
  show_colnames = FALSE
)

cat("クラスタリング版ヒートマップの作成が完了しました。\n")

# 統計情報の表示
cat("\n=== 286×286相関行列の統計情報 ===\n")
cat(sprintf("項目数: %d\n", ncol(cor_matrix_all)))
cat(sprintf("総相関ペア数: %d\n", ncol(cor_matrix_all) * (ncol(cor_matrix_all) - 1) / 2))

# 対角線を除いた相関係数の統計
cor_values <- cor_matrix_all[upper.tri(cor_matrix_all)]
cat(sprintf("相関係数の範囲: %.3f ～ %.3f\n", min(cor_values), max(cor_values)))
cat(sprintf("平均相関係数: %.3f\n", mean(cor_values)))
cat(sprintf("相関の標準偏差: %.3f\n", sd(cor_values)))

# 強い相関の数をカウント
strong_thresholds <- c(0.3, 0.5, 0.7, 0.9)
for (threshold in strong_thresholds) {
  strong_count <- sum(abs(cor_values) >= threshold)
  percentage <- (strong_count / length(cor_values)) * 100
  cat(sprintf("強い相関 (|r| >= %.1f): %d個 (%.2f%%)\n", 
              threshold, strong_count, percentage))
}

# 相関分布ヒストグラム
cat("\n相関係数の分布ヒストグラムを作成中...\n")
hist(cor_values, 
     breaks = 50, 
     main = "Distribution of 286×286 Correlation Coefficients",
     xlab = "Correlation Coefficient", 
     ylab = "Frequency",
     col = "lightblue",
     border = "white")
abline(v = 0, col = "red", lty = 2, lwd = 2)

cat("\n=== 286×286大規模データテストが完了しました！ ===\n")
