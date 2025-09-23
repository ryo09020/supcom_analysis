# -------------------------------------------------------------------------
# 相関の低い項目自動選択スクリプト
# 目的：全項目から互いに相関の低い項目セットを自動選択し、ヒートマップを作成
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Step 0: パッケージのインストールと読み込み
# -------------------------------------------------------------------------

library(pheatmap)
library(tidyverse)
library(ggplot2)

# -------------------------------------------------------------------------
# Step 1: 設定項目
# -------------------------------------------------------------------------

# ▼ ユーザーが設定する項目 ▼
file_path <- "raw_data/dummy_data_286items.csv"

# 相関閾値の設定（この値未満の相関を「低い相関」とする）
CORRELATION_THRESHOLD <- 0.3  # 0.3未満を低相関とする（調整可能）

# 最大選択項目数の設定
MAX_ITEMS <- 20  # 最大何項目まで選択するか

# 分析したい全項目の列名を指定してください
all_target_columns <- c("Item_001","Item_002","Item_003","Item_004","Item_005","Item_006","Item_007","Item_008","Item_009","Item_010","Item_011","Item_012","Item_013","Item_014","Item_015","Item_016","Item_017","Item_018","Item_019","Item_020","Item_021","Item_022","Item_023","Item_024","Item_025","Item_026","Item_027","Item_028","Item_029","Item_030","Item_031","Item_032","Item_033","Item_034","Item_035","Item_036","Item_037","Item_038","Item_039","Item_040","Item_041","Item_042","Item_043")

# 各項目の表示用ラベル
all_item_labels <- c("Item_001","Item_002","Item_003","Item_004","Item_005","Item_006","Item_007","Item_008","Item_009","Item_010","Item_011","Item_012","Item_013","Item_014","Item_015","Item_016","Item_017","Item_018","Item_019","Item_020","Item_021","Item_022","Item_023","Item_024","Item_025","Item_026","Item_027","Item_028","Item_029","Item_030","Item_031","Item_032","Item_033","Item_034","Item_035","Item_036","Item_037","Item_038","Item_039","Item_040","Item_041","Item_042","Item_043")

# -------------------------------------------------------------------------
# Step 2: データの読み込みと前処理
# -------------------------------------------------------------------------

cat("データを読み込み中...\n")
full_data <- read.csv(file_path)

cat("データを前処理中...\n")
analysis_data <- full_data %>%
  select(all_of(all_target_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

colnames(analysis_data) <- all_item_labels

cat(paste("元のデータ行数:", nrow(full_data), "\n"))
cat(paste("前処理後のデータ行数:", nrow(analysis_data), "\n"))
cat(paste("分析項目数:", ncol(analysis_data), "\n\n"))

# -------------------------------------------------------------------------
# Step 3: 全項目間の相関行列を計算
# -------------------------------------------------------------------------

cat("全項目間の相関を計算中...\n")
cor_matrix_full <- cor(analysis_data, use = "everything")

# -------------------------------------------------------------------------
# Step 4: 相関の低い項目を自動選択するアルゴリズム
# -------------------------------------------------------------------------

#' 相関の低い項目を自動選択する関数
#' @param cor_matrix 相関行列
#' @param threshold 相関閾値
#' @param max_items 最大選択項目数
#' @return 選択された項目のインデックス
select_uncorrelated_items <- function(cor_matrix, threshold = 0.3, max_items = 20) {
  
  cat("相関の低い項目を自動選択中...\n")
  cat(paste("相関閾値:", threshold, "\n"))
  cat(paste("最大選択項目数:", max_items, "\n\n"))
  
  n_items <- ncol(cor_matrix)
  item_names <- colnames(cor_matrix)
  
  # 絶対値相関行列を作成（対角線は0にする）
  abs_cor <- abs(cor_matrix)
  diag(abs_cor) <- 0
  
  # 各項目の平均絶対相関を計算
  mean_abs_cor <- rowMeans(abs_cor)
  
  # 平均絶対相関が低い順にソート
  sorted_indices <- order(mean_abs_cor)
  
  selected_items <- c()
  
  # 貪欲法で項目を選択
  for (i in sorted_indices) {
    if (length(selected_items) >= max_items) break
    
    item_name <- item_names[i]
    
    # 既に選択された項目との相関をチェック
    if (length(selected_items) == 0) {
      # 最初の項目は無条件で選択
      selected_items <- c(selected_items, i)
      cat(paste("選択項目", length(selected_items), ":", item_name, 
                "(平均絶対相関:", round(mean_abs_cor[i], 3), ")\n"))
    } else {
      # 既選択項目との最大絶対相関を計算
      max_cor_with_selected <- max(abs(cor_matrix[i, selected_items]))
      
      if (max_cor_with_selected < threshold) {
        selected_items <- c(selected_items, i)
        cat(paste("選択項目", length(selected_items), ":", item_name, 
                  "(平均絶対相関:", round(mean_abs_cor[i], 3), 
                  ", 既選択項目との最大相関:", round(max_cor_with_selected, 3), ")\n"))
      } else {
        cat(paste("除外項目:", item_name, 
                  "(既選択項目との最大相関:", round(max_cor_with_selected, 3), "> 閾値", threshold, ")\n"))
      }
    }
  }
  
  cat(paste("\n最終選択項目数:", length(selected_items), "\n"))
  return(selected_items)
}

# 項目選択を実行
selected_indices <- select_uncorrelated_items(
  cor_matrix_full, 
  threshold = CORRELATION_THRESHOLD, 
  max_items = MAX_ITEMS
)

selected_item_names <- colnames(cor_matrix_full)[selected_indices]
cat("\n=== 選択された項目一覧 ===\n")
for (i in 1:length(selected_item_names)) {
  cat(paste(i, ":", selected_item_names[i], "\n"))
}

# -------------------------------------------------------------------------
# Step 5: 選択された項目のデータと相関行列を作成
# -------------------------------------------------------------------------

cat("\n選択された項目のデータを準備中...\n")

# 選択された項目のデータ
selected_data <- analysis_data[, selected_indices]
selected_cor_matrix <- cor_matrix_full[selected_indices, selected_indices]

# 選択結果の統計情報
cat("\n=== 選択結果の統計情報 ===\n")
cat(paste("選択項目数:", ncol(selected_data), "\n"))

# 選択された項目間の相関の分布
selected_cor_values <- selected_cor_matrix[upper.tri(selected_cor_matrix)]
cat(paste("選択項目間の相関範囲:", round(min(selected_cor_values), 3), "～", round(max(selected_cor_values), 3), "\n"))
cat(paste("選択項目間の平均絶対相関:", round(mean(abs(selected_cor_values)), 3), "\n"))
cat(paste("選択項目間の中央値絶対相関:", round(median(abs(selected_cor_values)), 3), "\n"))

# 全項目間の相関との比較
all_cor_values <- cor_matrix_full[upper.tri(cor_matrix_full)]
cat(paste("全項目間の平均絶対相関:", round(mean(abs(all_cor_values)), 3), "\n"))
cat(paste("相関低減効果:", round(mean(abs(all_cor_values)) - mean(abs(selected_cor_values)), 3), "\n\n"))

# -------------------------------------------------------------------------
# Step 6: 選択された項目のヒートマップ作成
# -------------------------------------------------------------------------

cat("選択項目のヒートマップを作成中...\n")

n_selected <- ncol(selected_cor_matrix)

# 項目数に応じてパラメータを設定
if (n_selected <= 10) {
  show_numbers <- TRUE
  font_size <- max(8, 120 / n_selected)
  row_font_size <- max(10, 150 / n_selected)
  col_font_size <- max(10, 150 / n_selected)
  main_title_size <- 16
} else if (n_selected <= 20) {
  show_numbers <- TRUE
  font_size <- max(6, 100 / n_selected)
  row_font_size <- max(8, 120 / n_selected)
  col_font_size <- max(8, 120 / n_selected)
  main_title_size <- 14
} else {
  show_numbers <- FALSE
  font_size <- max(4, 80 / n_selected)
  row_font_size <- max(6, 100 / n_selected)
  col_font_size <- max(6, 100 / n_selected)
  main_title_size <- 12
}

# 色の境界値を設定
breaks <- seq(-1, 1, length.out = 101)

# ヒートマップ作成
p_selected <- pheatmap(
  selected_cor_matrix,
  main = paste("Selected Uncorrelated Items (n =", n_selected, ")"),
  display_numbers = show_numbers,
  number_format = "%.2f",
  fontsize_number = font_size,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100),
  breaks = breaks,
  border_color = "white",
  fontsize_row = row_font_size,
  fontsize_col = col_font_size,
  fontsize = main_title_size,
  angle_col = 45,
  treeheight_row = 50,
  treeheight_col = 50,
  silent = TRUE
)

# 選択項目のヒートマップを保存
output_filename_selected <- sprintf("selected_uncorrelated_items_%d_threshold%.2f.png", n_selected, CORRELATION_THRESHOLD)

ggsave(
  filename = output_filename_selected,
  plot = p_selected,
  width = 10,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat(sprintf("選択項目のヒートマップが '%s' として保存されました。\n", output_filename_selected))

# -------------------------------------------------------------------------
# Step 7: 比較用の全項目ヒートマップも作成
# -------------------------------------------------------------------------

cat("\n比較用の全項目ヒートマップを作成中...\n")

# 全項目のヒートマップパラメータ
n_all <- ncol(cor_matrix_full)
if (n_all <= 30) {
  show_numbers_all <- TRUE
  font_size_all <- max(4, 80 / n_all)
  row_font_size_all <- max(6, 100 / n_all)
  col_font_size_all <- max(6, 100 / n_all)
  main_title_size_all <- 14
} else {
  show_numbers_all <- FALSE
  font_size_all <- max(3, 60 / n_all)
  row_font_size_all <- max(4, 80 / n_all)
  col_font_size_all <- max(4, 80 / n_all)
  main_title_size_all <- 12
}

p_all <- pheatmap(
  cor_matrix_full,
  main = paste("All Items (n =", n_all, ")"),
  display_numbers = show_numbers_all,
  number_format = "%.2f",
  fontsize_number = font_size_all,
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  color = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100),
  breaks = breaks,
  border_color = "white",
  fontsize_row = row_font_size_all,
  fontsize_col = col_font_size_all,
  fontsize = main_title_size_all,
  angle_col = 45,
  treeheight_row = 30,
  treeheight_col = 30,
  silent = TRUE
)

output_filename_all <- sprintf("all_items_comparison_%ditems.png", n_all)

ggsave(
  filename = output_filename_all,
  plot = p_all,
  width = 12,
  height = 12,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat(sprintf("全項目比較用ヒートマップが '%s' として保存されました。\n", output_filename_all))

# -------------------------------------------------------------------------
# Step 8: 選択された項目をCSVで出力
# -------------------------------------------------------------------------

# 選択された項目の情報をCSVで保存
selected_items_info <- data.frame(
  Item_Index = selected_indices,
  Item_Name = selected_item_names,
  Mean_Abs_Correlation_with_All = round(rowMeans(abs(cor_matrix_full[selected_indices, ])), 3),
  Mean_Abs_Correlation_with_Selected = round(rowMeans(abs(selected_cor_matrix)), 3),
  stringsAsFactors = FALSE
)

csv_filename <- sprintf("selected_uncorrelated_items_threshold%.2f.csv", CORRELATION_THRESHOLD)
write.csv(selected_items_info, csv_filename, row.names = FALSE)

cat(sprintf("\n選択項目情報が '%s' として保存されました。\n", csv_filename))

# -------------------------------------------------------------------------
# Step 9: LPA用の項目リスト出力
# -------------------------------------------------------------------------

cat("\n=== LPA分析用の項目リスト ===\n")
cat("以下の項目をLPA分析に使用することをお勧めします：\n\n")

cat("# LPA用の項目設定（コピペ用）\n")
cat("TARGET_COLUMNS <- c(\n")
for (i in 1:length(selected_item_names)) {
  if (i < length(selected_item_names)) {
    cat(paste('  "', selected_item_names[i], '",\n', sep = ""))
  } else {
    cat(paste('  "', selected_item_names[i], '"\n', sep = ""))
  }
}
cat(")\n\n")

# 選択項目の元の列名も出力（ラベルではなく実際の列名が必要な場合）
original_selected_columns <- all_target_columns[selected_indices]
cat("# 元の列名でのLPA用項目設定\n")
cat("TARGET_COLUMNS <- c(\n")
for (i in 1:length(original_selected_columns)) {
  if (i < length(original_selected_columns)) {
    cat(paste('  "', original_selected_columns[i], '",\n', sep = ""))
  } else {
    cat(paste('  "', original_selected_columns[i], '"\n', sep = ""))
  }
}
cat(")\n\n")

# -------------------------------------------------------------------------
# Step 10: 要約統計
# -------------------------------------------------------------------------

cat("=== 最終要約 ===\n")
cat(paste("元項目数:", ncol(cor_matrix_full), "\n"))
cat(paste("選択項目数:", length(selected_item_names), "\n"))
cat(paste("項目削減率:", round((1 - length(selected_item_names)/ncol(cor_matrix_full)) * 100, 1), "%\n"))
cat(paste("相関閾値:", CORRELATION_THRESHOLD, "\n"))
cat(paste("選択項目間の最大絶対相関:", round(max(abs(selected_cor_values)), 3), "\n"))
cat(paste("選択項目間の平均絶対相関:", round(mean(abs(selected_cor_values)), 3), "\n\n"))

cat("選択された項目は互いに低い相関を持ち、LPA分析に適しています。\n")
cat("生成されたファイル:\n")
cat(paste("- ", output_filename_selected, " (選択項目のヒートマップ)\n"))
cat(paste("- ", output_filename_all, " (全項目比較用ヒートマップ)\n"))
cat(paste("- ", csv_filename, " (選択項目情報CSV)\n\n"))

cat("=== 項目選択スクリプトの実行が完了しました ===\n")
