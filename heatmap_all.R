# -------------------------------------------------------------------------
# Step 0: パッケージのインストールと読み込み
# -------------------------------------------------------------------------

# pheatmapパッケージをまだインストールしていない場合は、以下の行のコメントを外して実行してください
# install.packages("pheatmap")
# install.packages("tidyverse")
# install.packages("ggplot2")

# パッケージを読み込みます
library(pheatmap)
library(tidyverse)
library(ggplot2)


# -------------------------------------------------------------------------
# Step 1: データの読み込みと準備
# -------------------------------------------------------------------------

# ▼ ユーザーが設定する項目 ▼
# ご自身のCSVファイルへのパスを指定してください
file_path <- "raw_data/dummy_data_286items.csv"

# データをデータフレームとして読み込みます
# ファイルの文字コードがShift-JISの場合は fileEncoding = "CP932" を追加してください
# 例: read.csv(file_path, fileEncoding = "CP932")
full_data <- read.csv(file_path)
names(full_data)

# ▼ ユーザーが設定する項目 ▼
# 分析したい全項目の列名を指定してください
# (お手元のデータに合わせて正確な列名に書き換えてください)
all_target_columns <- c("Item_001","Item_002","Item_003","Item_004","Item_005","Item_006","Item_007","Item_008","Item_009","Item_010","Item_011","Item_012","Item_013","Item_014","Item_015","Item_016","Item_017","Item_018","Item_019","Item_020","Item_021","Item_022","Item_023","Item_024","Item_025","Item_026","Item_027","Item_028","Item_029","Item_030","Item_031","Item_032","Item_033","Item_034","Item_035","Item_036","Item_037","Item_038","Item_039","Item_040","Item_041","Item_042","Item_043")

# ▼ 図表で表示する項目名を設定してください ▼
# 各項目の表示用ラベル（英語推奨）
all_item_labels <- c("Item_001","Item_002","Item_003","Item_004","Item_005","Item_006","Item_007","Item_008","Item_009","Item_010","Item_011","Item_012","Item_013","Item_014","Item_015","Item_016","Item_017","Item_018","Item_019","Item_020","Item_021","Item_022","Item_023","Item_024","Item_025","Item_026","Item_027","Item_028","Item_029","Item_030","Item_031","Item_032","Item_033","Item_034","Item_035","Item_036","Item_037","Item_038","Item_039","Item_040","Item_041","Item_042","Item_043")

# データの読み込みと前処理
cat("... データを前処理中\n")

# 分析対象の列を抽出
analysis_data <- full_data %>%
  select(all_of(all_target_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%  # 数値に変換（変換できないものはNAになる）
  na.omit()  # 欠損値（数値に変換できなかったものを含む）を含む行を除外

# データの確認
cat(paste("元のデータ行数:", nrow(full_data), "\n"))
cat(paste("前処理後のデータ行数:", nrow(analysis_data), "\n"))
cat(paste("除外されたレコード数:", nrow(full_data) - nrow(analysis_data), "\n"))

# 列名をラベルに変更
colnames(analysis_data) <- all_item_labels


# -------------------------------------------------------------------------
# Step 2: 相関行列の計算
# -------------------------------------------------------------------------

# 全項目間の相関行列を計算
cat("全項目間の相関を計算中...\n")
cor_matrix_all <- cor(analysis_data, use = "everything")

# 計算結果の確認（任意）
cat("相関行列の計算が完了しました。\n")
# print(cor_matrix_all)


# -------------------------------------------------------------------------
# Step 3: ヒートマップの描画（スパコン対応版）
# -------------------------------------------------------------------------

cat("全項目間の相関ヒートマップを作成中...\n")

# 項目数に応じてパラメータを設定 (この部分は元のコードと同じ)
n_items <- ncol(cor_matrix_all)
cat(sprintf("項目数: %d\n", n_items))

if (n_items <= 30) {
  show_numbers <- TRUE
  font_size <- max(6, 120 / n_items)
  row_font_size <- max(8, 150 / n_items)
  col_font_size <- max(8, 150 / n_items)
  main_title_size <- 14
} else if (n_items <= 50) {
  show_numbers <- TRUE
  font_size <- max(4, 80 / n_items)
  row_font_size <- max(6, 120 / n_items)
  col_font_size <- max(6, 120 / n_items)
  main_title_size <- 12
} else if (n_items <= 100) {
  show_numbers <- FALSE
  font_size <- 4
  row_font_size <- max(4, 100 / n_items)
  col_font_size <- max(4, 100 / n_items)
  main_title_size <- 10
} else {
  show_numbers <- FALSE
  font_size <- 3
  row_font_size <- max(3, 80 / n_items)
  col_font_size <- max(3, 80 / n_items)
  main_title_size <- 8
}


# pheatmapを直接描画せず、オブジェクトとして作成
p <- pheatmap(
  cor_matrix_all,
  main = "Correlation Matrix",
  display_numbers = show_numbers,
  number_format = "%.2f",
  fontsize_number = font_size,
  cluster_rows = TRUE,      # クラスタリングを有効にする場合
  cluster_cols = TRUE,      # クラスタリングを有効にする場合
  # cluster_rows = FALSE,   # クラスタリングしない場合はこちら
  # cluster_cols = FALSE,
  color = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100),
  border_color = "white",
  fontsize_row = row_font_size,
  fontsize_col = col_font_size,
  fontsize = main_title_size,
  angle_col = 45,
  treeheight_row = 50, # treeheightを0にするとクラスタリングが見えないので適宜調整
  treeheight_col = 50,
  silent = TRUE
)

# ファイル名を設定
output_filename <- sprintf("correlation_heatmap_%ditems.png", n_items)


# ggsave関数を使って、プロットを確実に正方形でファイルに保存
cat(sprintf("ヒートマップを '%s' として正方形で保存します...\n", output_filename))
ggsave(
  filename = output_filename,
  plot = p,           # pheatmapオブジェクトを指定
  width = 12,         # 幅を12インチに指定
  height = 12,        # 高さを12インチに指定 (幅と同じ値にする)
  units = "in",       # 単位をインチに
  dpi = 300,          # 解像度 (論文投稿用)
  bg = "white"        # 背景色を白に指定
)

cat(sprintf("ヒートマップが '%s' として保存されました。\n", output_filename))
cat("論文品質ヒートマップの作成が完了しました。\n")


# -------------------------------------------------------------------------
# Step 4: 統計情報の表示
# -------------------------------------------------------------------------

cat("\n=== 相関行列の統計情報 ===\n")
cat(paste("項目数:", ncol(cor_matrix_all), "\n"))
cat(paste("総相関ペア数:", ncol(cor_matrix_all) * (ncol(cor_matrix_all) - 1) / 2, "\n"))

# 対角線を除いた相関係数の統計
cor_values <- cor_matrix_all[upper.tri(cor_matrix_all)]
cat(paste("相関係数の範囲: ", round(min(cor_values), 3), " ～ ", round(max(cor_values), 3), "\n"))
cat(paste("相関係数の平均: ", round(mean(cor_values), 3), "\n"))
cat(paste("相関係数の標準偏差: ", round(sd(cor_values), 3), "\n"))

# 強い相関（|r| > 0.7）のペアを表示
strong_cor_threshold <- 0.7
strong_cor_indices <- which(abs(cor_matrix_all) > strong_cor_threshold & cor_matrix_all != 1, arr.ind = TRUE)

if (nrow(strong_cor_indices) > 0) {
  cat(paste("\n強い相関 (|r| >", strong_cor_threshold, ") のペア:\n"))
  # 上三角行列の部分だけをユニークに表示するための修正
  printed_pairs <- c()
  for (i in seq_len(nrow(strong_cor_indices))) {
    row_idx <- strong_cor_indices[i, "row"]
    col_idx <- strong_cor_indices[i, "col"]
    # ペアをソートして重複を避ける
    pair_key <- paste(sort(c(rownames(cor_matrix_all)[row_idx], colnames(cor_matrix_all)[col_idx])), collapse="-")
    
    if (!(pair_key %in% printed_pairs)) {
      cor_value <- cor_matrix_all[row_idx, col_idx]
      cat(sprintf("%s vs %s: r = %.3f\n",
                  rownames(cor_matrix_all)[row_idx],
                  colnames(cor_matrix_all)[col_idx],
                  cor_value))
      printed_pairs <- c(printed_pairs, pair_key)
    }
  }
} else {
  cat(paste("\n強い相関 (|r| >", strong_cor_threshold, ") のペアはありませんでした。\n"))
}

cat("\n=== heatmap_all.R の実行が完了しました ===\n")