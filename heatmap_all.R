# -------------------------------------------------------------------------
# Step 0: パッケージのインストールと読み込み
# -------------------------------------------------------------------------

# pheatmapパッケージをまだインストールしていない場合は、以下の行のコメントを外して実行してください
# install.packages("pheatmap")
# install.packages("tidyverse")

# パッケージを読み込みます
library(pheatmap)
library(tidyverse)


# -------------------------------------------------------------------------
# Step 1: データの読み込みと準備
# -------------------------------------------------------------------------

# ▼ ユーザーが設定する項目 ▼
# ご自身のCSVファイルへのパスを指定してください
file_path <- "raw_data/dummy_data.csv" 

# データをデータフレームとして読み込みます
# ファイルの文字コードがShift-JISの場合は fileEncoding = "CP932" を追加してください
# 例: read.csv(file_path, fileEncoding = "CP932")
full_data <- read.csv(file_path)
names(full_data)

# ▼ ユーザーが設定する項目 ▼
# 分析したい全項目の列名を指定してください
# (お手元のデータに合わせて正確な列名に書き換えてください)
all_target_columns <- c("X542690_00", "X542700_00", "X542710_00", "X542720_00", "X542730_00")

# ▼ 図表で表示する項目名を設定してください ▼
# 各項目の表示用ラベル（英語推奨）
all_item_labels <- c("Item A", "Item B", "Item C", "Item D", "Item E")

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
# Step 3: ヒートマップの描画
# -------------------------------------------------------------------------

# 全項目間の相関ヒートマップを作成
cat("全項目間の相関ヒートマップを作成中...\n")
pheatmap(
  cor_matrix_all,                              # 表示する相関行列
  main = "Correlation Heatmap for All Selected Items",  # グラフのタイトル（英語）
  display_numbers = TRUE,                      # セルに相関係数の数値を表示する
  number_format = "%.3f",                      # 表示する数値の小数点以下の桁数
  fontsize_number = 12,                        # セル内の数値のフォントサイズ
  cluster_rows = FALSE,                        # 行をクラスタリングしない（元の順序を維持）
  cluster_cols = FALSE,                        # 列をクラスタリングしない（元の順序を維持）
  color = colorRampPalette(c("blue", "white", "red"))(100), # 色の指定（青:負相関, 白:無相関, 赤:正相関）
  cellwidth = 50,                              # セルの幅を調整
  cellheight = 50,                             # セルの高さを調整
  border_color = "grey60"                      # セルの境界線の色
)

cat("ヒートマップの作成が完了しました。\n")


# -------------------------------------------------------------------------
# Step 4: クラスタリング版ヒートマップ（オプション）
# -------------------------------------------------------------------------

# クラスタリングを有効にしたヒートマップも作成
cat("クラスタリング版ヒートマップを作成中...\n")
pheatmap(
  cor_matrix_all,                              # 表示する相関行列
  main = "Correlation Heatmap (with Clustering)",  # グラフのタイトル（英語）
  display_numbers = TRUE,                      # セルに相関係数の数値を表示する
  number_format = "%.3f",                      # 表示する数値の小数点以下の桁数
  fontsize_number = 12,                        # セル内の数値のフォントサイズ
  cluster_rows = TRUE,                         # 行をクラスタリングする
  cluster_cols = TRUE,                         # 列をクラスタリングする
  color = colorRampPalette(c("blue", "white", "red"))(100), # 色の指定
  cellwidth = 50,                              # セルの幅を調整
  cellheight = 50,                             # セルの高さを調整
  border_color = "grey60"                      # セルの境界線の色
)

cat("クラスタリング版ヒートマップの作成が完了しました。\n")


# -------------------------------------------------------------------------
# Step 5: 統計情報の表示
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
  for (i in 1:nrow(strong_cor_indices)) {
    row_idx <- strong_cor_indices[i, 1]
    col_idx <- strong_cor_indices[i, 2]
    cor_value <- cor_matrix_all[row_idx, col_idx]
    cat(sprintf("%s vs %s: r = %.3f\n", 
                rownames(cor_matrix_all)[row_idx],
                colnames(cor_matrix_all)[col_idx],
                cor_value))
  }
} else {
  cat(paste("\n強い相関 (|r| >", strong_cor_threshold, ") のペアはありませんでした。\n"))
}


# -------------------------------------------------------------------------
# Step 6: 画像保存機能（オプション）
# -------------------------------------------------------------------------

# 画像を保存したい場合は、以下のコメントを外して使用してください

# 1. 通常版ヒートマップを保存
# png("correlation_heatmap_all_items.png", width = 1000, height = 1000, res = 150)
# pheatmap(cor_matrix_all, 
#          main = "Correlation Heatmap for All Selected Items",
#          display_numbers = TRUE, number_format = "%.3f", fontsize_number = 12,
#          cluster_rows = FALSE, cluster_cols = FALSE,
#          color = colorRampPalette(c("blue", "white", "red"))(100),
#          cellwidth = 50, cellheight = 50, border_color = "grey60")
# dev.off()

# 2. クラスタリング版ヒートマップを保存
# png("correlation_heatmap_all_items_clustered.png", width = 1000, height = 1000, res = 150)
# pheatmap(cor_matrix_all,
#          main = "Correlation Heatmap (with Clustering)",
#          display_numbers = TRUE, number_format = "%.3f", fontsize_number = 12,
#          cluster_rows = TRUE, cluster_cols = TRUE,
#          color = colorRampPalette(c("blue", "white", "red"))(100),
#          cellwidth = 50, cellheight = 50, border_color = "grey60")
# dev.off()

# 3. PDF版での保存
# pdf("correlation_heatmap_all_items.pdf", width = 12, height = 12)
# pheatmap(cor_matrix_all, 
#          main = "Correlation Heatmap for All Selected Items",
#          display_numbers = TRUE, number_format = "%.3f", fontsize_number = 12,
#          cluster_rows = FALSE, cluster_cols = FALSE,
#          color = colorRampPalette(c("blue", "white", "red"))(100),
#          cellwidth = 50, cellheight = 50, border_color = "grey60")
# dev.off()

cat("\n=== heatmap_all.R の実行が完了しました ===\n")
cat("作成されたヒートマップ:\n")
cat("1. 通常版: 元の項目順序を維持\n")
cat("2. クラスタリング版: 相関の高い項目を近くに配置\n")
cat("\n項目の設定:\n")
for (i in 1:length(all_target_columns)) {
  cat(sprintf("- %s: %s\n", all_target_columns[i], all_item_labels[i]))
}
