# -------------------------------------------------------------------------
# Step 0: パッケージのインストールと読み込み
# -------------------------------------------------------------------------

# pheatmapパッケージをまだインストールしていない場合は、以下の行のコメントを外して実行してください
# install.packages("pheatmap")

# パッケージを読み込みます
library(pheatmap)


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
# データからTIPI-JとNEO-FFIの列名を指定してください
# (お手元のデータに合わせて正確な列名に書き換えてください)
tipi_j_columns <- c("X542690_00","X542700_00")
neo_ffi_columns <- c("X542690_00","X542700_00")
# それぞれのテストのデータだけを抽出します
tipi_j_data <- full_data[, tipi_j_columns]
neo_ffi_data <- full_data[, neo_ffi_columns]


# -------------------------------------------------------------------------
# Step 2: 相関行列の計算
# -------------------------------------------------------------------------

# cor()関数を使って、TIPI-Jの各項目とNEO-FFIの各項目の間の相関行列を計算します
# use="complete.obs" は、欠損値がある行を無視して計算するオプションです
cor_matrix <- cor(tipi_j_data, neo_ffi_data, use = "complete.obs")

# 計算結果の確認（任意）
# print(cor_matrix)


# -------------------------------------------------------------------------
# Step 3: ヒートマップの描画
# -------------------------------------------------------------------------

# pheatmap()関数でヒートマップを描画します
pheatmap(
  cor_matrix,                                  # 表示する相関行列
  main = "TIPI-JとNEO-FFIの項目間相関ヒートマップ",  # グラフのタイトル
  display_numbers = TRUE,                      # セルに相関係数の数値を表示する
  number_format = "%.2f",                      # 表示する数値の小数点以下の桁数
  fontsize_number = 10,                        # セル内の数値のフォントサイズ
  cluster_rows = FALSE,                        # 行（TIPI-J）をクラスタリングしない（元の順序を維持）
  cluster_cols = FALSE,                        # 列（NEO-FFI）をクラスタリングしない（元の順序を維持）
  color = colorRampPalette(c("blue", "white", "red"))(100) # 色の指定（青:負相関, 白:無相関, 赤:正相関）
)

# もし画像をファイルとして保存したい場合は、以下のようにします
# png("correlation_heatmap.png", width = 800, height = 600)
# pheatmap(...) # 上記のpheatmapコードをここに記述
# dev.off()