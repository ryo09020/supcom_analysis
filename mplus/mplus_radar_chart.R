#################################################################
# Mplus LPAプロファイル可視化Rスクリプト (.outファイル読込版)
#
# 目的：
# - 指定されたMplusの.outファイルからLPAの結果を読み込む。
# - 結果をレーダーチャートで可視化する（重ね描き・個別）。
#
# 使用方法：
# 1. Mplusの.outファイルと、このRスクリプトを同じフォルダに置くか、
#    下記のファイルパスを正しく指定する。
# 2. R/RStudioでこのスクリプトを実行する。
#
# 注意事項：
# - Mplusの入力データは標準化済み(Z-score)であることを想定しています。
#   レーダーチャートの軸は±2で固定されています。
# - 下記の`lpa_target_columns`の項目名は、MplusのUSEVARIABLESで
#   指定した変数名（大文字・小文字を区別）と完全に一致させてください。
#################################################################


# ---------------------------------------------------------------
# 1. パッケージの準備
# ---------------------------------------------------------------
# 必要なパッケージのリスト
packages <- c("tidyverse", "MplusAutomation", "fmsb")

# インストールされていないパッケージをインストール
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# パッケージの読み込み
lapply(packages, library, character.only = TRUE)

cat("Step 1: パッケージの読み込みが完了しました。\n")


# ---------------------------------------------------------------
# ★★★★★ ここでMplusの出力ファイルと項目名を指定 ★★★★★
# ---------------------------------------------------------------
# 1. Mplusの出力ファイル名を指定
# (このRスクリプトと同じディレクトリにある場合)
#mplus_output_file <- "tipi-j/2.out"
mplus_output_file <- file.choose()

# (別の場所にある場合はフルパスを指定)
# mplus_output_file <- "path/to/your/file/2.out"

# 2. 分析対象の項目名を指定（MplusのUSEVARIABLESと完全に一致させる）
lpa_target_columns <- c("ITEMA", "ITEMB", "ITEMC", "ITEMD", "ITEME")

# 3. プロットのラベルに使用する新しい項目名（英語推奨）
new_item_labels <- c("Item A", "Item B", "Item C", "Item D", "Item E")
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# 2. Mplusの結果を読み込み
# ---------------------------------------------------------------
cat("Step 2: Mplusの結果ファイルを読み込みます...\n")

# ファイルの存在確認
if (!file.exists(mplus_output_file)) {
  stop(paste("エラー: 指定されたMplusファイル '", mplus_output_file, "' が見つかりません。", sep=""))
}

# MplusAutomationで結果を読み込む
# targetにファイルの場所、filefilterにファイル名を指定
mplus_model <- readModels(
  target = dirname(mplus_output_file),
  filefilter = basename(mplus_output_file)
)

cat(paste("'", basename(mplus_output_file), "' の読み込みが完了しました。\n", sep=""))


# ---------------------------------------------------------------
# 3. プロット用データの準備
# ---------------------------------------------------------------
cat("Step 3: プロット用データを作成します...\n")

# Mplusのパラメータ推定値から、各クラスの平均値(Means)を抽出
# Mplusの出力では、標準化されていない結果(unstandardized)に平均値が含まれる
estimated_means <- mplus_model$parameters$unstandardized

# レーダーチャート用のデータに整形
radar_data <- estimated_means %>%
  # paramHeaderが".Means"で終わる行（=クラス平均値）を抽出
  filter(str_detect(paramHeader, "\\.Means$")) %>%
  # paramが分析対象の項目名である行のみを抽出
  filter(param %in% lpa_target_columns) %>%
  # クラス番号を抽出（例: "C#1.Means" -> "1"）
  mutate(Class = str_extract(paramHeader, "(?<=C#)\\d+")) %>%
  # 必要な列を選択
  select(Class, item = param, mean_z_score = est) %>%
  # 縦長から横長データに変換
  pivot_wider(names_from = item, values_from = mean_z_score) %>%
  # 指定された項目名の順序に列を並び替える
  select(Class, all_of(lpa_target_columns)) %>%
  # クラス番号でソート
  arrange(as.numeric(Class)) %>%
  # Class列を行名に設定
  column_to_rownames("Class")

# 列名をプロット用のラベルに変更
colnames(radar_data) <- new_item_labels

cat("プロット用データの準備が完了しました。\n")
cat("検出されたクラス数: ", nrow(radar_data), "\n\n")


# ---------------------------------------------------------------
# 4. レーダーチャートの作成と表示（全クラス重ね描き）
# ---------------------------------------------------------------
cat("Step 4: 全クラスを重ねたレーダーチャートを作成します...\n")

# レーダーチャートの軸範囲を設定（±2標準偏差）
max_value <- 2.0
min_value <- -2.0

# fmsbパッケージ用のデータフレームを作成（1行目に最大値, 2行目に最小値を追加）
radar_df_all <- rbind(
  rep(max_value, ncol(radar_data)),
  rep(min_value, ncol(radar_data)),
  radar_data
)

# 色の設定
colors <- rainbow(nrow(radar_data))
border_colors <- rainbow(nrow(radar_data))

# レーダーチャートの描画
radarchart(
  radar_df_all,
  axistype = 1,
  # Aesthetics for the polygons
  pcol = border_colors,
  pfcol = NA, # 塗りつぶしなし
  plwd = 2,   # 線の太さ
  plty = 1,   # 線の種類
  # Aesthetics for the grid
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = c("-2", "-1", "0", "1", "2"),
  cglwd = 0.8,
  # Aesthetics for the variable labels
  vlcex = 1.2,
  title = paste("LPA Profiles from Mplus (", nrow(radar_data), " Clusters)\nAxis: Z-scores (±2 SD)", sep="")
)

# 凡例の追加
legend(
  x = "topright",
  legend = paste("Cluster", rownames(radar_data)),
  bty = "n",
  pch = 20,
  col = border_colors,
  text.col = "black",
  cex = 1.2,
  pt.cex = 2
)

cat("重ね描きのレーダーチャートが正常に作成されました。\n")


# ---------------------------------------------------------------
# 5. クラスターごとの個別レーダーチャート作成
# ---------------------------------------------------------------
cat("Step 5: クラスターごとの個別レーダーチャートを作成します...\n")

# グラフィックデバイスの設定を保存
par_original <- par(no.readonly = TRUE)

# クラスター数に応じてプロットのグリッドを設定
n_clusters <- nrow(radar_data)
if (n_clusters <= 2) {
  par(mfrow = c(1, n_clusters))
} else if (n_clusters <= 4) {
  par(mfrow = c(2, 2))
} else if (n_clusters <= 6) {
  par(mfrow = c(2, 3))
} else {
  par(mfrow = c(3, 3)) # 9個まで対応
}

# 各クラスターごとにレーダーチャートを作成
for (i in 1:n_clusters) {
  # 各クラスター用のデータフレーム作成
  individual_radar_df <- rbind(
    rep(max_value, ncol(radar_data)),  # 最大値
    rep(min_value, ncol(radar_data)),  # 最小値
    radar_data[i, , drop = FALSE]      # 該当クラスターのデータ
  )

  # 個別レーダーチャートの描画
  radarchart(
    individual_radar_df,
    axistype = 1,
    pcol = border_colors[i],
    pfcol = scales::alpha(border_colors[i], 0.2), # 薄い色で塗りつぶし
    plwd = 3,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = c("-2", "-1", "0", "1", "2"),
    cglwd = 0.8,
    vlcex = 1.0,
    title = paste("Cluster", rownames(radar_data)[i], "Profile")
  )
}

# グリッド設定を元に戻す
par(par_original)

cat("クラスターごとの個別レーダーチャートが正常に作成されました。\n")
cat("\n分析とプロットの作成が完了しました。\n")