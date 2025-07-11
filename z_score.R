#################################################################
# LPAプロファイル可視化Rスクリプト（単一モデル実行バージョン）
#
# 目的：
# - 指定された単一のクラスター数でLPAを実行する。
# - 適合度指標（AIC, BIC）とクラス所属率をコンソールに出力する。
# - 結果を棒グラフとレーダーチャートで可視化する（描画は英語表記）。
#
# 使用方法：
# 1. RStudioでの実行（推奨）:
#    - 下記でファイルパスを直接指定するか、file.choose()でファイル選択
# 2. source()関数での実行:
#    source("z_score.R")
#################################################################

# ---------------------------------------------------------------
# 1. パッケージの準備
# ---------------------------------------------------------------
# 必要なパッケージのリスト
packages <- c("tidyverse", "tidyLPA", "knitr", "fmsb")

# インストールされていないパッケージをインストール
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# パッケージの読み込み
lapply(packages, library, character.only = TRUE)

cat("Step 1: パッケージの読み込みが完了しました。\n")


# ---------------------------------------------------------------
# ★★★★★ ここで分析・可視化したいクラスター数を指定 ★★★★★
# ---------------------------------------------------------------
CHOSEN_N_PROFILES <- 4
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# 2. データの準備
# ---------------------------------------------------------------
cat("Step 2: データの準備を開始します...\n")

# 分析対象の列名を指定
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")
# プロットのラベルに使用する項目名（英語）
new_item_labels <- c("Item A", "Item B", "Item C", "Item D", "Item E")

# ---------------------------------------------------------------
# ★★★★★ ここで入力ファイルを指定 ★★★★★
# ---------------------------------------------------------------
input_file <- "raw_data/dummy_data.csv"

# 方法2: ファイル選択ダイアログを使用（下記のコメントアウトを外す）
# input_file <- file.choose()

# 方法3: 特定のディレクトリのファイルを指定（例）
# input_file <- "mplus/physiological_test_1st_n12151.csv"
# ---------------------------------------------------------------

# ファイルの存在確認
if (!file.exists(input_file)) {
  stop(paste("エラー: 指定されたファイル '", input_file, "' が見つかりません。", sep=""))
}

# ファイルの拡張子チェック
if (!grepl("\\.(csv|CSV)$", input_file)) {
  warning("警告: 指定されたファイルはCSVファイルではない可能性があります。")
}


# # ヘッダーがない場合
# read_csv(input_file, col_names = FALSE)

# # または特定の行をスキップしたい場合
# read_csv(input_file, skip = 2)  # 最初の2行をスキップ

# データの読み込み
cat(paste("ファイルを読み込み中:", basename(input_file), "\n"))
df_analysis <- read_csv(input_file) %>%
  select(all_of(lpa_target_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit() %>%
  scale() %>%
  as.data.frame()

cat("分析用データの準備が完了しました。\n")


# ---------------------------------------------------------------
# 3. LPAの実行と適合度指標の表示
# ---------------------------------------------------------------
cat(paste("Step 3: ", CHOSEN_N_PROFILES, "クラスターモデルのLPAを実行します...\n", sep=""))

lpa_model <- estimate_profiles(df_analysis, n_profiles = CHOSEN_N_PROFILES)
fit_indices <- get_fit(lpa_model)
class_stats <- get_data(lpa_model) %>%
  count(Class) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2)) %>%
  rename(N = n)

cat("\n--------------------------------------------------\n")
cat(paste("--- ", CHOSEN_N_PROFILES, "-Cluster Model: Fit Indices & Class Stats ---\n", sep=""))
cat("\n[Fit Indices]\n")
print(fit_indices %>% select(LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
cat("\n[Class Membership]\n")
print(class_stats)
cat("--------------------------------------------------\n\n")


# ---------------------------------------------------------------
# 4. プロット用データの準備
# ---------------------------------------------------------------
cat("Step 4: プロット用データを作成します...\n")

plot_data <- get_data(lpa_model) %>%
  pivot_longer(
    cols = all_of(lpa_target_columns),
    names_to = "item",
    values_to = "z_score"
  ) %>%
  group_by(Class, item) %>%
  summarise(mean_z_score = mean(z_score), .groups = "drop") %>%
  mutate(item = factor(item, levels = lpa_target_columns, labels = new_item_labels))

cat("プロット用データの準備が完了しました。\n")

# ---------------------------------------------------------------
# 5. 棒グラフの作成と表示
# ---------------------------------------------------------------
cat("Step 5: 棒グラフを作成します...\n")
profile_plot_bar <- ggplot(plot_data, aes(x = item, y = mean_z_score, fill = item)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
  facet_grid(. ~ Class, labeller = labeller(Class = ~paste("Cluster", .x))) +
  labs(
    title = paste("Profiles for", CHOSEN_N_PROFILES, "Cluster Model (Bar Chart)"),
    subtitle = "Mean Z-scores for each item within each cluster",
    x = "Item",
    y = "Mean Z-score"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12),
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(2, "lines")
  )
print(profile_plot_bar)
cat("棒グラフが正常に作成されました。\n")

# ---------------------------------------------------------------
# 6. レーダーチャートの作成と表示
# ---------------------------------------------------------------
cat("Step 6: レーダーチャートを作成します...\n")

# レーダーチャート用のデータ準備
radar_data <- plot_data %>%
  select(Class, item, mean_z_score) %>%
  pivot_wider(names_from = item, values_from = mean_z_score) %>%
  column_to_rownames("Class")

# レーダーチャートの軸範囲を設定（±2標準偏差：約95%のデータをカバー）
max_value <- 2.0  # +2標準偏差（約97.7%のデータがこの範囲内）
min_value <- -2.0  # -2標準偏差

# レーダーチャート用のデータフレーム作成（fmsbパッケージ用）
# 最初の行に最大値、2行目に最小値を配置
radar_df <- rbind(
  rep(max_value, ncol(radar_data)),  # 最大値の行
  rep(min_value, ncol(radar_data)),  # 最小値の行
  radar_data                         # 実際のデータ
)

# 色の設定（半透過効果を使わない）
colors <- rainbow(nrow(radar_data))  # alpha値を削除
border_colors <- rainbow(nrow(radar_data))

# レーダーチャートの描画（半透過効果なし）
radarchart(
  radar_df,
  axistype = 1,
  pcol = border_colors,
  pfcol = NA,  # 塗りつぶしを無効にして警告を回避
  plwd = 2,
  plty = 1,
  cglcol = "grey",
  cglty = 1,
  axislabcol = "grey",
  caxislabels = c("-2", "-1", "0", "1", "2"),  # ±2標準偏差を基準にしたラベル
  cglwd = 0.8,
  vlcex = 1.2,
  title = paste("Profiles for", CHOSEN_N_PROFILES, "Cluster Model (Radar Chart)\nAxis: Z-scores (±2 SD)")
)

# 凡例の追加
legend(
  x = "topright",
  legend = paste("Cluster", 1:nrow(radar_data)),
  bty = "n",
  pch = 20,
  col = border_colors,
  text.col = "black",
  cex = 1,
  pt.cex = 2
)

cat("レーダーチャートが正常に作成されました。\n")

# ---------------------------------------------------------------
# 7. クラスターごとの個別レーダーチャート作成
# ---------------------------------------------------------------
cat("Step 7: クラスターごとの個別レーダーチャートを作成します...\n")

# クラスター数に応じてグリッドを設定
n_clusters <- nrow(radar_data)
if (n_clusters <= 2) {
  par(mfrow = c(1, n_clusters))
} else if (n_clusters <= 4) {
  par(mfrow = c(2, 2))
} else if (n_clusters <= 6) {
  par(mfrow = c(2, 3))
} else {
  par(mfrow = c(3, 3))
}

# 各クラスターごとにレーダーチャートを作成
for (i in 1:n_clusters) {
  # 各クラスター用のデータフレーム作成
  individual_radar_df <- rbind(
    rep(2.0, ncol(radar_data)),      # 最大値
    rep(-2.0, ncol(radar_data)),     # 最小値
    radar_data[i, ]                  # 該当クラスターのデータ
  )
  
  # 個別レーダーチャートの描画
  radarchart(
    individual_radar_df,
    axistype = 1,
    pcol = border_colors[i],
    pfcol = NA,  # 塗りつぶしを無効にして警告を回避
    plwd = 3,
    plty = 1,
    cglcol = "grey",
    cglty = 1,
    axislabcol = "grey",
    caxislabels = c("-2", "-1", "0", "1", "2"),
    cglwd = 0.8,
    vlcex = 1.0,
    title = paste("Cluster", i, "Profile")
  )
}

# グリッド設定をリセット
par(mfrow = c(1, 1))

cat("クラスターごとの個別レーダーチャートが正常に作成されました。\n")
cat("\n分析とプロットの作成が完了しました。\n")
