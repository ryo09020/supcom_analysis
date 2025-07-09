#################################################################
# LPAプロファイル可視化Rスクリプト（単一モデル実行バージョン）
#
# 目的：
# - 指定された単一のクラスター数でLPAを実行する。
# - 適合度指標（AIC, BIC）とクラス所属率をコンソールに出力する。
# - 結果を棒グラフとレーダーチャートで可視化する（描画は英語表記）。
#################################################################

# ---------------------------------------------------------------
# 1. パッケージの準備
# ---------------------------------------------------------------
# 必要なパッケージのリスト
packages <- c("tidyverse", "tidyLPA", "knitr")

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

# データの読み込み、選択、標準化を一括処理
if (!file.exists("dummy_data.csv")) {
  cat("dummy_data.csvが見つかりません。ダミーデータを生成します。\n")
  set.seed(123)
  dummy_data <- as.data.frame(matrix(runif(500 * 5, 1, 5), ncol = 5))
  colnames(dummy_data) <- lpa_target_columns
  write.csv(dummy_data, "dummy_data.csv", row.names = FALSE)
}

df_analysis <- read_csv("dummy_data.csv") %>%
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
print(fit_indices %>% select(AIC, BIC, Entropy, SABIC), row.names = FALSE)
cat("\n[Class Membership]\n")
print(class_stats, row.names = FALSE)
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


# ★★★★★ ここが最後の修正箇所です ★★★★★
# ---------------------------------------------------------------
# 4.5. レーダーチャート用のデータ補間 (最終修正版)
# ---------------------------------------------------------------
cat("Step 4.5: レーダーチャート用にデータを補間します...\n")

radar_interpolated_data <- plot_data %>%
  mutate(item_numeric = as.numeric(item)) %>%
  group_by(Class) %>%
  # reframe内では、グループ化された列名を直接参照します (`.x`は不要)
  reframe({
    # xは項目番号(1, 2, 3, 4, 5)、yはZスコア
    x_points <- item_numeric
    y_points <- mean_z_score
    
    # 多角形を閉じるため、始点(1番目の点)の情報を終点として追加
    x_closed <- c(x_points, max(x_points) + 1)
    y_closed <- c(y_points, y_points[1])
    
    # approxで補間を実行
    interpolation_result <- approx(x_closed, y_closed, n = 200)
    
    # 結果をtibbleで返す
    tibble(
      item_interpolated = interpolation_result$x,
      z_score_interpolated = interpolation_result$y
    )
  })

cat("データ補間が完了しました。\n")


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
# 6. レーダーチャートの作成と表示 (線画バージョン)
# ---------------------------------------------------------------
cat("\nStep 6: レーダーチャートを作成します (線画バージョン)...\n")

profile_plot_radar <- ggplot(
  radar_interpolated_data,
  aes(
    x = item_interpolated,
    y = z_score_interpolated,
    group = Class, 
    color = as.factor(Class) # 色分けのみ指定
  )
) +
  # geom_path()で線を描画する
  geom_path(linewidth = 1.2) +
  coord_polar(start = 0) +
  ylim(-2, 2) +
  scale_x_continuous(
    breaks = 1:length(new_item_labels),
    labels = new_item_labels
  ) +
  # 凡例は色(color)のみに設定
  scale_color_discrete(name = "Cluster", labels = ~paste("Cluster", .)) +
  labs(
    title = paste("Profiles for", CHOSEN_N_PROFILES, "Cluster Model (Radar Chart)"),
    subtitle = "Comparison of mean z-scores across clusters",
    x = "",
    y = ""
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10),
    legend.title = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

print(profile_plot_radar)

cat("レーダーチャートが正常に作成されました。\n")

