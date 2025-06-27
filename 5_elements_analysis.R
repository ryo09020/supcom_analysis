# -------------------------------------------------------------------------
# Step 1: 準備 (パッケージのインストールと読み込み)
# -------------------------------------------------------------------------
# パッケージの読み込み
library(readr)
library(dplyr)
library(tidyLPA)
library(ggplot2)
library(knitr)

# -------------------------------------------------------------------------
# Step 2: データの準備 (CSVファイルの読み込み)
# -------------------------------------------------------------------------
# ★ ご自身のCSVファイルを指定します
file_path <- "dummy_data.csv"
raw_data <- read_csv(file_path)

# -------------------------------------------------------------------------
# Step 3: LPAの実行と最適なプロファイル数（クラスター数）の検討
# -------------------------------------------------------------------------
# ★ 分析したい列の「番号」を指定します
target_column_indices <- c(8, 9, 10, 11, 12)

# 指定した番号の列だけを抽出
lpa_data <- raw_data %>%
  select(all_of(target_column_indices))

# 複数のプロファイル数でLPAモデルを推定し、比較
lpa_models <- lpa_data %>%
  estimate_profiles(n_profiles = 2:5)

# 適合度指標（BICなど）を確認
get_fit(lpa_models) %>% kable()

# -------------------------------------------------------------------------
# Step 4: 最適なモデルの選択とプロファイルの可視化・解釈
# -------------------------------------------------------------------------
# ★ 上記の結果を見て、最もBICが低かったプロファイル数を指定してください
optimal_n_profiles <- 3

# 最適なモデルを再度推定
best_model <- lpa_data %>%
  estimate_profiles(n_profiles = optimal_n_profiles)


# ▼▼▼ グラフ描画コード ▼▼▼

# 1. モデルから各クラスターの平均値データを取得
profile_estimates <- get_estimates(best_model)

# 【デバッグ用】もし再度エラーが出たら、この下の行のコメント(#)を外して実行し、
# ターミナルに表示される列名を確認してください。
# print(colnames(profile_estimates))

# 2. ggplotで扱いやすいようにデータを整形
# 【修正点】'Variable' という列名がエラーの原因だったため、'Category' に変更しました
plot_data <- profile_estimates %>%
  filter(Parameter == "MEAN") %>%
  select(Category, Class, Estimate) %>% # 'Variable' を 'Category' に変更
  rename(
    Item = Category,                   # こちらも 'Category' に変更
    Cluster = Class,
    Mean = Estimate
  )

# 3. ggplot2を使ってプロファイルプロットを描画
ggplot(plot_data, aes(x = Item, y = Mean, group = factor(Cluster), color = factor(Cluster))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Latent Profile Analysis Results",
    subtitle = paste(optimal_n_profiles, "Profile Solution"),
    x = "Items",
    y = "Mean Score",
    color = "Cluster"
  ) +
  theme_minimal()

# -------------------------------------------------------------------------
# Step 5: 元のデータへのクラスタリング結果の結合
# -------------------------------------------------------------------------
# このステップ以降は変更ありません
cluster_assignment <- get_data(best_model) %>%
  select(Class)

final_data <- bind_cols(raw_data, cluster_assignment) %>%
  rename(Cluster = Class)

# -------------------------------------------------------------------------
# Step 6: 結果の詳細な分析と可視化
# -------------------------------------------------------------------------
cluster_summary <- final_data %>%
  count(Cluster) %>%
  mutate(Percentage = n / sum(n) * 100)

print("Cluster Sizes and Proportions:")
print(cluster_summary)

ggplot(cluster_summary, aes(x = factor(Cluster), y = n, fill = factor(Cluster))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(n, " (", round(Percentage, 1), "%)")), vjust = -0.5) +
  labs(
    title = "Number of Members per Cluster",
    x = "Cluster",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


final_data_for_plot <- final_data %>%
  rename(Age_at_Participation = `参加時年齢`)

ggplot(final_data_for_plot, aes(x = factor(Cluster), y = Age_at_Participation, fill = factor(Cluster))) +
  geom_boxplot() +
  labs(
    title = "Age Distribution per Cluster",
    x = "Cluster",
    y = "Age at Participation"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
