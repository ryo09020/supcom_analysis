# -------------------------------------------------------------------------
# クラスター別箱ひげ図（共変量調整済み）
# -------------------------------------------------------------------------
# このスクリプトは、CSVファイルからクラスター情報を読み取り、
# 選択した項目について共変量で調整した後の箱ひげ図を作成します。
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Step 0: パッケージのインストールと読み込み
# -------------------------------------------------------------------------

# 必要なパッケージをインストール（まだインストールしていない場合）
# install.packages(c("ggplot2", "dplyr", "tidyr", "viridis", "gridExtra"))

# パッケージを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)
library(gridExtra)


# -------------------------------------------------------------------------
# Step 1: ユーザー設定項目
# -------------------------------------------------------------------------

# ▼ ユーザーが設定する項目 ▼
# CSVファイルのパス
file_path <- "raw_data/dummy_data_with_clusters_sorted.csv"

# クラスター列の名前
cluster_column <- "Class"

# 分析したい項目名（複数選択可能）
target_items <- c("X542690_00", "X542700_00", "X542710_00", "X542720_00", "X542730_00")

# 共変量として調整したい項目（例：性別、年齢など）
covariates <- c("sex", "age")

# 図のタイトル
plot_title <- "Distribution of cluster-specific item values (adjusted for covariates)"

# 出力ファイル名
output_filename <- "cluster_boxplot_adjusted.png"


# -------------------------------------------------------------------------
# Step 2: データの読み込みと前処理
# -------------------------------------------------------------------------

cat("データを読み込み中...\n")

# データを読み込み
data <- read.csv(file_path, stringsAsFactors = FALSE)

# 列名を確認
cat("利用可能な列名:\n")
print(names(data))

# 必要な列が存在するかチェック
required_columns <- c(cluster_column, target_items, covariates)
missing_columns <- required_columns[!required_columns %in% names(data)]

if (length(missing_columns) > 0) {
  cat("エラー: 以下の列がデータに見つかりません:\n")
  print(missing_columns)
  cat("列名を確認して、スクリプトの設定を修正してください。\n")
  stop("必要な列が見つかりません")
}

# データの前処理
cat("データを前処理中...\n")

# 必要な列のみを選択
analysis_data <- data %>%
  select(all_of(required_columns)) %>%
  # 数値列を数値型に変換
  mutate(across(all_of(c(target_items, covariates)), as.numeric)) %>%
  # クラスター列を因子型に変換
  mutate(!!sym(cluster_column) := as.factor(!!sym(cluster_column))) %>%
  # 欠損値を除外
  na.omit()

cat(paste("元のデータ行数:", nrow(data), "\n"))
cat(paste("前処理後のデータ行数:", nrow(analysis_data), "\n"))
cat(paste("除外されたレコード数:", nrow(data) - nrow(analysis_data), "\n"))

# クラスターの数を確認
n_clusters <- length(unique(analysis_data[[cluster_column]]))
cat(paste("クラスター数:", n_clusters, "\n"))
cat("各クラスターのサンプル数:\n")
print(table(analysis_data[[cluster_column]]))


# -------------------------------------------------------------------------
# Step 3: 共変量調整
# -------------------------------------------------------------------------

cat("共変量調整を実行中...\n")

# 調整済みデータを格納するデータフレーム
adjusted_data <- analysis_data

# 各ターゲット項目について共変量調整を実行
for (item in target_items) {
  cat(paste("項目", item, "を調整中...\n"))
  
  # 共変量を含む回帰式を作成
  covariates_formula <- paste(covariates, collapse = " + ")
  formula_str <- paste(item, "~", covariates_formula)
  
  # 線形回帰モデルを実行
  lm_model <- lm(as.formula(formula_str), data = analysis_data)
  
  # 残差を計算（共変量の影響を除去した値）
  residuals_value <- residuals(lm_model)
  
  # 全体平均を加えて調整済み値を計算
  adjusted_values <- residuals_value + mean(analysis_data[[item]], na.rm = TRUE)
  
  # 調整済みデータに保存
  adjusted_data[[item]] <- adjusted_values
}

cat("共変量調整が完了しました。\n")


# -------------------------------------------------------------------------
# Step 4: データの整形（ggplot用）
# -------------------------------------------------------------------------

# ワイド形式からロング形式に変換
plot_data <- adjusted_data %>%
  select(all_of(c(cluster_column, target_items))) %>%
  pivot_longer(
    cols = all_of(target_items),
    names_to = "Item",
    values_to = "Value"
  ) %>%
  # ファクター順序を設定
  mutate(
    Item = factor(Item, levels = target_items),
    Cluster = factor(!!sym(cluster_column))
  )


# -------------------------------------------------------------------------
# Step 5: 箱ひげ図の作成
# -------------------------------------------------------------------------

cat("Creating boxplots...\n")

# 基本の箱ひげ図（透明度を削除）
p1 <- ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  geom_jitter(width = 0.2, size = 0.8) +
  facet_wrap(~ Item, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = plot_title,
    subtitle = paste("Covariate adjustment:", paste(covariates, collapse = ", ")),
    x = "Cluster",
    y = "Adjusted Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 0),
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )

# 統計情報を含む箱ひげ図（透明度を削除）
p2 <- ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black") +
  facet_wrap(~ Item, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = paste(plot_title, "(with mean)"),
    subtitle = paste("◇: Mean value, Covariate adjustment:", paste(covariates, collapse = ", ")),
    x = "Cluster",
    y = "Adjusted Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 0),
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )

# バイオリンプロット
p3 <- ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
  geom_violin() +
  geom_boxplot(width = 0.1, fill = "white") +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 2, 
               fill = "red", color = "black") +
  facet_wrap(~ Item, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(name = "Cluster") +
  labs(
    title = paste(plot_title, "(violin plot)"),
    subtitle = paste("◇: Mean value, Covariate adjustment:", paste(covariates, collapse = ", ")),
    x = "Cluster",
    y = "Adjusted Value"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 0),
    strip.text = element_text(size = 10),
    legend.position = "bottom"
  )


# -------------------------------------------------------------------------
# Step 6: 統計的検定とサマリー
# -------------------------------------------------------------------------

cat("\n=== 統計的検定結果 ===\n")

# 各項目について分散分析を実行
anova_results <- list()

for (item in target_items) {
  cat(paste("\n--- Item:", item, "---\n"))
  
  # 項目別データを抽出
  item_data <- plot_data %>% filter(Item == item)
  
  # 分散分析
  anova_model <- aov(Value ~ Cluster, data = item_data)
  anova_summary <- summary(anova_model)
  
  # 結果を保存
  anova_results[[item]] <- anova_summary
  
  # p値を取得
  p_value <- anova_summary[[1]][["Pr(>F)"]][1]
  
  cat(paste("ANOVA p-value:", round(p_value, 4), "\n"))
  if (p_value < 0.05) {
    cat("→ Significant difference between clusters (p < 0.05)\n")
  } else {
    cat("→ No significant difference between clusters (p >= 0.05)\n")
  }
  
  # 各クラスターの記述統計
  descriptive_stats <- item_data %>%
    group_by(Cluster) %>%
    summarise(
      N = n(),
      Mean = round(mean(Value, na.rm = TRUE), 3),
      SD = round(sd(Value, na.rm = TRUE), 3),
      Median = round(median(Value, na.rm = TRUE), 3),
      .groups = 'drop'
    )
  
  print(descriptive_stats)
}


# -------------------------------------------------------------------------
# Step 7: 図の保存（個別保存）
# -------------------------------------------------------------------------

# ファイルサイズを項目数に応じて調整
width_size <- max(12, length(target_items) * 2)
height_size <- max(10, ceiling(length(target_items) / 3) * 4)

cat(paste("\nSaving boxplot with scatter points as: boxplot_scatter.png\n"))
ggsave(
  filename = "boxplot_scatter.png",
  plot = p1,
  width = width_size,
  height = height_size,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Saved boxplot_scatter.png\n")

cat(paste("Saving boxplot with mean as: boxplot_mean.png\n"))
ggsave(
  filename = "boxplot_mean.png",
  plot = p2,
  width = width_size,
  height = height_size,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Saved boxplot_mean.png\n")

cat(paste("Saving violin plot as: violin_plot.png\n"))
ggsave(
  filename = "violin_plot.png",
  plot = p3,
  width = width_size,
  height = height_size,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat("Saved violin_plot.png\n")

# -------------------------------------------------------------------------
# Step 8: クラスターごと・項目ごとの棒グラフ作成
# -------------------------------------------------------------------------
cat("\nCreating barplot of mean values for each item by cluster...\n")
bar_data <- plot_data %>%
  group_by(Cluster, Item) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    .groups = 'drop'
  )

barplot <- ggplot(bar_data, aes(x = Item, y = Mean, fill = Item)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Cluster, nrow = 1) +
  scale_fill_viridis_d(name = "Item") +
  labs(
    title = "Mean value of each item by cluster",
    x = "Item",
    y = "Mean (Adjusted Value)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )

barplot_filename <- "cluster_barplot.png"
ggsave(
  filename = barplot_filename,
  plot = barplot,
  width = max(12, n_clusters * 4),
  height = 6,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat(paste("Barplot saved as", barplot_filename, "\n"))


# -------------------------------------------------------------------------
# Step 9: データの出力（オプション）
# -------------------------------------------------------------------------

# 調整済みデータをCSVとして保存（オプション）
# adjusted_data_filename <- "adjusted_data.csv"
# write.csv(adjusted_data, adjusted_data_filename, row.names = FALSE)
# cat(paste("調整済みデータが", adjusted_data_filename, "として保存されました。\n"))

cat("\n=== スクリプト実行完了 ===\n")
cat("作成された図:\n")
cat("1. 散布点付き箱ひげ図 (boxplot_scatter.png)\n")
cat("2. 平均値付き箱ひげ図 (boxplot_mean.png)\n")
cat("3. バイオリンプロット (violin_plot.png)\n")
cat("4. クラスター別棒グラフ (cluster_barplot.png)\n")
cat("共変量調整により、指定した変数の影響を除去した値で比較しています。\n")
