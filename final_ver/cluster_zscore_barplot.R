# -------------------------------------------------------------------------
# クラスター別z-score棒グラフ（共変量調整済み）
# -------------------------------------------------------------------------
# このスクリプトは、共変量調整後にz-score化を行い、
# クラスター別・項目別の棒グラフを作成します。
# -------------------------------------------------------------------------

# -------------------------------------------------------------------------
# Step 0: パッケージのインストールと読み込み
# -------------------------------------------------------------------------

# 必要なパッケージをインストール（まだインストールしていない場合）
# install.packages(c("ggplot2", "dplyr", "tidyr", "viridis"))

# パッケージを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)


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

# 共変量として調整したい項目（例：性別、年齢、教育歴など）
covariates <- c("sex", "age", "final_education")

# 図のタイトル
plot_title <- "Z-score of cluster-specific item values (adjusted for covariates)"

# 出力ファイル名のプレフィックス
output_prefix <- "cluster_zscore"


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
  # final_educationを教育年数に変換
  mutate(final_education = case_when(
    final_education == 1 ~ 9,
    final_education == 2 ~ 12,
    final_education == 3 ~ 14,
    final_education == 4 ~ 14,
    final_education == 5 ~ 16,
    final_education == 6 ~ 18,
    final_education == 7 ~ NA_real_,
    TRUE ~ NA_real_
  )) %>%
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
# Step 4: Z-score化
# -------------------------------------------------------------------------

cat("Z-score化を実行中...\n")

# z-score化されたデータを格納するデータフレーム
zscore_data <- adjusted_data

# 各ターゲット項目についてz-score化を実行
for (item in target_items) {
  cat(paste("項目", item, "をz-score化中...\n"))
  
  # z-score化: (値 - 平均) / 標準偏差
  item_mean <- mean(adjusted_data[[item]], na.rm = TRUE)
  item_sd <- sd(adjusted_data[[item]], na.rm = TRUE)
  
  zscore_data[[item]] <- (adjusted_data[[item]] - item_mean) / item_sd
  
  cat(paste("  平均:", round(item_mean, 3), "標準偏差:", round(item_sd, 3), "\n"))
}

cat("Z-score化が完了しました。\n")


# -------------------------------------------------------------------------
# Step 5: クラスター別・項目別の平均z-scoreを計算
# -------------------------------------------------------------------------

cat("クラスター別・項目別の統計を計算中...\n")

# ワイド形式からロング形式に変換
plot_data <- zscore_data %>%
  select(all_of(c(cluster_column, target_items))) %>%
  pivot_longer(
    cols = all_of(target_items),
    names_to = "Item",
    values_to = "ZScore"
  ) %>%
  # ファクター順序を設定
  mutate(
    Item = factor(Item, levels = target_items),
    Cluster = factor(!!sym(cluster_column))
  )

# クラスター別・項目別の統計を計算
summary_stats <- plot_data %>%
  group_by(Cluster, Item) %>%
  summarise(
    N = n(),
    Mean_ZScore = mean(ZScore, na.rm = TRUE),
    SD_ZScore = sd(ZScore, na.rm = TRUE),
    SE_ZScore = SD_ZScore / sqrt(N),  # 標準誤差
    .groups = 'drop'
  )

# 統計結果を表示
cat("\n=== クラスター別・項目別のZ-score統計 ===\n")
print(summary_stats)


# -------------------------------------------------------------------------
# Step 6: 棒グラフの作成
# -------------------------------------------------------------------------

cat("Z-score棒グラフを作成中...\n")

# クラスターごとに分割して項目を横に並べる棒グラフ（cluster_boxplot_adjusted.Rの方式を参考）
p1 <- ggplot(summary_stats, aes(x = Item, y = Mean_ZScore, fill = Item)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_errorbar(aes(ymin = Mean_ZScore - SE_ZScore, ymax = Mean_ZScore + SE_ZScore),
                width = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  facet_wrap(~ Cluster, nrow = 1) +
  scale_fill_viridis_d(name = "Item") +
  labs(
    title = plot_title,
    subtitle = paste("Error bars: Standard Error, Covariate adjustment:", paste(covariates, collapse = ", ")),
    x = "Items",
    y = "Mean Z-score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 12),
    legend.position = "bottom"
  )


# -------------------------------------------------------------------------
# Step 7: 統計的検定
# -------------------------------------------------------------------------

cat("\n=== 統計的検定結果（Z-score） ===\n")

# 各項目について分散分析を実行
anova_results <- list()

for (item in target_items) {
  cat(paste("\n--- Item:", item, "---\n"))
  
  # 項目別データを抽出
  item_data <- plot_data %>% filter(Item == item)
  
  # 分散分析
  anova_model <- aov(ZScore ~ Cluster, data = item_data)
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
}


# -------------------------------------------------------------------------
# Step 8: 図の保存
# -------------------------------------------------------------------------

# ファイルサイズを調整（クラスター数と項目数に応じて）
width_size <- max(12, length(target_items) * n_clusters * 0.8)
height_size <- 8

cat("\nZ-score棒グラフを保存中...\n")

# 全データを一つの棒グラフとして保存
ggsave(
  filename = paste0(output_prefix, "_all_clusters.png"),
  plot = p1,
  width = width_size,
  height = height_size,
  units = "in",
  dpi = 300,
  bg = "white"
)
cat(paste("Saved", paste0(output_prefix, "_all_clusters.png"), "\n"))


# -------------------------------------------------------------------------
# Step 9: 結果の要約をCSVとして保存
# -------------------------------------------------------------------------

# 統計結果をCSVとして保存
summary_filename <- paste0(output_prefix, "_summary.csv")
write.csv(summary_stats, summary_filename, row.names = FALSE)
cat(paste("統計要約を", summary_filename, "として保存しました。\n"))

# z-score化されたデータも保存（オプション）
zscore_filename <- paste0(output_prefix, "_data.csv")
write.csv(zscore_data, zscore_filename, row.names = FALSE)
cat(paste("Z-score化されたデータを", zscore_filename, "として保存しました。\n"))


# -------------------------------------------------------------------------
# Step 10: 実行完了メッセージ
# -------------------------------------------------------------------------

cat("\n=== スクリプト実行完了 ===\n")
cat("作成された図:\n")
cat(paste("1. 全クラスター統合棒グラフ (", paste0(output_prefix, "_all_clusters.png"), ")\n", sep=""))
cat("\n保存されたデータ:\n")
cat(paste("1. 統計要約 (", summary_filename, ")\n", sep=""))
cat(paste("2. Z-score化データ (", zscore_filename, ")\n", sep=""))
cat("\n共変量調整とZ-score化により、標準化された比較が可能になりました。\n")
cat("Z-score = 0が全体平均を表し、正の値は平均より高く、負の値は平均より低いことを示します。\n")
cat("全てのクラスターと項目が一つのグラフに統合されています。\n")
