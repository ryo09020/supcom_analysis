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
file_path <- "raw_data/dummy_data.csv"

# クラスター列の名前
cluster_column <- "Class"

# 分析したい項目名（複数選択可能）
target_items <- c("X542690_00", "X542700_00", "X542710_00", "X542720_00", "X542730_00")

# 項目カテゴリの指定（リスク因子/保護因子など）
risk_factor_items <- c("X542690_00", "X542700_00")
protective_factor_items <- c("X542710_00", "X542720_00", "X542730_00")

# 項目ラベル（凡例・軸用）。必要に応じて値を書き換えてください
item_labels <- c(
  X542690_00 = "X542690_00",
  X542700_00 = "X542700_00",
  X542710_00 = "X542710_00",
  X542720_00 = "X542720_00",
  X542730_00 = "X542730_00"
)

# 共変量として調整したい項目（例：性別、年齢など）
covariates <- c( "age")

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

# 項目カテゴリの検証と整備
item_category_df <- tibble(
  Item = target_items,
  Category = case_when(
    Item %in% risk_factor_items ~ "Risk Factor",
    Item %in% protective_factor_items ~ "Protective Factor",
    TRUE ~ "Unspecified"
  )
)

unknown_category_items <- item_category_df %>% filter(Category == "Unspecified") %>% pull(Item)
if (length(unknown_category_items) > 0) {
  warning(
    "ターゲット項目のうちカテゴリが未指定の項目があります: ",
    paste(unknown_category_items, collapse = ", "),
    call. = FALSE
  )
}

duplicated_assignments <- intersect(risk_factor_items, protective_factor_items)
if (length(duplicated_assignments) > 0) {
  stop(
    "同じ項目がリスク因子と保護因子の両方に指定されています: ",
    paste(duplicated_assignments, collapse = ", "),
    call. = FALSE
  )
}

# 項目ラベルの整備
if (length(item_labels) == 0) {
  item_labels <- setNames(target_items, target_items)
}

if (is.null(names(item_labels)) || any(names(item_labels) == "")) {
  stop("item_labels は target_items と同じ名前を持つ名前付きベクトルで指定してください。", call. = FALSE)
}

missing_label_items <- setdiff(target_items, names(item_labels))
if (length(missing_label_items) > 0) {
  warning(
    "item_labels に指定されていない項目があります。該当項目は列名をラベルとして使用します: ",
    paste(missing_label_items, collapse = ", "),
    call. = FALSE
  )
  item_labels <- c(item_labels, setNames(missing_label_items, missing_label_items))
}

extra_label_items <- setdiff(names(item_labels), target_items)
if (length(extra_label_items) > 0) {
  warning(
    "target_items に含まれない項目が item_labels に指定されています（無視されます）: ",
    paste(extra_label_items, collapse = ", "),
    call. = FALSE
  )
}

item_label_lookup <- item_labels[target_items]
item_label_lookup[is.na(item_label_lookup)] <- target_items[is.na(item_label_lookup)]

if (anyDuplicated(item_label_lookup) > 0) {
  warning("item_labels の値が重複しています。凡例や軸でラベルが重複して表示されます。", call. = FALSE)
}

legend_labels <- unname(item_label_lookup)
axis_labels <- setNames(item_label_lookup, target_items)
label_replacements <- as.list(item_label_lookup)

# 色ベクトルの作成
make_palette <- function(colors, n) {
  if (n <= 0) return(character(0))
  if (length(colors) == 1) return(rep(colors, n))
  grDevices::colorRampPalette(colors)(n)
}

risk_colors <- make_palette(c("#ffccd5", "#ff4d6d"), length(risk_factor_items))
protective_colors <- make_palette(c("#f7fff5", "#1b9e77"), length(protective_factor_items))

item_color_map <- tibble(
  Item = c(risk_factor_items, protective_factor_items),
  Color = c(risk_colors, protective_colors)
) %>%
  right_join(item_category_df, by = "Item") %>%
  mutate(
    Color = dplyr::coalesce(Color, "#bdbdbd")
  )

fill_values <- item_color_map$Color
names(fill_values) <- item_color_map$Item
fill_values <- fill_values[target_items]
fill_values[is.na(fill_values)] <- "#bdbdbd"

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
  ) %>%
  left_join(item_category_df, by = "Item") %>%
  mutate(
    Item_Label = dplyr::recode(as.character(Item), !!!label_replacements, .default = as.character(Item)),
    Item_Label = factor(Item_Label, levels = item_label_lookup)
  )

# クラスター別・項目別の統計を計算
summary_stats <- plot_data %>%
  group_by(Cluster, Item) %>%
  summarise(
    N = n(),
    Mean_ZScore = mean(ZScore, na.rm = TRUE),
    SD_ZScore = sd(ZScore, na.rm = TRUE),
    SE_ZScore = SD_ZScore / sqrt(N),  # 標準誤差
    Category = first(Category),
    Item_Label = first(Item_Label),
    .groups = 'drop'
  ) %>%
  mutate(
    Item = factor(as.character(Item), levels = target_items),
    Item_Label = factor(as.character(Item_Label), levels = item_label_lookup)
  ) %>%
  arrange(Cluster, Item)

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
  scale_fill_manual(
    name = "Item",
    values = fill_values,
    breaks = target_items,
    labels = legend_labels
  ) +
  scale_x_discrete(breaks = target_items, labels = axis_labels, drop = FALSE) +
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
