# nクラスター時のTIPIカラムのzscoreの分布を棒グラフで表示するプログラム

# 必要なライブラリを読み込み
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

# ===== 設定エリア =====
# クラスター数をここで設定
n_clusters <- 3

# TIPIカラムの定義
tipi_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# データファイルのパス
data_path <- "dummy_data.csv"

# クラスターカラム名
cluster_column <- "cluster"
# ==================

# zscoreを計算する関数
calculate_zscore <- function(x) {
  return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
}

# メイン実行関数
run_analysis <- function() {
  
  # データ読み込み
  data <- read.csv(data_path)
  
  # 各TIPIカラムのzscoreを計算
  data_zscore <- data %>%
    select(all_of(c(cluster_column, tipi_columns))) %>%
    mutate(across(all_of(tipi_columns), calculate_zscore))
  
  # データを長い形式に変換
  data_long <- data_zscore %>%
    pivot_longer(cols = all_of(tipi_columns), 
                 names_to = "tipi_item", 
                 values_to = "zscore") %>%
    filter(!is.na(zscore))
  
  # クラスターごとの平均zscoreを計算
  cluster_means <- data_long %>%
    group_by(!!sym(cluster_column), tipi_item) %>%
    summarise(mean_zscore = mean(zscore, na.rm = TRUE), .groups = 'drop')
  
  # 棒グラフを作成
  plots <- list()
  
  for (i in 1:n_clusters) {
    cluster_data <- cluster_means %>%
      filter(!!sym(cluster_column) == i)
    
    if (nrow(cluster_data) > 0) {
      p <- ggplot(cluster_data, aes(x = tipi_item, y = mean_zscore, fill = tipi_item)) +
        geom_bar(stat = "identity", alpha = 0.8) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = paste("クラスター", i, "のTIPI項目別zscoreの平均"),
             x = "TIPI項目",
             y = "平均zscore") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "none") +
        scale_fill_brewer(type = "qual", palette = "Set3")
      
      plots[[i]] <- p
    }
  }
  
  # グラフを表示
  if (length(plots) > 1) {
    grid.arrange(grobs = plots, ncol = min(3, length(plots)))
  } else {
    print(plots[[1]])
  }
  
  # 結果を出力
  print("クラスターごとのTIPI項目別zscore:")
  print(cluster_means)
  
  return(cluster_means)
}

# 実行
# run_analysis()