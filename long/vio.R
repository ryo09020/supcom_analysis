# ------------------------------------------------------------------
# Rスクリプト：縦断比較バイオリンプロット（項目ラベルのカスタム対応版）
# (コメント・メッセージは日本語、グラフのラベルは英語)
# ------------------------------------------------------------------

# 1. 必要なライブラリの読み込み
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------------
# (重要) ファイル名と列名の指定
# ------------------------------------------------------------------
# ★★★ ここでご自身のデータに合わせて設定を変更してください ★★★

# 2-1. ファイル名の指定
file_time1 <- "time1.csv"
file_time2 <- "time2_with_class.csv" # 前回スクリプトで作成したファイル

# 2-2. 読み込む列名の指定
class_column <- "class" 

# 2-3. ★★★ 分析したい「項目」の列名（CSVファイル上）を指定 ★★★
# (例: c("Q1", "Q2", "Q3_total") など)
target_items <- c("subscale_A", "subscale_B", "total_score") 


# 2-4. ★★★ 項目ラベルのマッピング（対応表）を指定 ★★★
# グラフのパネル（facet）に表示するラベルを定義します。
# ` "CSV上の列名" = "グラフに表示したいラベル" ` の形式で指定します。
item_labels_map <- c(
  "subscale_A"  = "Subscale A (e.g., Quality of Life)",
  "subscale_B"  = "Subscale B (e.g., Depression)",
  "total_score" = "Total Score (e.g., Overall Well-being)"
)
# (注意: ここで指定するラベルは、target_items と一致している必要があります)

# ★★★ 設定はここまで ★★★
# ------------------------------------------------------------------


# 3. データの読み込みと前処理 (T1, T2)
df_t1 <- read_csv(file_time1) %>% mutate(time = "Time 1")
df_t2 <- read_csv(file_time2) %>% mutate(time = "Time 2")

# 4. T1とT2のデータを縦に結合
df_combined <- bind_rows(df_t1, df_t2)

# 5. データを縦長形式に変換
df_long <- df_combined %>%
  select(all_of(class_column), time, all_of(target_items)) %>%
  pivot_longer(
    cols = all_of(target_items),
    names_to = "item_name",
    values_to = "value"
  ) %>%
  mutate(
    class = factor(!!sym(class_column)), 
    time = factor(time, levels = c("Time 1", "Time 2")),
    
    # ★★★ 項目名をマッピング（対応表）に基づいて「表示用ラベル」に変換 ★★★
    # ここでは item_name 自体を上書きするのではなく、
    # factor の levels (順序) をCSV上の列名順 (target_items) に、
    # labels (表示名) をマッピング (item_labels_map) に設定します。
    item_name = factor(
      item_name, 
      levels = target_items, # データの順序
      labels = item_labels_map  # 表示するラベル
    )
  ) %>%
  filter(!is.na(value)) 

# 6. プロットの作成
violin_plot <- ggplot(df_long, aes(x = class, y = value, fill = time)) +
  
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.7, trim = FALSE) +
  geom_boxplot(
    width = 0.1, 
    position = position_dodge(width = 0.9), 
    fill = "white",
    outlier.size = 0.5
  ) +
  
  # ------------------------------------------------------------------
  # ★★★ `facet_wrap` の `labeller` を使用（より堅牢な方法）★★★
  # 
  # (上記 5. の factor() でのラベル設定がうまくいかない場合や、
  #  よりggplot2の標準的な方法を使いたい場合は、こちらのコメントアウトを
  #  解除して、上記 5. の factor() の 'labels' 部分を削除してください)
  # 
  # item_labeller <- as_labeller(item_labels_map)
  # facet_wrap(~ item_name, scales = "free_y", labeller = item_labeller) +
  # ------------------------------------------------------------------

  # 上記 5. の factor() でラベルを設定した場合、facet_wrap はシンプルでOK
  facet_wrap(~ item_name, scales = "free_y") +

  # ラベルとタイトルを英語に設定
  labs(
    title = "Longitudinal Comparison by Class and Item",
    subtitle = "Time 1 vs Time 2",
    x = "Class",
    y = "Value",
    fill = "Timepoint"
  ) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

# 7. プロットの表示
print(violin_plot)

# 8. (オプション) プロットを画像ファイルとして保存
# ggsave("longitudinal_violin_plot_custom_labels.png", plot = violin_plot, width = 12, height = 7, dpi = 300)
# print("プロットを 'longitudinal_violin_plot_custom_labels.png' として保存しました。")