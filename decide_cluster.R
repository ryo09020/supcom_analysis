#################################################################
# 潜在プロファイル分析(LPA)と適合度指標の計算・出力 Rスクリプト (最終確定版 v7)
# (VLMR-LRTのみ計算するバージョン)
#################################################################

# 1. 準備：必要なパッケージの読み込み
#----------------------------------------------------------------
cat("Step 1: パッケージを読み込んでいます...\n")
library(tidyverse)
library(tidyLPA)


# 2. データの準備
#----------------------------------------------------------------
cat("Step 2: データの準備を開始します...\n")
my_data <- read_csv("dummy_data.csv")
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

df_selected <- my_data %>%
  select(all_of(lpa_target_columns)) %>%
  mutate(across(everything(), as.numeric))

df_scaled <- as.data.frame(scale(df_selected))
df_analysis <- na.omit(df_scaled)
cat("分析データが準備できました。\n")


# 3. 潜在プロファイル分析の実行 (プロファイル数 2-10)
#----------------------------------------------------------------
cat("\nStep 3: 潜在プロファイル分析を実行します...\n")
# VLMR-LRTを計算するため、モデルを "VVI" (models = 6) に固定します。
lpa_models <- estimate_profiles(df_analysis, n_profiles = 2:10, models = 6)
cat("LPAの計算が完了しました。\n")


# 4. 適合度指標の計算と整形
#----------------------------------------------------------------
cat("\nStep 4: 適合度指標の計算と整形を開始します...\n")

# 4-1. 基本的な適合度指標の取得
fit_indices <- get_fit(lpa_models)

# 4-2. 各クラスの所属割合 (%) の計算
class_proportions <- get_data(lpa_models) %>%
  count(classes_number, Class) %>%
  group_by(classes_number) %>%
  mutate(proportion = n / sum(n)) %>%
  summarise(`% in each class` = paste(round(proportion * 100), collapse = "/"), .groups = 'drop') %>%
  rename(Profiles = classes_number)

# B-LRTの計算は不要とのことで、該当セクションは削除しました。


# 5. 全ての指標を一つの表に統合
#----------------------------------------------------------------
cat("\nStep 5: 全ての指標を一つの表に統合します...\n")

# fit_indicesの列名を変更
renamed_table <- fit_indices %>%
  rename(
    Profiles = Classes,
    `Log-likelihood` = LogLik,
    `Sample-Size Adjusted BIC` = SABIC
  )

# 'VLMR_p' 列が存在するかどうかをチェックし、処理を分岐
if ("VLMR_p" %in% colnames(renamed_table)) {
  renamed_table <- renamed_table %>%
    rename(`VLMR-LRT p-value` = VLMR_p)
} else {
  renamed_table <- renamed_table %>%
    mutate(`VLMR-LRT p-value` = NA)
}

# 必要な列を選択し、クラス割合の結果を結合
final_table <- renamed_table %>%
  select(
    Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`, Entropy, `VLMR-LRT p-value`
  ) %>%
  left_join(class_proportions, by = "Profiles") %>%
  mutate(
    across(c(`Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`), ~round(.x, 2)),
    across(c(Entropy, `VLMR-LRT p-value`), ~round(.x, 3), .names = "{.col}")
  )


# 6. 最終結果の表示とファイルへの保存
#----------------------------------------------------------------
cat("\n-------------------- 分析結果 (VLMR-LRTのみ) --------------------\n")
print(final_table, n = Inf)
cat("---------------------------------------------------------------------\n")

output_filename <- "lpa_fit_indices_vlrt_only.csv"
write_csv(final_table, output_filename)
cat(paste("\n分析結果が '", output_filename, "' という名前で保存されました。\n", sep=""))