#################################################################
# 潜在プロファイル分析(LPA)と適合度指標の計算・出力 Rスクリプト (最終確定版 v8)
# (B-LRT計算を含むフルバージョン)
#################################################################

# 1. 準備：必要なパッケージの読み込み
#----------------------------------------------------------------
cat("Step 1: パッケージを読み込んでいます...\n")
library(tidyverse)
library(tidyLPA)
library(mclust) # ★★★★★ B-LRTの計算に必要なので追加 ★★★★★


# 2. データの準備
#----------------------------------------------------------------
cat("Step 2: データの準備を開始します...\n")

# データの読み込み（必要に応じて skip = 1 をつけてください）
my_data <- read_csv("dummy_data.csv")

# Step 2-1: LPAの分析に使用する列を、ここで明確に指定します。
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# 【段階1】指定した列を選択し、確実に数値型に変換する
df_selected <- my_data %>%
  select(all_of(lpa_target_columns)) %>%
  mutate(across(everything(), as.numeric))

# 【段階2】段階1で作ったデータを、標準化し、欠損値を取り除く
df_scaled <- as.data.frame(scale(df_selected))
df_analysis <- na.omit(df_scaled)


# 分析にどの列が使われたか、最終的なデータ構造を確認
cat("LPAの分析に使用される列は以下の通りです:\n")
print(colnames(df_analysis))


# 3. 潜在プロファイル分析の実行 (プロファイル数 2-10)
#----------------------------------------------------------------
cat("\nStep 3: 潜在プロファイル分析を実行します...\n")
# models = 6 を指定することで、VLMR-LRTとB-LRTの両方に対応したモデルを計算します
lpa_models <- estimate_profiles(df_analysis, n_profiles = 2:10, models = 6)
cat("LPAの計算が完了しました。\n")


# 4. 適合度指標の計算と整形
#----------------------------------------------------------------
cat("\nStep 4: 適合度指標の計算と整形を開始します...\n")
fit_indices <- get_fit(lpa_models)

class_proportions <- get_data(lpa_models) %>%
  count(classes_number, Class) %>%
  group_by(classes_number) %>%
  mutate(proportion = n / sum(n)) %>%
  summarise(
    `% in each class` = paste(round(proportion * 100), collapse = "/"),
    .groups = 'drop'
  ) %>%
  rename(Profiles = classes_number)

# ★★★★★ ここにB-LRTの計算コードを追加 ★★★★★
# 4-3. B-LRT P-value (Bootstrap Likelihood Ratio Test) の計算
cat("B-LRT P-value の計算を開始します (非常に時間がかかる場合があります)...\n")
blrt_results <- tibble(Profiles = 2:10, `B-LRT P-value` = NA_real_)
for (k in 2:10) {
  cat(paste("  計算中:", k, "クラス vs", k - 1, "クラス...\n"))
  # modelName = "VVI" は、Step 3の models = 6 と対応しています。
  lrt_test <- mclustBootstrapLRT(df_analysis, modelName = "VVI", maxG = k, nboot = 200)
  blrt_results$`B-LRT P-value`[blrt_results$Profiles == k] <- lrt_test$p.value[k - 1]
}
cat("B-LRT P-value の計算が完了しました。\n")


# 5. 全ての指標を一つの表に統合
#----------------------------------------------------------------
cat("\nStep 5: 全ての指標を一つの表に統合します...\n")

renamed_table <- fit_indices %>%
  rename(
    Profiles = Classes,
    `Log-likelihood` = LogLik,
    `Sample-Size Adjusted BIC` = SABIC
  )

if ("VLMR_p" %in% colnames(renamed_table)) {
  renamed_table <- renamed_table %>%
    rename(`VLMR-LRT p-value` = VLMR_p)
} else {
  renamed_table <- renamed_table %>%
    mutate(`VLMR-LRT p-value` = NA)
}

# 必要な列を選択し、残りの情報を結合します
final_table <- renamed_table %>%
  select(
    Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`, Entropy, `VLMR-LRT p-value`
  ) %>%
  # ★★★★★ B-LRTの結果を結合する処理を追加 ★★★★★
  left_join(blrt_results, by = "Profiles") %>%
  left_join(class_proportions, by = "Profiles") %>%
  mutate(
    across(c(`Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`), ~round(.x, 2)),
    # ★★★★★ B-LRTのp値も丸める処理を追加 ★★★★★
    across(c(Entropy, `VLMR-LRT p-value`, `B-LRT P-value`), ~round(.x, 3), .names = "{.col}")
  ) %>%
  # ★★★★★ 最終的な列の順番にB-LRTを追加 ★★★★★
  select(
    Profiles, AIC, BIC, `Sample-Size Adjusted BIC`, Entropy, `Log-likelihood`,
    `VLMR-LRT p-value`, `B-LRT P-value`, `% in each class`
  )


# 6. 最終結果の表示とファイルへの保存
#----------------------------------------------------------------
cat("\n-------------------- 分析結果 (フルバージョン) --------------------\n")
print(final_table, n = Inf)
cat("---------------------------------------------------------------------\n")

output_filename <- "lpa_fit_indices_full_results.csv"
write_csv(final_table, output_filename)
cat(paste("\n分析結果が '", output_filename, "' という名前で保存されました。\n", sep=""))