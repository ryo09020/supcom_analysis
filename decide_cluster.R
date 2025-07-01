#################################################################
# 潜在プロファイル分析(LPA)と適合度指標の計算 Rスクリプト (最終完成版)
#################################################################

# 1. 準備：必要なパッケージのインストールと読み込み
#----------------------------------------------------------------
# install.packages("tidyverse")
# install.packages("tidyLPA")
# install.packages("mclust")

library(tidyverse)
library(tidyLPA)
library(mclust)


# 2. データの準備
#----------------------------------------------------------------
cat("Step 2: データの準備を開始します...\n")
# "dummy_data.csv" の部分は、実際のファイル名に合わせてください。
my_data <- read_csv("dummy_data.csv")
tipi_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# Step 2-1: 必要な列を選択
df_selected <- my_data[, tipi_columns]
# Step 2-2: 選択した列を標準化
df_scaled <- as.data.frame(scale(df_selected))
# Step 2-3: 欠損値を含む行を削除
df_analysis <- na.omit(df_scaled)

cat("分析データが準備できました。\n")
print(head(df_analysis))


# 3. 潜在プロファイル分析の実行 (クラスター数 2-10)
#----------------------------------------------------------------
cat("\nStep 3: 潜在プロファイル分析を実行します...\n")
lpa_models <- estimate_profiles(df_analysis, n_profiles = 2:10)
cat("LPAの計算が完了しました。\n")


# 4. 適合度指標の計算
#----------------------------------------------------------------
cat("\nStep 4: 適合度指標の計算を開始します...\n")

# 4-1. 基本的な適合度指標の取得
fit_indices <- get_fit(lpa_models)

# 4-2. 各クラスの所属割合 (%) の計算
data_with_classes <- get_data(lpa_models)
grouped_data <- group_by(data_with_classes, model_number, Class)
summarised_data1 <- summarise(grouped_data, n = n(), .groups = 'drop_last')
mutated_data <- mutate(summarised_data1, proportion = n / sum(n))
summarised_data2 <- summarise(mutated_data, class_proportions_text = paste(round(proportion * 100), collapse = "/"))
# ここで作成される'Profiles'列は、後で'fit_indices'由来のデータと結合するために使います。
class_proportions <- rename(summarised_data2, Profiles = model_number)

# 4-3. B-LRT P-value (Bootstrap Likelihood Ratio Test) の計算
cat("B-LRT P-value の計算を開始します (時間がかかる場合があります)...\n")
blrt_results <- tibble(Profiles = 2:10, `B-LRT P-value` = NA_real_)
for (k in 2:10) {
  cat(paste("計算中: ", k, "クラス vs", k - 1, "クラス...\n"))
  lrt_test <- mclustBootstrapLRT(df_analysis, modelName = "VVI", maxG = k, nboot = 200)
  blrt_results$`B-LRT P-value`[blrt_results$Profiles == k] <- lrt_test$p.value[k - 1]
}
cat("B-LRT P-value の計算が完了しました。\n")


# 5. 全ての指標を一つの表に統合
#----------------------------------------------------------------
cat("\nStep 5: 全ての指標を一つの表に統合します...\n")

# Step 5-1: fit_indicesの列名を、最終的な表で使う名前に統一します。
# 【最終修正】ご報告いただいた実際の列名に合わせて、renameの対象を修正しました。
renamed_table <- rename(fit_indices,
                        Profiles = Classes,
                        `Adjusted BIC` = SABIC,
                        LogLikelihood = LogLik)

# Step 5-2: 最終的に使用する列を選択します
selected_columns <- c("Profiles", "AIC", "BIC", "Adjusted BIC", "Entropy", "LogLikelihood")
results_table <- renamed_table[, selected_columns]

# Step 5-3: B-LRTとクラス割合の結果を結合
results_table <- left_join(results_table, blrt_results, by = "Profiles")
results_table <- left_join(results_table, class_proportions, by = "Profiles")

# Step 5-4: 残りの列を追加・整形
results_table <- mutate(results_table,
                        `VLMR-LRT P-value` = "N/A in R",
                        `B-LRT P-value` = round(`B-LRT P-value`, 4))

# Step 5-5: 最終的な列の順序に並び替え
results_table <- rename(results_table, `% in each class` = class_proportions_text)
final_columns_order <- c("Profiles", "AIC", "BIC", "Adjusted BIC", "Entropy", "LogLikelihood",
                         "VLMR-LRT P-value", "B-LRT P-value", "% in each class")
final_table <- results_table[, final_columns_order]


# 6. 最終結果の表示
#----------------------------------------------------------------
cat("\n-------------------- 分析結果 --------------------\n")
print(final_table, n = Inf)
cat("--------------------------------------------------\n")