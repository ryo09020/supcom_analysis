# install.packages("dplyr") # もしdplyrがなければインストールしてください
# install.packages("effectsize") # 効果量計算のために追加
library(dplyr)
library(effectsize)

# --- ★★★ 母集団の情報をここに入力 ★★★ ---
# e-Statなどで調べた日本の全体の最新のデータを入力してください。
# 以下は仮の数値です。

# 1. 日本の平均年齢
population_mean_age <- 48.4

# 2. 日本の男女比 (男性=0, 女性=1 の順で)
# 例: 男性が38%、女性が62%の場合
population_sex_proportions <- c(0.487, 0.513) 

# 3. 日本の平均教育年数
population_mean_education <- 12.9


# --- サンプルデータの準備 ---
# 前のステップで作成したtime1のデータを読み込みます。
# このスクリプトと同じフォルダに "time1_data.csv" があることを確認してください。
my_sample_data <- read.csv("time1_data.csv")


# --- ここからが分析の本番 ---
cat("--- サンプルの代表性の検証 ---\n\n")

# 最終学歴を教育年数（数値）に変換
my_sample_data <- my_sample_data %>%
  mutate(years_of_education = case_when(
    final_education == 1 ~ 9,
    final_education == 2 ~ 12,
    final_education == 3 ~ 14,
    final_education == 4 ~ 14,
    final_education == 5 ~ 16,
    final_education == 6 ~ 18,
    final_education == 7 ~ NA_real_,
    TRUE ~ NA_real_
  ))
cat("--- 最終学歴を教育年数に変換しました ---\n\n")


# 結果を格納するための空のデータフレームを作成
results_summary <- data.frame()


# (A) 年齢 (age) の比較: 1サンプルのt検定
cat("--- 変数 'age' の比較 (1サンプルのt検定) ---\n")
t_test_age <- t.test(my_sample_data$age, mu = population_mean_age)
effect_size_age <- cohens_d(my_sample_data$age, mu = population_mean_age)

print(t_test_age)
cat(paste("Cohen's d:", effect_size_age$Cohens_d, "\n\n"))

results_summary <- rbind(results_summary, data.frame(
  variable = "age",
  test_type = "One-sample t-test",
  population_value = population_mean_age,
  sample_mean = mean(my_sample_data$age),
  statistic_name = "t-value",
  statistic_value = t_test_age$statistic,
  p_value = t_test_age$p.value,
  effect_size_name = "Cohen's d",
  effect_size_value = effect_size_age$Cohens_d
))


# (B) 性別 (sex) の比較: カイ二乗適合度検定
cat("--- 変数 'sex' の比較 (カイ二乗適合度検定) ---\n")
observed_sex_counts <- table(my_sample_data$sex)

# サンプルの男女比を計算・表示
sample_sex_proportions <- prop.table(observed_sex_counts)
cat("サンプルの男女比:\n")
cat(paste("  男性 (0):", round(sample_sex_proportions[1] * 100, 1), "%\n"))
cat(paste("  女性 (1):", round(sample_sex_proportions[2] * 100, 1), "%\n"))

chi_test_sex <- chisq.test(observed_sex_counts, p = population_sex_proportions)
effect_size_sex <- cohens_w(observed_sex_counts, p = population_sex_proportions)

print(chi_test_sex)
cat(paste("Cohen's W:", effect_size_sex$Cohens_w, "\n\n"))

results_summary <- rbind(results_summary, data.frame(
  variable = "sex",
  test_type = "Chi-squared Goodness-of-Fit",
  population_value = paste(population_sex_proportions, collapse = "/"),
  sample_mean = NA,
  statistic_name = "chi-squared",
  statistic_value = chi_test_sex$statistic,
  p_value = chi_test_sex$p.value,
  effect_size_name = "Cohen's W",
  effect_size_value = effect_size_sex$Cohens_w
))


# (C) 教育年数 (years_of_education) の比較: 1サンプルのt検定
cat("--- 変数 'years_of_education' の比較 (1サンプルのt検定) ---\n")
t_test_edu <- t.test(my_sample_data$years_of_education, mu = population_mean_education)
effect_size_edu <- cohens_d(my_sample_data$years_of_education, mu = population_mean_education)

print(t_test_edu)
cat(paste("Cohen's d:", effect_size_edu$Cohens_d, "\n\n"))

results_summary <- rbind(results_summary, data.frame(
  variable = "years_of_education",
  test_type = "One-sample t-test",
  population_value = population_mean_education,
  sample_mean = mean(my_sample_data$years_of_education, na.rm = TRUE),
  statistic_name = "t-value",
  statistic_value = t_test_edu$statistic,
  p_value = t_test_edu$p.value,
  effect_size_name = "Cohen's d",
  effect_size_value = effect_size_edu$Cohens_d
))


# 6. 結果をCSVファイルに出力
output_file <- "sample_vs_jp_population_results.csv"
write.csv(results_summary, output_file, row.names = FALSE, fileEncoding = "UTF-8")

cat("------------------------------------------------------------\n")
cat(paste("サンプルと母集団の比較結果を '", output_file, "' に出力しました。\n", sep=""))
cat("------------------------------------------------------------\n")
