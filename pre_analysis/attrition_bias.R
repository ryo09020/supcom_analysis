# install.packages("dplyr") # もしdplyrがなければインストールしてください
# install.packages("effectsize") # 効果量計算のために追加
library(dplyr)
library(effectsize)

# 1. データの読み込み
# 実際にはご自身のファイルパスを指定してください
t1_data <- read.csv("time1_data.csv")
t2_data <- read.csv("time2_data.csv")

# 2. 追跡完了者と脱落者を特定
# Time1のデータに「status」列を追加して、両者を区別する
t1_data <- t1_data %>%
  mutate(status = ifelse(id %in% t2_data$id, "Completer", "Dropout"))

# 3. 最終学歴を教育年数（数値）に変換
# case_whenを使って、カテゴリを数値にマッピングします
t1_data <- t1_data %>%
  mutate(years_of_education = case_when(
    final_education == "高卒"     ~ 12,
    final_education == "短大卒"   ~ 14,
    final_education == "大卒"     ~ 16,
    final_education == "大学院卒" ~ 18,
    TRUE ~ NA_real_ # それ以外の値はNA（欠損値）にする
  ))

# 4. 欠落バイアスの検証
# 結果を格納するための空のデータフレームを作成
results_summary <- data.frame()

# (A) 年齢 (age) の比較: t検定
t_test_age <- t.test(age ~ status, data = t1_data)
effect_size_age <- cohens_d(age ~ status, data = t1_data)
results_summary <- rbind(results_summary, data.frame(
  variable = "age",
  test_type = "t-test",
  statistic_name = "t-value",
  statistic_value = t_test_age$statistic,
  p_value = t_test_age$p.value,
  effect_size_name = "Cohen's d",
  effect_size_value = effect_size_age$Cohens_d,
  is_significant = ifelse(t_test_age$p.value < 0.05, "Yes", "No")
))

# (B) 性別 (sex) の比較: カイ二乗検定
sex_table <- table(t1_data$status, t1_data$sex)
chi_test_sex <- chisq.test(sex_table)
effect_size_sex <- cohens_w(sex_table)
results_summary <- rbind(results_summary, data.frame(
  variable = "sex",
  test_type = "chi-squared test",
  statistic_name = "chi-squared",
  statistic_value = chi_test_sex$statistic,
  p_value = chi_test_sex$p.value,
  effect_size_name = "Cohen's W",
  effect_size_value = effect_size_sex$Cohens_w,
  is_significant = ifelse(chi_test_sex$p.value < 0.05, "Yes", "No")
))

# (C) 教育年数 (years_of_education) の比較: t検定
t_test_edu <- t.test(years_of_education ~ status, data = t1_data)
effect_size_edu <- cohens_d(years_of_education ~ status, data = t1_data)
results_summary <- rbind(results_summary, data.frame(
  variable = "years_of_education",
  test_type = "t-test",
  statistic_name = "t-value",
  statistic_value = t_test_edu$statistic,
  p_value = t_test_edu$p.value,
  effect_size_name = "Cohen's d",
  effect_size_value = effect_size_edu$Cohens_d,
  is_significant = ifelse(t_test_edu$p.value < 0.05, "Yes", "No")
))


# 5. フローチャート用の記述統計量を計算
# 全体の人数と脱落率
n_total <- nrow(t1_data)
n_completers <- sum(t1_data$status == "Completer")
n_dropouts <- sum(t1_data$status == "Dropout")
dropout_rate <- (n_dropouts / n_total) * 100

# 男女別の人数
sex_counts <- t1_data %>%
  group_by(status, sex) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(sex_label = ifelse(sex == 0, "Male", "Female"))

completer_male_count <- sex_counts$count[sex_counts$status == "Completer" & sex_counts$sex_label == "Male"]
completer_female_count <- sex_counts$count[sex_counts$status == "Completer" & sex_counts$sex_label == "Female"]
dropout_male_count <- sex_counts$count[sex_counts$status == "Dropout" & sex_counts$sex_label == "Male"]
dropout_female_count <- sex_counts$count[sex_counts$status == "Dropout" & sex_counts$sex_label == "Female"]

# サマリーデータフレームを作成
flowchart_summary <- data.frame(
  Metric = c("Total Participants (Time 1)",
             "Completers (Time 2)",
             "Dropouts",
             "Attrition Rate (%)",
             "Completers: Male (count)",
             "Completers: Female (count)",
             "Dropouts: Male (count)",
             "Dropouts: Female (count)"),
  Value = c(n_total,
            n_completers,
            n_dropouts,
            round(dropout_rate, 2),
            completer_male_count,
            completer_female_count,
            dropout_male_count,
            dropout_female_count)
)


# 6. 全ての結果を1つのCSVファイルに出力
output_file <- "attrition_analysis_summary.csv"

# まずフローチャート用サマリーを書き込む
write.csv(flowchart_summary, output_file, row.names = FALSE, fileEncoding = "UTF-8")

# 追記モード(append = TRUE)で、間に空白行を一行追加する
write.table("\n--- Statistical Test Results ---\n", output_file, append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)

# 最後に統計的検定の結果を追記する
# write.csvはappendをサポートしていないため、write.tableを使い、カンマ区切りを指定する
write.table(results_summary, 
            file = output_file, 
            append = TRUE, 
            sep = ",", 
            row.names = FALSE, 
            col.names = TRUE, # 2つ目の表のヘッダーを書き込む
            fileEncoding = "UTF-8")

cat("------------------------------------------------------------\n")
cat(paste("全ての集計・検証結果を '", output_file, "' に出力しました。\n", sep=""))
cat("------------------------------------------------------------\n")
