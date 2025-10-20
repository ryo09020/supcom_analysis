################################################################################
# MMSEクラス比較スクリプト
#  - クラスターごとのMMSEスコアを比較し、統計量・効果量・多重比較を出力
#  - 64歳以下でMMSEが取得されていない場合は満点(30点)として補完
#  - 実測MMSEがある対象だけのクラス別統計も同時出力
################################################################################

# --- 必要なパッケージのインストール（初回のみ） ---
# install.packages(c("rstatix", "dunn.test", "dplyr", "tidyr", "readr", "glue"))

# --- ライブラリの読み込み ---
library(dplyr)
library(tidyr)
library(readr)
library(rstatix)
library(dunn.test)
library(glue)

invisible(utils::globalVariables(
  c(
    "mmse_adjusted", "mmse_source", "mmse_raw_numeric",
    "Count", "Mean", "Median", "SD", "Variance",
    "Observed_Count", "Imputed_Count", "p_value_kruskal"
  )
))

################################################################################
# 【ユーザー設定】ここから
################################################################################

# 1. 分析対象のCSVファイルのパス
input_csv_path <- "../lpa/dummy_data_with_clusters_sorted.csv"

# 2. クラスター分けに使用する列名
class_column_name <- "Class"

# 3. MMSEスコアの列名
mmse_column_name <- "MMSE"

# 4. 年齢列（数値に変換可能な列）
age_column_name <- "参加時年齢"

# 5. 補完に使用する年齢閾値とMMSEスコア
age_threshold <- 64          # 64歳以下を満点補完の対象とする
imputed_full_score <- 30     # MMSE満点

# 6. 出力ファイル名
output_adjusted_csv <- "mmse_class_analysis_adjusted.csv"
output_observed_csv <- "mmse_class_analysis_observed_only.csv"

# 7. サンプルデータを使用する場合（本番データで実行する際は FALSE に変更）
use_sample_data <- TRUE

################################################################################
# 【ユーザー設定】ここまで
################################################################################

# ---------------------------------------------------------------
# サンプルデータ生成（必要な場合のみ使用）
# ---------------------------------------------------------------
create_sample_mmse_data <- function() {
  set.seed(123)
  tibble(
    ID = 1:120,
    Class = sample(1:4, 120, replace = TRUE),
    参加時年齢 = sample(50:75, 120, replace = TRUE),
    MMSE = sample(c(15:30, NA, "未受検"), 120, replace = TRUE, prob = c(rep(1, 16), 5, 3))
  ) %>%
    mutate(
      MMSE = if_else(dplyr::row_number() %% 7 == 0, NA_character_, as.character(.data$MMSE)),
      `参加時年齢` = if_else(
        dplyr::row_number() %% 11 == 0,
        runif(dplyr::n(), 45, 63),
        .data$`参加時年齢`
      )
    )
}

# ---------------------------------------------------------------
# MMSE補完処理
# ---------------------------------------------------------------
prepare_mmse_data <- function(df, class_col, age_col, mmse_col, age_limit, full_score) {
  if (!(class_col %in% colnames(df))) {
    stop(glue("エラー: クラス列 '{class_col}' がデータに存在しません。"))
  }
  if (!(age_col %in% colnames(df))) {
    stop(glue("エラー: 年齢列 '{age_col}' がデータに存在しません。"))
  }
  if (!(mmse_col %in% colnames(df))) {
    stop(glue("エラー: MMSE列 '{mmse_col}' がデータに存在しません。"))
  }

  df[[class_col]] <- as.factor(df[[class_col]])

  df %>%
    mutate(
      age_numeric = suppressWarnings(as.numeric(.data[[age_col]])),
      mmse_raw_numeric = suppressWarnings(readr::parse_number(as.character(.data[[mmse_col]]))),
      mmse_adjusted = dplyr::case_when(
        !is.na(.data$mmse_raw_numeric) ~ .data$mmse_raw_numeric,
        !is.na(.data$age_numeric) & .data$age_numeric <= age_limit ~ full_score,
        TRUE ~ NA_real_
      ),
      mmse_source = dplyr::case_when(
        !is.na(.data$mmse_raw_numeric) ~ "observed",
        !is.na(.data$age_numeric) & .data$age_numeric <= age_limit ~ "imputed_under_threshold",
        TRUE ~ "missing"
      )
    )
}

# ---------------------------------------------------------------
# クラス別記述統計の作成（ワイド形式）
# ---------------------------------------------------------------
create_descriptive_table <- function(df, class_col) {
  df %>%
    group_by(across(all_of(class_col))) %>%
    summarise(
      Count = dplyr::n(),
      Mean = mean(.data$mmse_adjusted, na.rm = TRUE),
      Median = median(.data$mmse_adjusted, na.rm = TRUE),
      SD = sd(.data$mmse_adjusted, na.rm = TRUE),
      Variance = var(.data$mmse_adjusted, na.rm = TRUE),
      Observed_Count = sum(.data$mmse_source == "observed", na.rm = TRUE),
      Imputed_Count = sum(.data$mmse_source == "imputed_under_threshold", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = all_of(class_col),
      values_from = c("Count", "Mean", "Median", "SD", "Variance", "Observed_Count", "Imputed_Count"),
      names_sep = "_"
    )
}

# ---------------------------------------------------------------
# Dunn検定結果の整形
# ---------------------------------------------------------------
format_dunn_results <- function(dunn_obj) {
  if (is.null(dunn_obj)) {
    return("Not calculated")
  }
  paste(
    dunn_obj$comparisons,
    ": p =",
    format.pval(dunn_obj$P.adjusted, digits = 3, eps = 0.001),
    collapse = "; "
  )
}

# ---------------------------------------------------------------
# メイン分析処理
# ---------------------------------------------------------------
run_mmse_analysis <- function(df, class_col, age_col, mmse_col, age_limit, full_score) {
  prepared <- prepare_mmse_data(df, class_col, age_col, mmse_col, age_limit, full_score)

  analysis_df <- prepared %>% filter(!is.na(.data$mmse_adjusted))
  observed_df <- prepared %>% filter(!is.na(.data$mmse_raw_numeric))

  if (nrow(analysis_df) == 0) {
    stop("有効なMMSEスコアが存在しないため、分析を実行できません。")
  }

  class_levels <- analysis_df[[class_col]]
  if (length(unique(class_levels)) < 2) {
    stop("クラスが1種類のみのため、クラス間比較が実行できません。")
  }

  # 記述統計
  desc_table <- create_descriptive_table(analysis_df, class_col)

  # クラスカル・ウォリス検定
  kw_formula <- reformulate(class_col, "mmse_adjusted")
  kw_test <- kruskal.test(kw_formula, data = analysis_df)
  eta_sq <- kruskal_effsize(kw_formula, data = analysis_df)$effsize

  # Dunnの多重比較
  dunn_res <- dunn.test::dunn.test(analysis_df$mmse_adjusted, analysis_df[[class_col]], method = "bonferroni")
  dunn_text <- format_dunn_results(dunn_res)

  effect_label <- case_when(
    eta_sq < 0.01 ~ "negligible",
    eta_sq < 0.06 ~ "small",
    eta_sq < 0.14 ~ "medium",
    TRUE ~ "large"
  )

  main_result <- tibble(Item = mmse_col) %>%
    bind_cols(desc_table) %>%
    mutate(
      H_Value = kw_test$statistic,
      df = kw_test$parameter,
      p_value_kruskal = kw_test$p.value,
      Eta_Squared = eta_sq,
      Effect_Size = effect_label,
      PostHoc_Dunn_Bonferroni = dunn_text,
      Significant = if_else(.data$p_value_kruskal < 0.05, "Yes", "No")
    )

  observed_summary <- observed_df %>%
    group_by(across(all_of(class_col))) %>%
    summarise(
      Count = dplyr::n(),
      Mean = mean(.data$mmse_raw_numeric, na.rm = TRUE),
      Median = median(.data$mmse_raw_numeric, na.rm = TRUE),
      SD = sd(.data$mmse_raw_numeric, na.rm = TRUE),
      Min = min(.data$mmse_raw_numeric, na.rm = TRUE),
      Max = max(.data$mmse_raw_numeric, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    prepared_data = prepared,
    adjusted_results = main_result,
    observed_summary = observed_summary
  )
}

# ---------------------------------------------------------------
# 実行フロー
# ---------------------------------------------------------------
cat("=== MMSEクラス比較 分析開始 ===\n\n")

if (use_sample_data) {
  cat("サンプルデータを使用します。\n\n")
  raw_data <- create_sample_mmse_data()
} else {
  cat(glue("📁 データ読み込み: {input_csv_path}\n"))
  raw_data <- read_csv(input_csv_path, locale = locale(encoding = "UTF-8"))
}

analysis_outputs <- run_mmse_analysis(
  df = raw_data,
  class_col = class_column_name,
  age_col = age_column_name,
  mmse_col = mmse_column_name,
  age_limit = age_threshold,
  full_score = imputed_full_score
)

adjusted_results <- analysis_outputs$adjusted_results
observed_summary <- analysis_outputs$observed_summary
prepared_data <- analysis_outputs$prepared_data

cat("--- 補完後MMSEに基づくクラス比較 ---\n")
print(adjusted_results %>% select(Item, p_value_kruskal, Eta_Squared, Effect_Size, Significant))
cat("\n詳細:\n")
print(adjusted_results)

cat(glue("\n補完後MMSEの結果を '{output_adjusted_csv}' として保存します。\n"))
write_csv(adjusted_results, output_adjusted_csv)

cat("\n--- 実測MMSEのみのクラス別統計 ---\n")
if (nrow(observed_summary) == 0) {
  cat("実測MMSEが存在しませんでした。\n")
} else {
  print(observed_summary)
  cat(glue("\n実測MMSEのクラス別統計を '{output_observed_csv}' として保存します。\n"))
  write_csv(observed_summary, output_observed_csv)
}

# 追加情報: 補完状況の集計
cat("\n--- 補完状況のサマリー ---\n")
source_summary <- prepared_data %>%
  count(mmse_source) %>%
  mutate(割合 = round(n / sum(n) * 100, 2))
print(source_summary)

cat("\n=== 分析完了 ===\n")
