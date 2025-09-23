################################################################################
# 改良版: クラスカル・ウォリス検定と多重比較、効果量を一括で実行するスクリプト
# エンコーディング問題とdeprecated warningを修正
################################################################################

# --- 必要なパッケージのインストール（初回のみ） ---
# install.packages(c("rstatix", "dunn.test", "dplyr", "tidyr", "readr"))

# --- ライブラリの読み込み ---
library(rstatix)
library(dunn.test)
library(dplyr)
library(tidyr)
library(readr)

################################################################################
# 【ユーザー設定】ここから下を自分の環境に合わせて変更してください
################################################################################

# 1. 分析対象のCSVファイルのパスを指定
csv_file_path <- "test_4class_data.csv"

# 2. 分析したい項目（列名）をベクトルで指定
# 実際のデータに合わせた心理測定項目を指定
columns_to_analyze <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# 3. クラス（グループ）分けに使われる列名を指定
class_column_name <- "Class"

# 4. 結果を保存するCSVファイルの名前を指定
output_csv_name <- "444analysis_results_improved.csv"

# 5. サンプルデータを使用するかどうか
use_sample_data <- FALSE # 改良版ダミーデータを使用

################################################################################
# 【ユーザー設定】はここまで
################################################################################

#' 指定された項目に対してクラスごとの統計分析を実行する関数（改良版）
#' 
#' @param file_path データが入ったCSVファイルのパス
#' @param columns_to_test 分析したい項目名のベクトル
#' @param class_column クラス分けに使われる列名
#' @return 分析結果をまとめたデータフレーム
analyze_by_class_improved <- function(file_path, columns_to_test, class_column) {
  
  # ファイルの存在チェック
  if (!file.exists(file_path)) {
    stop(paste("エラー: ファイルが見つかりません ->", file_path))
  }
  
  # データの読み込み（readr::read_csvを使用してエンコーディング問題を回避）
  df <- read_csv(file_path, locale = locale(encoding = "UTF-8"))
  
  # クラス列を因子型に変換
  df[[class_column]] <- as.factor(df[[class_column]])
  
  # 結果を格納するための空のリストを作成
  results_list <- list()
  
  # 指定された各項目についてループ処理
  for (col in columns_to_test) {
    
    # 記述統計量の計算（deprecated warningを修正）
    desc_stats <- df %>%
      group_by(across(all_of(class_column))) %>%
      summarise(
        Mean = mean(.data[[col]], na.rm = TRUE),
        Median = median(.data[[col]], na.rm = TRUE),
        SD = sd(.data[[col]], na.rm = TRUE),
        Variance = var(.data[[col]], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      pivot_wider(
        names_from = all_of(class_column),
        values_from = c("Mean", "Median", "SD", "Variance"),
        names_sep = "_"
      )
    
    # クラスカル・ウォリス検定
    kw_formula <- reformulate(class_column, col)
    kw_test <- kruskal.test(kw_formula, data = df)
    
    # η² (イータ二乗) の計算
    eta_squared <- kruskal_effsize(kw_formula, data = df)$effsize
    
    # Post-hoc Dunn検定 (Bonferroni補正)
    dunn_res <- dunn.test(df[[col]], df[[class_column]], method = "bonferroni")
    
    # 多重比較結果の整理
    posthoc_results <- paste(
      dunn_res$comparisons, 
      ": p =", 
      format.pval(dunn_res$P.adjusted, digits = 3, eps = 0.001),
      collapse = "; "
    )
    
    # 効果量の解釈を追加
    effect_interpretation <- case_when(
      eta_squared < 0.01 ~ "negligible",
      eta_squared < 0.06 ~ "small", 
      eta_squared < 0.14 ~ "medium",
      TRUE ~ "large"
    )
    
    # 全ての結果を結合
    current_result <- tibble(Item = col) %>%
      bind_cols(desc_stats) %>%
      mutate(
        H_Value = kw_test$statistic,
        df = kw_test$parameter,
        p_value_kruskal = kw_test$p.value,
        Eta_Squared = eta_squared,
        Effect_Size = effect_interpretation,
        PostHoc_Dunn_Bonferroni = posthoc_results,
        Significant = ifelse(.data$p_value_kruskal < 0.05, "Yes", "No")
      )
    
    results_list[[col]] <- current_result
  }
  
  # リストを一つのデータフレームに結合して返す
  final_results <- bind_rows(results_list)
  return(final_results)
}

# --- メイン処理：関数の実行と結果の出力 ---

cat("=== 改良版クラス間比較分析 開始 ===\n\n")

# 分析を実行
analysis_result_df <- analyze_by_class_improved(
  file_path = csv_file_path,
  columns_to_test = columns_to_analyze,
  class_column = class_column_name
)

# 結果をコンソールに表示
cat("--- 分析結果の要約 ---\n")
print(analysis_result_df %>% 
        select(Item, p_value_kruskal, Eta_Squared, Effect_Size, Significant))

cat("\n--- 詳細な分析結果 ---\n")
print(analysis_result_df)

# 結果をCSVファイルとして保存（UTF-8エンコーディング）
write_csv(analysis_result_df, output_csv_name)

cat(paste("\n=== 完了 ===\n"))
cat(paste("結果を '", output_csv_name, "' に保存しました。\n", sep=""))

# 有意な結果の項目をハイライト
significant_items <- analysis_result_df %>% 
  filter(Significant == "Yes") %>% 
  pull(Item)

if(length(significant_items) > 0) {
  cat("\n有意差が見られた項目:\n")
  for(item in significant_items) {
    result <- analysis_result_df %>% filter(Item == item)
    cat(sprintf("- %s: p = %.3f, η² = %.3f (%s effect)\n", 
                item, result$p_value_kruskal, result$Eta_Squared, result$Effect_Size))
  }
} else {
  cat("\n有意差が見られた項目はありませんでした。\n")
}