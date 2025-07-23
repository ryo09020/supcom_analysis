# ------------------------------------------------------------------------------
# Mplus LPA 結果集計スクリプト (修正版)
# ------------------------------------------------------------------------------

### Step 1: パッケージの読み込みとフォルダの指定 ###

# 必要なパッケージを読み込みます
library(MplusAutomation)
library(dplyr)
library(stringr)
library(purrr) # 新しく purrr パッケージを使います

# 分析結果が保存されているフォルダ名を指定します
results_folder <- "tipi-j"


### Step 2: Mplusの出力ファイル (.out) を一括で読み込む (修正) ###

# readModels()がINPUTセクションの解析でエラーを起こす問題に対処するため、
# `what`引数で必要な「要約(summaries)」と「クラス分類結果(class_counts)」だけを
# 明示的に読み込むように変更しました。
message("Mplusの出力ファイルを読み込んでいます...")
all_models <- readModels(
  target = results_folder,
  what = c("summaries", "class_counts"),
  quiet = TRUE # 詳細なパースエラーの出力を抑制します
)
message("読み込みが完了しました。")


### Step 3: 適合度指標とクラス割合を抽出して表を作成する (修正) ###

# 適合度指標（AIC, BICなど）を抽出します。
# 非推奨の`extractModelSummaries`に代わり、readModelsの結果から直接データフレームを作成します。
fit_indices <- all_models %>%
  # 各モデルの$summaries部分を抽出し、一つのデータフレームに結合します
  map_dfr(~.x$summaries, .id = "Filename") %>%
  # 何らかの理由で読み込めなかったモデル（行が全てNAになる）を除外します
  filter(!is.na(AIC))

# モデルのファイル名からクラス数を抽出します
fit_indices <- fit_indices %>%
  mutate(
    Profiles = as.integer(str_extract(Filename, "\\d+"))
  ) %>%
  # Profiles（クラス数）を基準に昇順で並び替えます
  arrange(Profiles)

# 各モデルからクラスごとの所属人数の割合 (%) を抽出します
class_proportions <- all_models %>%
  # map_chr関数で各モデルからクラス割合を抽出し、文字列に整形します
  map_chr(function(model) {
    # `model$class_counts$modelEstimated$proportion` に割合が格納されています
    if (!is.null(model$class_counts$modelEstimated)) {
      props <- model$class_counts$modelEstimated$proportion
      # パーセンテージに変換し、小数点以下1桁に丸めます
      props_percent <- round(props * 100, 1)
      # "80.5%, 19.5%" のような文字列に変換して返します
      paste(props_percent, collapse = "%, ")
    } else {
      # 読み込み失敗などで情報がない場合はNAを返します
      NA_character_
    }
  })

# 抽出したクラス割合をデータフレームに変換します
proportion_df <- data.frame(
  Filename = names(class_proportions),
  Class_Percentages = class_proportions
)


### Step 4: 適合度指標とクラス割合を結合し、最終的な表を完成させる ###

# 適合度指標の表 (fit_indices) とクラス割合の表 (proportion_df) を結合します
summary_table <- left_join(fit_indices, proportion_df, by = "Filename")

# 最終的に必要な列を選び、名前を分かりやすく変更します
summary_table <- summary_table %>%
  select(
    Profiles,
    LogLikelihood = LL,
    AIC,
    BIC,
    aBIC, # Adjusted BIC
    Entropy,
    VLMR_p_value = any_of("T11_VLMR_PValue"),
    BLRT_p_value = any_of("BLRT_PValue"),
    Class_Percentages
  )


### Step 5: 結果の表示とファイルへの保存 ###

# 作成した要約表をコンソールに表示します
print(summary_table)


# 結果をCSVファイルとして保存します
write.csv(summary_table, "LPA_summary_table.csv", row.names = FALSE, fileEncoding = "UTF-8")

message("\n✅ 結果を 'LPA_summary_table.csv' として保存しました。")