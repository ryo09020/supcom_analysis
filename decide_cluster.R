#################################################################
# LPA 適合度指標 比較表作成 Rスクリプト (BLRT, VLMR p値 両対応)
# (GeminiによるVLMR p値計算ロジック修正版)
#################################################################

# ---------------------------------------------------------------
# 1. パッケージの準備
# ---------------------------------------------------------------
# 必要なパッケージリスト
packages <- c("tidyverse", "tidyLPA")
# インストールされていないパッケージを特定
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
# 新規パッケージをインストール
if(length(new_packages)) install.packages(new_packages)
# 全てのパッケージを読み込み
lapply(packages, library, character.only = TRUE)
cat("✅ Step 1: パッケージの読み込みが完了しました。\n")

# ---------------------------------------------------------------
# ★ クラスター数の範囲を指定
# ---------------------------------------------------------------
# 例えば、2クラスから5クラスまでを比較
PROFILE_RANGE <- 1:5

# ---------------------------------------------------------------
# 2. データの準備
# ---------------------------------------------------------------
cat("Step 2: データの準備を開始します...\n")
# 分析に使用する変数名（列名）を指定
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")
# 入力ファイル名を指定
input_file <- "raw_data/dummy_data.csv"

# ファイル存在チェック
if (!file.exists(input_file)) {
  stop(paste("❌ エラー: 指定されたファイル '", input_file, "' が見つかりません。", sep=""))
}

# データの読み込み、前処理（NA削除、標準化など）
df_analysis <- readr::read_csv(input_file) %>%
  select(all_of(lpa_target_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  tidyr::drop_na() %>%
  scale() %>%
  as.data.frame()
cat("✅ 分析用データの準備が完了しました。\n")
cat(paste("分析対象のサンプルサイズ (N):", nrow(df_analysis), "\n"))


# ---------------------------------------------------------------
# 3. LPAの実行 (複数モデル)
# ---------------------------------------------------------------
cat(paste0("Step 3: ", min(PROFILE_RANGE), " から ", max(PROFILE_RANGE)," クラスのLPAを実行します...\n"))
cat("... BLRTの計算を含むため、モデル数によっては時間がかかります。\n")
# tidyLPAで複数のクラス数のモデルを一度に推定
lpa_models <- tidyLPA::estimate_profiles(
  df_analysis,
  n_profiles = PROFILE_RANGE,
  boot_for_p = TRUE # BLRT p-valueを計算
)
cat("✅ LPAの計算が完了しました。\n")

# ---------------------------------------------------------------
# 4. 適合度指標の比較表を作成 (VLMR p値も計算)
# ---------------------------------------------------------------
cat("Step 4: 適合度指標の比較表を作成します...\n")
# まずは基本的な適合度指標を取得
fit_indices <- tidyLPA::get_fit(lpa_models)
N <- nrow(df_analysis)

# --- パラメータ数をモデルオブジェクトから直接取得 ---
npar_vec <- sapply(lpa_models, function(mod) {
  return(mod$model$df)
})
fit_indices$Parameters <- npar_vec


# --- VLMR p値をループで順次計算 ---
fit_indices$VLMR_p <- NA_real_

for (k in 2:nrow(fit_indices)) {
  null_model <- fit_indices[k - 1, ]
  alt_model  <- fit_indices[k, ]
  
  cat(sprintf("... VLMR p-value: %dクラス vs %dクラス を比較中\n", alt_model$Classes, null_model$Classes))
  
  lmr_result <- tidyLPA::calc_lrt(
    n = N,
    null_ll = null_model$LogLik,
    null_param = null_model$Parameters,
    null_classes = null_model$Classes,
    alt_ll = alt_model$LogLik,
    alt_param = alt_model$Parameters,
    alt_classes = alt_model$Classes
  )
  
  fit_indices$VLMR_p[k] <- lmr_result[4]
}


# --- ★【修正箇所】各モデル・クラスの所属割合（％）を計算 ---
# 'Classes' を 'classes_number' に修正しました
class_proportions <- get_data(lpa_models) %>%
  count(classes_number, Class) %>%
  group_by(classes_number) %>%
  summarise(
    `% in each class` = paste(round(n / sum(n) * 100), collapse = "/"),
    .groups = "drop"
  ) %>%
  rename(Profiles = classes_number) # 最終的な列名をProfilesに変更


# --- 比較表を整形 ---
final_comparison_table <- fit_indices %>%
  rename(
    Profiles = Classes,
    `Log-likelihood` = LogLik,
    `Sample-Size Adjusted BIC` = SABIC,
    `BLRT p-value` = BLRT_p,
    `VLMR p-value` = VLMR_p
  ) %>%
  select(
    Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`,
    Entropy, `BLRT p-value`, `VLMR p-value`
  ) %>%
  left_join(class_proportions, by = "Profiles") %>%
  mutate(
    across(c(`Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`), ~round(.x, 2)),
    across(c(Entropy, `BLRT p-value`, `VLMR p-value`), ~round(.x, 3))
  )

cat("✅ 比較表の作成が完了しました。\n")

# ---------------------------------------------------------------
# 5. 結果の出力 (CSVファイル)
# ---------------------------------------------------------------
output_filename <- "lpa_comparison_table.csv"
readr::write_csv(final_comparison_table, output_filename)
cat(paste0("📈 分析結果が '", output_filename, "' という名前で保存されました。\n"))

# 最終的な比較表をコンソールにも表示
print(final_comparison_table)