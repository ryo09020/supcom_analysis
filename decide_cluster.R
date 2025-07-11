#################################################################
# LPA 適合度指標 比較表作成 Rスクリプト (複数モデル実行版)
#
# 目的：
# - 指定された範囲のクラスター数でLPAを一括実行する。
# - 各モデルの適合度指標（AIC, BIC, SABIC, Entropy, BLRT_p）と
#   クラス所属率をまとめた比較表を作成する。
# - 結果をCSVファイルとして保存する。
#################################################################

# ---------------------------------------------------------------
# 1. パッケージの準備
# ---------------------------------------------------------------
# 必要なパッケージのリスト
packages <- c("tidyverse", "tidyLPA")

# インストールされていないパッケージをインストール
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# パッケージの読み込み
lapply(packages, library, character.only = TRUE)

cat("✅ Step 1: パッケージの読み込みが完了しました。\n")


# ---------------------------------------------------------------
# ★★★★★ ここで分析したいクラスター数の範囲を指定 ★★★★★
# ---------------------------------------------------------------
PROFILE_RANGE <- 2:10
# ---------------------------------------------------------------


# ---------------------------------------------------------------
# 2. データの準備
# ---------------------------------------------------------------
cat("Step 2: データの準備を開始します...\n")

# 分析対象の列名を指定
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

# ---------------------------------------------------------------
# ★★★★★ ここで入力ファイルを指定 ★★★★★
# ---------------------------------------------------------------
input_file <- "dummy_data.csv"
# ---------------------------------------------------------------

# ファイルの存在確認
if (!file.exists(input_file)) {
  stop(paste("❌ エラー: 指定されたファイル '", input_file, "' が見つかりません。", sep=""))
}

# データの読み込みと前処理
cat(paste("... ファイルを読み込み中:", basename(input_file), "\n"))
df_analysis <- read_csv(input_file) %>%
  select(all_of(lpa_target_columns)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit() %>%
  scale() %>%
  as.data.frame()

cat("✅ 分析用データの準備が完了しました。\n")


# ---------------------------------------------------------------
# 3. LPAの実行 (複数モデル)
# ---------------------------------------------------------------
cat(paste("Step 3: ", min(PROFILE_RANGE), "から", max(PROFILE_RANGE), "クラスターのLPAを実行します...\n", sep=""))
cat("... BLRTの計算を含むため、モデル数によっては時間がかかります。\n")

lpa_models <- estimate_profiles(
  df_analysis,
  n_profiles = PROFILE_RANGE,
  boot_for_p = TRUE  # BLRT p-valueを計算
)

cat("✅ LPAの計算が完了しました。\n")


# ---------------------------------------------------------------
# 4. 適合度指標の比較表を作成
# ---------------------------------------------------------------
cat("Step 4: 適合度指標の比較表を作成します...\n")

# 4-1. 基本的な適合度指標を取得
fit_indices <- get_fit(lpa_models)

# 4-2. 各モデル・各クラスの所属割合 (%) を計算
class_proportions <- get_data(lpa_models) %>%
  count(classes_number, Class) %>%
  group_by(classes_number) %>%
  summarise(
    `% in each class` = paste(round(n / sum(n) * 100), collapse = "/"),
    .groups = 'drop'
  ) %>%
  rename(Profiles = classes_number)

# 4-3. 全ての指標を一つの表に統合
final_comparison_table <- fit_indices %>%
  rename(
    Profiles = Classes,
    `Log-likelihood` = LogLik,
    `Sample-Size Adjusted BIC` = SABIC,
    `BLRT p-value` = BLRT_p
  ) %>%
  select(
    Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`,
    Entropy, `BLRT p-value`
  ) %>%
  left_join(class_proportions, by = "Profiles") %>%
  mutate(
    across(c(`Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`), ~round(.x, 2)),
    across(c(Entropy, `BLRT p-value`), ~round(.x, 3))
  )

cat("✅ 比較表の作成が完了しました。\n")


# ---------------------------------------------------------------
# 5. 結果の出力 (CSVファイル)
# ---------------------------------------------------------------
output_filename <- "lpa_comparison.csv"
write_csv(final_comparison_table, output_filename)
cat(paste("📈 分析結果が '", output_filename, "' という名前で保存されました。\n", sep=""))