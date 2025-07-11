#################################################################
# LPAプロファイル分析とクラスター所属情報出力 Rスクリプト
#
# 目的：
# - 指定されたクラスター数でLPA（潜在プロファイル分析）を実行する。
# - 元のデータに、各個人が所属するクラスター番号を追加する。
# - クラスター情報を付与した新しいCSVファイルを出力する。
# - 結果を棒グラフとレーダーチャートで可視化する。
#
# 使用方法：
# 1. 下記の「パラメーター設定」セクションで、クラスター数や入力ファイルを指定する。
# 2. RStudioでスクリプト全体を実行するか、source()関数で実行する。
#    source("assign_clusters.R")
# 3. スクリプトの実行後、入力ファイルと同じ場所に「元のファイル名_with_clusters.csv」という名前で結果が出力される。
#################################################################

# ---------------------------------------------------------------
# 1. パッケージの準備
# ---------------------------------------------------------------
# 必要なパッケージのリスト
packages <- c("tidyverse", "tidyLPA", "knitr", "fmsb")

# インストールされていないパッケージをインストール
new_packages <- packages[!(packages %in% installed.packages()["Package"])]
if(length(new_packages)) install.packages(new_packages)

# パッケージの読み込み
lapply(packages, library, character.only = TRUE)

cat("Step 1: パッケージの読み込みが完了しました。\n")


# ---------------------------------------------------------------
# 2. パラメーター設定
# ---------------------------------------------------------------

# ★★★★★ ここで分析・可視化したいクラスター数を指定 ★★★★★
CHOSEN_N_PROFILES <- 4

# ★★★★★ ここで分析対象の列名を指定 ★★★★★
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")
# プロットのラベルに使用する項目名（英語）
new_item_labels <- c("Item A", "Item B", "Item C", "Item D", "Item E")

# ★★★★★ ここで入力ファイルを指定 ★★★★★
input_file <- "raw_data/dummy_data.csv"
# 例: input_file <- "mplus/physiological_test_1st_n12151.csv"

# ---------------------------------------------------------------
# 3. データの準備
# ---------------------------------------------------------------

cat("Step 2: データの準備を開始します...\n")

# ファイルの存在確認
if (!file.exists(input_file)) {
  stop(paste("エラー: 指定されたファイル '", input_file, "' が見つかりません。", sep=""))
}

# 元のデータを読み込み、行ID（row_id）を付与
df_original <- read_csv(input_file) %>%
  mutate(row_id = row_number())

# LPA分析用のデータを作成
# - 分析対象の列とrow_idを選択
# - 対象列を数値に変換し、NAを含む行を削除
df_for_lpa <- df_original %>%
  select(row_id, all_of(lpa_target_columns)) %>%
  mutate(across(all_of(lpa_target_columns), as.numeric)) %>%
  na.omit()

# Zスコアに標準化（LPA用）
# row_idは標準化から除外してスケールし、後で結合する
df_to_scale <- df_for_lpa %>% select(-row_id)
df_scaled <- as.data.frame(scale(df_to_scale))

# row_idを再度結合
df_analysis <- bind_cols(df_for_lpa %>% select(row_id), df_scaled)

cat(paste("分析用データ準備完了。対象者: ", nrow(df_analysis), "名\n", sep=""))


# ---------------------------------------------------------------
# 4. LPAの実行と適合度指標の表示
# ---------------------------------------------------------------

cat(paste("Step 3: ", CHOSEN_N_PROFILES, "クラスターモデルのLPAを実行します...\n", sep=""))

# LPA実行（row_idを分析から除外）
lpa_model <- estimate_profiles(df_analysis %>% select(-row_id), n_profiles = CHOSEN_N_PROFILES)

# 適合度指標の表示
fit_indices <- get_fit(lpa_model)
class_stats <- get_data(lpa_model) %>%
  count(Class) %>%
  mutate(Percentage = round(n / sum(n) * 100, 2)) %>%
  rename(N = n)

cat("\n--------------------------------------------------\n")
cat(paste("--- ", CHOSEN_N_PROFILES, "-Cluster Model: Fit Indices & Class Stats ---\n", sep=""))
cat("\n[Fit Indices]\n")
print(fit_indices %>% select(LogLik, AIC, BIC, SABIC, Entropy, BLRT_p))
cat("\n[Class Membership]\n")
print(class_stats)
cat("--------------------------------------------------\n\n")


# ---------------------------------------------------------------
# 5. 元データへのクラスター情報結合とCSV出力
# ---------------------------------------------------------------

cat("Step 4: 元のデータにクラスター情報を結合し、CSVファイルとして出力します...\n")

# クラスター割り当て結果を取得
lpa_results <- get_data(lpa_model)

# 分析に使用したデータ（df_for_lpa）にクラスター結果（Class）を結合
# これにより、元の行を特定するためのrow_idとクラスター番号が紐づく
results_with_id <- bind_cols(df_for_lpa %>% select(row_id), lpa_results %>% select(Class))

# 元のデータ（df_original）に、row_idをキーとしてクラスター情報を結合
df_final <- left_join(df_original, results_with_id, by = "row_id")

# クラスター番号でソート（NAは最後にまとめる）
# これにより、同じクラスターのメンバーが連続して並び、分析しやすくなります。
df_final_sorted <- df_final %>%
  arrange(Class)

# 出力ファイル名を生成（例: "dummy_data.csv" -> "dummy_data_with_clusters_sorted.csv"）
output_dir <- dirname(input_file)
output_filename <- sub("(\\.csv|)$", "_with_clusters_sorted.csv", basename(input_file), ignore.case = TRUE)
output_path <- file.path(output_dir, output_filename)

# ソートしたデータフレームをCSVファイルとして書き出し
write_csv(df_final_sorted, output_path)


cat(paste("クラスター情報を付与したファイルが正常に出力されました。\n -> ", output_path, "\n\n"))


# ---------------------------------------------------------------
# 6. クラスターごとの平均年齢の算出と表示
# ---------------------------------------------------------------

cat("Step 5: クラスターごとの平均年齢を算出します...\n")

# 年齢データが存在するかチェック
age_columns <- c("参加時年齢", "受信時年齢")
available_age_columns <- age_columns[age_columns %in% colnames(df_final)]

if (length(available_age_columns) > 0) {
  cat("\n--------------------------------------------------\n")
  cat("--- Cluster-wise Average Age Analysis ---\n\n")
  
  for (age_col in available_age_columns) {
    cat(paste("[", age_col, "の平均値]\n", sep=""))
    
    # クラスターごとの平均年齢を計算（NAは除外）
    age_stats <- df_final %>%
      filter(!is.na(Class) & !is.na(.data[[age_col]])) %>%
      group_by(Class) %>%
      summarise(
        N = n(),
        Mean_Age = round(mean(.data[[age_col]], na.rm = TRUE), 2),
        SD_Age = round(sd(.data[[age_col]], na.rm = TRUE), 2),
        Min_Age = min(.data[[age_col]], na.rm = TRUE),
        Max_Age = max(.data[[age_col]], na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(Class)
    
    # 結果を表示
    print(age_stats)
    cat("\n")
  }
  
  # 全体の平均年齢も表示
  for (age_col in available_age_columns) {
    overall_mean <- round(mean(df_final[[age_col]], na.rm = TRUE), 2)
    cat(paste("全体の", age_col, "平均: ", overall_mean, "歳\n", sep=""))
  }
  
  cat("--------------------------------------------------\n\n")
  
} else {
  cat("注意: 年齢データ（'参加時年齢'、'受信時年齢'）が見つかりませんでした。\n")
  cat("データに年齢列が含まれている場合は、列名を確認してください。\n\n")
}

cat("分析が完了しました。\n")
