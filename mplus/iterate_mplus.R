# -------------------------------------------------------------------------
# Mplus 潜在プロファイル分析（LPA）自動化スクリプト（最終完成版）
# -------------------------------------------------------------------------

### Step 1: パッケージの読み込みと、あなたの環境に合わせた設定 ###

# MplusAutomationパッケージを読み込みます
library(MplusAutomation)

# --- あなたが設定する項目 (ここから) ---

# 1. あなたのPCでのMplus実行ファイルの「絶対パス」を指定してください
mplus_executable_path <- "/Applications/mplusdemo/mpdemo"

# 2. あなたのデータファイル名（ヘッダー無し）を指定してください
#    このRスクリプトと、このデータファイルは同じフォルダに置いてください
data_filename <- "physiological_test_1st_n12151.csv"

# 3. 分析したいクラスター数の範囲を指定してください
class_range <- 2:3

# --- あなたが設定する項目 (ここまで) ---


### Step 2: 分析の準備 ###

# 結果を保存するためのディレクトリ「tipi-j」を作成します
dir.create("tipi-j", showWarnings = FALSE)

# Mplusの90文字制限エラーを回避するため、データファイルを分析用ディレクトリにコピーします
file.copy(from = data_filename, to = "tipi-j", overwrite = TRUE)

# 後で元の場所に戻るために、現在の作業ディレクトリを記憶しておきます
original_wd <- getwd()


### Step 3: ループ処理で、.inpファイルの作成とMplusの実行を繰り返す ###

# 指定された範囲のクラスター数で、順番に処理を実行します
for (k in class_range) {
  
  # ---- ループの中で、毎回.inpファイルの中身を文字列として作成します ----
  mplus_syntax_string <- paste0(
    'TITLE:
      LPA for Class ', k, ';

    DATA:
      ! Mplusと同じフォルダにデータがあるので、ファイル名だけで指定します
      FILE IS "', data_filename, '";

    VARIABLE:
      NAMES ARE
        ID gender birthdate age_entry age_scan scan_date course_code
        tipi_n tipi_e tipi_o tipi_a tipi_c;
      USEVARIABLES ARE
        tipi_n tipi_e tipi_o tipi_a tipi_c;
      
      ! ここでクラス数を動的に指定します
      CLASSES = c(', k, '); 

      ! 欠損値がある場合は、この下の行のコメント「!」を解除し、
      ! 実際の欠損値の記号に合わせてください (例: -999)
      ! MISSING ARE ALL (-999);

    ANALYSIS:
      TYPE = MIXTURE;
      ESTIMATOR = MLR;
      STARTS = 200 50; ! 計算の安定性を高めるためのおまじない

    OUTPUT:
      TECH11 TECH14;

    SAVEDATA:
      SAVE = CPROBABILITIES;
      FILE IS tipi_profiles_c', k, '.dat;

    PLOT:
      TYPE = PLOT3;
    '
  )
  # --------------------------------------------------------------------
  
  # 作成した文字列を、.inpファイルとして「tipi-j」フォルダ内に書き出します
  inp_filename <- paste0("tipi-j/", k, ".inp")
  writeLines(mplus_syntax_string, con = inp_filename)
  
  # 作業場所を一時的に'tipi-j'ディレクトリに移動します
  setwd("tipi-j")
  
  # Mplusを実行します。この時、.inpファイルは「今いる場所」にあるのでファイル名だけで指定します
  message("Running Mplus for: ", k, ".inp")
  system2(mplus_executable_path, args = paste0(k, ".inp"))
  
  # 重要：作業場所を元の場所に戻します
  setwd(original_wd)
}


### Step 4: 全ての結果(.out)を読み込み、サマリー表を作成・保存 ###

# "tipi-j"ディレクトリにある全ての.outファイルを読み込みます
all_results <- readModels("tipi-j", quiet = TRUE)

# 適合度指標をまとめた基本の表を作成します
summary_table <- LatexSummaryTable(
  all_results,
  keepCols = c("Title", "Parameters", "LL", "AIC", "BIC", "aBIC", "Entropy", "T11_LMR_PValue", "BLRT_PValue"),
  sortBy = "Title"
)

# 各クラスの割合（%）を抽出して、表の新しい列として追加します
class_proportions <- sapply(all_results, function(x) {
  # モデルが正常に終了した場合のみ割合を取得
  if (!is.null(x$results$class_counts$mostLikely)) {
    proportions <- x$results$class_counts$mostLikely$proportion
    paste(round(proportions * 100, 1), collapse = "/")
  } else {
    # エラーで終了した場合はNA（空欄）にする
    NA
  }
})
summary_table$"% in each class" <- class_proportions

# 表の列名を最終的に整えます
colnames(summary_table)[colnames(summary_table) == "LL"] <- "Log Likelihood"
colnames(summary_table)[colnames(summary_table) == "aBIC"] <- "Adjusted BIC"
colnames(summary_table)[colnames(summary_table) == "T11_LMR_PValue"] <- "VLMR-LRT P-value"
colnames(summary_table)[colnames(summary_table) == "BLRT_PValue"] <- "B-LRT P-value"
summary_table$Title <- gsub("LPA for Class |;", "", summary_table$Title)
colnames(summary_table)[colnames(summary_table) == "Title"] <- "Profiles"


### Step 5: 結果の表示とファイルへの保存 ###

# RStudioのコンソールに最終的なサマリー表を表示します
print(summary_table)

# サマリー表をCSVファイルとして保存します（Excelで開けます）
write.csv(summary_table, "tipi-j/lpa_summary_table_final.csv", row.names = FALSE)

message("\n全ての処理が完了しました。")
message("'tipi-j'フォルダ内に全ての関連ファイルと、結果をまとめた'lpa_summary_table_final.csv'が作成されました。")
