# ------------------------------------------------------------------------------
# Mplus 潜在プロファイル分析（LPA）自動実行スクリプト
# ------------------------------------------------------------------------------
#
# ■ このスクリプトの目的
#   Mplusを使用して、指定された範囲のクラスター数（例: 2〜15）のLPAを
#   全自動で実行し、結果ファイル（.inp, .out, .dat）を生成します。
#
# ■ 使い方
#   1. 下の「Step 1: あなたの環境に合わせた設定」の3項目を、ご自身の環境に合わせて
#      書き換えます。
#   2. このRスクリプトと、分析に使うデータファイル（ヘッダー無し）を
#      PC上の同じフォルダに置きます。
#   3. このスクリプトを実行します。
#
# ■ 実行後に何が起こるか
#   - 「tipi-j」という名前のフォルダが新しく作成されます。
#   - Mplusが裏で何度も自動実行され、全ての関連ファイルが「tipi-j」フォルダ内に
#     整理されて保存されます。
#
# ------------------------------------------------------------------------------

### Step 1: パッケージの読み込みと、あなたの環境に合わせた設定 ###

# MplusAutomationパッケージを読み込みます
library(MplusAutomation)

# --- あなたが設定する項目 (ここから) ---

# 1. あなたのPCでのMplus実行ファイルの「絶対パス」を指定してください
mplus_executable_path <- "/Applications/mplusdemo/mpdemo"

# 2. あなたのデータファイル名（ヘッダー無し）を指定してください
#    このRスクリプトと、このデータファイルは同じフォルダに置いてください
data_filename <- "standardized_data_for_mplus.csv"

# 3. 分析したいクラスター数の範囲を指定してください
class_range <- 2:5

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
        z_tipi_n z_tipi_e z_tipi_o z_tipi_a z_tipi_c;
      USEVARIABLES ARE
        z_tipi_n z_tipi_e z_tipi_o z_tipi_a z_tipi_c;

      ! ここでクラス数を動的に指定します
      CLASSES = c(', k, '); 
      
      ! 欠損値がある場合は、この下の行のコメント「!」を解除し、
      ! 実際の欠損値の記号に合わせてください (例: -999)
      ! MISSING ARE ALL (-999);

    ANALYSIS:
      TYPE = MIXTURE;
      ESTIMATOR = MLR;
      STARTS = 200 50;
      LRTSTARTS = 200 50 200 50; ! BLRTの計算を安定させるためのおまじない

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

message("\\n全てのMplusの分析が完了しました。")


