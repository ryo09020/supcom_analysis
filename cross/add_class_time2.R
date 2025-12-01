#!/usr/bin/env Rscript

# ==============================================================================
# Add Class Column to Time2 Data
# ==============================================================================
# 目的: Time1データとTime2データの両方に存在するIDのみを抽出し、
#       Time1のClass列をTime2のデータかつ(time1,2で両方にIDが存在するデータ)に付加して新しいCSVを作成する。
# ==============================================================================

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
})

# ==============================================================================
# 【ユーザー設定エリア】
# ==============================================================================

# 1. 入力ファイルパス (Time1: Class情報があるファイル)
TIME1_FILE <- "raw_data/time1_data.csv"

# 2. 入力ファイルパス (Time2: Class情報を付加したいファイル)
TIME2_FILE <- "raw_data/time2_data.csv"

# 3. 出力ファイルパス
OUTPUT_FILE <- "raw_data/time2_data_with_class.csv"

# 4. ID列の名前 (両方のファイルで共通の名前であること)
ID_COLUMN <- "ID"

# 5. Class列の名前 (Time1ファイルにある列名)
CLASS_COLUMN <- "Class"

# ==============================================================================
# メイン処理
# ==============================================================================

main <- function() {
    cat("=== Add Class Column Script Started ===\n")

    # 1. ファイルの存在確認
    if (!file.exists(TIME1_FILE)) {
        stop(sprintf("❌ Time1ファイルが見つかりません: %s", TIME1_FILE))
    }
    if (!file.exists(TIME2_FILE)) {
        stop(sprintf("❌ Time2ファイルが見つかりません: %s", TIME2_FILE))
    }

    # 2. データの読み込み
    cat(sprintf("📁 Time1データを読み込んでいます: %s\n", TIME1_FILE))
    df1 <- read_csv(TIME1_FILE, show_col_types = FALSE)

    cat(sprintf("📁 Time2データを読み込んでいます: %s\n", TIME2_FILE))
    df2 <- read_csv(TIME2_FILE, show_col_types = FALSE)

    # 3. 列の存在確認
    if (!ID_COLUMN %in% names(df1)) {
        stop(sprintf("❌ Time1データにID列 '%s' が見つかりません。", ID_COLUMN))
    }
    if (!CLASS_COLUMN %in% names(df1)) {
        stop(sprintf("❌ Time1データにClass列 '%s' が見つかりません。", CLASS_COLUMN))
    }
    if (!ID_COLUMN %in% names(df2)) {
        stop(sprintf("❌ Time2データにID列 '%s' が見つかりません。", ID_COLUMN))
    }

    # 4. データの結合
    cat("🔄 データを処理しています...\n")

    # 手順:
    # 1. Time1とTime2の両方に存在する共通のIDを見つける (inner_joinで実現)
    # 2. その共通IDを持つレコードをTime2から抽出する
    # 3. 抽出したTime2のレコードに、Time1のClass情報を付加する

    # Time1からIDとClassだけを抽出（付加する情報）
    class_info <- df1 %>%
        select(all_of(c(ID_COLUMN, CLASS_COLUMN)))

    # Time2をベースに、共通IDを持つ行だけを残し、Classを付加
    df2_with_class <- df2 %>%
        inner_join(class_info, by = ID_COLUMN)

    # 5. 結果の確認
    n_time2 <- nrow(df2)
    n_matched <- nrow(df2_with_class)

    cat(sprintf("   Time2の元レコード数: %d\n", n_time2))
    cat(sprintf("   抽出された共通レコード数: %d (%.1f%%)\n", n_matched, (n_matched / n_time2) * 100))
    cat(sprintf("   除外されたレコード数: %d\n", n_time2 - n_matched))

    # 6. 保存
    # 出力ディレクトリの作成
    output_dir <- dirname(OUTPUT_FILE)
    if (!dir.exists(output_dir) && output_dir != ".") {
        dir.create(output_dir, recursive = TRUE)
    }

    write_csv(df2_with_class, OUTPUT_FILE)
    cat(sprintf("✅ ファイルを保存しました: %s\n", normalizePath(OUTPUT_FILE)))

    cat("=== Done ===\n")
}

# スクリプト実行
main()
