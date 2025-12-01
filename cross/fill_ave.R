#!/usr/bin/env Rscript

# ==============================================================================
# Missing Value Imputation (Mean Filling)
# ==============================================================================
# 目的: 指定されたカラムの欠損値（NA）を、そのカラムの平均値で埋める。
# ==============================================================================

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
})

# ==============================================================================
# 【ユーザー設定エリア】
# ==============================================================================

# 1. デフォルトの入力ファイルパス（コマンドライン引数がない場合に使用）
DEFAULT_INPUT_FILE <- "raw_data/dummy_data.csv"

# 2. 欠損値を平均値で埋める対象のカラム名リスト
#    ここに処理したいカラム名を列挙してください。
TARGET_COLUMNS <- c(
    "age",
    "finaledu_int"
    # 必要に応じて追加: "column_name1", "column_name2"
)

# 3. 出力ファイル名のサフィックス
OUTPUT_SUFFIX <- "_filled"

# ==============================================================================
# 関数定義
# ==============================================================================

fill_na_with_mean <- function(data, target_cols) {
    processed_data <- data

    for (col in target_cols) {
        if (col %in% names(processed_data)) {
            # 数値型か確認
            if (is.numeric(processed_data[[col]])) {
                # 平均値の計算（NAを除外）
                col_mean <- mean(processed_data[[col]], na.rm = TRUE)

                # NAの数をカウント
                na_count <- sum(is.na(processed_data[[col]]))

                if (na_count > 0) {
                    # NAを平均値で置換
                    processed_data[[col]][is.na(processed_data[[col]])] <- col_mean
                    cat(sprintf("✅ %s: %d 個のNAを平均値 (%.2f) で埋めました。\n", col, na_count, col_mean))
                } else {
                    cat(sprintf("ℹ️ %s: NAはありませんでした。\n", col))
                }
            } else {
                warning(sprintf("⚠️ %s は数値型ではないためスキップしました。\n", col))
            }
        } else {
            warning(sprintf("⚠️ %s はデータに存在しません。\n", col))
        }
    }

    return(processed_data)
}

process_file <- function(input_path) {
    if (!file.exists(input_path)) {
        stop(sprintf("❌ 入力ファイルが見つかりません: %s", input_path))
    }

    cat(sprintf("📁 データを読み込んでいます: %s\n", input_path))
    data <- read_csv(input_path, show_col_types = FALSE)

    # 処理実行
    cat("🔄 欠損値処理を開始します...\n")
    filled_data <- fill_na_with_mean(data, TARGET_COLUMNS)

    # 出力パス作成
    dir_name <- dirname(input_path)
    base_name <- tools::file_path_sans_ext(basename(input_path))
    output_path <- file.path(dir_name, paste0(base_name, OUTPUT_SUFFIX, ".csv"))

    # 保存
    write_csv(filled_data, output_path)
    cat(sprintf("\n✅ 処理完了。ファイルを保存しました: %s\n", normalizePath(output_path)))
}

# ==============================================================================
# メイン処理
# ==============================================================================

main <- function() {
    # コマンドライン引数の取得
    args <- commandArgs(trailingOnly = TRUE)

    input_file <- if (length(args) >= 1) args[[1]] else DEFAULT_INPUT_FILE

    tryCatch(
        {
            process_file(input_file)
        },
        error = function(e) {
            cat(sprintf("\n❌ エラーが発生しました: %s\n", e$message))
            quit(status = 1)
        }
    )
}

if (sys.nframe() == 0) {
    main()
}
