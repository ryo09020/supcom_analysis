#!/usr/bin/env Rscript

# ==============================================================================
# Simple Violin Plot Generator (Raw Data)
# ==============================================================================
# 目的: 指定された下位項目について、クラスごとのバイオリンプロットを作成する。
#       共変量調整（ANCOVA）は行わず、生のデータ分布を表示する。
#       データ不足でANCOVAが実行できない場合などに使用。
# ==============================================================================

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(ggplot2)
    library(tidyr)
})

# グローバル変数の警告抑制
utils::globalVariables(c("value", "class_factor", "item", "item_label", "Mean", "SD", "n"))

# ==============================================================================
# 【ユーザー設定エリア】
# ==============================================================================

# 1. 入力ファイルパス
INPUT_FILE <- "raw_data/dummy_data_with_clusters_sorted.csv"

# 2. クラス列の名前
CLASS_COLUMN <- "Class"

# 3. 各検査票の設定（コードとラベル）
#    vioplot.Rと同じ設定を使用
SCALE_CONFIG <- list(
    "NEO-FFI" = list(
        name = "NEO-FFI",
        items = list(
            "542640_00" = "Neuroticism",
            "542650_00" = "Extraversion",
            "542660_00" = "Openness",
            "542670_00" = "Agreeableness",
            "542680_00" = "Conscientiousness"
        )
    ),
    "TAC-24" = list(
        name = "TAC-24",
        items = list(
            "542740_00" = "Catharsis",
            "542750_00" = "Giving Up",
            "542760_00" = "Info Seeking",
            "542770_00" = "Distraction",
            "542780_00" = "Avoidant Thinking",
            "542790_00" = "Pos. Reappraisal",
            "542800_00" = "Planning",
            "542810_00" = "Resp. Transfer",
            "542820_00" = "Support Seeking",
            "542830_00" = "Avoidance",
            "542840_00" = "Reappraisal & Distraction"
        )
    ),
    "IES-R" = list(
        name = "IES-R",
        items = list(
            "542850_00" = "Intrusion",
            "542860_00" = "Avoidance",
            "542870_00" = "Hyperarousal",
            "542880_00" = "Total"
        )
    ),
    "J-PSS" = list(
        name = "J-PSS",
        items = list(
            "542890_00" = "Total"
        )
    ),
    "POMS" = list(
        name = "POMS",
        items = list(
            "542900_00" = "Tension-Anxiety",
            "542910_00" = "Depression",
            "542920_00" = "Anger-Hostility",
            "542930_00" = "Vigor",
            "542940_00" = "Fatigue",
            "542950_00" = "Confusion",
            "542960_00" = "TMD"
        )
    ),
    "DASS-15" = list(
        name = "DASS-15",
        items = list(
            "542970_00" = "Depression",
            "542980_00" = "Anxiety",
            "542990_00" = "Stress"
        )
    ),
    "GHQ-30" = list(
        name = "GHQ-30",
        items = list(
            # "543000_00" = "General Illness",
            "543010_00" = "Somatic Symptoms",
            "543020_00" = "Sleep Disturbance",
            "543030_00" = "Social Dysfunction",
            "543040_00" = "Anxiety/Dysphoria",
            "543050_00" = "Severe Depression"
            # "543060_00" = "Total"
        )
    ),
    "SES" = list(
        name = "SES",
        items = list(
            "543070_00" = "Total"
        )
    ),
    "TAS-20" = list(
        name = "TAS-20",
        items = list(
            "543080_00" = "DIF",
            "543090_00" = "DDF",
            "543100_00" = "EOT",
            "543110_00" = "Total"
        )
    ),
    "STSS" = list(
        name = "STSS",
        items = list(
            "543120_00" = "Bodily Sensations",
            "543130_00" = "Over-adaptation",
            "543140_00" = "Poor Health Mgmt",
            "543150_00" = "Total"
        )
    ),
    "Edinburgh" = list(
        name = "Edinburgh",
        items = list(
            "543160_00" = "Total"
        )
    ),
    "MMSE" = list(
        name = "MMSE",
        items = list(
            "516484_00" = "Total"
        )
    ),
    "Verval Fluency" = list(
        name = "Verval Fluency",
        items = list(
            "520000_00" = "Letter fluency task",
            "520010_00" = "Category fluency task"
        )
    ),
    "Digit Symbol Test" = list(
        name = "Digit Symbol Test",
        items = list(
            "520040_00" = "Total"
        )
    ),
    "JART" = list(
        name = "JART",
        items = list(
            "520120_00" = "Error Count",
            "520130_00" = "FSIQ",
            "520140_00" = "VIQ",
            "520150_00" = "PIQ"
        )
    )
)

# 4. 出力設定
OUTPUT_DIR <- "plots_simple" # 出力先のフォルダ名

# ==============================================================================
# 関数定義
# ==============================================================================

# 設定の取得
get_scale_config <- function(scale_name) {
    if (!scale_name %in% names(SCALE_CONFIG)) {
        stop(sprintf("❌ 指定された検査票 '%s' は設定にありません。", scale_name))
    }
    return(SCALE_CONFIG[[scale_name]])
}

# データの読み込みと前処理
load_and_prep_data <- function(file_path, class_col, items) {
    if (!file.exists(file_path)) {
        stop(sprintf("❌ 入力ファイルが見つかりません: %s", file_path))
    }

    cat(sprintf("📁 データを読み込んでいます: %s\n", file_path))
    data <- read_csv(file_path, show_col_types = FALSE)

    # 必須列の確認
    required_cols <- c(class_col, names(items))
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
        # データがない場合でも、少なくともクラス列があれば処理を続行できる可能性があるが、
        # ここでは警告を出して、後続の処理でチェックする
        # warning(sprintf("⚠️ 以下の列がデータに見つかりません: %s", paste(missing_cols, collapse = ", ")))
    }

    # クラス列をファクター化
    if (class_col %in% names(data)) {
        data[[class_col]] <- as.factor(data[[class_col]])
        # ラベルを "Profile X" に変更
        levels(data[[class_col]]) <- paste0("Profile ", levels(data[[class_col]]))
    }

    return(data)
}

# 検査票ごとの処理関数
process_scale <- function(scale_name) {
    cat(sprintf("\n=== Processing Scale: %s ===\n", scale_name))

    # 設定の読み込み
    config <- get_scale_config(scale_name)
    target_items <- config$items

    # 出力ファイル名の決定
    output_dir <- OUTPUT_DIR
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    output_file <- paste0(scale_name, "_simple_violin_plots.png")
    full_output_path <- file.path(output_dir, output_file)

    # 1. データ読み込み
    df <- load_and_prep_data(INPUT_FILE, CLASS_COLUMN, target_items)

    # 2. データ整形
    cat("📊 データを整形しています...\n")

    # 必要な列だけ抽出
    cols_to_keep <- c(CLASS_COLUMN, names(target_items))
    # 存在する列のみ
    cols_to_keep <- intersect(cols_to_keep, names(df))

    df_subset <- df %>% select(all_of(cols_to_keep))

    # ターゲット項目を数値型に変換
    available_items <- intersect(names(target_items), names(df_subset))

    cat(sprintf("  Found %d / %d items for %s\n", length(available_items), length(target_items), scale_name))

    if (length(available_items) == 0) {
        cat(sprintf("⚠️ Scale '%s' のプロット可能な項目がデータに存在しません。スキップします。\n", scale_name))
        cat(sprintf("   Expected items: %s\n", paste(names(target_items), collapse = ", ")))
        return(NULL)
    }

    # ロング形式データ作成
    long_df <- df_subset %>%
        pivot_longer(
            cols = all_of(available_items),
            names_to = "item",
            values_to = "value"
        ) %>%
        mutate(
            class_factor = as.factor(.data[[CLASS_COLUMN]]),
            # 項目コードをラベルに変換
            item_label = factor(item, levels = names(target_items), labels = unlist(target_items))
        ) %>%
        filter(!is.na(value), !is.na(class_factor))

    # 数値変換（念のため）
    long_df$value <- suppressWarnings(as.numeric(long_df$value))
    long_df <- long_df %>% filter(!is.na(value))

    if (nrow(long_df) == 0) {
        cat(sprintf("⚠️ Scale '%s' の有効なデータ行がありません。スキップします。\n", scale_name))
        return(NULL)
    }

    # 3. 記述統計量の計算（平均、SD、N数）
    summary_df <- long_df %>%
        group_by(item, item_label, class_factor) %>%
        summarise(
            Mean = mean(value, na.rm = TRUE),
            SD = sd(value, na.rm = TRUE),
            n = n(),
            .groups = "drop"
        ) %>%
        mutate(
            ymin = Mean - SD, # エラーバー用（平均 ± 1SD）
            ymax = Mean + SD
        )

    # コンソールに出力
    print(summary_df %>% select(item_label, class_factor, n, Mean, SD))

    # 4. プロット作成（facet_wrap使用）
    cat("📈 プロットを作成しています...\n")

    p <- ggplot(long_df, aes(x = class_factor, y = value, fill = class_factor)) +
        # バイオリンプロット
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +

        # 平均値とSD（エラーバー）
        # 生データの要約統計量を表示
        geom_pointrange(
            data = summary_df,
            aes(y = Mean, ymin = ymin, ymax = ymax),
            color = "black", size = 0.8, shape = 18
        ) +



        # ファセット（項目ごとに分割）
        facet_wrap(~item_label, scales = "free_y") +

        # デザイン調整
        scale_fill_brewer(palette = "Set2") +
        labs(
            title = paste(scale_name, "Raw Scores by Class (No Adjustment)"),
            x = "Psychological profile",
            y = "Score"
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 18, face = "bold")
        )

    # 5. 保存
    ggsave(full_output_path, p, width = 12, height = 8, dpi = 300)
    cat(sprintf("✅ プロットを保存しました: %s\n", normalizePath(full_output_path)))
}

# ==============================================================================
# メイン処理
# ==============================================================================

main <- function() {
    cat("=== Simple Violin Plot Generation Started (Raw Data) ===\n")

    # 全てのスケールを処理
    for (scale_name in names(SCALE_CONFIG)) {
        tryCatch(
            {
                process_scale(scale_name)
            },
            error = function(e) {
                cat(sprintf("\n❌ Scale '%s' の処理中に予期せぬエラーが発生しました: %s\n", scale_name, e$message))
            }
        )
    }

    cat("\n=== All Done ===\n")
}

# スクリプト実行
main()
