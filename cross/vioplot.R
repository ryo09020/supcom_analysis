#!/usr/bin/env Rscript

# ==============================================================================
# Violin Plot Generator with 95% CI
# ==============================================================================
# 目的: 指定された下位項目について、クラスごとのバイオリンプロットを作成し、
#       95%信頼区間（平均値 ± 1.96 * SE）を描画する。
#       複数の下位項目を1枚の画像にまとめて出力する。
# ==============================================================================

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(ggplot2)
    library(tidyr)
    library(emmeans) # 推定周辺平均（共変量調整）用
})

# グローバル変数の警告抑制
utils::globalVariables(c("value", "class_factor", "emmean", "lower.CL", "upper.CL", "item", "item_label"))

# ==============================================================================
# 【ユーザー設定エリア】
# ==============================================================================

# 1. 入力ファイルパス
INPUT_FILE <- "raw_data/dummy_data_with_clusters_sorted.csv"

# 2. クラス列の名前
CLASS_COLUMN <- "Class"

# 3. 使用する検査票の選択
#    以下のリストから選択してください: "DASS-15", "GHQ-30", "TAC-24", "IES-R", "POMS"
SELECTED_SCALE <- "DASS-15"

# 4. 各検査票の設定（コードとラベル）
SCALE_CONFIG <- list(
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
            "543010_00" = "Somatic Symptoms",
            "543020_00" = "Sleep Disturbance",
            "543030_00" = "Social Dysfunction",
            "543040_00" = "Anxiety/Depression",
            "543050_00" = "Severe Depression"
        )
    ),
    "TAC-24" = list(
        name = "TAC-24",
        items = list(
            "542820_00" = "Prob. Solv. & Support",
            "542830_00" = "Avoidance",
            "542840_00" = "Pos. Reappraisal & Distraction"
        )
    ),
    "IES-R" = list(
        name = "IES-R",
        items = list(
            "542850_00" = "Intrusion",
            "542860_00" = "Avoidance",
            "542870_00" = "Hyperarousal"
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
            "542950_00" = "Confusion"
        )
    ),
    "STSS" = list(
        name = "STSS",
        items = list(
            "543120_00" = "Diff. Ident. Bodily Sensations",
            "543130_00" = "Over-adaptation",
            "543140_00" = "Lack of Health Mgmt",
            "543150_00" = "Total Score"
        )
    )
)

# 5. 共変量（データに含まれていることを確認する列）
COVARIATES <- c("age", "sex", "finaledu_int")

# 6. 出力設定
OUTPUT_DIR <- "plots" # 出力先のフォルダ名
OUTPUT_FILE <- NULL # NULLの場合、自動生成: "ScaleName_violin_plots.png"

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
load_and_prep_data <- function(file_path, class_col, items, covariates) {
    if (!file.exists(file_path)) {
        stop(sprintf("❌ 入力ファイルが見つかりません: %s", file_path))
    }

    cat(sprintf("📁 データを読み込んでいます: %s\n", file_path))
    data <- read_csv(file_path, show_col_types = FALSE)

    # 必須列の確認
    required_cols <- c(class_col, names(items), covariates)
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
        warning(sprintf("⚠️ 以下の列がデータに見つかりません: %s", paste(missing_cols, collapse = ", ")))
    }

    # クラス列をファクター化
    if (class_col %in% names(data)) {
        data[[class_col]] <- as.factor(data[[class_col]])
    }

    return(data)
}

# 95%信頼区間の計算（共変量調整あり）
calc_adjusted_means <- function(data, item_col, class_col, covariates) {
    # モデル式の作成: item ~ class + cov1 + cov2 ...
    formula_str <- paste(item_col, "~", class_col, "+", paste(covariates, collapse = " + "))
    model <- lm(as.formula(formula_str), data = data)

    # 推定周辺平均の計算
    emm <- emmeans(model, specs = class_col)
    emm_df <- as.data.frame(emm)

    # 列名を統一（emmeansの出力は class_col, emmean, SE, df, lower.CL, upper.CL）
    # プロット用にリネーム
    emm_df <- emm_df %>%
        rename(
            class_factor = all_of(class_col),
            y = emmean,
            ymin = lower.CL,
            ymax = upper.CL
        )
    return(emm_df)
}

# ==============================================================================
# メイン処理
# ==============================================================================

main <- function() {
    cat("=== Violin Plot Generation Started ===\n")

    # 設定の読み込み
    config <- get_scale_config(SELECTED_SCALE)
    target_items <- config$items
    scale_name <- config$name

    cat(sprintf("📌 Selected Scale: %s\n", scale_name))

    # 出力ファイル名の決定
    output_dir <- OUTPUT_DIR
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    output_file <- OUTPUT_FILE
    if (is.null(output_file)) {
        output_file <- paste0(scale_name, "_violin_plots.png")
    }

    full_output_path <- file.path(output_dir, output_file)

    # 1. データ読み込み
    df <- load_and_prep_data(INPUT_FILE, CLASS_COLUMN, target_items, COVARIATES)

    # 2. データ整形と調整済み平均の計算
    cat("📊 データを整形し、調整済み平均を計算しています...\n")

    # ロング形式に変換（プロット用）
    # 必要な列だけ抽出
    cols_to_keep <- c(CLASS_COLUMN, COVARIATES, names(target_items))
    # 存在する列のみ
    cols_to_keep <- intersect(cols_to_keep, names(df))

    df_subset <- df %>% select(all_of(cols_to_keep))

    # ターゲット項目をロング形式に
    # key: item code, value: score
    # pivot_longerを使うために、項目コードのみをcolsに指定
    available_items <- intersect(names(target_items), names(df_subset))

    if (length(available_items) == 0) {
        stop("❌ プロット可能な項目がデータに存在しません。")
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

    # 共変量の欠損除外
    for (cov in COVARIATES) {
        if (cov %in% names(long_df)) {
            long_df <- long_df %>% filter(!is.na(.data[[cov]]))
        }
    }

    # 調整済み平均値を各項目ごとに計算して結合
    adj_means_list <- list()

    for (code in available_items) {
        # その項目のデータだけ抽出
        item_data <- df_subset %>%
            filter(!is.na(.data[[code]]))

        # 共変量欠損除外
        for (cov in COVARIATES) {
            if (cov %in% names(item_data)) {
                item_data <- item_data %>% filter(!is.na(.data[[cov]]))
            }
        }

        if (nrow(item_data) > 0) {
            # 計算
            means <- calc_adjusted_means(item_data, code, CLASS_COLUMN, COVARIATES)
            means$item <- code
            means$item_label <- target_items[[code]]
            adj_means_list[[length(adj_means_list) + 1]] <- means
        }
    }

    if (length(adj_means_list) == 0) {
        stop("❌ 調整済み平均の計算に失敗しました（データ不足の可能性があります）。")
    }

    adj_means_df <- bind_rows(adj_means_list) %>%
        mutate(
            item_label = factor(item, levels = names(target_items), labels = unlist(target_items))
        )

    # 3. プロット作成（facet_wrap使用）
    cat("📈 プロットを作成しています...\n")

    p <- ggplot(long_df, aes(x = class_factor, y = value, fill = class_factor)) +
        # バイオリンプロット
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +

        # 調整済み平均値と95%信頼区間
        geom_pointrange(
            data = adj_means_df,
            aes(y = y, ymin = ymin, ymax = ymax),
            color = "black", size = 0.8, shape = 18
        ) +

        # ファセット（項目ごとに分割）
        facet_wrap(~item_label, scales = "free_y") +

        # デザイン調整
        scale_fill_brewer(palette = "Set2") +
        labs(
            title = paste(scale_name, "Scores by Class"),
            x = "Class",
            y = "Score"
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            strip.text = element_text(size = 12, face = "bold")
        )

    # 4. 保存
    ggsave(full_output_path, p, width = 12, height = 8, dpi = 300)
    cat(sprintf("\n✅ プロットを保存しました: %s\n", normalizePath(full_output_path)))
    cat("=== Done ===\n")
}

# スクリプト実行
if (sys.nframe() == 0) {
    main()
}
