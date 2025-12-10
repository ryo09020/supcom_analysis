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
#    (SCALE_CONFIG内の全ての検査票を順次処理します)

# 4. 各検査票の設定（コードとラベル）
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
    )
)

# 5. 共変量（データに含まれていることを確認する列）
COVARIATES <- c("age", "sex", "final_edu_int")

# 6. 出力設定
OUTPUT_DIR <- "plots" # 出力先のフォルダ名
# OUTPUT_FILE は自動生成されるため削除

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
        # ラベルを "Profile X" に変更
        levels(data[[class_col]]) <- paste0("Profile ", levels(data[[class_col]]))
    }

    return(data)
}

# 95%信頼区間の計算（共変量調整あり）
calc_adjusted_means <- function(data, item_col, class_col, covariates) {
    # モデル式の作成: item ~ class + cov1 + cov2 ...
    # 変数名が数字で始まる場合などに備えてバッククォートで囲む
    formula_str <- paste0("`", item_col, "` ~ ", class_col, " + ", paste(covariates, collapse = " + "))
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

    output_file <- paste0(scale_name, "_violin_plots.png")
    full_output_path <- file.path(output_dir, output_file)

    # 1. データ読み込み
    # 注意: ここで毎回ファイルを読み込むのは非効率ですが、
    # load_and_prep_data内で列の存在チェックを行っているため、
    # 安全性を優先してそのままにします。
    # (最適化するなら、mainで一度だけ読み込んで、必要な列チェックだけここで行うように変更できます)
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
        warning(sprintf("⚠️ Scale '%s' のプロット可能な項目がデータに存在しません。スキップします。\n", scale_name))
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
            tryCatch(
                {
                    means <- calc_adjusted_means(item_data, code, CLASS_COLUMN, COVARIATES)
                    means$item <- code
                    means$item_label <- target_items[[code]]
                    adj_means_list[[length(adj_means_list) + 1]] <- means
                },
                error = function(e) {
                    warning(sprintf("⚠️ 項目 '%s' の調整済み平均計算でエラーが発生しました: %s\n", code, e$message))
                }
            )
        }
    }

    if (length(adj_means_list) == 0) {
        warning(sprintf("⚠️ Scale '%s' の調整済み平均の計算に失敗しました（データ不足の可能性があります）。スキップします。\n", scale_name))
        return(NULL)
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
            plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 16),
            axis.text.x = element_text(angle = 45, hjust = 1),
            strip.text = element_text(size = 18, face = "bold")
        )

    # 4. 保存
    ggsave(full_output_path, p, width = 12, height = 8, dpi = 300)
    cat(sprintf("✅ プロットを保存しました: %s\n", normalizePath(full_output_path)))
}

# ==============================================================================
# メイン処理
# ==============================================================================

main <- function() {
    cat("=== Violin Plot Generation Started (Batch Mode) ===\n")

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
