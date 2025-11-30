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
    library(patchwork) # 複数プロットの結合用
    library(emmeans) # 推定周辺平均（共変量調整）用
})

# グローバル変数の警告抑制
utils::globalVariables(c("value", "class_factor", "emmean", "lower.CL", "upper.CL"))

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
    )
)

# 5. 共変量（データに含まれていることを確認する列）
COVARIATES <- c("age", "sex", "finaledu_int")

# 6. 出力ファイル名（自動生成されますが、手動で指定も可能）
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

# ダミーデータの生成（テスト用）
generate_dummy_data <- function(file_path, class_col, items, covariates, n_rows = 100) {
    cat("⚠️ 入力ファイルが見つからないため、ダミーデータを生成します...\n")

    set.seed(123)
    classes <- sample(1:3, n_rows, replace = TRUE)

    df <- data.frame(row_id = 1:n_rows)
    df[[class_col]] <- classes

    # 共変量の生成
    df$age <- sample(20:60, n_rows, replace = TRUE)
    df$sex <- sample(0:1, n_rows, replace = TRUE)
    df$finaledu_int <- sample(c(12, 16), n_rows, replace = TRUE)

    # 項目の生成
    for (code in names(items)) {
        # クラスごとに少し分布を変える
        base_mean <- ifelse(classes == 1, 10, ifelse(classes == 2, 15, 20))
        df[[code]] <- rnorm(n_rows, mean = base_mean, sd = 5)
        # 負の値を0にするなどの調整
        df[[code]] <- pmax(0, round(df[[code]]))
    }

    # ディレクトリ作成
    dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)
    write_csv(df, file_path)
    cat(sprintf("✅ ダミーデータを保存しました: %s\n", file_path))
    return(df)
}

# データの読み込みと前処理
load_and_prep_data <- function(file_path, class_col, items, covariates) {
    if (!file.exists(file_path)) {
        # ファイルがない場合はダミーデータを生成して使用
        return(generate_dummy_data(file_path, class_col, items, covariates))
    }

    cat(sprintf("📁 データを読み込んでいます: %s\n", file_path))
    data <- read_csv(file_path, show_col_types = FALSE)

    # 必須列の確認
    required_cols <- c(class_col, names(items), covariates)
    missing_cols <- setdiff(required_cols, names(data))

    if (length(missing_cols) > 0) {
        # ダミーデータ生成を試みる（既存ファイルに列が足りない場合など）
        # ここでは警告を出して、もし項目が足りなければダミーデータを作るか、
        # あるいは単に警告だけで進むか。今回は警告のみとする。
        warning(sprintf("⚠️ 以下の列がデータに見つかりません: %s", paste(missing_cols, collapse = ", ")))

        # もし分析対象の項目が一つもない場合は、テスト用にダミー列を追加するなどの処置も考えられるが、
        # ユーザーがコードを書き換える前提なので、ここではそのままにする。
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

# 個別のバイオリンプロット作成
create_violin_plot <- function(data, item_col, item_label, class_col, covariates) {
    # 数値型に変換（念のため）
    plot_data <- data %>%
        mutate(
            value = suppressWarnings(as.numeric(.data[[item_col]])),
            class_factor = as.factor(.data[[class_col]]) # 明示的にファクター化
        ) %>%
        filter(!is.na(value), !is.na(class_factor))

    # 共変量の欠損も除外
    for (cov in covariates) {
        plot_data <- plot_data %>% filter(!is.na(.data[[cov]]))
    }

    # 調整済み平均値の計算
    adj_means <- calc_adjusted_means(plot_data, "value", "class_factor", covariates)

    p <- ggplot(plot_data, aes(x = class_factor, y = value, fill = class_factor)) +
        # バイオリンプロット（生の分布）
        geom_violin(trim = FALSE, alpha = 0.5, color = NA) +

        # 調整済み平均値と95%信頼区間（エラーバー）
        # data引数で調整済みデータフレームを指定
        geom_pointrange(
            data = adj_means,
            aes(y = y, ymin = ymin, ymax = ymax),
            color = "black", size = 0.8, shape = 18
        ) +

        # デザイン調整
        scale_fill_brewer(palette = "Set2") +
        labs(
            title = NULL,
            x = "Class",
            y = item_label
        ) +
        theme_minimal() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold", hjust = 0.5),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 14)
        )

    return(p)
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
    output_file <- OUTPUT_FILE
    if (is.null(output_file)) {
        output_file <- paste0(scale_name, "_violin_plots.png")
    }

    # 1. データ読み込み
    df <- load_and_prep_data(INPUT_FILE, CLASS_COLUMN, target_items, COVARIATES)

    # 2. 各項目のプロット作成
    plot_list <- list()

    for (code in names(target_items)) {
        label <- target_items[[code]]
        cat(sprintf("📊 プロット作成中: %s (%s)\n", label, code))

        if (code %in% names(df)) {
            p <- create_violin_plot(df, code, label, CLASS_COLUMN, COVARIATES)
            plot_list[[length(plot_list) + 1]] <- p
        } else {
            cat(sprintf("   ⚠️ 列 '%s' が存在しないためスキップします。\n", code))
        }
    }

    if (length(plot_list) == 0) {
        stop("❌ プロット可能な項目がありませんでした。")
    }

    # 3. プロットの結合（patchworkを使用）
    combined_plot <- wrap_plots(plot_list) +
        plot_annotation(
            title = paste(scale_name, "Scores by Class"),
            theme = theme(
                plot.title = element_text(size = 16, face = "bold", hjust = 0.5)
            )
        )

    # 4. 保存
    ggsave(output_file, combined_plot, width = 12, height = 6, dpi = 300)
    cat(sprintf("\n✅ プロットを保存しました: %s\n", normalizePath(output_file)))
    cat("=== Done ===\n")
}

# スクリプト実行
if (sys.nframe() == 0) {
    main()
}
