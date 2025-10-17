#!/usr/bin/env Rscript

#################################################################
# GAM 可視化スクリプト
# 年齢などの説明変数と特性スコアの非線形な関係を可視化
#################################################################

# ================================================================
# 設定変数
# ================================================================

INPUT_FILE <- "sample_gam.csv"
X_COLUMN <- "age"
Y_COLUMN <- "extraversion"
OUTPUT_DIR <- "outputs"
OUTPUT_PREFIX <- NULL            # NULL の場合は入力ファイル名から自動生成

SMOOTH_K <- 10L
GAM_FAMILY <- gaussian()         # 文字列 ("gaussian") や family オブジェクトでも可
SCATTER_ALPHA <- 0.6
PLOT_WIDTH <- 7
PLOT_HEIGHT <- 5
PLOT_DPI <- 300
PREDICT_POINTS <- 200L
CI_MULTIPLIER <- 2
SAVE_SUMMARY <- TRUE

# ================================================================
# パッケージ読み込み
# ================================================================

suppressMessages({
  suppressWarnings({
    library(readr)
    library(dplyr)
    library(ggplot2)
    library(mgcv)
    library(rlang)
  })
})

# ================================================================
# ユーティリティ関数
# ================================================================

validate_settings <- function() {
  if (!is.character(INPUT_FILE) || length(INPUT_FILE) != 1 || INPUT_FILE == "") {
    stop("INPUT_FILE は非空文字列で指定してください。")
  }
  if (!is.character(X_COLUMN) || length(X_COLUMN) != 1 || X_COLUMN == "") {
    stop("X_COLUMN は非空文字列で指定してください。")
  }
  if (!is.character(Y_COLUMN) || length(Y_COLUMN) != 1 || Y_COLUMN == "") {
    stop("Y_COLUMN は非空文字列で指定してください。")
  }
  if (!is.numeric(SMOOTH_K) || length(SMOOTH_K) != 1 || SMOOTH_K < 3) {
    stop("SMOOTH_K は 3 以上の数値で指定してください。")
  }
  if (!is.numeric(PREDICT_POINTS) || length(PREDICT_POINTS) != 1 || PREDICT_POINTS < 10) {
    stop("PREDICT_POINTS は 10 以上の数値で指定してください。")
  }
  if (!is.numeric(CI_MULTIPLIER) || length(CI_MULTIPLIER) != 1 || CI_MULTIPLIER <= 0) {
    stop("CI_MULTIPLIER は 0 より大きい数値で指定してください。")
  }
}

resolve_family <- function() {
  if (inherits(GAM_FAMILY, "family")) {
    return(GAM_FAMILY)
  }
  if (is.function(GAM_FAMILY)) {
    fam <- GAM_FAMILY()
    if (!inherits(fam, "family")) {
      stop("GAM_FAMILY で指定した関数が family オブジェクトを返しません。")
    }
    return(fam)
  }
  if (is.character(GAM_FAMILY)) {
    fam_fun <- tryCatch(match.fun(GAM_FAMILY), error = function(e) NULL)
    if (is.null(fam_fun)) {
      stop("指定された family 関数が見つかりません: ", GAM_FAMILY)
    }
    fam <- fam_fun()
    if (!inherits(fam, "family")) {
      stop("GAM_FAMILY で指定した文字列が family オブジェクトを返しません: ", GAM_FAMILY)
    }
    return(fam)
  }
  stop("GAM_FAMILY は family オブジェクト、文字列、または family を返す関数で指定してください。")
}

resolve_output_prefix <- function() {
  if (is.null(OUTPUT_PREFIX) || OUTPUT_PREFIX == "") {
    tools::file_path_sans_ext(basename(INPUT_FILE))
  } else {
    OUTPUT_PREFIX
  }
}

ensure_output_dir <- function() {
  if (!dir.exists(OUTPUT_DIR)) {
    dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
  }
}

load_data <- function() {
  if (!file.exists(INPUT_FILE)) {
    stop("指定されたファイルが見つかりません: ", INPUT_FILE)
  }
  cat("データ読み込み中:", basename(INPUT_FILE), "\n")
  data <- readr::read_csv(INPUT_FILE, show_col_types = FALSE)
  cat("データサイズ:", nrow(data), "行", ncol(data), "列\n")
  data
}

prepare_data <- function(data) {
  missing_cols <- setdiff(c(X_COLUMN, Y_COLUMN), colnames(data))
  if (length(missing_cols) > 0) {
    stop("指定された列がデータ内に見つかりません: ", paste(missing_cols, collapse = ", "))
  }

  df <- data |>
    dplyr::select(dplyr::all_of(c(X_COLUMN, Y_COLUMN))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))

  df <- df[stats::complete.cases(df), , drop = FALSE]

  cat("分析対象:", nrow(df), "件\n")
  if (nrow(df) < 10) {
    warning("有効なレコードが 10 件未満です。GAM の推定結果が不安定になる可能性があります。")
  }

  df
}

fit_gam_model <- function(clean_data) {
  family_obj <- resolve_family()
  formula_text <- sprintf("%s ~ s(%s, k = %d)", Y_COLUMN, X_COLUMN, SMOOTH_K)
  cat("GAM 当てはめ中:", formula_text, "\n")
  mgcv::gam(
    formula = stats::as.formula(formula_text),
    data = clean_data,
    family = family_obj,
    method = "REML"
  )
}

create_scatter_plot <- function(clean_data) {
  ggplot2::ggplot(
    clean_data,
    ggplot2::aes(x = .data[[X_COLUMN]], y = .data[[Y_COLUMN]])
  ) +
    ggplot2::geom_point(alpha = SCATTER_ALPHA, color = "#3182bd") +
    ggplot2::geom_smooth(
      method = "gam",
      formula = stats::as.formula(paste("y ~ s(x, k =", SMOOTH_K, ")")),
      se = TRUE,
      color = "#08519c",
      fill = "#bdd7e7"
    ) +
    ggplot2::labs(
      title = sprintf("Scatter: %s vs %s", Y_COLUMN, X_COLUMN),
      x = X_COLUMN,
      y = Y_COLUMN
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

create_effect_plot <- function(gam_model, clean_data) {
  x_seq <- seq(
    from = min(clean_data[[X_COLUMN]], na.rm = TRUE),
    to = max(clean_data[[X_COLUMN]], na.rm = TRUE),
    length.out = PREDICT_POINTS
  )

  newdata <- stats::setNames(data.frame(x_seq), X_COLUMN)
  pred_terms <- predict(gam_model, newdata = newdata, type = "terms", se.fit = TRUE)

  term_names <- colnames(pred_terms$fit)
  smooth_term <- grep(sprintf("^s\\(%s\\)$", X_COLUMN), term_names, value = TRUE)
  if (length(smooth_term) == 0) {
    smooth_term <- term_names[1]
    warning("対応する平滑項が見つからなかったため、最初の項を使用します。")
  }

  effect <- pred_terms$fit[, smooth_term]
  se_effect <- pred_terms$se.fit[, smooth_term]

  effect_df <- data.frame(
    x = x_seq,
    effect = effect,
    lower = effect - CI_MULTIPLIER * se_effect,
    upper = effect + CI_MULTIPLIER * se_effect
  )
  names(effect_df)[names(effect_df) == "x"] <- X_COLUMN

  ggplot2::ggplot(
    effect_df,
    ggplot2::aes(x = .data[[X_COLUMN]], y = .data[["effect"]])
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data[["lower"]], ymax = .data[["upper"]]),
      fill = "#c6dbef",
      alpha = 0.6
    ) +
    ggplot2::geom_line(color = "#08306b", linewidth = 1) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#636363") +
    ggplot2::labs(
      title = sprintf("GAM smooth effect: s(%s)", X_COLUMN),
      x = X_COLUMN,
      y = sprintf("Effect on %s", Y_COLUMN),
      caption = sprintf("Shaded area: ±%s × standard error", CI_MULTIPLIER)
    ) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
}

save_plot <- function(plot, filename) {
  filepath <- file.path(OUTPUT_DIR, filename)
  cat("プロット保存中:", filepath, "\n")
  ggplot2::ggsave(filepath, plot = plot, width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
}

save_gam_summary <- function(gam_model, prefix) {
  summary_path <- file.path(OUTPUT_DIR, sprintf("%s_gam_summary.txt", prefix))
  cat("GAM サマリー保存中:", summary_path, "\n")
  capture.output(
    {
      cat("GAM formula:\n")
      print(formula(gam_model))
      cat("\n")
      print(summary(gam_model))
    },
    file = summary_path
  )
}

# ================================================================
# メイン処理
# ================================================================

main <- function() {
  cat("=== GAM 可視化スクリプト ===\n")
  validate_settings()
  ensure_output_dir()

  data <- load_data()
  clean_data <- prepare_data(data)

  gam_model <- fit_gam_model(clean_data)

  cat("\n--- 可視化・保存 ---\n")
  prefix <- resolve_output_prefix()
  scatter_plot <- create_scatter_plot(clean_data)
  save_plot(scatter_plot, sprintf("%s_scatter.png", prefix))

  effect_plot <- create_effect_plot(gam_model, clean_data)
  save_plot(effect_plot, sprintf("%s_gam_effect.png", prefix))

  if (isTRUE(SAVE_SUMMARY)) {
    save_gam_summary(gam_model, prefix)
  }

  cat("\n=== 処理完了 ===\n")
  cat(
    "設定: smooth k =", SMOOTH_K,
    ", family =",
    if (is.character(GAM_FAMILY)) GAM_FAMILY else class(resolve_family())[1],
    "\n"
  )
}

# 実行
main()

