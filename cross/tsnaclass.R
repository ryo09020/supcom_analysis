#!/usr/bin/env Rscript

#################################################################
# t-SNE クラス別可視化スクリプト
#
# 指定した特徴量で t-SNE を実行し、CSV のクラス列を用いて
# 1〜10 クラスを色分けして散布図を作成します。
#################################################################

# ================================================================
# 設定変数
# ================================================================

# 入力ファイルと使用する列
INPUT_FILE <- "raw_data/dummy_data_with_clusters_sorted.csv"
FEATURE_COLUMNS <- c(
	"X542690_00", "X542700_00", "X542710_00",
	"X542720_00", "X542730_00"
)
CLASS_COLUMN <- "Class"

# 1〜10 の範囲で可視化したいクラス数を指定（1 -> クラス1のみ, 5 -> クラス1〜5）
CLASS_COUNT <- 3L

# スケーリング設定（"minmax", "minmax_trimmed", "zscore" のいずれか）
SCALING_METHOD <- "minmax_trimmed"

# t-SNE パラメータ
TSNE_PERPLEXITY <- 30
TSNE_ITER <- 1000
TSNE_SEED <- 42

# 出力設定
OUTPUT_PREFIX <- "tsnaclass"
OUTPUT_WIDTH <- 10
OUTPUT_HEIGHT <- 8
OUTPUT_DPI <- 300

# ================================================================
# パッケージ読み込み
# ================================================================

suppressMessages({
	suppressWarnings({
		library(readr)
		library(dplyr)
		library(ggplot2)
		library(Rtsne)
		library(rlang)
	})
})

# ================================================================
# ユーティリティ関数
# ================================================================

validate_settings <- function() {
	if (!is.character(INPUT_FILE) || INPUT_FILE == "") {
		stop("INPUT_FILE は非空文字列で指定してください。")
	}
	if (!is.numeric(CLASS_COUNT) || length(CLASS_COUNT) != 1) {
		stop("CLASS_COUNT は数値で指定してください。")
	}
	if (CLASS_COUNT < 1 || CLASS_COUNT > 10) {
		stop("CLASS_COUNT は 1〜10 の範囲で指定してください。")
	}
	valid_scaling <- c("minmax", "minmax_trimmed", "zscore")
	if (!(SCALING_METHOD %in% valid_scaling)) {
		stop("SCALING_METHOD は ", paste(valid_scaling, collapse = ", "), " のいずれかで指定してください。")
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

apply_scaling <- function(data, method) {
	if (method == "minmax") {
		scaled <- lapply(data, function(x) {
			rng <- range(x, na.rm = TRUE)
			if (diff(rng) == 0) {
				return(rep(0.5, length(x)))
			}
			(x - rng[1]) / diff(rng)
		})
	} else if (method == "minmax_trimmed") {
		scaled <- lapply(data, function(x) {
			q05 <- quantile(x, 0.05, na.rm = TRUE, names = FALSE)
			q95 <- quantile(x, 0.95, na.rm = TRUE, names = FALSE)
			trimmed <- pmax(pmin(x, q95), q05)
			rng <- range(trimmed, na.rm = TRUE)
			if (diff(rng) == 0) {
				return(rep(0.5, length(x)))
			}
			(trimmed - rng[1]) / diff(rng)
		})
	} else if (method == "zscore") {
		scaled <- lapply(data, function(x) {
			mu <- mean(x, na.rm = TRUE)
			sigma <- stats::sd(x, na.rm = TRUE)
			if (is.na(sigma) || sigma == 0) {
				return(rep(0, length(x)))
			}
			(x - mu) / sigma
		})
	} else {
		stop("未対応のスケーリング方法です: ", method)
	}
	as.data.frame(scaled)
}

coerce_class_values <- function(values) {
	if (is.factor(values)) {
		values <- as.character(values)
	}

	if (is.character(values)) {
		numeric_like <- grepl("^-?\\d+$", values)
		if (!all(numeric_like | is.na(values))) {
			stop("クラス列には整数のみを指定してください。")
		}
		values <- as.integer(values)
	}

	if (!is.numeric(values)) {
		stop("クラス列を数値に変換できませんでした。")
	}

	values
}

prepare_data <- function(data) {
	missing_features <- FEATURE_COLUMNS[!(FEATURE_COLUMNS %in% colnames(data))]
	if (length(missing_features) > 0) {
		stop("指定した特徴量列が見つかりません: ", paste(missing_features, collapse = ", "))
	}
	if (!(CLASS_COLUMN %in% colnames(data))) {
		stop("クラス列が見つかりません: ", CLASS_COLUMN)
	}

	df <- data |>
		dplyr::select(dplyr::all_of(c(FEATURE_COLUMNS, CLASS_COLUMN))) |>
		dplyr::mutate(dplyr::across(dplyr::all_of(FEATURE_COLUMNS), as.numeric))

	class_values <- coerce_class_values(df[[CLASS_COLUMN]])

	target_classes <- seq_len(CLASS_COUNT)
	in_range <- class_values %in% target_classes
	if (!all(in_range | is.na(class_values))) {
		drop_count <- sum(!in_range & !is.na(class_values))
		cat("指定範囲外のクラスを除外:", drop_count, "件\n")
	}

	df <- df[in_range & stats::complete.cases(df[FEATURE_COLUMNS]), , drop = FALSE]
	class_values <- class_values[in_range]

	cat("分析対象:", nrow(df), "名\n")
	if (nrow(df) < 10) {
		warning("有効な行が 10 未満です。t-SNE の結果が不安定になる可能性があります。")
	}

	scaled_features <- apply_scaling(dplyr::select(df, dplyr::all_of(FEATURE_COLUMNS)), SCALING_METHOD)

	list(
		scaled = scaled_features,
		classes = class_values
	)
}

run_tsne <- function(scaled_data) {
	cat("t-SNE 実行中...\n")
	if (TSNE_PERPLEXITY >= nrow(scaled_data)) {
		stop("perplexity がデータ行数以上です。TSNE_PERPLEXITY を小さくしてください。")
	}
	set.seed(TSNE_SEED)
	tsne <- Rtsne::Rtsne(
		scaled_data,
		dims = 2,
		perplexity = TSNE_PERPLEXITY,
		max_iter = TSNE_ITER,
		check_duplicates = FALSE
	)
	scores <- as.data.frame(tsne$Y)
	colnames(scores) <- c("tSNE1", "tSNE2")
	scores
}

create_tsne_plot <- function(tsne_scores, class_values) {
	if (nrow(tsne_scores) != length(class_values)) {
		stop("t-SNE 結果とクラス列の長さが一致しません。")
	}

	available_classes <- sort(unique(class_values))
	labels <- paste0("Class ", available_classes)

	base_colors <- c(
		"#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
		"#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf"
	)
	class_palette <- base_colors[available_classes]

	plot_data <- tsne_scores |>
		dplyr::mutate(
			class = factor(class_values, levels = available_classes, labels = labels)
		)

	ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$tSNE1, y = .data$tSNE2, color = .data$class)) +
		ggplot2::geom_point(alpha = 0.75, size = 2.4) +
		ggplot2::scale_color_manual(values = class_palette) +
		ggplot2::labs(
			title = "t-SNE Projection Colored by Class",
			x = "t-SNE Dimension 1",
			y = "t-SNE Dimension 2",
			color = "Class"
		) +
		ggplot2::theme_minimal(base_size = 12) +
		ggplot2::theme(
			plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 14),
			panel.grid.major = ggplot2::element_line(color = "gray90"),
			panel.grid.minor = ggplot2::element_blank(),
			panel.border = ggplot2::element_rect(color = "gray70", fill = NA, linewidth = 1),
			legend.position = "right"
		)
}

save_tsne_plot <- function(plot) {
	filename <- sprintf("%s_class%s.png", OUTPUT_PREFIX, CLASS_COUNT)
	cat("プロット保存中:", filename, "\n")
	ggplot2::ggsave(filename, plot = plot, width = OUTPUT_WIDTH, height = OUTPUT_HEIGHT, dpi = OUTPUT_DPI)
}

# ================================================================
# メイン処理
# ================================================================

main <- function() {
	cat("=== t-SNE クラス可視化スクリプト ===\n")
	validate_settings()

	data <- load_data()
	prepared <- prepare_data(data)

	tsne_scores <- run_tsne(prepared$scaled)

	cat("\n--- 可視化・保存 ---\n")
	tsne_plot <- create_tsne_plot(tsne_scores, prepared$classes)
	save_tsne_plot(tsne_plot)

	cat("\n=== 処理完了 ===\n")
	cat("設定: ", SCALING_METHOD, " スケーリング、クラス 1〜", CLASS_COUNT, " を色分け\n", sep = "")
}

# 実行
main()
