# ------------------------------------------------------------------
# Rスクリプト：縦断比較バイオリンプロット（項目ラベルのカスタム対応版）
# (コメント・メッセージは日本語、グラフのラベルは英語)
# ------------------------------------------------------------------

# 1. 必要なライブラリの読み込み
# install.packages("tidyverse")
library(tidyverse)

# ------------------------------------------------------------------
# (重要) ファイル名と列名の指定
# ------------------------------------------------------------------
# ★★★ ここでご自身のデータに合わせて設定を変更してください ★★★

# 2-1. ファイル名の指定
file_time1 <- "time1.csv"
file_time2 <- "time2_with_class.csv" # 前回スクリプトで作成したファイル

# 2-2. 読み込む列名の指定
class_column <- "class"

# 2-2-1. 出力先ディレクトリとファイル名
output_dir <- "longitudinal_outputs" # すべての結果を格納するディレクトリ
output_violin_file <- "longitudinal_violin_plot_custom_labels_colored.png"
output_boxplot_file <- "longitudinal_boxplot_custom_labels_colored.png"
output_stats_file <- "longitudinal_summary_stats.csv"

# 2-3. ★★★ 分析で扱う「項目キー（表示順）」を指定 ★★★
# ここで指定した順序がグラフの表示順になります。
target_items <- c("subscale_A", "subscale_B", "total_score")


# 2-4. ★★★ 項目ラベルのマッピング（対応表）を指定 ★★★
# ` 項目キー = グラフに表示したいラベル ` の形式で指定します。
item_labels_map <- c(
  subscale_A = "Subscale A (e.g., Quality of Life)",
  subscale_B = "Subscale B (e.g., Depression)",
  total_score = "Total Score (e.g., Overall Well-being)"
)
# (注意: names(item_labels_map) は target_items と一致している必要があります)


# 2-5. ★★★ 時点ごとの列名マッピングを指定 ★★★
# 例: Time 1 のファイルでは "subscale_A_time1" という列が、
#     Time 2 のファイルでは "subscaleA_post" という列が、いずれも同じ「項目 subscale_A」を表す場合、
#     下記のように指定します。
time1_item_map <- c(
  subscale_A = "subscale_A",
  subscale_B = "subscale_B",
  total_score = "total_score"
)

time2_item_map <- c(
  subscale_A = "subscale_A",
  subscale_B = "subscale_B",
  total_score = "total_score"
)

# ★★★ 設定はここまで ★★★
# ------------------------------------------------------------------

# 内部ユーティリティと検証 --------------------------------------------------

if (length(target_items) == 0) {
  stop("target_items が空です。比較したい項目キーを指定してください。", call. = FALSE)
}

if (any(duplicated(target_items))) {
  stop("target_items 内に重複があります。ユニークな項目キーのみを指定してください。", call. = FALSE)
}

if (is.null(names(item_labels_map)) || any(names(item_labels_map) == "")) {
  stop("item_labels_map は target_items を名前に持つ名前付きベクトルで指定してください。", call. = FALSE)
}

missing_label_keys <- setdiff(target_items, names(item_labels_map))
if (length(missing_label_keys) > 0) {
  stop(
    paste0(
      "item_labels_map に次の項目キーのラベルが定義されていません: ",
      paste(missing_label_keys, collapse = ", ")
    ),
    call. = FALSE
  )
}

item_labels_map <- item_labels_map[target_items]
item_display_labels <- unname(item_labels_map)

validate_item_map <- function(item_map, target_items, dataset_label) {
  if (length(item_map) == 0) {
    stop(paste0(dataset_label, ": item_map が空です。項目キーと列名の対応を指定してください。"), call. = FALSE)
  }
  if (is.null(names(item_map)) || any(names(item_map) == "")) {
    stop(paste0(dataset_label, ": item_map は target_items を名前に持つ必要があります。"), call. = FALSE)
  }
  missing_keys <- setdiff(target_items, names(item_map))
  if (length(missing_keys) > 0) {
    stop(
      paste0(
        dataset_label,
        ": item_map に次の項目キーが不足しています: ",
        paste(missing_keys, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  selected_map <- item_map[target_items]
  if (any(is.na(selected_map))) {
    stop(paste0(dataset_label, ": item_map に NA が含まれています。"), call. = FALSE)
  }
  duplicate_sources <- selected_map[duplicated(selected_map)]
  if (length(duplicate_sources) > 0) {
    stop(
      paste0(
        dataset_label,
        ": item_map で同じ列名が複数の項目に割り当てられています: ",
        paste(unique(duplicate_sources), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  selected_map
}

rename_columns_with_map <- function(df, selected_map) {
  renamed_df <- df
  for (idx in seq_along(selected_map)) {
    new_name <- names(selected_map)[idx]
    old_name <- selected_map[[idx]]
    if (identical(new_name, old_name)) {
      next
    }
    matching_cols <- names(renamed_df) == old_name
    if (!any(matching_cols)) {
      stop(paste0("列", old_name, "がデータ内に存在しません。"), call. = FALSE)
    }
    names(renamed_df)[matching_cols] <- new_name
  }
  renamed_df
}

prepare_timepoint_data <- function(file_path, time_label, item_map, target_items, class_column) {
  cat(sprintf("📥 %s (%s) を読み込み中...\n", time_label, file_path))
  df_raw <- readr::read_csv(file_path, show_col_types = FALSE)
  selected_map <- validate_item_map(item_map, target_items, time_label)
  required_columns <- unique(c(class_column, selected_map))
  missing_columns <- setdiff(required_columns, names(df_raw))
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        time_label,
        ": データに以下の列が見つかりません: ",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  df_standardized <- rename_columns_with_map(df_raw, selected_map)
  missing_after_rename <- setdiff(target_items, names(df_standardized))
  if (length(missing_after_rename) > 0) {
    stop(
      paste0(
        time_label,
        ": 項目キーに対応する列のリネーム後の存在確認で失敗しました: ",
        paste(missing_after_rename, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  df_standardized[target_items] <- lapply(df_standardized[target_items], function(col) {
    if (is.list(col)) {
      stop(paste0(time_label, ": ", deparse(substitute(col)), " 列がリスト型です。前処理でベクトル化してください。"), call. = FALSE)
    }
    if (is.factor(col)) {
      col <- as.character(col)
    }
    suppressWarnings(as.numeric(col))
  })
  if (!class_column %in% names(df_standardized)) {
    stop(paste0(time_label, ": クラス列 '", class_column, "' がデータ内に存在しません。"), call. = FALSE)
  }
  df_standardized[[class_column]] <- as.character(df_standardized[[class_column]])
  df_standardized[["time"]] <- time_label
  df_standardized <- df_standardized[, c(class_column, target_items, "time"), drop = FALSE]
  cat(sprintf("✅ %s: 項目名を統一しました。\n", time_label))
  df_standardized
}

time1_label <- "Time 1"
time2_label <- "Time 2"
time_levels <- c(time1_label, time2_label)


# 3. データの読み込みと前処理 (T1, T2)
df_t1 <- prepare_timepoint_data(file_time1, time1_label, time1_item_map, target_items, class_column)
df_t2 <- prepare_timepoint_data(file_time2, time2_label, time2_item_map, target_items, class_column)

# 4. T1とT2のデータを縦に結合
df_combined <- dplyr::bind_rows(df_t1, df_t2)

# 5. データを縦長形式に変換
df_long <- df_combined %>%
  select(all_of(class_column), time, all_of(target_items)) %>%
  pivot_longer(
    cols = all_of(target_items),
    names_to = "item_name",
    values_to = "value"
  ) %>%
  mutate(
    item_key = item_name,
    class = factor(!!sym(class_column)),
    time = factor(time, levels = time_levels),
    item_name = factor(
      item_key,
      levels = target_items,
      labels = item_display_labels
    )
  ) %>%
  filter(!is.na(value), !is.na(class))

summary_stats <- df_long %>%
  group_by(class, time, item_key, item_name) %>%
  summarise(
    n = dplyr::n(),
    mean = mean(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    variance = var(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(item_key, time, class)

# 6. プロットの作成
# クラスラベルに "Profile " を追加
levels(df_long$class) <- paste0("Profile ", levels(df_long$class))

violin_plot <- ggplot(df_long, aes(x = class, y = value, fill = time)) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.5, trim = FALSE) +

  # ------------------------------------------------------------------
  # ★★★ `facet_wrap` の `labeller` を使用（より堅牢な方法）★★★
  #
  # (上記 5. の factor() でのラベル設定がうまくいかない場合や、
  #  よりggplot2の標準的な方法を使いたい場合は、こちらのコメントアウトを
  #  解除して、上記 5. の factor() の 'labels' 部分を削除してください)
  #
  # item_labeller <- as_labeller(item_labels_map)
  # facet_wrap(~ item_name, scales = "free_y", labeller = item_labeller) +
  # ------------------------------------------------------------------

  # 上記 5. の factor() でラベルを設定した場合、facet_wrap はシンプルでOK
  facet_wrap(~item_name, scales = "free_y") +

  # ラベルとタイトルを英語に設定
  labs(
    title = "Longitudinal Comparison by Class and Item",
    subtitle = "Time 1 vs Time 2",
    x = "Psychological profile", # Changed from "Class" to match simple_vioplot.R
    y = "Score", # Changed from "Value"
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 18, face = "bold")
  )

# 7. プロットの表示
print(violin_plot)

# 8. 箱ひげ図の作成
box_plot <- ggplot(df_long, aes(x = class, y = value, fill = time)) +
  geom_boxplot(position = position_dodge(width = 0.9), outlier.size = 0.6) +
  facet_wrap(~item_name, scales = "free_y") +
  labs(
    title = "Longitudinal Boxplot by Class and Item",
    subtitle = "Time 1 vs Time 2",
    x = "Class",
    y = "Value",
    fill = "Timepoint"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 14)
  )

print(box_plot)

# 9. 出力ディレクトリを作成
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
}

violin_path <- file.path(output_dir, output_violin_file)
boxplot_path <- file.path(output_dir, output_boxplot_file)
stats_path <- file.path(output_dir, output_stats_file)

# 10. プロットおよび統計情報を保存
ggsave(
  filename = violin_path,
  plot = violin_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)
cat(sprintf("🖼️ バイオリンプロットを '%s' に保存しました。\n", normalizePath(violin_path)))

ggsave(
  filename = boxplot_path,
  plot = box_plot,
  width = 12,
  height = 7,
  dpi = 300,
  bg = "white"
)

cat(sprintf("🖼️ 箱ひげ図を '%s' に保存しました。\n", normalizePath(boxplot_path)))

readr::write_csv(summary_stats, stats_path)
cat(sprintf("🧮 要約統計を '%s' に保存しました。\n", normalizePath(stats_path)))
