# ---
# 人口ピラミッド、積み上げヒストグラム、集計CSV作成 Rスクリプト
# (英語表記・色調整版)
# ---

# 0. 必要なライブラリのインストール（初回のみ）
# 以下の行のコメントを解除して実行してください。
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")

# 1. ライブラリの読み込み
library(ggplot2)
library(dplyr)
library(readr)

#' 人口ピラミッド、積み上げヒストグラム、集計CSVを作成・保存する関数
#'
#' @param csv_file_path 読み込むCSVファイルのパス
#' @param age_col 年齢データが入っているカラム名（文字列）
#' @param sex_col 性別データ（0=男性, 1=女性）が入っているカラム名（文字列）
#' @param output_filename (ベースとなる)保存する画像ファイル名 (デフォルト: "population_pyramid_en_color.png")
#'                      このファイル名を基準に、_counts.csv と _stacked.png が追加で生成されます。
#'
#' @return ggplotオブジェクト（ピラミッド図）
create_population_plots_en_color <- function(csv_file_path, 
                                             age_col, 
                                             sex_col, 
                                             output_filename = "population_pyramid_en_color.png") {
  
  # 2. データの読み込み
  tryCatch({
    data <- read_csv(csv_file_path, col_types = cols(.default = "c")) %>%
      mutate(
        !!age_col := as.numeric(.data[[age_col]]),
        !!sex_col := as.numeric(.data[[sex_col]])
      )
  }, error = function(e) {
    stop(paste0("CSVファイルの読み込みに失敗しました: ", csv_file_path, "\nエラー: ", e$message))
  })

  # 3. 欠損値のチェックとコンソールへの出力
  cat("\n--- 欠損値チェック ---\n")
  if (!age_col %in% names(data)) {
    stop(paste0("指定された年齢カラム '", age_col, "' がCSV内に見つかりません。"))
  }
  if (!sex_col %in% names(data)) {
    stop(paste0("指定された性別カラム '", sex_col, "' がCSV内に見つかりません。"))
  }
  
  age_na_count <- sum(is.na(data[[age_col]]))
  sex_na_count <- sum(is.na(data[[sex_col]]))
  
  cat(paste0("年齢カラム (", age_col, ") の欠損数: ", age_na_count, "\n"))
  cat(paste0("性別カラム (", sex_col, ") の欠損数: ", sex_na_count, "\n"))
  cat("----------------------\n\n")
  
  # 4. データの前処理
  
  processed_data <- data %>%
    filter(
      !is.na(.data[[age_col]]) & 
      !is.na(.data[[sex_col]]) &
      .data[[sex_col]] %in% c(0, 1)
    )

  if (nrow(processed_data) == 0) {
    stop("有効なデータ（欠損がなく、性別が0または1）が0件です。データを確認してください。")
  }

  min_age <- floor(min(processed_data[[age_col]]) / 5) * 5
  max_age <- ceiling(max(processed_data[[age_col]]) / 5) * 5
  
  # 集計データ (これがCSV保存、ピラミッド図、積み上げ図のベースになる)
  pyramid_data <- processed_data %>%
    mutate(
      SexLabel = ifelse(.data[[sex_col]] == 0, "Male", "Female")
    ) %>%
    mutate(
      AgeGroup = cut(
        .data[[age_col]],
        breaks = seq(min_age, max_age, by = 5),
        right = FALSE, 
        include.lowest = TRUE,
        labels = paste0(seq(min_age, max_age - 5, by = 5), "-", seq(min_age + 4, max_age - 1, by = 5))
      )
    ) %>%
    group_by(AgeGroup, SexLabel) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    filter(!is.na(AgeGroup))

  if (nrow(pyramid_data) == 0) {
    stop("集計できるデータがありませんでした。データ内容を確認してください。")
  }

  # ---
  # ★ 1. 集計データ (実人数) をCSVに保存
  # ---
  # 正規表現で拡張子を "_counts.csv" に置き換える
  csv_output_filename <- sub("(\\.[^.]+)$", "_counts.csv", output_filename)
  tryCatch({
    write_csv(pyramid_data %>% select(AgeGroup, SexLabel, Count), csv_output_filename)
    cat(paste0("集計データを '", csv_output_filename, "' として保存しました。\n"))
  }, error = function(e) {
    cat(paste0("CSVファイルの保存に失敗しました: ", e$message, "\n"))
  })

  # ---
  # ★ 2. 人口ピラミッドの作成と保存
  # ---
  
  # ピラミッド図用に男性のデータをマイナスにする
  pyramid_data_plot <- pyramid_data %>%
    mutate(
      PlotCount = ifelse(SexLabel == "Male", Count * -1, Count)
    )

  max_abs_count <- max(abs(pyramid_data_plot$PlotCount))
  axis_break_unit <- round(max_abs_count / 3, -floor(log10(max_abs_count / 3)))
  axis_limit <- ceiling(max_abs_count / axis_break_unit) * axis_break_unit

  g_pyramid <- ggplot(pyramid_data_plot, aes(x = PlotCount, y = AgeGroup, fill = SexLabel)) +
    geom_col(width = 0.9) +
    coord_flip() +
    labs(
      title = "Age & Sex Distribution (Population Pyramid)",
      x = "Count",
      y = "Age Group",
      fill = "Sex"
    ) +
    scale_x_continuous(
      labels = function(x) abs(x),
      limits = c(-axis_limit, axis_limit),
      breaks = seq(-axis_limit, axis_limit, by = axis_break_unit)
    ) +
    scale_fill_manual(
      values = c("Male" = "#0072B2", "Female" = "#CC0000"),
      labels = c("Female", "Male") 
    ) + 
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom"
    )

  tryCatch({
    ggsave(output_filename, plot = g_pyramid, width = 8, height = 7, dpi = 300)
    cat(paste0("ピラミッド図を '", output_filename, "' として保存しました。\n"))
  }, error = function(e) {
    cat(paste0("ピラミッド図の保存に失敗しました: ", e$message, "\n"))
  })
  
  # ---
  # ★ 3. 積み上げヒストグラムの作成と保存
  # ---
  # 正規表現で拡張子を "_stacked.png" に置き換える
  stacked_output_filename <- sub("(\\.[^.]+)$", "_stacked.png", output_filename)
  
  g_stacked <- ggplot(pyramid_data, aes(x = AgeGroup, y = Count, fill = SexLabel)) +
    geom_col(position = "stack", width = 0.9) +
    labs(
      title = "Age & Sex Distribution (Stacked Histogram)",
      x = "Age Group",
      y = "Count",
      fill = "Sex"
    ) +
    # ピラミッド図と同じ色の定義を使用
    scale_fill_manual(
      values = c("Male" = "#0072B2", "Female" = "#CC0000"),
      labels = c("Female", "Male")
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      # X軸のラベルが重ならないように角度をつける
      axis.text.x = element_text(angle = 45, hjust = 1) 
    )

  tryCatch({
    ggsave(stacked_output_filename, plot = g_stacked, width = 10, height = 7, dpi = 300)
    cat(paste0("積み上げヒストグラムを '", stacked_output_filename, "' として保存しました。\n"))
  }, error = function(e) {
    cat(paste0("積み上げヒストグラムの保存に失敗しました: ", e$message, "\n"))
  })

  # メインのピラミッド図を返す
  return(g_pyramid)
}

# ---
# (A) サンプルデータの作成 (この部分は実際の使用時は不要です)
# ---
cat("サンプルCSVファイル 'my_data.csv' を作成します...\n")
sample_data <- data.frame(
  participant_id = 1:10000,
  age_data = sample(18:85, 10000, replace = TRUE),
  gender_code = sample(c(0, 1), 10000, replace = TRUE, prob = c(0.48, 0.52))
)
sample_data$age_data[sample(1:10000, 50)] <- NA
sample_data$gender_code[sample(1:10000, 30)] <- NA
write_csv(sample_data, "my_data.csv")
cat("サンプルCSVファイルを作成しました。\n\n")


# ---
# (B) 関数の実行 (ここが本番です)
# ---

# 1. "my_data.csv" を実際のファイルパスに書き換えてください。
# 2. "age_data" を実際の「年齢」カラム名に書き換えてください。
# 3. "gender_code" を実際の「性別」カラム名に書き換えてください。
# 4. "population_pyramid.png" を、ベースとしたいファイル名に変更してください。

print("ピラミッド図、積み上げヒストグラム、CSVの作成を開始します...")
tryCatch({
  
  pyramid_plot <- create_population_plots_en_color(
    csv_file_path = "time1_data.csv",   # <- 実際のファイルパスに変更
    age_col = "age",            # <- 実際の年齢カラム名に変更
    sex_col = "sex",         # <- 実際の性別カラム名に変更
    output_filename = "population_pyramid.png" # <- ベースとなるファイル名
  )
  
  # RStudioなどの対話環境でプロットを表示する場合
  # print(pyramid_plot) 
  
}, error = function(e) {
  cat("\nエラーが発生しました: ", e$message, "\n")
})