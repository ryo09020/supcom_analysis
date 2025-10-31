# ------------------------------------------------------------------
# Rスクリプト：第二期データに第一期のクラス情報を結合する
# (ファイル冒頭で列名を指定するバージョン)
# ------------------------------------------------------------------

# 1. 必要なライブラリの読み込み
# もし 'tidyverse' がインストールされていない場合は、先にインストールしてください
# install.packages("tidyverse")
library(tidyverse)
# library(readr) # readrはtidyverseに含まれています
# library(dplyr) # dplyrはtidyverseに含まれています

# ------------------------------------------------------------------
# (重要) ファイル名と列名の指定
# ------------------------------------------------------------------
# ★★★ ここでご自身のデータに合わせて設定を変更してください ★★★

# 2-1. ファイル名の指定
file_time1 <- "time1.csv"
file_time2 <- "time2.csv"
output_file <- "time2_with_class.csv" # 保存するファイル名

# 2-2. 読み込む列名の指定
# T1とT2で共通の「個人ID」が格納されている列名
id_column_name <- "ID" 

# T1データに含まれる「クラス情報」が格納されている列名
class_column_name <- "class" 

# ★★★ 設定はここまで ★★★
# ------------------------------------------------------------------


# 3. データの読み込み
df_time1 <- read_csv(file_time1)
df_time2 <- read_csv(file_time2)


# 3.1 必須列の存在チェック
required_cols_time1 <- c(id_column_name, class_column_name)
missing_time1 <- setdiff(required_cols_time1, names(df_time1))
if (length(missing_time1) > 0) {
  stop(
    paste0(
      "time1 データに必要な列が見つかりません: ",
      paste(missing_time1, collapse = ", " )
    ),
    call. = FALSE
  )
}

if (!(id_column_name %in% names(df_time2))) {
  stop(
    paste0("time2 データに列 '", id_column_name, "' が見つかりません。"),
    call. = FALSE
  )
}

# 3.2 ID・クラス列の整形（型と余白を揃える）
normalize_id <- function(x) {
  x |>
    as.character() |>
    stringr::str_trim() |>
    dplyr::na_if("")
}

df_time1 <- df_time1 |>
  mutate(
    !!id_column_name := normalize_id(.data[[id_column_name]]),
    !!class_column_name := ifelse(
      is.na(.data[[class_column_name]]),
      NA,
      .data[[class_column_name]]
    )
  )

df_time2 <- df_time2 |>
  mutate(!!id_column_name := normalize_id(.data[[id_column_name]]))

# 3.3 IDの欠損を除外（連結できないため）
df_time1 <- df_time1 |>
  filter(!is.na(.data[[id_column_name]]), !is.na(.data[[class_column_name]]))

df_time2 <- df_time2 |>
  filter(!is.na(.data[[id_column_name]]))

# 4. 第一期のデータからIDとclassのみを抽出
# all_of() を使うことで、上記で指定した変数名（文字列）で列を選択できます
df_time1_class_info <- df_time1 %>%
  select(all_of(id_column_name), all_of(class_column_name)) %>%
  distinct()

# 4.1 同じIDで複数のクラスが存在しないか確認
conflicting_ids <- df_time1_class_info %>%
  group_by(.data[[id_column_name]]) %>%
  summarise(n_class = n_distinct(.data[[class_column_name]]), .groups = "drop") %>%
  filter(n_class > 1)

if (nrow(conflicting_ids) > 0) {
  stop(
    paste0(
      "time1 データで複数のクラスに属している ID が存在します: ",
      paste(conflicting_ids[[id_column_name]], collapse = ", ")
    ),
    call. = FALSE
  )
}

# 5. 第二期のデータに第一期のclass情報を結合
# (id_column_name の値が一致する行をT1とT2で探して結合します)
df_time2_with_class <- df_time2 %>%
  left_join(df_time1_class_info, by = id_column_name)

# 5.1 結合できなかったIDの件数を表示
unmatched_ids <- df_time2_with_class %>%
  filter(is.na(.data[[class_column_name]])) %>%
  pull(.data[[id_column_name]])

if (length(unmatched_ids) > 0) {
  message(
    paste0(
      "⚠️ time2 に存在するが time1 でクラス情報が見つからなかった ID が ",
      length(unmatched_ids),
      " 件あります。"
    )
  )
}


# 6. 結果の確認
print("--- 結合後のデータ（先頭） ---")
head(df_time2_with_class)

# class列に NA (欠損値) があるか確認
# (T2には存在するがT1には存在しないIDがある場合、NAになります)
na_count <- sum(is.na(df_time2_with_class[[class_column_name]]))
print(paste0("--- ", class_column_name, "列のNAの数 ---"))
print(na_count)


# 7. 結合したデータを新しいCSVファイルとして保存
write_csv(df_time2_with_class, output_file)

print(paste0("処理が完了しました。'", output_file, "' が作成されました。"))