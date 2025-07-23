# データ整理用のパッケージも読み込む
# install.packages("dplyr") # 初回のみ
library(dplyr)
library(tidyLPA)

# Rに組み込まれているirisデータセットを使用
# 3つの変数(Sepal.Length, Sepal.Width, Petal.Length)をLPAにかける
# クラスター数を1から4まで試す
results <- iris %>%
  select(Sepal.Length, Sepal.Width, Petal.Length) %>%
  estimate_profiles(n_profiles = 1:4)

# 結果の要約（適合度指標）を表示
get_fit(results)