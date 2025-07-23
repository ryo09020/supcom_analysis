# MplusAutomationパッケージを読み込みます
library(MplusAutomation)

# まずは1つのモデルだけを読み込んで、中身を詳しく見てみましょう
# "tipi-j/2.out" の部分は、ご自身のファイル名に合わせてください
single_model_check <- readModels("tipi-j/2.out", 
                                 what = c("summaries", "class_counts"), 
                                 quiet = TRUE)

# 読み込まれた「summaries」の中に、どのような列名があるか確認します
print("自動で読み込まれたsummariesの列名一覧:")
names(single_model_check$summaries)