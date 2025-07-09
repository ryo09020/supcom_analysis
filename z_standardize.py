# -*- coding: utf-8 -*-
"""
このスクリプトは、指定されたCSVファイルを読み込み、
特定の列（ID、性別など）を除いたすべての数値列を標準化（z-score化）し、
結果を 'z_[元のファイル名]' という名前の新しいCSVファイルに保存します。
"""

import pandas as pd

# 1. 入力ファイル名を指定
input_filename = 'dummy_data.csv'

# 2. 出力ファイル名を生成
output_filename = f'z_{input_filename}'

# 3. CSVファイルの読み込み
# 指定したファイルが見つからない場合は、ここでエラーが発生して処理が終了します。
df = pd.read_csv(input_filename, header=0)

# 4. 標準化から除外する項目（列名）を指定
columns_to_exclude = ["ID", "性別", "生年月日", "参加時年齢", "受信時年齢", "受信日", "コースコード"]

# 標準化する項目を決定
all_columns = df.columns.tolist()
columns_to_standardize = [col for col in all_columns if col not in columns_to_exclude]

# 決定された項目を標準化
df_copy = df.copy()
for column in columns_to_standardize:
    mean = df_copy[column].mean()
    std = df_copy[column].std()
    df_copy[column] = (df_copy[column] - mean) / std

# 5. 生成したファイル名で新しいCSVファイルに書き出し
# encoding='utf-8-sig' はExcelで開いた際の文字化けを防ぎます。
df_copy.to_csv(output_filename, index=False, encoding='utf-8-sig')

print(f"処理が完了し、'{output_filename}' に保存されました。")