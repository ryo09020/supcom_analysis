# -*- coding: utf-8 -*-
"""
このスクリプトは、指定されたCSVファイルを読み込み、
特定の列（ID、性別など）を除いた上で選択した列についてのみ標準化（z-score化）を行います。
残りの部分はそのまま出力csvに含めます。
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

# 4. 標準化から除外する項目（列名）を指定（これらはそのまま出力に含める）
columns_to_exclude = ["ID", "性別", "生年月日", "参加時年齢", "受信時年齢", "受信日", "コースコード"]

# 5. 標準化したい項目を指定（これらの標準化値を追加で出力）
columns_to_standardize = ["542690_00", "542700_00", "542710_00", "542720_00", "542730_00"]

# 決定された項目を標準化
df_copy = df.copy()

# 数値変換と欠損値除去を追加
for column in columns_to_standardize:
    if column in df_copy.columns:
        df_copy[column] = pd.to_numeric(df_copy[column], errors='coerce')

df_copy = df_copy.dropna(subset=columns_to_standardize)

for column in columns_to_standardize:
    if column in df_copy.columns:
        mean = df_copy[column].mean()
        std = df_copy[column].std()
        df_copy[column] = (df_copy[column] - mean) / std

# 5. 生成したファイル名で新しいCSVファイルに書き出し
# encoding='utf-8-sig' はExcelで開いた際の文字化けを防ぎます。
df_copy.to_csv(output_filename, index=False, encoding='utf-8-sig')

print(f"処理が完了し、'{output_filename}' に保存されました。")
print(f"元データ行数: {len(df)}, 処理後データ行数: {len(df_copy)}")