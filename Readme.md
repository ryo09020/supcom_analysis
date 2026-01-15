# supcom_analysis

このリポジトリは、横断解析（クラスター/LPA）と縦断解析（Time1/Time2）で使用した R スクリプト群をまとめたものです。

## 前提（実行環境）

- 実行は基本的に RStudio で [supcon_analysis.Rproj](supcon_analysis.Rproj) を開いて行います。
- 各スクリプトは冒頭に「ユーザー設定」があり、入力CSVパス・列名・出力先をそこで指定します。
- 相対パスは、原則としてプロジェクトルート（このリポジトリ直下）からの相対として扱います。

## データ配置の考え方（最低限）

スクリプトによって参照するパスが異なるため、どのファイルを入力にするかは必ずスクリプト冒頭の設定を見て合わせます。

- 横断解析の入力/中間生成物（例: `*_with_clusters_sorted.csv`）
    - 典型的には [cross/raw_data/](cross/raw_data/) や `raw_data/` 配下に置く想定です。
- 縦断解析の入力（例: `time1.csv`, `time2.csv`, `time2_with_class.csv`）
    - 典型的には [long/](long/) 直下に置く想定です（スクリプトの設定で変更可）。

## 実験手順（修論に書く用：実行フロー）

### 横断解析（LPA → 可視化 → クラス整理 → 可視化 → 次元削減 → 特徴比較 → APOE）

1. **LPA を実行し、クラス付与CSVと適合度指標を出力**
     - スクリプト: [cross/lpa_flow_by_age.R](cross/lpa_flow_by_age.R)
     - 目的: 指定項目で LPA（潜在プロファイル分析）を行い、クラス数比較表と、元データに `Class` を付与したCSVを作る
     - 主要設定:
         - `INPUT_FILE`, `TARGET_COLUMNS`
         - `PROFILE_RANGE`（比較するクラス数範囲）
         - `FINAL_CLUSTERS`（最終採用クラス数）
         - `OUTPUT_DIRECTORY`（例: `outputs`）
     - 出力（デフォルト例）:
         - 適合度比較表: `outputs/lpa_comparison_table2.csv`
         - クラス付与CSV: `outputs/<元CSV名>_with_clusters_sorted.csv`

2. **共変量調整後の z-score 棒グラフ + 素点のバイオリン/箱ひげ図を出力**
     - スクリプト: [cross/cluster_zscore_barplot.R](cross/cluster_zscore_barplot.R)
     - 目的: `Class` 付きCSVを読み込み、（任意で共変量調整 →）z-score化してクラス別プロファイルを可視化
     - 主要設定:
         - `file_path`（入力CSV）
         - `cluster_column`（例: `Class`）
         - `target_items`（分析項目）
         - `covariates`（共変量。空なら調整なし）
         - `output_dir`, `output_prefix`
     - 出力（デフォルト例）:
         - `cluster_zscore_output/cluster_zscore_all_clusters.png`
         - `cluster_zscore_output/cluster_zscore_violin_raw.png`
         - `cluster_zscore_output/cluster_zscore_boxplot_raw.png`
         - `cluster_zscore_output/cluster_zscore_summary.csv`

3. **棒グラフの並びに合わせて class 番号の並び替え（再ラベル）**
     - スクリプト: [cross/class_num_change.R](cross/class_num_change.R)
     - 目的: `conversion_map`（old → new）に従ってクラス番号を再割当し、新しいCSVを作る
     - 主要設定:
         - `input_file`, `class_column`
         - `conversion_map`（例: `c("1"="3", "2"="1", "3"="2")`）
         - `output_dir`, `output_filename`
     - 出力（デフォルト例）: `final_data/class_relabelled.csv`

4. **再度 2 を実行し、並び替え後の z-score 図表を作る → その後に t-SNE / PCA を行う**
     - z-score 図表: [cross/cluster_zscore_barplot.R](cross/cluster_zscore_barplot.R)
     - t-SNE: [cross/tsnaclass.R](cross/tsnaclass.R)
     - PCA: [cross/pcaclass.R](cross/pcaclass.R)
     - 目的: クラス番号を整理した後のデータで、散布図（次元削減）によるクラス分離を確認
     - 主要設定（t-SNE/PCA共通）:
         - `INPUT_FILE`（Class付きCSV）
         - `FEATURE_COLUMNS`（使用特徴量）
         - `CLASS_COLUMN`, `CLASS_COUNT`
         - `SCALING_METHOD`
     - 出力（デフォルト例）:
         - `tsnaclass_*.png`, `pcaclass_*.png`（保存先は各スクリプトの `OUTPUT_PREFIX` 等に依存）

5. **クラスター間の特徴（項目）比較を行う**
     - スクリプト: [cross/improved_comparision_items_by_class.R](cross/improved_comparision_items_by_class.R)
     - 目的: クラスカル・ウォリス検定 + Dunn検定（多重比較）+ 効果量（η²）を項目ごとに出力
     - 主要設定:
         - `csv_file_path`, `columns_to_analyze`, `class_column_name`
         - `output_csv_name`
     - 出力: 指定した `output_csv_name`（例: `444analysis_results_improved.csv`）

6. **APOE（e4）をクラスごとに解析**
     - スクリプト: [cross/apoe.R](cross/apoe.R)
     - 目的: VCF から rs429358/rs7412 を抽出して e4 の保有状況を計算し、クラス別に集計
     - 主要設定（`config`）:
         - `vcf`（VCF.gz と index）, `csv`（IDとClassの表）
         - `id_column`, `class_column`
         - `bcftools`（環境によりパスが異なる）
     - 出力:
         - `apoe_e4_summary_by_class.csv`
         - `apoe_subject_level_genotypes.csv`

### 縦断解析（Time1/Time2 の共通IDで揃える → 可視化・モデル）

0. **Time1/Time2 の両方に存在するIDのみを抽出したCSVを作成**
     - スクリプト: [long/check_id_overlap.R](long/check_id_overlap.R)
     - 目的: `time1.csv` と `time2_with_class.csv` の ID を突き合わせ、共通IDだけ残したCSVを作る
     - 主要設定:
         - `file1`, `file2`, `id_column`
     - 出力（ファイル名に行数が付く）:
         - `time1_common_n<行数>.csv`
         - `time2_common_n<行数>.csv`

1. **IES-R / GHQ / MMSE の縦断プロット作成（バイオリン/箱ひげなど）**
     - スクリプト:
         - [long/vio.R](long/vio.R)（任意の項目セットを target_items で指定して描く）
         - [long/violin_mmse_65plus.R](long/violin_mmse_65plus.R)（MMSEを65歳以上に限定した専用版）
     - 出力（デフォルト例）:
         - `longitudinal_outputs/` 配下にPNGと要約CSV

2. （オプション）**スパゲッティプロットや LMM 解析**
     - スパゲッティ:
         - [long/spaghetti_transition.R](long/spaghetti_transition.R)
         - [long/spaghetti_mmse_65plus.R](long/spaghetti_mmse_65plus.R)
     - 線形混合効果モデル（クラス差・時間変化・交互作用）:
         - [long/linear_mixed_effects_analysis.R](long/linear_mixed_effects_analysis.R)

## 各スクリプトの使い方（短いカタログ）

### basis/（データ確認・基礎集計）

- [basis/age_sex_plot.R](basis/age_sex_plot.R): 年齢×性別の人口ピラミッド/積み上げ図と集計CSVを出力（入力CSV/列名を関数引数で指定）
- [basis/basisdata.R](basis/basisdata.R): 性別別の年齢統計（平均/中央値/人数）をコンソール出力（入力CSV/列名を指定）
- [basis/time_range.R](basis/time_range.R): 日付列の最小/最大をコンソール出力（入力CSV/日付列名を指定）

### cross/（横断解析）

- [cross/lpa_flow_by_age.R](cross/lpa_flow_by_age.R): LPA統合フロー（クラス数比較表 + Class付与CSVを出力）
- [cross/vlmr.R](cross/vlmr.R): LPA統合フローの別版（`TARGET_COLUMNS` が `var1..` のため、テンプレ/旧版の可能性）
- [cross/cluster_zscore_barplot.R](cross/cluster_zscore_barplot.R): 共変量調整→z-score化→棒/バイオリン/箱ひげ + 統計出力
- [cross/class_num_change.R](cross/class_num_change.R): クラス番号の再ラベル（old→new）
- [cross/tsnaclass.R](cross/tsnaclass.R): t-SNE によるクラス可視化（入力CSV + 特徴量 + Class列）
- [cross/pcaclass.R](cross/pcaclass.R): PCA によるクラス可視化（入力CSV + 特徴量 + Class列）
- [cross/improved_comparision_items_by_class.R](cross/improved_comparision_items_by_class.R): Kruskal-Wallis + Dunn + 効果量
- [cross/ancova2.R](cross/ancova2.R): ANCOVA（共変量あり）+ 多重比較（Bonferroni）
- [cross/ancova_comparison_items_by_class.R](cross/ancova_comparison_items_by_class.R): ANCOVA（多数尺度版）
- [cross/chi_squared_test.R](cross/chi_squared_test.R): カテゴリ変数の独立性検定（χ²）+ Cramer’s V
- [cross/vioplot.R](cross/vioplot.R): 共変量調整した推定平均と95%CIを重ねたバイオリン図（尺度設定 `SCALE_CONFIG`）
- [cross/viovio.R](cross/viovio.R): vioplot系のバリエーション（尺度設定が一部コメントアウト）
- [cross/time2violin.R](cross/time2violin.R): 指定尺度を一括でバイオリン図（共変量調整 + 95%CI）
- [cross/simple_vioplot.R](cross/simple_vioplot.R): 共変量調整なしの簡易バイオリン図（生データ分布）
- [cross/violin_only.R](cross/violin_only.R): [cross/cluster_zscore_barplot.R](cross/cluster_zscore_barplot.R) から抽出した最小構成のバイオリン図
- [cross/finaledu.R](cross/finaledu.R): `final_education` を学歴年数 `final_edu_int` に変換してCSV保存
- [cross/fill_ave.R](cross/fill_ave.R): 欠損を平均で補完してCSV保存（指定列のみ）
- [cross/add_class_time2.R](cross/add_class_time2.R): Time2にTime1のClassを付与（IDの共通部分のみ残す）
- [cross/apoe.R](cross/apoe.R): APOE e4（VCF + bcftools）をクラス別集計

### long/（縦断解析）

- [long/check_id_overlap.R](long/check_id_overlap.R): Time1/Time2 の共通IDだけ残したCSVを作る
- [long/time2addclass.R](long/time2addclass.R): Time2にTime1のクラスを結合（left_join版）
- [long/vio.R](long/vio.R): 任意項目の縦断バイオリン/箱ひげ + 要約CSV（列名マップ対応）
- [long/violin_mmse_65plus.R](long/violin_mmse_65plus.R): MMSEのみ・65歳以上の縦断バイオリン/箱ひげ
- [long/spaghetti_transition.R](long/spaghetti_transition.R): 共通IDのみでスパゲッティ（全般項目）
- [long/spaghetti_mmse_65plus.R](long/spaghetti_mmse_65plus.R): MMSE・65歳以上のスパゲッティ
- [long/linear_mixed_effects_analysis.R](long/linear_mixed_effects_analysis.R): LMM（時間・クラス・交互作用、対比較、係数）
