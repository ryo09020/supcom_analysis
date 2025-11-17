#!/usr/bin/env Rscript

# --- シンプル設定 --------------------------------------------------------
# 必要なのはこのブロックだけです。自分の環境に合わせて書き換えてください。
config <- list(
    vcf = "repository69K_n10388_APOE_2SNPs.bcp.vcf.gz",
    csv = "/path/to/your_id_class_table.csv",
    id_column = "ID",
    class_column = "Class",
    snp_ids = c("rs429358", "rs7412"),
    bcftools = "/usr/local/software/ubuntu-20.04/bioinfo/bcftools/1.22/bin/bcftools",
    output_summary = "apoe_e4_summary_by_class.csv",
    subject_output = "apoe_subject_level_genotypes.csv",
    keep_temporary = FALSE
)

# --- 入力チェック --------------------------------------------------------
required_files <- c(config$vcf, config$csv)
missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
    stop(sprintf("Missing files: %s", paste(missing, collapse = ", ")))
}

bcftools_path <- config$bcftools
if (!file.exists(bcftools_path)) {
    resolved <- Sys.which(bcftools_path)
    if (!nzchar(resolved)) {
        stop(sprintf("Cannot find bcftools at '%s'. Set config$bcftools to the full path.", bcftools_path))
    }
    bcftools_path <- resolved
}

index_candidates <- c(paste0(config$vcf, ".csi"), paste0(config$vcf, ".tbi"))
if (!any(file.exists(index_candidates))) {
    stop("VCF index (.csi/.tbi) not found. Run 'bcftools index <vcf>' first.")
}

# --- データ読み込み (CSV) ------------------------------------------------
read_input_data <- function(path, id_col, class_col) {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    stopifnot(id_col %in% names(df), class_col %in% names(df))
    df[[id_col]] <- as.character(df[[id_col]])
    df <- df[!is.na(df[[id_col]]) & df[[id_col]] != "", ]
    df
}

class_df <- read_input_data(config$csv, config$id_column, config$class_column)
csv_sample_ids <- unique(class_df[[config$id_column]])
if (!length(csv_sample_ids)) stop("No IDs found in CSV.")

# --- VCF/CSV ID照合 (★ ここが新しいロジック) ---------------------------
message("Listing samples from VCF header (this may take a moment)...")
vcf_list_args <- c("query", "-l", config$vcf)
vcf_sample_ids_raw <- system2(bcftools_path, vcf_list_args, stdout = TRUE, stderr = TRUE)
status <- attr(vcf_sample_ids_raw, "status")
if (!is.null(status) && status != 0) {
    stop(paste(c("bcftools query -l failed (VCF sample list):", vcf_sample_ids_raw), collapse = "\n"))
}
vcf_sample_ids <- trimws(vcf_sample_ids_raw)

# CSVとVCFの両方に存在するID（共通ID）を抽出
common_sample_ids <- intersect(csv_sample_ids, vcf_sample_ids)

if (!length(common_sample_ids)) {
    stop("No common sample IDs found between the CSV and the VCF file.")
}
message(sprintf("Found %d total IDs in CSV.", length(csv_sample_ids)))
message(sprintf("Found %d total IDs in VCF.", length(vcf_sample_ids)))
message(sprintf("Proceeding with %d common IDs.", length(common_sample_ids)))

# 共通IDだけを一時ファイルに書き出す
temp_dir <- file.path(tempdir(), paste0("apoe_", Sys.getpid()))
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
sample_file <- file.path(temp_dir, "common_samples.txt")
writeLines(common_sample_ids, sample_file)

# --- bcftools query ------------------------------------------------------
# (染色体名は 'chr19' で確定)
apoe_region <- "chr19:44908684-44908822"

snp_expr <- paste(sprintf('ID=="%s"', config$snp_ids), collapse = " || ")

# (フォーマット文字列をファイルに書き出す)
format_file <- file.path(temp_dir, "format.txt")
writeLines("%ID\t%REF\t%ALT[\t%GT]\n", format_file)

# (クエリ引数から -i を削除し、-S には共通IDのファイル (sample_file) を指定)
query_args <- c(
    "query",
    "-f", format_file,
    "-r", apoe_region,
    "-S", sample_file,  # ★ 共通IDリストを使用
    config$vcf
)

query_file <- file.path(temp_dir, "apoe_genotypes.tsv")
stderr_output <- system2(bcftools_path, query_args, stdout = query_file, stderr = TRUE)
status <- attr(stderr_output, "status")

# ★★★ ここからが強化されたエラーチェック ★★★

# 1. 物理的にファイルが作成されたかを確認 (サイレントフェイル対策)
if (!file.exists(query_file)) {
    stop(paste(
        c("bcftools query FAILED to create the output file.",
          "This means the command failed silently (status might be 0).",
          "--- Captured stderr output from bcftools ---",
          stderr_output,
          "----------------------------------------------"),
        collapse = "\n"
    ))
}

# 2. 念のため、従来のステータスチェックも行う
if (!is.null(status) && status != 0) {
    stop(paste(c("bcftools query failed with non-zero status:", stderr_output), collapse = "\n"))
}

# 3. ファイルが空でないかも確認
if (file.info(query_file)$size == 0) {
    stop("bcftools query ran but produced an EMPTY output file (0 bytes). Check region or inputs.")
}

message("bcftools query successful, reading results...")


geno_raw <- read.table(query_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE)
expected_cols <- 3 + length(common_sample_ids) # ★ 共通IDの数でチェック
if (ncol(geno_raw) != expected_cols) {
    stop("Unexpected bcftools output columns. Check that ID values match VCF sample names.")
}
colnames(geno_raw) <- c("SNP_ID", "REF", "ALT", common_sample_ids) # ★ 共通IDで命名

# --- APOE e4 計算 --------------------------------------------------------
get_base <- function(code, ref, alt_string) {
    if (is.na(code) || code == ".") return(NA_character_)
    idx <- suppressWarnings(as.integer(code))
    if (is.na(idx)) return(NA_character_)
    if (idx == 0) return(ref)
    alt_vec <- strsplit(alt_string, ",", fixed = TRUE)[[1]]
    if (idx > length(alt_vec)) return(NA_character_)
    alt_vec[idx]
}

decode_gt <- function(gt, ref, alt) {
    if (is.na(gt) || gt %in% c(".", "./.", ".|.")) return(rep(NA_character_, 2))
    sep <- if (grepl("|", gt, fixed = TRUE)) "\\|" else "/"
    parts <- strsplit(gt, sep)[[1]]
    if (length(parts) == 1) parts <- rep(parts, 2)
    vapply(parts, get_base, character(1), ref = ref, alt_string = alt)
}

calc_e4_dosage <- function(gt1, gt2, ref1, alt1, ref2, alt2) {
    alleles1 <- decode_gt(gt1, ref1, alt1)
    alleles2 <- decode_gt(gt2, ref2, alt2)
    if (all(is.na(alleles1)) || all(is.na(alleles2))) return(NA_real_)
    min(sum(alleles1 == "C", na.rm = TRUE), sum(alleles2 == "C", na.rm = TRUE))
}

snp_rows <- split(geno_raw, geno_raw$SNP_ID)
if (!all(config$snp_ids %in% names(snp_rows))) {
    stop("Requested SNP IDs not found in VCF query output (within the specified region).")
}

# ★ 共通ID (common_sample_ids) に基づいて subject_level を作成
subject_level <- data.frame(
    SAMPLE = common_sample_ids,
    genotype_rs429358 = as.character(snp_rows[[config$snp_ids[1]]][1, common_sample_ids]),
    genotype_rs7412 = as.character(snp_rows[[config$snp_ids[2]]][1, common_sample_ids]),
    stringsAsFactors = FALSE
)

ref1 <- toupper(snp_rows[[config$snp_ids[1]]]$REF[1])
alt1 <- toupper(snp_rows[[config$snp_ids[1]]]$ALT[1])
ref2 <- toupper(snp_rows[[config$snp_ids[2]]]$REF[1])
alt2 <- toupper(snp_rows[[config$snp_ids[2]]]$ALT[1])

subject_level$e4_dosage <- mapply(
    calc_e4_dosage,
    subject_level$genotype_rs429358,
    subject_level$genotype_rs7412,
    MoreArgs = list(ref1 = ref1, alt1 = alt1, ref2 = ref2, alt2 = alt2)
)

subject_level$e4_carrier <- subject_level$e4_dosage > 0
subject_level$e4_homozygote <- subject_level$e4_dosage == 2

# ★ all.x = TRUE でマージ (CSVの全IDが残り、VCFになかった人はNAになる)
merged <- merge(class_df, subject_level, by.x = config$id_column, by.y = "SAMPLE", all.x = TRUE)

summary_df <- do.call(rbind, lapply(split(merged, merged[[config$class_column]]), function(df) {
    df <- df[!is.na(df[[config$id_column]]), , drop = FALSE]
    n_total <- nrow(df) # ★ クラスの総数 (CSV基準)
    n_genotyped <- sum(!is.na(df$e4_dosage)) # ★ VCFにもいて遺伝子型が取れた数
    data.frame(
        Class = df[[config$class_column]][1],
        n_total = n_total,
        n_genotyped = n_genotyped,
        n_e4_carriers = sum(df$e4_carrier, na.rm = TRUE),
        n_e4_homozygotes = sum(df$e4_homozygote, na.rm = TRUE),
        carrier_ratio = if (n_genotyped > 0) sum(df$e4_carrier, na.rm = TRUE) / n_genotyped else NA_real_,
        mean_e4_dosage = if (n_genotyped > 0) mean(df$e4_dosage, na.rm = TRUE) else NA_real_,
        stringsAsFactors = FALSE
    )
}))
summary_df <- summary_df[order(summary_df$Class), ]

write.csv(summary_df, config$output_summary, row.names = FALSE)
write.csv(merged, config$subject_output, row.names = FALSE)

message(sprintf("Class summary written to %s", config$output_summary))
message(sprintf("Subject-level file written to %s", config$subject_output))

if (!config$keep_temporary) unlink(temp_dir, recursive = TRUE, force = TRUE)