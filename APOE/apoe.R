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
    output_chisq = "apoe_chisq_results.csv",
    keep_temporary = FALSE
)

# --- Library Loading ---
# We need library(rstatix) for Cramer's V if available, otherwise manual calculation
if (!requireNamespace("rstatix", quietly = TRUE)) {
    warning("Package 'rstatix' not found. Cramer's V will be calculated manually or skipped.")
}

# ... (Existing Input Check Code) ...

# --- Helper Functions for Chi-Squared Analysis ---

# Function to calculate Cramer's V manually if rstatix is missing
calc_cramers_v <- function(tbl) {
    if (requireNamespace("rstatix", quietly = TRUE)) {
        return(rstatix::cramer_v(tbl))
    } else {
        chisq <- suppressWarnings(chisq.test(tbl)$statistic)
        n <- sum(tbl)
        k <- min(dim(tbl)) - 1
        return(sqrt(chisq / (n * k)))
    }
}

# Function to perform Pairwise Chi-squared tests
perform_pairwise_chisq <- function(df, group_col, target_col, is_allele_data = FALSE) {
    classes <- unique(as.character(df[[group_col]]))
    classes <- classes[!is.na(classes)]

    pairwise_p_values <- c()
    pairwise_names <- c()

    class_pairs <- combn(classes, 2, simplify = FALSE)

    for (pair in class_pairs) {
        c1 <- pair[1]
        c2 <- pair[2]

        if (is_allele_data) {
            # For allele data: df has columns Class, Allele
            sub_df <- df[df[[group_col]] %in% c(c1, c2), ]
            tbl <- table(sub_df[[group_col]], sub_df[[target_col]])
        } else {
            # For subject data: df has columns Class, Target (e.g. e4_carrier)
            sub_df <- df[df[[group_col]] %in% c(c1, c2), ]
            tbl <- table(sub_df[[group_col]], sub_df[[target_col]])
        }

        # Remove empty rows/cols
        tbl <- tbl[rowSums(tbl) > 0, colSums(tbl) > 0, drop = FALSE]

        if (nrow(tbl) < 2 || ncol(tbl) < 2) {
            p_val <- NA
        } else {
            p_val <- suppressWarnings(chisq.test(tbl)$p.value)
        }

        pairwise_p_values <- c(pairwise_p_values, p_val)
        pairwise_names <- c(pairwise_names, paste(c1, "vs", c2))
    }

    # Bonferroni adjustment
    adj_p_values <- p.adjust(pairwise_p_values, method = "bonferroni")

    paste(
        paste(pairwise_names, "p =", format.pval(adj_p_values, digits = 3, eps = 0.001)),
        collapse = "; "
    )
}

# ... (Existing VCF Processing Code) ...

# --- APOE Allele Determination ---
# rs429358 (C/T), rs7412 (C/T)
# e2: T, T
# e3: T, C
# e4: C, C
# Note: This logic assumes forward strand and specific REF/ALT.
# We need to map the actual genotypes (e.g. "C/C", "C/T") to alleles.
# Since we already have e4_dosage, we can infer some, but for full e2/e3/e4 we need to look at the combinations.

# Helper to determine alleles for a single subject
# Returns a vector of 2 alleles (e.g. c("e3", "e4"))
determine_alleles <- function(gt1, gt2, ref1, alt1, ref2, alt2) {
    # Decode genotypes to bases
    bases1 <- decode_gt(gt1, ref1, alt1) # rs429358
    bases2 <- decode_gt(gt2, ref2, alt2) # rs7412

    if (any(is.na(bases1)) || any(is.na(bases2))) {
        return(c(NA, NA))
    }

    # Haplotype estimation is tricky without phased data.
    # However, for APOE, the common alleles are:
    # e2: rs429358=T, rs7412=T
    # e3: rs429358=T, rs7412=C
    # e4: rs429358=C, rs7412=C
    # (rs429358=C, rs7412=T is rare e1 or similar, usually ignored or treated as rare)

    # We have unphased genotypes.
    # e.g. rs429358 = C/T, rs7412 = C/C
    # Possible pairs: (C,C) + (T,C) -> e4 + e3

    # Let's count C's and T's
    # rs429358 (112): C is associated with e4. T is e2/e3.
    # rs7412 (158): C is e3/e4. T is e2.

    # Simplified logic based on genotypes:
    # e2/e2: 429358=T/T, 7412=T/T
    # e2/e3: 429358=T/T, 7412=C/T
    # e2/e4: 429358=C/T, 7412=C/T (Ambiguous with e1/e3? But e2/e4 is more likely)
    # e3/e3: 429358=T/T, 7412=C/C
    # e3/e4: 429358=C/T, 7412=C/C
    # e4/e4: 429358=C/C, 7412=C/C

    # Count C alleles for each SNP
    c1 <- sum(bases1 == "C") # rs429358 (C=Arg, T=Cys) -> C is e4
    t2 <- sum(bases2 == "T") # rs7412 (C=Arg, T=Cys) -> T is e2

    # Determine Genotype String
    # e4 count = c1
    # e2 count = t2
    # e3 count = 2 - c1 - t2

    alleles <- c()
    if (c1 > 0) alleles <- c(alleles, rep("e4", c1))
    if (t2 > 0) alleles <- c(alleles, rep("e2", t2))
    rem <- 2 - length(alleles)
    if (rem > 0) alleles <- c(alleles, rep("e3", rem))

    return(sort(alleles))
}

# Apply to subject_level
allele_list <- mapply(
    determine_alleles,
    subject_level$genotype_rs429358,
    subject_level$genotype_rs7412,
    MoreArgs = list(ref1 = ref1, alt1 = alt1, ref2 = ref2, alt2 = alt2),
    SIMPLIFY = FALSE
)

subject_level$Allele1 <- sapply(allele_list, function(x) x[1])
subject_level$Allele2 <- sapply(allele_list, function(x) x[2])
subject_level$APOE_Genotype <- sapply(allele_list, function(x) paste(x, collapse = "/"))

# Merge again with class_df to include new columns
merged <- merge(class_df, subject_level, by.x = config$id_column, by.y = "SAMPLE", all.x = TRUE)

# ... (Existing Summary Code) ...

# --- Chi-Squared Analysis ---

chisq_results_list <- list()

# 1. e4 Carrier Status (Yes/No)
# Create table
tbl_carrier <- table(merged[[config$class_column]], merged$e4_carrier)
# Remove NA
tbl_carrier <- tbl_carrier[, colnames(tbl_carrier) %in% c("FALSE", "TRUE"), drop = FALSE]

if (ncol(tbl_carrier) > 1 && nrow(tbl_carrier) > 1) {
    chisq_carrier <- suppressWarnings(chisq.test(tbl_carrier))
    cramer_carrier <- calc_cramers_v(tbl_carrier)
    pairwise_carrier <- perform_pairwise_chisq(merged, config$class_column, "e4_carrier")

    chisq_results_list[["e4_Carrier"]] <- data.frame(
        Item = "e4_Carrier",
        X_squared = chisq_carrier$statistic,
        df = chisq_carrier$parameter,
        p_value = chisq_carrier$p.value,
        Cramers_V = cramer_carrier,
        Significant = ifelse(chisq_carrier$p.value < 0.05, "Yes", "No"),
        Pairwise_Comparisons = pairwise_carrier,
        stringsAsFactors = FALSE
    )
}

# 2. Allele Counts (e2, e3, e4)
# We need to reshape to long format: Class, Allele
# Each subject contributes 2 alleles
alleles_long <- rbind(
    data.frame(Class = merged[[config$class_column]], Allele = merged$Allele1),
    data.frame(Class = merged[[config$class_column]], Allele = merged$Allele2)
)
alleles_long <- alleles_long[!is.na(alleles_long$Allele), ]

tbl_allele <- table(alleles_long$Class, alleles_long$Allele)

if (ncol(tbl_allele) > 1 && nrow(tbl_allele) > 1) {
    chisq_allele <- suppressWarnings(chisq.test(tbl_allele))
    cramer_allele <- calc_cramers_v(tbl_allele)
    pairwise_allele <- perform_pairwise_chisq(alleles_long, "Class", "Allele", is_allele_data = TRUE)

    chisq_results_list[["Allele_Distribution"]] <- data.frame(
        Item = "Allele_Distribution",
        X_squared = chisq_allele$statistic,
        df = chisq_allele$parameter,
        p_value = chisq_allele$p.value,
        Cramers_V = cramer_allele,
        Significant = ifelse(chisq_allele$p.value < 0.05, "Yes", "No"),
        Pairwise_Comparisons = pairwise_allele,
        stringsAsFactors = FALSE
    )
}

# Combine and Save
if (length(chisq_results_list) > 0) {
    chisq_df <- do.call(rbind, chisq_results_list)
    write.csv(chisq_df, config$output_chisq, row.names = FALSE)
    message(sprintf("Chi-squared results written to %s", config$output_chisq))
} else {
    message("No Chi-squared tests could be performed (insufficient data).")
}

# ... (Cleanup Code) ...
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

# (一時ディレクトリをスペースなしで作成)
temp_dir <- file.path(getwd(), "apoe_temp_dir_fixed")
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
message(sprintf("Using temporary directory: %s", temp_dir))

# --- VCF/CSV ID照合 (system() を使用) ------------------
message("Listing samples from VCF header (using system())...")

vcf_list_file <- file.path(temp_dir, "vcf_sample_list.txt")
vcf_list_stderr <- file.path(temp_dir, "vcf_list.stderr.txt")

vcf_list_cmd <- paste(
    bcftools_path, "query", "-l", config$vcf,
    ">", vcf_list_file,
    "2>", vcf_list_stderr
)
vcf_list_status <- system(vcf_list_cmd)

if (vcf_list_status != 0) {
    stderr_output <- readLines(vcf_list_stderr)
    stop(paste(c("bcftools query -l failed via system().", "Captured stderr output:", stderr_output), collapse = "\n"))
}

vcf_sample_ids <- readLines(vcf_list_file)
vcf_sample_ids <- trimws(vcf_sample_ids)

# CSVとVCFの両方に存在するID（共通ID）を抽出
common_sample_ids <- intersect(csv_sample_ids, vcf_sample_ids)

if (!length(common_sample_ids)) {
    stop("No common sample IDs found between the CSV and the VCF file.")
}
message(sprintf("Found %d total IDs in CSV.", length(csv_sample_ids)))
message(sprintf("Found %d total (true) IDs in VCF.", length(vcf_sample_ids)))
message(sprintf("Proceeding with %d common IDs.", length(common_sample_ids)))


# --- bcftools query (system() を使用) ------------------------------------
apoe_region <- "chr19:44908684-44908822"

# ★ 修正: 共通IDのリストを書き出す
sample_file <- file.path(temp_dir, "common_samples.txt")
writeLines(common_sample_ids, sample_file)


query_file <- file.path(temp_dir, "apoe_genotypes.tsv")
stderr_file <- file.path(temp_dir, "bcftools.stderr.txt") # エラー出力用

# ★★★ 最終修正点 ★★★
# 1. format.txt は使わず、フォーマット文字列をR内で定義
format_string <- "%ID\t%REF\t%ALT[\t%GT]\n"

# 2. cmd_string で、-f にファイルパスではなく shQuote() で保護した文字列を渡す
cmd_string <- paste(
    bcftools_path,
    "query",
    "-f", shQuote(format_string), # ★ 修正
    "-r", shQuote(apoe_region),
    "-S", sample_file,
    config$vcf
)

# 3. リダイレクション
full_command <- paste(
    cmd_string,
    ">", query_file,
    "2>", stderr_file
)

message(sprintf("Executing system() command (inline format string)..."))
status <- system(full_command) # system() はステータスコード (0=成功) を返す

# 4. エラーチェック
if (status != 0) {
    stderr_output <- readLines(stderr_file)
    stop(paste(c("bcftools query failed via system(). Exit status was not 0.", "Captured stderr output:", stderr_output), collapse = "\n"))
}
if (!file.exists(query_file) || file.info(query_file)$size == 0) {
    stderr_output <- if (file.exists(stderr_file)) readLines(stderr_file) else "No stderr file found."
    stop(paste(c("bcftools query FAILED to create the output file.", "Captured stderr output:", stderr_output), collapse = "\n"))
}

# --- 読み込み -------------------------------------------------------------

# (期待する列数は 3 + 共通IDの数)
expected_cols <- 3 + length(common_sample_ids)

geno_raw <- read.table(query_file,
    header = FALSE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = "character"
)

# 2. 列数チェック (★ ここが 9907 == 9907 となるはず)
if (ncol(geno_raw) != expected_cols) {
    stop(sprintf("Unexpected bcftools output columns. Expected %d (3+common samples), but got %d.", expected_cols, ncol(geno_raw)))
}

# 3. 「共通ID」を列名として設定
colnames(geno_raw) <- c("SNP_ID", "REF", "ALT", common_sample_ids)

geno_filtered <- geno_raw

# --- APOE e4 計算 (変更なし) ------------------------------------------
get_base <- function(code, ref, alt_string) {
    if (is.na(code) || code == ".") {
        return(NA_character_)
    }
    idx <- suppressWarnings(as.integer(code))
    if (is.na(idx)) {
        return(NA_character_)
    }
    if (idx == 0) {
        return(ref)
    }
    alt_vec <- strsplit(alt_string, ",", fixed = TRUE)[[1]]
    if (idx > length(alt_vec)) {
        return(NA_character_)
    }
    alt_vec[idx]
}

decode_gt <- function(gt, ref, alt) {
    if (is.na(gt) || gt %in% c(".", "./.", ".|.")) {
        return(rep(NA_character_, 2))
    }
    sep <- if (grepl("|", gt, fixed = TRUE)) "\\|" else "/"
    parts <- strsplit(gt, sep)[[1]]
    if (length(parts) == 1) parts <- rep(parts, 2)
    vapply(parts, get_base, character(1), ref = ref, alt_string = alt)
}

calc_e4_dosage <- function(gt1, gt2, ref1, alt1, ref2, alt2) {
    alleles1 <- decode_gt(gt1, ref1, alt1)
    alleles2 <- decode_gt(gt2, ref2, alt2)
    if (all(is.na(alleles1)) || all(is.na(alleles2))) {
        return(NA_real_)
    }
    min(sum(alleles1 == "C", na.rm = TRUE), sum(alleles2 == "C", na.rm = TRUE))
}

snp_rows <- split(geno_filtered, geno_filtered$SNP_ID)
if (!all(config$snp_ids %in% names(snp_rows))) {
    stop("Requested SNP IDs not found in VCF query output (within the specified region).")
}

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

# --- APOE Allele Determination ---
# Apply determine_alleles to subject_level
allele_list <- mapply(
    determine_alleles,
    subject_level$genotype_rs429358,
    subject_level$genotype_rs7412,
    MoreArgs = list(ref1 = ref1, alt1 = alt1, ref2 = ref2, alt2 = alt2),
    SIMPLIFY = FALSE
)

subject_level$Allele1 <- sapply(allele_list, function(x) x[1])
subject_level$Allele2 <- sapply(allele_list, function(x) x[2])
subject_level$APOE_Genotype <- sapply(allele_list, function(x) paste(x, collapse = "/"))

merged <- merge(class_df, subject_level, by.x = config$id_column, by.y = "SAMPLE", all.x = TRUE)

summary_df <- do.call(rbind, lapply(split(merged, merged[[config$class_column]]), function(df) {
    df <- df[!is.na(df[[id_column]]), , drop = FALSE]
    n_total <- nrow(df)
    n_genotyped <- sum(!is.na(df$e4_dosage))
    data.frame(
        Class = df[[config$class_column]][1],
        n_total = n_total,
        n_genotyped = n_genotyped,
        n_e4_carriers = sum(df$e4_carrier, na.rm = TRUE),
        n_e4_homozygotes = sum(df$e4_homozygote, na.rm = TRUE),
        carrier_ratio = if (n_genotyped > 0) sum(df$e4_carrier, na.rm = TRUE) / n_genotyped else NA_real_,
        homozygote_ratio = if (n_genotyped > 0) sum(df$e4_homozygote, na.rm = TRUE) / n_genotyped else NA_real_,
        mean_e4_dosage = if (n_genotyped > 0) mean(df$e4_dosage, na.rm = TRUE) else NA_real_,
        stringsAsFactors = FALSE
    )
}))
summary_df <- summary_df[order(summary_df$Class), ]

write.csv(summary_df, config$output_summary, row.names = FALSE)
write.csv(merged, config$subject_output, row.names = FALSE)

message(sprintf("Class summary written to %s", config$output_summary))
message(sprintf("Subject-level file written to %s", config$subject_output))

# --- Chi-Squared Analysis ---

chisq_results_list <- list()

# 1. e4 Carrier Status (Yes/No)
# Create table
tbl_carrier <- table(merged[[config$class_column]], merged$e4_carrier)
# Remove NA
tbl_carrier <- tbl_carrier[, colnames(tbl_carrier) %in% c("FALSE", "TRUE"), drop = FALSE]

if (ncol(tbl_carrier) > 1 && nrow(tbl_carrier) > 1) {
    chisq_carrier <- suppressWarnings(chisq.test(tbl_carrier))
    cramer_carrier <- calc_cramers_v(tbl_carrier)
    pairwise_carrier <- perform_pairwise_chisq(merged, config$class_column, "e4_carrier")

    chisq_results_list[["e4_Carrier"]] <- data.frame(
        Item = "e4_Carrier",
        X_squared = chisq_carrier$statistic,
        df = chisq_carrier$parameter,
        p_value = chisq_carrier$p.value,
        Cramers_V = cramer_carrier,
        Significant = ifelse(chisq_carrier$p.value < 0.05, "Yes", "No"),
        Pairwise_Comparisons = pairwise_carrier,
        stringsAsFactors = FALSE
    )
}

# 2. Allele Counts (e2, e3, e4)
# We need to reshape to long format: Class, Allele
# Each subject contributes 2 alleles
alleles_long <- rbind(
    data.frame(Class = merged[[config$class_column]], Allele = merged$Allele1),
    data.frame(Class = merged[[config$class_column]], Allele = merged$Allele2)
)
alleles_long <- alleles_long[!is.na(alleles_long$Allele), ]

tbl_allele <- table(alleles_long$Class, alleles_long$Allele)

if (ncol(tbl_allele) > 1 && nrow(tbl_allele) > 1) {
    chisq_allele <- suppressWarnings(chisq.test(tbl_allele))
    cramer_allele <- calc_cramers_v(tbl_allele)
    pairwise_allele <- perform_pairwise_chisq(alleles_long, "Class", "Allele", is_allele_data = TRUE)

    chisq_results_list[["Allele_Distribution"]] <- data.frame(
        Item = "Allele_Distribution",
        X_squared = chisq_allele$statistic,
        df = chisq_allele$parameter,
        p_value = chisq_allele$p.value,
        Cramers_V = cramer_allele,
        Significant = ifelse(chisq_allele$p.value < 0.05, "Yes", "No"),
        Pairwise_Comparisons = pairwise_allele,
        stringsAsFactors = FALSE
    )
}

# Combine and Save
if (length(chisq_results_list) > 0) {
    chisq_df <- do.call(rbind, chisq_results_list)
    write.csv(chisq_df, config$output_chisq, row.names = FALSE)
    message(sprintf("Chi-squared results written to %s", config$output_chisq))
} else {
    message("No Chi-squared tests could be performed (insufficient data).")
}

if (!config$keep_temporary) {
    message(sprintf("Cleaning up temporary directory: %s", temp_dir))
    unlink(temp_dir, recursive = TRUE, force = TRUE)
}
