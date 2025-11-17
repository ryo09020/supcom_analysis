#!/usr/bin/env Rscript

# --- User-configurable defaults -------------------------------------------
# ここを編集して、対象データや出力ファイルを指定してください。
config <- list(
	vcf = "repository69K_n10388_APOE_2SNPs.bcp.vcf.gz",
	csv = "/path/to/your_id_class_table.csv",
	id_column = "ID",
	class_column = "Class",
	snp_ids = c("rs429358", "rs7412"),
	bcftools = "bcftools",
	output_summary = "apoe_e4_summary_by_class.csv",
	subject_output = "apoe_subject_level_genotypes.csv",
	keep_temporary = FALSE,
	threads = 1
)

dry_run <- identical(tolower(Sys.getenv("APOE_DRY_RUN", "false")), "true")

normalize_snp_ids <- function(x) {
	if (length(x) == 1) {
		x <- unlist(strsplit(x, ",", fixed = TRUE))
	}
	unique(trimws(as.character(x)))
}

cfg <- config
cfg$snp_ids <- normalize_snp_ids(cfg$snp_ids)
cfg$threads <- as.integer(cfg$threads)

if (length(cfg$snp_ids) != 2) {
	stop("This workflow expects exactly two SNP IDs (rs429358 and rs7412 by default).")
}

if (dry_run) {
	message("APOE_DRY_RUN=TRUE detected. Skipping heavy I/O. Current configuration:")
	print(cfg)
	quit(save = "no", status = 0)
}

required_files <- c(cfg$vcf, cfg$csv)
missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
	stop(sprintf("The following required files were not found: %s", paste(missing, collapse = ", ")))
}

bcftools_path <- cfg$bcftools
if (!nzchar(Sys.which(bcftools_path))) {
	stop(sprintf("bcftools executable '%s' was not found on this system. Load the module or adjust --bcftools.", bcftools_path))
}

snp_ids <- cfg$snp_ids

read_input_data <- function(path, id_col, class_col) {
	df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
	if (!(id_col %in% names(df))) {
		stop(sprintf("Column '%s' was not found in %s", id_col, path))
	}
	if (!(class_col %in% names(df))) {
		stop(sprintf("Column '%s' was not found in %s", class_col, path))
	}
	df[[id_col]] <- as.character(df[[id_col]])
	df <- df[!is.na(df[[id_col]]) & df[[id_col]] != "", ]
	df
}

class_df <- read_input_data(cfg$csv, cfg$id_column, cfg$class_column)
sample_ids <- unique(class_df[[cfg$id_column]])

if (length(sample_ids) == 0) {
	stop("No valid IDs were found in the supplied CSV.")
}

temp_dir <- file.path(tempdir(), paste0("apoe_bcftools_", Sys.getpid()))
dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)

sample_file <- file.path(temp_dir, "samples.txt")
writeLines(sample_ids, con = sample_file)

index_candidates <- c(paste0(cfg$vcf, ".csi"), paste0(cfg$vcf, ".tbi"))
index_exists <- any(file.exists(index_candidates))

run_command <- function(cmd, args, stdout = NULL) {
	stderr_output <- system2(cmd, args, stdout = stdout, stderr = TRUE)
	status <- attr(stderr_output, "status")
	if (!is.null(status) && status != 0) {
		stop(sprintf(
			"Command failed (%s %s)\n%s",
			cmd,
			paste(args, collapse = " "),
			paste(stderr_output, collapse = "\n")
		))
	}
	invisible(stderr_output)
}

if (!index_exists) {
	message("Index not found. Creating bcftools index...")
	run_command(
		bcftools_path,
		c("index", sprintf("--threads=%d", cfg$threads), cfg$vcf)
	)
}

query_file <- file.path(temp_dir, "apoe_genotypes.tsv")
filter_clause <- paste(sprintf('ID=="%s"', snp_ids), collapse = " || ")

query_args <- c(
	"query",
	sprintf("--threads=%d", cfg$threads),
	"-f", "%ID\t%REF\t%ALT[\t%GT]\n",
	"-i", filter_clause,
	"-S", sample_file,
	cfg$vcf
)

run_command(bcftools_path, query_args, stdout = query_file)

geno_raw <- tryCatch(
	read.table(query_file, header = FALSE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE),
	error = function(e) stop("Failed to read bcftools query output: ", e$message)
)

expected_cols <- 3 + length(sample_ids)
if (ncol(geno_raw) != expected_cols) {
	stop("Unexpected number of columns in bcftools output. Check that sample IDs match the VCF header.")
}

colnames(geno_raw) <- c("SNP_ID", "REF", "ALT", sample_ids)

missing_snps <- setdiff(snp_ids, unique(geno_raw$SNP_ID))
if (length(missing_snps) > 0) {
	stop(sprintf("The following SNP IDs were not found in the VCF: %s", paste(missing_snps, collapse = ", ")))
}

extract_snp_matrix <- function(target_id) {
	row <- geno_raw[geno_raw$SNP_ID == target_id, , drop = FALSE]
	list(
		ref = toupper(row$REF[1]),
		alt = toupper(row$ALT[1]),
		genotypes = as.character(row[1, sample_ids])
	)
}

snp_data <- lapply(snp_ids, extract_snp_matrix)
names(snp_data) <- snp_ids

get_base <- function(code, ref, alt_string) {
	if (is.null(code) || is.na(code) || code == ".") {
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
	if (length(parts) == 1) {
		parts <- rep(parts, 2)
	}
	alleles <- vapply(parts, get_base, character(1), ref = ref, alt_string = alt)
	toupper(alleles)
}

calc_e4_dosage <- function(gt1, gt2, ref1, alt1, ref2, alt2) {
	alleles1 <- decode_gt(gt1, ref1, alt1)
	alleles2 <- decode_gt(gt2, ref2, alt2)
	if (all(is.na(alleles1)) || all(is.na(alleles2))) {
		return(NA_real_)
	}
	count_c1 <- sum(alleles1 == "C", na.rm = TRUE)
	count_c2 <- sum(alleles2 == "C", na.rm = TRUE)
	min(count_c1, count_c2)
}

subject_level <- data.frame(
	SAMPLE = sample_ids,
	genotype_rs429358 = snp_data[[snp_ids[1]]]$genotypes,
	genotype_rs7412 = snp_data[[snp_ids[2]]]$genotypes,
	stringsAsFactors = FALSE
)

subject_level$e4_dosage <- mapply(
	calc_e4_dosage,
	subject_level$genotype_rs429358,
	subject_level$genotype_rs7412,
	MoreArgs = list(
		ref1 = snp_data[[snp_ids[1]]]$ref,
		alt1 = snp_data[[snp_ids[1]]]$alt,
		ref2 = snp_data[[snp_ids[2]]]$ref,
		alt2 = snp_data[[snp_ids[2]]]$alt
	)
)

subject_level$e4_carrier <- subject_level$e4_dosage > 0
subject_level$e4_homozygote <- subject_level$e4_dosage == 2

merged <- merge(
	class_df,
	subject_level,
	by.x = cfg$id_column,
	by.y = "SAMPLE",
	all.x = TRUE
)

summaries <- lapply(split(merged, merged[[cfg$class_column]]), function(df) {
	df <- df[!is.na(df[[cfg$id_column]]), , drop = FALSE]
	n_total <- nrow(df)
	n_genotyped <- sum(!is.na(df$e4_dosage))
	n_carrier <- sum(df$e4_carrier, na.rm = TRUE)
	n_homo <- sum(df$e4_homozygote, na.rm = TRUE)
	mean_dosage <- if (n_genotyped > 0) mean(df$e4_dosage, na.rm = TRUE) else NA_real_
	data.frame(
		Class = df[[cfg$class_column]][1],
		n_total = n_total,
		n_genotyped = n_genotyped,
		n_e4_carriers = n_carrier,
		n_e4_homozygotes = n_homo,
		carrier_ratio_genotyped = if (n_genotyped > 0) n_carrier / n_genotyped else NA_real_,
		carrier_ratio_total = if (n_total > 0) n_carrier / n_total else NA_real_,
		mean_e4_dosage = mean_dosage,
		stringsAsFactors = FALSE
	)
})

summary_df <- do.call(rbind, summaries)
summary_df <- summary_df[order(summary_df$Class), ]

write.csv(summary_df, cfg$output_summary, row.names = FALSE)
write.csv(merged, cfg$subject_output, row.names = FALSE)

message(sprintf("Class summary written to %s", cfg$output_summary))
message(sprintf("Subject-level file written to %s", cfg$subject_output))

if (!cfg$keep_temporary) {
	unlink(temp_dir, recursive = TRUE, force = TRUE)
} else {
	message(sprintf("Temporary files retained in %s", temp_dir))
}
