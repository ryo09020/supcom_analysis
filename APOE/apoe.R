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

parse_bcftools_version <- function(version_lines) {
	if (length(version_lines) == 0) {
		return(list(raw = NA_character_, num = NA))
	}
	first_line <- version_lines[1]
	match <- regexpr("\\d+(\\.\\d+)+", first_line)
	if (match == -1) {
		return(list(raw = first_line, num = NA))
	}
	numeric_str <- regmatches(first_line, match)
	list(raw = first_line, num = tryCatch(numeric_version(numeric_str), error = function(e) NA))
}

get_bcftools_version <- function(path) {
	ver_lines <- tryCatch(
		system2(path, "--version", stdout = TRUE, stderr = TRUE),
		error = function(e) character(0)
	)
	parse_bcftools_version(ver_lines)
}

detect_query_flags <- function(path) {
	help_lines <- tryCatch(
		system2(path, c("query", "--help"), stdout = TRUE, stderr = TRUE),
		error = function(e) character(0)
	)
	supports <- function(pattern) any(grepl(pattern, help_lines, fixed = TRUE))
	list(
		format = if (supports("--format")) "--format" else "-f",
		include = if (supports("--include")) "--include" else "-i",
		samples = if (supports("--samples-file")) "--samples-file" else "-S"
	)
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
resolved_bcftools <- Sys.which(bcftools_path)
if (!nzchar(resolved_bcftools)) {
	stop(sprintf("bcftools executable '%s' was not found on this system. Load the module or adjust --bcftools.", bcftools_path))
}
bcftools_path <- resolved_bcftools

bcftools_version <- get_bcftools_version(bcftools_path)
if (!is.na(bcftools_version$raw)) {
	message(sprintf("bcftools detected: %s", bcftools_version$raw))
}

min_supported <- numeric_version("1.2")
if (!is.na(bcftools_version$num) && bcftools_version$num < min_supported) {
	stop(sprintf(
		"This workflow requires bcftools >= %s (detected %s). Please load a newer module (1.22+ recommended).",
		as.character(min_supported),
		as.character(bcftools_version$raw)
	))
}

supports_threads_flag <- !is.na(bcftools_version$num) && bcftools_version$num >= numeric_version("1.3")
query_flags <- detect_query_flags(bcftools_path)
format_flag <- query_flags$format
include_flag <- query_flags$include
samples_flag <- query_flags$samples

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
	stop(paste0(
		"No VCF index (.csi or .tbi) was found next to ", cfg$vcf,
		". Please create one manually, e.g.\n",
		"  bcftools index ", cfg$vcf
	))
}

query_file <- file.path(temp_dir, "apoe_genotypes.tsv")
filter_clause <- paste(sprintf('ID=="%s"', snp_ids), collapse = " || ")

thread_flag <- NULL
if (!is.null(cfg$threads) && !is.na(cfg$threads)) {
	if (cfg$threads > 1) {
		if (supports_threads_flag) {
			thread_flag <- sprintf("--threads=%d", cfg$threads)
		} else {
			message("bcftools build does not advertise --threads support; running single-threaded instead.")
		}
	} else {
		message("Note: cfg$threads <= 1 so the --threads flag is omitted. Increase cfg$threads for parallel decoding.")
	}
}

query_args <- c(
	"query",
	thread_flag,
	format_flag, "%ID\t%REF\t%ALT[\t%GT]\n",
	include_flag, filter_clause,
	samples_flag, sample_file,
	cfg$vcf
)

query_args <- query_args[!is.null(query_args) & nzchar(query_args)]

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
