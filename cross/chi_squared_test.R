################################################################################
# Chi-Squared Test Script: Independence Test for Categorical Variables
# Analysis: Chi-squared Test + Cramer's V (Effect Size)
################################################################################

# --- Install required packages if missing ---
# install.packages(c("tidyverse", "rstatix"))

# --- Load Libraries ---
library(tidyverse)
library(rstatix) # For cramer_v

################################################################################
# 【User Settings】 Change these parameters to match your data
################################################################################

# 1. Path to the input CSV file
csv_file_path <- "cross/test_4class_data_ancova.csv"

# 2. Categorical items to analyze (Vector of column names)
# e.g., c("sex", "apoe_genotype", "diagnosis")
target_columns <- c("sex")

# 3. Column name for the Class/Group variable
group_column_name <- "Class"

# 4. Output CSV file name
output_csv_name <- "chisq_analysis_results.csv"

################################################################################
# End of User Settings
################################################################################

#' Perform Chi-Squared Test and Calculate Cramer's V
#'
#' @param file_path Path to the CSV file
#' @param target_cols Vector of categorical column names to analyze
#' @param group_col Name of the grouping column
#' @return A dataframe containing the analysis results
analyze_chisq <- function(file_path, target_cols, group_col) {
    # Check if file exists
    if (!file.exists(file_path)) {
        stop(paste("Error: File not found ->", file_path))
    }

    # Load data
    df <- read_csv(file_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

    # Check for missing columns
    required_cols <- c(target_cols, group_col)
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
        stop(paste("Error: Missing columns in data ->", paste(missing_cols, collapse = ", ")))
    }

    # Ensure group_col is a factor for proper level handling
    df[[group_col]] <- as.factor(df[[group_col]])

    # Initialize results list
    results_list <- list()

    # Loop through each target column
    for (col in target_cols) {
        # Create contingency table
        # table(Group, Target)
        cont_table <- table(df[[group_col]], df[[col]])

        # --- 1. Descriptive Statistics (Counts & Percentages) ---
        # Calculate counts and percentages for each class
        # Output format example: "Class1: Male 5(50%), Female 5(50%)"
        desc_stats_list <- list()
        classes <- levels(df[[group_col]])

        for (cls in classes) {
            # Subset data for this class
            class_data <- df[df[[group_col]] == cls, ]
            # Count categories
            counts <- table(class_data[[col]])
            total <- sum(counts)
            # Calculate percentages
            percents <- round(counts / total * 100, 1)

            # Format string
            # e.g., "Male: 5(50%)"
            stats_str <- paste(names(counts), ": ", counts, "(", percents, "%)", sep = "", collapse = ", ")
            desc_stats_list[[paste0("Stats_", cls)]] <- stats_str
        }

        # Convert list to dataframe row
        desc_stats_df <- as_tibble(desc_stats_list)

        # --- 2. Main Chi-squared Test ---
        # suppressWarnings to handle small cell counts warning quietly
        chisq_res <- suppressWarnings(chisq.test(cont_table))

        # Calculate Cramer's V
        cramer_res <- rstatix::cramer_v(cont_table)

        # --- 3. Pairwise Comparisons ---
        # Perform Chi-squared test for each pair of classes
        pairwise_p_values <- c()
        pairwise_names <- c()

        # Get all unique pairs of classes
        class_pairs <- combn(classes, 2, simplify = FALSE)

        for (pair in class_pairs) {
            class1 <- pair[1]
            class2 <- pair[2]

            # Subset data for the pair
            pair_df <- df %>% filter(.data[[group_col]] %in% c(class1, class2))
            pair_df[[group_col]] <- droplevels(pair_df[[group_col]])

            # Create table for pair
            pair_table <- table(pair_df[[group_col]], pair_df[[col]])

            # Run Chi-squared test
            pair_res <- suppressWarnings(chisq.test(pair_table))

            pairwise_p_values <- c(pairwise_p_values, pair_res$p.value)
            pairwise_names <- c(pairwise_names, paste(class1, "vs", class2))
        }

        # Apply Bonferroni adjustment
        adj_p_values <- p.adjust(pairwise_p_values, method = "bonferroni")

        # Format pairwise results
        pairwise_str <- paste(
            paste(pairwise_names, "p =", format.pval(adj_p_values, digits = 3, eps = 0.001)),
            collapse = "; "
        )

        # Format results
        current_result <- tibble(
            Item = col
        ) %>%
            bind_cols(desc_stats_df) %>%
            mutate(
                X_squared = chisq_res$statistic,
                df = chisq_res$parameter,
                p_value = chisq_res$p.value,
                Cramers_V = cramer_res,
                Significant = ifelse(chisq_res$p.value < 0.05, "Yes", "No"),
                Pairwise_Comparisons = pairwise_str
            )

        results_list[[col]] <- current_result
    }

    # Combine all results
    final_results <- bind_rows(results_list)
    return(final_results)
}

# --- Main Execution ---

cat("=== Chi-Squared Analysis Started ===\n")
cat(paste("Target File:", csv_file_path, "\n"))

# Run Analysis
tryCatch(
    {
        results_df <- analyze_chisq(
            file_path = csv_file_path,
            target_cols = target_columns,
            group_col = group_column_name
        )

        # Output Summary to Console
        cat("\n--- Analysis Summary ---\n")
        print(results_df)

        # Save to CSV
        write_csv(results_df, output_csv_name)
        cat(paste("\nResults saved to:", output_csv_name, "\n"))
    },
    error = function(e) {
        cat(paste("\nError occurred:\n"))
        print(e)
    }
)

cat("\n=== Done ===\n")
