################################################################################
# ANCOVA Analysis Script: Comparison of Items by Class with Covariates
# Covariates: age, sex, edu_num
# Analysis: ANCOVA (Type III SS) + Planned Pairwise Comparisons (Bonferroni)
################################################################################

# --- Install required packages if missing ---
# install.packages(c("tidyverse", "car", "emmeans"))

# --- Load Libraries ---
library(tidyverse)
library(car) # For Anova (Type III SS)
library(emmeans) # For estimated marginal means and pairwise comparisons

################################################################################
# 【User Settings】 Change these parameters to match your data
################################################################################

# 1. Path to the input CSV file
csv_file_path <- "cross/test_4class_data_ancova.csv"

# 2. Scale Configuration
# Define scales and their corresponding items
SCALE_CONFIG <- list(
    "CES-D" = list(
        name = "CES-D",
        items = list(
            "423286_00" = "CES-D"
        )
    ),
    "GDS-15" = list(
        name = "GDS-15",
        items = list(
            "520760_00" = "GDS-15"
        )
    ),
    "QIDS-SR-J" = list(
        name = "QIDS-SR-J",
        items = list(
            "426747_00" = "QIDS-SR-J"
        )
    ),
    "IES-R" = list(
        name = "IES-R",
        items = list(
            "541152_00" = "Intrusion",
            "541153_00" = "Avoidance",
            "541154_00" = "Hyperarousal",
            "541155_00" = "Total"
        )
    ),
    "GHQ-30" = list(
        name = "GHQ-30",
        items = list(
            "542002_00" = "General Illness",
            "542003_00" = "Somatic Symptoms",
            "542004_00" = "Sleep Disturbance",
            "542005_00" = "Social Dysfunction",
            "542006_00" = "Anxiety/Dysphoria",
            "542007_00" = "Severe Depression",
            "542008_00" = "Total"
        )
    ),
    "MoCa-J" = list(
        name = "MoCa-J",
        items = list(
            "516464_00" = "Total"
        )
    ),
    "MMSE" = list(
        name = "MMSE",
        items = list(
            "516484_00" = "Total"
        )
    )
)

# Extract all items to analyze from the config
columns_to_analyze <- unlist(lapply(SCALE_CONFIG, function(x) names(x$items)))

# Create a lookup dataframe for Scale and Subscale names
item_lookup <- bind_rows(lapply(names(SCALE_CONFIG), function(scale_key) {
    scale_info <- SCALE_CONFIG[[scale_key]]
    items <- scale_info$items
    tibble(
        Item = names(items),
        Scale = scale_info$name,
        Subscale = unlist(items)
    )
}))

# 3. Column name for the Class/Group variable
class_column_name <- "Class"

# 4. Covariate column names (Must exist in the CSV)
covariate_cols <- c("age", "sex", "edu_num")

# 5. Output CSV file name
output_csv_name <- "ancova_analysis_results.csv"

################################################################################
# End of User Settings
################################################################################

#' Perform ANCOVA and Pairwise Comparisons for specified items
#'
#' @param file_path Path to the CSV file
#' @param columns_to_test Vector of item names to analyze
#' @param class_column Name of the class column
#' @param covariates Vector of covariate column names
#' @return A dataframe containing the analysis results
analyze_ancova_by_class <- function(file_path, columns_to_test, class_column, covariates) {
    # Check if file exists
    if (!file.exists(file_path)) {
        stop(paste("Error: File not found ->", file_path))
    }

    # Load data
    df <- read_csv(file_path, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

    # Check for missing columns
    # Instead of stopping, we will only analyze columns that exist in the data
    available_cols <- intersect(columns_to_test, names(df))
    missing_cols <- setdiff(columns_to_test, names(df))

    if (length(available_cols) == 0) {
        stop("Error: None of the specified items were found in the data.")
    }

    if (length(missing_cols) > 0) {
        cat(paste("Warning: The following items were not found and will be skipped:", paste(missing_cols, collapse = ", "), "\n"))
    }

    # Update columns_to_test to only include available columns
    columns_to_test <- available_cols

    # Check for missing covariates (these are strictly required)
    missing_covariates <- setdiff(covariates, names(df))
    if (length(missing_covariates) > 0) {
        stop(paste("Error: Missing covariate columns ->", paste(missing_covariates, collapse = ", ")))
    }

    # Data Preprocessing
    # Convert Class and sex to factors
    df[[class_column]] <- as.factor(df[[class_column]])
    if ("sex" %in% names(df)) {
        df$sex <- as.factor(df$sex)
    }

    # Ensure numeric covariates are numeric
    numeric_covariates <- setdiff(covariates, "sex")
    for (cov in numeric_covariates) {
        df[[cov]] <- as.numeric(df[[cov]])
    }

    # Initialize results list
    results_list <- list()

    # Loop through each item
    for (col in columns_to_test) {
        # --- 1. Descriptive Statistics (Raw Means) ---
        desc_stats <- df %>%
            group_by(across(all_of(class_column))) %>%
            summarise(
                Mean = mean(as.numeric(.data[[col]]), na.rm = TRUE),
                SD = sd(as.numeric(.data[[col]]), na.rm = TRUE),
                N = sum(!is.na(.data[[col]])),
                .groups = "drop"
            ) %>%
            pivot_wider(
                names_from = all_of(class_column),
                values_from = c("Mean", "SD", "N"),
                names_sep = "_"
            )

        # --- 2. ANCOVA Model ---
        # Formula: Item ~ Class + age + sex + edu_num
        # We use backticks for the item name in case it starts with a number
        formula_str <- paste0("`", col, "` ~ ", class_column, " + ", paste(covariates, collapse = " + "))
        model <- lm(as.formula(formula_str), data = df)

        # Type III ANOVA for F-test
        # We use Type III because we have unbalanced designs usually and covariates
        anova_res <- car::Anova(model, type = 3)

        # Extract F and p for the Class effect
        # The row name for Class effect usually matches class_column
        class_row <- which(rownames(anova_res) == class_column)
        if (length(class_row) == 0) {
            # Fallback if name doesn't match exactly (unlikely but possible)
            class_row <- grep(class_column, rownames(anova_res))[1]
        }

        if (is.na(class_row)) {
            stop(paste("Could not find Class row in Anova results for item:", col))
        }

        # Check column names for F value (it can be "F" or "F value")
        f_col <- intersect(c("F", "F value"), colnames(anova_res))
        if (length(f_col) > 0) {
            f_value <- anova_res[class_row, f_col[1]]
        } else {
            f_value <- NA
        }

        p_value_ancova <- anova_res[class_row, "Pr(>F)"]

        # --- 2b. Effect Size (Partial Eta Squared) ---
        # eta_p^2 = SS_effect / (SS_effect + SS_residuals)
        ss_effect <- anova_res[class_row, "Sum Sq"]
        ss_resid <- anova_res["Residuals", "Sum Sq"]
        partial_eta_sq <- ss_effect / (ss_effect + ss_resid)

        # --- 3. Adjusted Means & Pairwise Comparisons ---
        # Calculate Estimated Marginal Means (EMMs) for Class
        emm <- emmeans(model, specs = as.formula(paste("~", class_column)))

        # Extract Adjusted Means
        emm_df <- as.data.frame(emm)
        # Pivot to wide format: Adj_Mean_Class1, Adj_Mean_Class2, ...
        adj_means_wide <- emm_df %>%
            select(all_of(class_column), emmean) %>%
            pivot_wider(
                names_from = all_of(class_column),
                values_from = emmean,
                names_prefix = "Adj_Mean_"
            )

        # Pairwise comparisons with Bonferroni adjustment
        pairs_res <- contrast(emm, method = "pairwise", adjust = "bonferroni")
        pairs_summary <- summary(pairs_res)

        # Calculate Cohen's d for pairwise comparisons
        # sigma(model) is the RMSE (residual standard deviation)
        eff_size_res <- emmeans::eff_size(emm, sigma = sigma(model), edf = df.residual(model), method = "pairwise")
        eff_size_summary <- summary(eff_size_res)

        # Merge p-values/estimates with effect sizes
        # Note: eff_size contrasts might be ordered differently or same. Usually same if method="pairwise".
        # We'll join by contrast name just to be safe, or assume order.
        # eff_size_summary has columns: contrast, effect.size, ...

        # Join pairs_summary and eff_size_summary
        # pairs_summary: contrast, estimate, SE, df, t.ratio, p.value
        combined_pairs <- left_join(
            as.data.frame(pairs_summary),
            as.data.frame(eff_size_summary) %>% select(contrast, effect.size),
            by = "contrast"
        )

        # Format pairwise results
        # "1 - 2: Diff=..., p=..., d=..."
        pairwise_str <- paste(
            paste0(
                combined_pairs$contrast,
                ": Diff=", round(combined_pairs$estimate, 2),
                ", p=", format.pval(combined_pairs$p.value, digits = 3, eps = 0.001),
                ", d=", round(combined_pairs$effect.size, 2)
            ),
            collapse = "; "
        )

        # --- 4. Combine Results ---
        current_result <- tibble(Item = col) %>%
            bind_cols(desc_stats) %>%
            bind_cols(adj_means_wide) %>%
            mutate(
                F_Value = f_value,
                p_value_ANCOVA = p_value_ancova,
                Partial_Eta_Squared = partial_eta_sq,
                Significant = ifelse(p_value_ancova < 0.05, "Yes", "No"),
                Pairwise_Comparisons = pairwise_str
            )

        results_list[[col]] <- current_result
    }

    # Combine all results
    final_results <- bind_rows(results_list)

    # Add Scale and Subscale information
    if (exists("item_lookup")) {
        final_results <- final_results %>%
            left_join(item_lookup, by = "Item") %>%
            relocate(Scale, Subscale, .before = Item)
    }

    return(final_results)
}

# --- Main Execution ---

cat("=== ANCOVA Analysis Started ===\n")
cat(paste("Target File:", csv_file_path, "\n"))

# Run Analysis
tryCatch(
    {
        results_df <- analyze_ancova_by_class(
            file_path = csv_file_path,
            columns_to_test = columns_to_analyze,
            class_column = class_column_name,
            covariates = covariate_cols
        )

        # Output Summary to Console
        cat("\n--- Analysis Summary ---\n")

        print(results_df %>% select(Item, F_Value, p_value_ANCOVA, Significant))

        # Save to CSV
        write_csv(results_df, output_csv_name)
        cat(paste("\nResults saved to:", output_csv_name, "\n"))

        # Highlight Significant Items
        sig_items <- results_df %>%
            filter(Significant == "Yes") %>%
            pull(Item)
        if (length(sig_items) > 0) {
            cat("\nSignificant Items (p < 0.05):\n")
            print(sig_items)
        } else {
            cat("\nNo significant items found.\n")
        }
    },
    error = function(e) {
        cat(paste("\nError occurred:\n"))
        print(e)
    }
)

cat("\n=== Done ===\n")
