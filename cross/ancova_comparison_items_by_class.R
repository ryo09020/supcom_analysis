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
    "NEO-FFI" = list(
        name = "NEO-FFI",
        items = list(
            "542640_00" = "Neuroticism",
            "542650_00" = "Extraversion",
            "542660_00" = "Openness",
            "542670_00" = "Agreeableness",
            "542680_00" = "Conscientiousness"
        )
    ),
    "TAC-24" = list(
        name = "TAC-24",
        items = list(
            "542740_00" = "Catharsis",
            "542750_00" = "Giving Up",
            "542760_00" = "Info Seeking",
            "542770_00" = "Distraction",
            "542780_00" = "Avoidant Thinking",
            "542790_00" = "Pos. Reappraisal",
            "542800_00" = "Planning",
            "542810_00" = "Resp. Transfer",
            "542820_00" = "Support Seeking",
            "542830_00" = "Avoidance",
            "542840_00" = "Reappraisal & Distraction"
        )
    ),
    "IES-R" = list(
        name = "IES-R",
        items = list(
            "542850_00" = "Intrusion",
            "542860_00" = "Avoidance",
            "542870_00" = "Hyperarousal",
            "542880_00" = "Total"
        )
    ),
    "J-PSS" = list(
        name = "J-PSS",
        items = list(
            "542890_00" = "Total"
        )
    ),
    "POMS" = list(
        name = "POMS",
        items = list(
            "542900_00" = "Tension-Anxiety",
            "542910_00" = "Depression",
            "542920_00" = "Anger-Hostility",
            "542930_00" = "Vigor",
            "542940_00" = "Fatigue",
            "542950_00" = "Confusion",
            "542960_00" = "TMD"
        )
    ),
    "DASS-15" = list(
        name = "DASS-15",
        items = list(
            "542970_00" = "Depression",
            "542980_00" = "Anxiety",
            "542990_00" = "Stress"
        )
    ),
    "GHQ-30" = list(
        name = "GHQ-30",
        items = list(
            # "543000_00" = "General Illness",
            "543010_00" = "Somatic Symptoms",
            "543020_00" = "Sleep Disturbance",
            "543030_00" = "Social Dysfunction",
            "543040_00" = "Anxiety/Dysphoria",
            "543050_00" = "Severe Depression"
            # "543060_00" = "Total"
        )
    ),
    "SES" = list(
        name = "SES",
        items = list(
            "543070_00" = "Total"
        )
    ),
    "TAS-20" = list(
        name = "TAS-20",
        items = list(
            "543080_00" = "DIF",
            "543090_00" = "DDF",
            "543100_00" = "EOT",
            "543110_00" = "Total"
        )
    ),
    "STSS" = list(
        name = "STSS",
        items = list(
            "543120_00" = "Bodily Sensations",
            "543130_00" = "Over-adaptation",
            "543140_00" = "Poor Health Mgmt",
            "543150_00" = "Total"
        )
    ),
    "Edinburgh" = list(
        name = "Edinburgh",
        items = list(
            "543160_00" = "Total"
        )
    ),
    "MMSE" = list(
        name = "MMSE",
        items = list(
            "516484_00" = "Total"
        )
    ),
    "Verval Fluency" = list(
        name = "Verval Fluency",
        items = list(
            "520000_00" = "Letter fluency task",
            "520010_00" = "Category fluency task"
        )
    ),
    "Digit Symbol Test" = list(
        name = "Digit Symbol Test",
        items = list(
            "520040_00" = "Total"
        )
    ),
    "JART" = list(
        name = "JART",
        items = list(
            "520120_00" = "Error Count",
            "520130_00" = "FSIQ",
            "520140_00" = "VIQ",
            "520150_00" = "PIQ"
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
