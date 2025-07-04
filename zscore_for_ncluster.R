#################################################################
# LPA Profile Visualization R Script (Horizontal Layout Version)
#################################################################

# 1. Preparation: Load necessary packages
#----------------------------------------------------------------
cat("Step 1: Loading packages...\n")
library(tidyverse)
library(tidyLPA)


#################################################################
# ★★★★★ Specify the number of profiles you want to visualize here ★★★★★
#----------------------------------------------------------------
CHOSEN_N_PROFILES <- 4
#################################################################


# 2. Data Preparation (Identical to the previous analysis)
#----------------------------------------------------------------
cat("Step 2: Preparing data...\n")
my_data <- read_csv("dummy_data.csv")
lpa_target_columns <- c("542690_00", "542700_00", "542710_00", "542720_00", "542730_00")

df_selected <- my_data %>%
  select(all_of(lpa_target_columns)) %>%
  mutate(across(everything(), as.numeric))
df_scaled <- as.data.frame(scale(df_selected))
df_analysis <- na.omit(df_scaled)
cat("Analysis data is ready.\n")


# 3. Re-run LPA and Calculate Fit Indices
#----------------------------------------------------------------
cat("Step 3: Re-running LPA and preparing fit indices...\n")
lpa_models <- estimate_profiles(df_analysis, n_profiles = 2:10, models = 6)
fit_indices <- get_fit(lpa_models)
class_proportions <- get_data(lpa_models) %>%
  count(classes_number, Class) %>%
  group_by(classes_number) %>%
  mutate(proportion = n / sum(n)) %>%
  summarise(`% in each class` = paste(round(proportion * 100), collapse = "/"), .groups = 'drop') %>%
  rename(Profiles = classes_number)
renamed_table <- fit_indices %>%
  rename(Profiles = Classes, `Log-likelihood` = LogLik, `Sample-Size Adjusted BIC` = SABIC)
if ("VLMR_p" %in% colnames(renamed_table)) {
  renamed_table <- renamed_table %>% rename(`VLMR-LRT p-value` = VLMR_p)
} else {
  renamed_table <- renamed_table %>% mutate(`VLMR-LRT p-value` = NA)
}
final_table <- renamed_table %>%
  select(Profiles, `Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`, Entropy, `VLMR-LRT p-value`) %>%
  left_join(class_proportions, by = "Profiles") %>%
  mutate(
    across(c(`Log-likelihood`, AIC, BIC, `Sample-Size Adjusted BIC`), ~round(.x, 2)),
    across(c(Entropy, `VLMR-LRT p-value`), ~round(.x, 3), .names = "{.col}")
  )
cat("Fit indices are ready.\n")


# 4. (For Verification) Display Fit Indices for the Chosen Profile Number
#----------------------------------------------------------------
cat(paste("\n--- Step 4: Fit Indices for the ", CHOSEN_N_PROFILES, "-Cluster Model ---\n", sep=""))
verification_row <- final_table %>%
  filter(Profiles == CHOSEN_N_PROFILES)
print(verification_row)
cat("---------------------------------------------------\n")


# 5. Prepare Data for Plotting
#----------------------------------------------------------------
cat("\nStep 5: Preparing data for plotting...\n")
plot_data <- get_data(lpa_models) %>%
  filter(classes_number == CHOSEN_N_PROFILES) %>%
  pivot_longer(
    cols = -c(model_number, classes_number, Class, Class_prob, Probability, id),
    names_to = "item",
    values_to = "z_score"
  ) %>%
  group_by(Class, item) %>%
  summarise(mean_z_score = mean(z_score), .groups = "drop") %>%
  mutate(item = factor(item, levels = paste0("X", lpa_target_columns)))
cat("Plotting data is ready.\n")
print(plot_data)


# 6. Create the Profile Plot (Bar Chart)
#----------------------------------------------------------------
cat("\nStep 6: Creating the plot...\n")
profile_plot <- ggplot(plot_data, aes(x = item, y = mean_z_score, fill = item)) +
  geom_col(width = 0.7) +
  geom_hline(yintercept = 0, color = "darkgrey") +
  
  # ★★★★★ ここを修正 ★★★★★
  # facet_wrap() の代わりに facet_grid() を使い、グラフを横一列に並べます。
  facet_grid(. ~ Class, labeller = labeller(Class = function(x) paste("Cluster", x))) +
  
  labs(
    title = paste("Profiles for", CHOSEN_N_PROFILES, "Cluster Model"),
    subtitle = "Mean Z-scores for each item within each cluster",
    x = "Item",
    y = "Mean Z-score"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "none"
  )

print(profile_plot)
cat("Plot created successfully.\n")