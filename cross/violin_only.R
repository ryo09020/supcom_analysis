#!/usr/bin/env Rscript

# Minimal violin-plot script extracted from cluster_zscore_barplot.R
# - Reads a CSV, keeps specified target items, and plots violin+boxplot per profile.
# - X-axis labels rotated 45 degrees (same idea as cross/vioplot.R) to avoid overlap.

suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(tidyr)
    library(ggplot2)
    library(viridis)
})

# ----------------------------
# User settings
# ----------------------------
file_path <- "final_ver/raw_data/dummy_data_with_clusters_sorted.csv"  # input CSV
cluster_column <- "Class"                                            # cluster col name
# items to plot
target_items <- c("X542690_00", "X542700_00", "X542710_00", "X542720_00", "X542730_00")
# optional display labels (same length as target_items)
target_item_labels <- c(
    "X542690_00",
    "X542700_00",
    "X542710_00",
    "X542720_00",
    "X542730_00"
)
plot_title <- "Violin plot of adjusted raw scores"
output_file <- "violin_only.png"  # set NULL if you do not want to save

# ----------------------------
# Load data
# ----------------------------
if (!file.exists(file_path)) stop(sprintf("File not found: %s", file_path))

data <- read_csv(file_path, show_col_types = FALSE)

required_cols <- c(cluster_column, target_items)
missing_cols <- setdiff(required_cols, names(data))
if (length(missing_cols) > 0) stop(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))

# cluster factor -> "Profile X"
data[[cluster_column]] <- as.factor(data[[cluster_column]])
levels(data[[cluster_column]]) <- paste0("Profile ", levels(data[[cluster_column]]))

# labels lookup
if (length(target_item_labels) != length(target_items)) {
    target_item_labels <- target_items
}
label_lookup <- setNames(target_item_labels, target_items)

# ----------------------------
# Long data for violin plot
# ----------------------------
plot_data <- data %>%
    select(all_of(required_cols)) %>%
    pivot_longer(cols = all_of(target_items), names_to = "Item", values_to = "Value") %>%
    mutate(
        Cluster = .data[[cluster_column]],
        Item_Label = factor(dplyr::recode(as.character(Item), !!!label_lookup), levels = target_item_labels)
    ) %>%
    filter(!is.na(Value), !is.na(Cluster))

# ----------------------------
# Violin plot
# ----------------------------
violin_plot <- ggplot(plot_data, aes(x = Cluster, y = Value, fill = Cluster)) +
    geom_violin(alpha = 0.7, trim = FALSE) +
    geom_boxplot(width = 0.12, fill = "white", color = "black", alpha = 0.7) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 2.5, fill = "red", color = "black") +
    facet_wrap(~Item_Label, scales = "free_y") +
    scale_fill_viridis_d(name = "Cluster") +
    labs(title = plot_title, x = "Psychological profile", y = "Value") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 24, hjust = 0.5, face = "bold"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),  # align with cluster_zscore_barplot.R
        strip.text = element_text(size = 18, face = "bold"),
        legend.position = "none"
    )

print(violin_plot)

if (!is.null(output_file)) {
    n_clusters <- length(unique(plot_data$Cluster))
    width_size <- max(12, n_clusters * 3)
    height_size <- max(8, ceiling(length(target_items) / 2) * 4)
    ggsave(output_file, violin_plot, width = width_size, height = height_size, dpi = 300, bg = "white")
    message(sprintf("Saved: %s (width=%.1f, height=%.1f)", output_file, width_size, height_size))
}
