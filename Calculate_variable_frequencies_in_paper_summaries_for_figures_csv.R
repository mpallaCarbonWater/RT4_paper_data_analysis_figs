# HEADER --------------------------------------------
# Script Name:  
#
# Script Description: 1. Reads the CSV file created from the file paper_summaries.xlsx, named paper_summaries_for_figures.csv
#                     2. Cleans the data for processing
#                     3. Calculates frequencies for each variable (which variables are reported most often?), and checks 
#                        which variables co-occur in the same row (i.e.,are most often reported in one study)
#
# Authors:   Marleen Pallandt, #add you name here
# Contact:   marleen.pallandt@natgeo.su.se
#
# Date created:     2025-08-20
#
# SETUP ------------------------------------
rm(list = ls())  # Remove all objects from the workspace for a clean start

# ---- 1. Load required R packages ----
library(readr)      # Provides read_csv/read_csv2 for easy CSV import
library(dplyr)      # tidy R
library(tidyr)      # tidy R
library(ggplot2)    # for plotting
library(viridis)    # for the plot colors
library(combinat)   # for combn, or use base::combn directly
library(ggtext)     # for custom colour labels
library(colorspace) # for custom colour labels

# ---- 2. Define the path to your CSV file ----
# Replace "YOUR_PATH/paper_summaries_for_figures.csv" with the actual path on your computer or copy the .csv 
#   file (check if it's the latest version!) to another directory
file_path <- "C:/Users/mapa7208/Box/RT4 - Temperature-water interactions/paper_summaries_for_figures_v2.csv"

# ---- 3. Read the header from line 6 ----
# Skip first 5 lines so line 6 becomes the header
data_raw <- read_csv(
  file = file_path,
  skip = 5,           # skip first 5 lines, so row 6 is used as header
  show_col_types = FALSE
)

# check result
colnames(data_raw)

# ---- 4. Robustly drop entirely-blank columns ---- 
# EXPLANATION: Excel sometimes writes "phantom" columns into CSV files. 
# This happens when the spreadsheet has extra empty cells far to the right.
# In the CSV, these show up as trailing commas at the end of each row, 
# which R interprets as extra empty columns.

# --- Helper function: checks if a whole column is blank ---
is_blank_col <- function(col) {
  ch <- as.character(col) # Convert to character (handles numeric/other types uniformly)
  ch[is.na(ch)] <- "" # Treat NA as empty string
  
  # Replace known problematic invisible chars with nothing
  ch <- gsub("\u00A0", "", ch, fixed = TRUE)   # non-breaking space (common when copy-pasting in Excel)
  ch <- gsub("\uFEFF", "", ch, fixed = TRUE)   # byte-order mark (BOM) sometimes sneaks in at file start
  
  ch2 <- trimws(ch) # Remove of whitespace (spaces, tabs, etc.)
  all(ch2 == "")    # Column is "blank" if every entry is empty after cleaning
}

# --- Apply helper function to all columns in data.frame data_raw ---
blank_cols_logical <- vapply(data_raw, is_blank_col, logical(1))

# --- Drop blank columns if found ---
if (any(blank_cols_logical)) {
  dropped_names <- names(data_raw)[blank_cols_logical]
  message("Dropping entirely-blank columns: ", paste(dropped_names, collapse = ", "))
  
  # Keep only non-blank columns
  data_raw <- data_raw[, !blank_cols_logical, drop = FALSE]
} else {
  message("No entirely-blank columns detected.")
}

# Sanity check
message("Columns after dropping blanks: ", ncol(data_raw)) # n should now be 56 columns

# Inspect the dataframe ----
head(data_raw)
names(data_raw) # see all variable names

# ---- 5. Filter studies that were marked as suitable ----
# The column 'Keep_entry' marks which studies we can select for our analysis
# Code searches for variations like "Yes", "yes", "Y", "yep", etc.
# Instead of manually listing all of them, we use a pattern search (regex).
# Other entries (e.g. "No", "Maybe", "n", blanks) will be dropped.

# Inspect what unique entries exist in Keep_entry
message("Unique values in Keep_entry: ", paste(unique(data_raw$Keep_entry), collapse = ", "))

data_clean <- data_raw %>%
  filter(Keep_entry=="Yes") # keep anything starting with "y" or "Y" (40 observations dropped)

# ---- 6. Check for different NA notations in columns 29-56 ----
# Variables of interest are located in column 29 - 56. Let's see if the notation makes sense

non_numeric_values <- lapply(data_clean[29:56], function(x) {
  vals <- unique(x)        # unique values
  vals[!grepl("^-?[0-9.]+$", vals) & !is.na(vals)]  # keep only non-numeric
})

# Print results (only show columns that had non-numeric values)
non_numeric_values <- non_numeric_values[sapply(non_numeric_values, length) > 0]
print(non_numeric_values)
# WARNING! this shows that some columns have notes or an 'X' notation, or different NA notations (maybe use grepl to filter these out)

# ---- 7. For now, directly convert non-numeric characters to NA ----
data_clean <- data_clean %>%
  mutate(across(29:56, as.numeric)) # gives warnings that some variables were coerced to NA (expected)

# ------------- PART 2 -------------

# ---- 8. quickly calculate frequencies. Which variables are reported most often?

# ---- Count non-NA values per variable ----
non_na_counts <- data_clean %>%
  select(29:56) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "n_values") %>%
  mutate(label = paste0(n_values, "/", nrow(data_clean))) %>%
  arrange(desc(n_values)) %>%
  mutate(Variable = factor(Variable, levels = rev(Variable)))  # reverse to get top-to-bottom

# ---- Visualize with barplot ----
ggplot(non_na_counts, aes(x = n_values, y = Variable)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = label), hjust = -0.1, size = 6) +  # increase label size
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +  # leave space for labels
  labs(
    title = "Number of reported values per variable",
    x = "Count",
    y = "Variable name"
  ) +
  theme_bw(base_size = 20) +  # increase base font size
  theme(
    axis.text.x = element_text(size = 18, face = "bold"),  # x-axis tick labels
    axis.text.y = element_text(size = 18, face = "bold"),  # y-axis tick labels
    axis.title.x = element_text(size = 20, face = "bold"), # x-axis title
    axis.title.y = element_text(size = 20, face = "bold"), # y-axis title
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5) # centered title
  )

# ---- Save plot ----
ggsave("Variable_frequencies_barplot_v2.png",
       width = 40, height = 40, units = "cm", dpi = 300)

# ---- 8. Which variables are reported together in one study most often?

# 1. Subset numeric columns 29-56
vars <- data_clean[, 29:56]

# 2. Create logical matrix: TRUE if non-NA
vars_logical <- !is.na(vars)

# 3. Compute co-occurrence matrix
co_occurrence <- t(vars_logical) %*% as.matrix(vars_logical)

# 4. Keep only upper triangle (remove duplicates & self-pairs)
co_occurrence[lower.tri(co_occurrence, diag = TRUE)] <- NA

# 5. Convert to tidy dataframe (only pairs with Count > 0)
co_occurrence_df <- as.data.frame(as.table(co_occurrence)) %>%
  filter(!is.na(Freq) & Freq > 0) %>%
  arrange(desc(Freq)) %>%
  rename(Var1 = Var1, Var2 = Var2, Count = Freq)

# ---- Custom variable ordering: Soil first (33–52), then Plant (25–32) ----
soil_vars  <- names(data_clean)[37:56]
plant_vars <- names(data_clean)[29:36]

var_order <- c(soil_vars, plant_vars)

# 6. Apply ordering to factors (ensure full factor levels are kept)
co_occurrence_df <- co_occurrence_df %>%
  mutate(
    Var1 = factor(as.character(Var1), levels = var_order),
    Var2 = factor(as.character(Var2), levels = var_order)
  )

# ---- Compute split positions by variable name (precise placement,, can be adapted to visual needs) ----
# Vertical line after "Bacteria_M" (i.e. between "Bacteria_M" and the following var)
x_intercept <- which(var_order == "Bacteria_M") + 0.5

# Horizontal line after "Bacteria_M" (i.e. between "Bacteria_M" and the following var)
y_intercept <- which(var_order == "Bacteria_M") + 0.5

# ---- 7. Plot heatmap with separator lines between soil and plant variables ----
ggplot(co_occurrence_df, aes(x = Var1, y = Var2, fill = Count)) +
  geom_tile(color = "white") +
  # keep unused factor levels visible (so positions match var_order)
  scale_x_discrete(drop = FALSE) +
  scale_y_discrete(drop = FALSE) +
  scale_fill_viridis(option = "viridis", direction = -1) +
  # separator lines at the computed positions
  geom_vline(xintercept = x_intercept, color = "red", size = 1.2) +
  geom_hline(yintercept = y_intercept, color = "red", size = 1.2) +
  labs(
    title = "Co-occurrence of reported variables",
    x = "Soil --> Plant variables",
    y = "Plant ---> soil variables",
    fill = "Count"
  ) +
  theme_bw(base_size = 20) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18, face = "bold"),
    axis.text.y = element_text(size = 18, face = "bold"),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )


# ---- 8. Save heatmap ----
ggsave("Variable_co-occurence_matrix_by_plant-soil_v2.png",
       width = 40, height = 40, units = "cm", dpi = 300)

# ---- 9. Find most common pairs of variables reported together (row-level) ----

# Subset variables of interest (numeric columns 25–52)
vars <- data_clean[, 29:56]

# Logical version: TRUE if variable is reported in a row
vars_logical <- !is.na(vars)

# Function to extract all variable pairs from one row
row_pairs <- apply(vars_logical, 1, function(row) {
  reported <- names(row)[row]  # variables reported in this row
  if (length(reported) >= 2) {
    t(combn(reported, 2))  # all unique pairs of different variables
  } else {
    NULL
  }
})

# Combine all pairs across rows into a single data.frame
all_pairs <- do.call(rbind, row_pairs)

# Convert to tibble and count frequencies
pairs_df <- as.data.frame(all_pairs) %>%
  rename(Var1 = V1, Var2 = V2) %>%
  # Remove any accidental self-pairs
  filter(Var1 != Var2) %>%
  # Ensure consistent ordering (alphabetical) so A+B == B+A
  rowwise() %>%
  mutate(
    Var_min = min(c(Var1, Var2)),
    Var_max = max(c(Var1, Var2))
  ) %>%
  ungroup() %>%
  select(Var1 = Var_min, Var2 = Var_max)

# Count frequencies of pairs across all rows
pair_counts <- pairs_df %>%
  count(Var1, Var2, sort = TRUE)

# ---- Top 20 most common variable pairs ----
print(head(pair_counts, 20))


# ---- Optional: visualize as bar plot ----

# Filter pairs with n >= 8
pair_counts_filtered <- pair_counts %>%
  filter(n >= 8) %>%
  arrange(desc(n)) %>%
  mutate(pair_label = paste(Var1, Var2, sep = " + "),
         y_pos = row_number())  # y positions for plotting

# Assign high-contrast colors to variables
vars <- unique(c(pair_counts_filtered$Var1, pair_counts_filtered$Var2))
var_colors <- setNames(rainbow_hcl(length(vars), c = 80, l = 70), vars)

# Colored y-axis labels
pair_counts_filtered <- pair_counts_filtered %>%
  rowwise() %>%
  mutate(pair_label_colored = paste0(
    "<span style='color:", var_colors[Var1], "'>", Var1, "</span>",
    " + ",
    "<span style='color:", var_colors[Var2], "'>", Var2, "</span>"
  )) %>%
  ungroup()

# Decide which bars get a label
# Only label the first occurrence of each unique count
pair_counts_filtered <- pair_counts_filtered %>%
  arrange(desc(n)) %>%
  group_by(n) %>%
  mutate(label_to_show = if_else(row_number() == 1, as.character(n), NA_character_)) %>%
  ungroup()

# Plot with grey bars and selective labels
ggplot(pair_counts_filtered, aes(x = n, y = reorder(pair_label_colored, n))) +
  geom_col(fill = "grey80", color = "black") +
  geom_text(aes(label = label_to_show), hjust = -0.1, size = 5, color = "black") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title = "Common pairs of reported variables (n ≥ 8)",
    x = "Count across studies",
    y = "Variable pairs"
  ) +
  theme_bw(base_size = 18) +
  theme(
    axis.text.y = element_markdown(size = 14, face = "bold"), # colored labels
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# save
ggsave("Reported_variables_common_pairs_v2.png",
       width=23, height=13, units = "in", dpi = 300)
