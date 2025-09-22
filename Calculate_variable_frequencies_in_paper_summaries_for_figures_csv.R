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

# ---- 3. Read the header from line 5 ----
# Skip first 4 lines so line 5 becomes the header
header_df <- read_csv(
  file = file_path,
  skip = 4,           # skips 1–4
  n_max = 1,          # read just one row (header)
  show_col_types = FALSE
)
column_names <- names(header_df)

# ---- 4. Read the actual data starting on line 7 ----
# Skip 6 lines (1–6), so line 7 is the first data row
data_raw <- read_csv(
  file = file_path,
  skip = 6,           # skips 1–6
  col_names = FALSE,  # we'll apply header manually
  show_col_types = FALSE
)

# ---- 5a. Apply the correct column names ----
colnames(data_raw) <- column_names

# ---- 5b. Robustly drop entirely-blank columns ---- 
# EXPLANATION: This extra step was needed because Excel created extra, invisible, blank columns in the .CSV file.
# These columns have an extra trailing comma at the end of each line. As a result R thinks there’s one more column. 
# Excel often does this if there are extra empty cells in the sheet.
is_blank_col <- function(col) {
  ch <- as.character(col)
  ch[is.na(ch)] <- ""
  # Replace known problematic invisible chars with nothing
  ch <- gsub("\u00A0", "", ch, fixed = TRUE)   # non-breaking space
  ch <- gsub("\uFEFF", "", ch, fixed = TRUE)   # BOM
  # Trim normal whitespace
  ch2 <- trimws(ch)
  all(ch2 == "")
}

blank_cols_logical <- vapply(data_raw, is_blank_col, logical(1))

if (any(blank_cols_logical)) {
  dropped_names <- names(data_raw)[blank_cols_logical]
  message("Dropping entirely-blank columns: ", paste(dropped_names, collapse = ", "))
  data_raw <- data_raw[, !blank_cols_logical, drop = FALSE]
} else {
  message("No entirely-blank columns detected.")
}

# Sanity check
message("Columns after dropping blanks: ", ncol(data_raw)) # n should be 56


# ---- 6. Inspect the dataframe ----
head(data_raw)
names(data_raw) # see all variable names

# ---- 7. Define clean names for the first 25 columns ----
new_names_first25 <- c( #selected manually from the csv file and shortened by chatGPT
  "Matrix_entry_by",             # who added this observation to the matrix
  "Response_reported",           # Boolean: Yes/No
  "Keep_entry",                  # Yes/No/Maybe (for now only select yes)
  "Matrix_number",               # matrix number paper format: Dxxx
  "First_Author",                # paper 1st author
  "Year",                        # year of publication
  "Title",                       # paper title
  "DOI",                         # paper DOI
  "Journal",                     # journal of publication
  "Long",                        # Longitude reported in the study (units will be unified in August/Sept!)
  "Lat",                         # Latitude reported in the study (units will be unified in August/Sept!)
  "Elevation",			             # Elevation as reported in the study
  "Ecosystem",                   # Ecosytem as reported in the study
  "Dominant_plant_type",         # Dominat plant type as reported in the study
  "Variable",                    # Variable manipulated in experiment (should be precipitation for all studies)
  "MAP_mm_yr",                   # Mean annual precipitation reported in the study
  "MAT_C",                       # Mean annual temperature reported in the study
  "Precip_change_dir",           # Direction and relative change of precipitation compared to control
  "Precip_perc_control",         # Precipitation as % of control
  "Annual_precip_change_dir",    # Direction and relative change of precipitation compared to control PER YEAR
  "Annual_Precip_perc_control",  # Precipitation as % of control PER YEAR
  "All_year",                    # Results reported for this time period (from "Time" in csv file)
  "Growing_season",              # Results reported for this time period (from "Time" in csv file)
  "Seasonal",                    # Results reported for this time period (from "Time" in csv file)
  "By_months"                    # Results reported for this time period (from "Time" in csv file)
)

# ---- 8. Apply new names to dataframe ----
colnames(data_raw)[1:25] <- new_names_first25

# ---- 9. Inspect updated names ----
colnames(data_raw)[1:30]   # check the first 30 names to verify

# ---- 10. Define new names for columns 26–57 ----
new_names_26_56 <- c(
  "Overall_time_years",  # Experiment duration
  "Reported_time",       # Time period for which results are reported in the study
  "Replicates_ok",       # Is the number of replicate sound?
  # variables of interest start here at column 29:
  "Plant_AB",            # Plant aboveground biomass
  "Plant_BB",            # Plant belowground biomass
  "Plant_ANPP",          # Plant aboveground Net Primary Productivity
  "Plant_BNPP",          # Plant belowgroundNet Primary Productivity
  "Plant_N",             # Plant N content
  "Plant_P",             # Plant P content
  "Plant_CNratio",       # Plant C:N ratio
  "Plant_CPratio",       # Plant C:P ratio
  "Rs",                  # Soil respiration (Ra + Rh)
  "Ra",                  # Autotrophic respiration (tree roots)
  "Rh",                  # Heterotrophic respiration (microbes)
  "Litter_Forestfloor",  # Total litter on forest floor
  "Soil_TC",             # Soil total carbon
  "Soil_TN",             # Soil total nitrogen
  "Soil_TP",             # Soil total phosphorus
  "Soil_TCNratio",       # Soil total C:N ratio
  "Soil_TCPratio",       # Soil total C:P ratio
  "Soil_DOC",            # Soil dissolved organic carbon
  "Soil_InorgN",         # Soil total inorganic nitrogen
  "Soil_AvP",            # Soil available phosphorus
  "Mic_C",               # microbial carbon
  "Mic_N",               # microbial nitrogen
  "Mic_P",               # microbial phosphorus
  "Mic_CNratio",         # microbial C:N ratio
  "Mic_CPratio",         # microbial C:P ratio
  "FBratio",             # fungal:bacteria ratio
  "Fungi_M",             # fungal biomass
  "Bacteria_M"           # microbial biomass
)

# ---- 11. Apply names to dataframe ----
colnames(data_raw)[26:56] <- new_names_26_56

# ---- 12. Inspect final result ----
colnames(data_raw)

# ---- 13. Check for different NA notations in columns 29-56 ----
non_numeric_values <- lapply(data_raw[29:56], function(x) {
  vals <- unique(x)        # unique values
  vals[!grepl("^-?[0-9.]+$", vals) & !is.na(vals)]  # keep only non-numeric
})

# Print results (only show columns that had non-numeric values)
non_numeric_values <- non_numeric_values[sapply(non_numeric_values, length) > 0]
print(non_numeric_values)
# WARNING! this shows that some columns have notes or an 'X' notation. Let's decide later how to handle these!

# ---- 13. For now, directly convert non-numeric characters to NA ----
data_raw <- data_raw %>%
  mutate(across(29:56, as.numeric)) # gives warnings that some variables were coerced to NA (expected)

# ------------- PART 2 -------------

# ---- 14. quickly calculate frequencies. Which variables are reported most often?

# ---- Count non-NA values per variable ----
non_na_counts <- data_raw %>%
  select(29:56) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "n_values") %>%
  mutate(label = paste0(n_values, "/", nrow(data_raw))) %>%
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

# ---- 15. Which variables are reported together in one study most often?

# 1. Subset numeric columns 25-52
vars <- data_raw[, 29:56]

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
soil_vars  <- names(data_raw)[37:56]
plant_vars <- names(data_raw)[29:36]

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

# ---- 16. Find most common pairs of variables reported together (row-level) ----

# Subset variables of interest (numeric columns 25–52)
vars <- data_raw[, 29:56]

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
