# HEADER --------------------------------------------
# Script Name:  
#
# Script Description: 1. Reads the CSV file created from the file paper_summaries.xlsx, named paper_summaries_for_figures.csv
#                     2. Cleans the data for processing
#                     3. Calculates frequencies for each variable (which variables are reported most often?), and checks 
#                        which variables co-occur in the same row (i.e.,are most often teported in one study)
#
# Author:   Marleen Pallandt, #add you name here
# Email:    marleen.pallandt@natgeo.su.se
#
# Date created:     2025-08-20
#
# SETUP ------------------------------------
rm(list = ls())  # Remove all objects from the workspace for a clean start

# ---- 1. Load required R packages ----
library(readr)   # Provides read_csv/read_csv2 for easy CSV import
library(dplyr)   # tidy R
library(tidyr)   # tidy R
library(ggplot2) # for plotting
library(viridis) # for the plot colors

# ---- 2. Define the path to your CSV file ----
# Replace "YOUR_PATH/paper_summaries_for_figures.csv" with the actual path on your computer or copy the .csv 
#   file (check if it's the latest version!) to another directory
file_path <- "C:/Users/mapa7208/Box/RT4 - Temperature-water interactions/paper_summaries_for_figures.csv"

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

# ---- 5. Apply the correct column names ----
colnames(data_raw) <- column_names

# ---- 6. Inspect the dataframe ----
head(data_raw)
names(data_raw) # see all variable names

# ---- 7. Define clean names for the first 21 columns ----
new_names_first21 <- c( #selected manually from the csv file and shortened by chatGPT
  "Matrix_entry_by",          # who added this observation to the matric
  "Matrix_number",            # matrix number paper format: Dxxx
  "First_Author",             # paper 1st author
  "Year",                     # year of publication
  "Title",                    # paper title
  "DOI",                      # paper DOI
  "Journal",                  # journal of publication
  "Long",                     # Longitude reported in the study (units will be unified in August/Sept!)
  "Lat",                      # Latitude reported in the study (units will be unified in August/Sept!)
  "Elevation",			      # as reported in the study
  "Ecosystem",                # as reported in the study
  "Dominant_plant_type",      # as reported in the study
  "Variable",                 # Variable manipulated in experiment (should be precipitation for all studies)
  "MAP_mm_yr",                # Mean annual precipitation reported in the study
  "MAT_C",                    # Mean annual temperature reported in the study
  "Precip_change_direction",  # Direction and relative change of precipitation compared to control
  "Precip_percent_control",   # Precipitation as % of control
  "All_year",                 # Results reported for this time period (from "Time" in csv file)
  "Growing_season",           # Results reported for this time period (from "Time" in csv file)
  "Seasonal",                 # Results reported for this time period (from "Time" in csv file)
  "By_months"                 # Results reported for this time period (from "Time" in csv file)
)

# ---- 8. Apply new names to dataframe ----
colnames(data_raw)[1:21] <- new_names_first21

# ---- 9. Inspect updated names ----
colnames(data_raw)[1:30]   # check the first 30 names to verify

# ---- 10. Define new names for columns 22–52 ----
new_names_22_52 <- c(
  "Overall_time_years",  # Experiment duration
  "Reported_time",       # Time period for which results are reported in the study
  "Replicates_ok",       # Is the number of replicate sound?
  # variables of interest start here at column 25:
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
colnames(data_raw)[22:52] <- new_names_22_52

# ---- 12. Inspect final result ----
colnames(data_raw)

# ---- 13. Check for different NA notations in columns 25-52 ----
non_numeric_values <- lapply(data_raw[25:52], function(x) {
  vals <- unique(x)        # unique values
  vals[!grepl("^-?[0-9.]+$", vals) & !is.na(vals)]  # keep only non-numeric
})

# Print results (only show columns that had non-numeric values)
non_numeric_values <- non_numeric_values[sapply(non_numeric_values, length) > 0]
print(non_numeric_values)
# WARNING! this shows that some columns have notes or an 'X' notation. Let's decide later how to handle these!

# ---- 13. For now, directly convert non-numeric characters to NA ----
data_raw <- data_raw %>%
  mutate(across(25:52, as.numeric)) # gives warnings that some variables were coerced to NA (expected)

# ------------- PART 2 -------------

# ---- 14. quickly calculate frequencies. Which variables are reported most often?

# ---- Count non-NA values per variable ----
non_na_counts <- data_raw %>%
  select(25:52) %>%
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
ggsave("Variable_frequencies_barplot.png",
       width = 40, height = 40, units = "cm", dpi = 300)

# ---- 15. Which variables are reported together in one study most often?

# 1. Get total non-NA counts per variable (same as before)
var_order <- data_raw %>%
  select(25:52) %>%
  summarise(across(everything(), ~ sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "n_values") %>%
  arrange(desc(n_values)) %>%
  pull(Variable)

# 2. Convert Var1 and Var2 to factors with same ordering
co_occurrence_df <- co_occurrence_df %>%
  mutate(
    Var1 = factor(Var1, levels = var_order),
    Var2 = factor(Var2, levels = var_order)
  )

# 3. Plot as heatmap with viridis color scale
ggplot(co_occurrence_df, aes(x = Var1, y = Var2, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "viridis", direction = -1) +  # yellow -> blue
  labs(
    title = "Co-occurrence of reported variables",
    x = "Variable name",
    y = "Variable name",
    fill = "Count"
  ) +
  theme_bw(base_size = 20) +  # increase base font size
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 18, face = "bold"),  # x-axis labels
    axis.text.y = element_text(size = 18, face = "bold"),  # y-axis labels
    axis.title.x = element_text(size = 20, face = "bold"),  # x-axis title
    axis.title.y = element_text(size = 20, face = "bold"),  # y-axis title
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5),  # centered title
    legend.title = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 16)
  )

# ---- Save heatmap ----
ggsave("Variable_co-occurence_matrix.png",
       width = 40, height = 40, units = "cm", dpi = 300)
