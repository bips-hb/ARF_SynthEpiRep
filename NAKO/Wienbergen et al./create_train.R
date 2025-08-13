# ==============================================================================
# Create training dataset for Wienbergen et al. study on myocardial infarction
#
# This script performs the following:
# - Loads categorical codebook data from the NAKO study
# - Selects relevant variables
# - Applies data cleaning, including:
#     - Handling of coded categorical variables using external codebook
#     - Conversion of key variables to factors with meaningful labels
# - Renames columns for analysis-friendly readability
# - Saves the cleaned dataset as an RDS file
# ==============================================================================

# Load required libraries
library(data.table)

# Set the data folder path (masked)
data_folder <- "<...>/Wienbergen_MI"

# Load raw data and categorical codes
raw <- fread(paste(data_folder, "raw", "raw.csv", sep = "/"))
cat_codes <- fread(paste(data_folder, "raw", "codierungen.csv", sep = "/"))

# Define relevant columns, numeric and categorical variables
relevant_cols <- names(raw)[-c(9,14)]
numeric_cols <- c("age","bmi","waist_hi")
cat_cols <- relevant_cols[!(relevant_cols %in% numeric_cols)]

# Identify which categorical columns need label conversion
cat_cols_2convert <- unique(cat_codes[,var])

# Subset relevant columns
train <- raw[,..relevant_cols]

# Apply label conversion for coded categorical variables
train[,(cat_cols_2convert) := {
  # Melt data to long format, merge with codebook, then reshape back
  sd_long <- merge(melt(.SD,variable.name = "var", value.name = "code", measure.vars = cat_cols_2convert), cat_codes, by = c("var","code"), sort = F)
  sd <- split(sd_long[,-"code"], by = "var", keep.by = F)
  as.data.table(sd)
}, .SDcols = cat_cols_2convert]

# Convert all categorical columns to factors
train[,(cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]

# Rename columns to more readable/analysis-friendly names
setnames(train, c("Case", "Age", "Sex", "Birth_Germany", "Education", "Smoking_status", "Alcohol_consumption", "BMI", "Waist_hip_ratio", "Hypertension", "Diabetes_mellitus", "Family_history_premature_MI"))

# Save cleaned training dataset
saveRDS(train, paste(data_folder, "train", "train.rds", sep = "/"))
