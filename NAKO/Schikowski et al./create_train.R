# ==============================================================================
# Create training dataset for Schikowski et al. study on blood pressure
#
# This script performs the following:
# - Loads raw blood pressure data from NAKO study
# - Selects relevant variables and applies data cleaning (e.g., filtering physiologically plausible values)
# - Recodes categorical variables and handles missing data
# - Restricts the sample to adults aged 20-69
# - Converts selected columns to factors with meaningful levels
# - Renames variables for analysis clarity
# - Saves the cleaned dataset as an RDS file
# ==============================================================================

# Load required libraries
library(data.table)

# Set the data folder path (masked)
data_folder <- "<...>/Schikowski_Blutdruck"

# Load raw data
raw <- fread(paste(data_folder, "raw", "exportfile141_unt_ger_mapped.csv", sep = "/"))

# Define relevant columns and categorical and numeric columns
relevant_cols <- c("df100_age", "df100_sex", "BD01", "BD02", "BD04", "BD05", "d_an_cv_6")
cat_cols <- c("df100_sex","d_an_cv_6")
numeric_cols <- relevant_cols[!(relevant_cols %in% c("df100_sex","d_an_cv_6"))]

# Subset relevant columns
train <- raw[,..relevant_cols]

# Set implausible blood pressure and age values to NA
train[, (numeric_cols) := lapply(.SD, function(x) replace(x, which(x>=300 | x<=8), NA)), .SDcols = numeric_cols]

# Keep participants with complete blood pressure readings and age between 20 and 69
train <- train[complete.cases(train[,-"d_an_cv_6"]),][df100_age >= 20 & df100_age <= 69 ,]

# Recode sex variable: 1 = male, else female
train[, df100_sex := fifelse(df100_sex == 1, "m", "f")]

# Recode hypertension self-report (d_an_cv_6) to binary: 1 = yes, 0 = no or missing
train[d_an_cv_6 != 1 | is.na(d_an_cv_6), d_an_cv_6 := 0]

# Convert categorical columns to factors
train[,(cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]

# Rename columns to analysis-friendly names
setnames(train,c("age","sex","systole_mess1", "diastole_mess1", "systole_mess2", "diastole_mess2", "hypertonie_eigenangabe"))

# Save cleaned training dataset
saveRDS(train, paste(data_folder, "train", "train.rds", sep = "/"))
