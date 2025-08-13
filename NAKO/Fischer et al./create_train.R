# ==============================================================================
# Create training dataset for Fischer et al. study on anthropometry
#
# This script performs the following:
# - Loads raw anthropometric and imaging data from NAKO study
# - Selects relevant variables and applies data cleaning
#   (e.g., handling missing codes, unit corrections)
# - Converts key categorical variables to factors with meaningful labels
# - Merges study site codes for better interpretability
# - Reorders and renames columns for analysis readiness
# - Saves the cleaned dataset as an RDS file
# ==============================================================================

# Load required libraries
library(data.table)

# Set the data folder path (masked)
data_folder <- "<...>/Fischer_Anthopometrie"

# Load raw data and codebook for study sites
raw <- fread(paste(data_folder, "raw", "exportfile92_unt_ger_mapped.csv", sep = "/"))
StudZ_codes <- fread(paste(data_folder, "raw", "codierung_StudZ.csv", sep = "/"))

# Define relevant columns and categorical columns
relevant_cols <- c("StudZ","anthro_groe","anthro_gew","anthro_taillenumfang","anthro_hueftumfang",
                   "df100_age","df100_sex","anthro_fettmasse","d_usa_sat1","d_usa_sat2","d_usa_vat1","d_usa_vat2")
cat_cols <- c("StudZ","df100_sex")

# Subset relevant columns and clean data
train <- raw[,..relevant_cols]
train[, (relevant_cols) := lapply(.SD, function(x) replace(x, which(x==9999 | x==999.9 | x<=0), NA))]

# Correct height units if needed (e.g., from meters to cm)
train[anthro_groe < 3, anthro_groe := anthro_groe * 100]

# Remove rows missing critical demographic info
train <- train[complete.cases(train[, .(df100_age, df100_sex)]), ]

# Recoding sex variable to "m"/"f"
train[,df100_sex := fifelse(df100_sex == 1, "m", "f")]

# Merge study site codes for better interpretability
train[,StudZ := merge(train,StudZ_codes, by.x = "StudZ", by.y = "code", sort = F)[,cat]]

# Convert categorical columns to factors
train[,(cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]

# Rename columns to analysis-friendly names
setnames(train,c("untersuchungsort", "groesse", "gewicht", "taillenumfang", "hueftumfang", "age", "sex", "fettmasse", "sat1", "sat2", "vat1", "vat2"))

# Reorder columns for convenience
setcolorder(train,c("untersuchungsort", "age", "sex", "groesse", "gewicht"))

# Save cleaned training dataset
saveRDS(train, paste(data_folder, "train", "train.rds", sep = "/"))