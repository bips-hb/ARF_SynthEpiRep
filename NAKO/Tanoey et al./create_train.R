# ==============================================================================
# Create training dataset for Tanoey et al. study on childhood and familial factors
# related to diabetes outcomes in adulthood.
#
# This script performs the following:
# - Loads and merges raw and supplementary data sources
# - Cleans and recodes variables including age, sex, family history,
#   and diabetes diagnoses
# - Renames and prepares variables for synthesis and statistical analysis
# - Creates full task-specific datasets for different manuscript tables
#   and saves them as RDS files
# ==============================================================================

# Load required libraries
library(data.table)

# Load custom analysis functions for task-specific processing
source("NAKO/Tanoey et al./analysis_functions.R")

# Set the data folder path (masked)
data_folder <- "<...>/Tanoey_Diabetes"


# ------------------------------------------------------------------------------
# Full Dataset Generation: Load, Merge, Clean, and Recode Diabetes-Related Data
# ------------------------------------------------------------------------------

# Load raw files
raw <- fread(paste(data_folder, "raw", "exportfile314.csv", sep = "/"))
raw_nachlieferung <- fread(paste(data_folder, "raw", "exportfile314_nachl_nur_jahr.csv", sep = "/"))
raw_match_dm1_age <- fread(paste(data_folder, "raw", "match_dm1_age.csv", sep = "/"))
raw_match_w_orig <- fread(paste(data_folder, "raw", "match_w_orig.csv", sep = "/"))

# Merge all sources into main dataset
raw <- merge(raw,raw_nachlieferung, by= "ID")
raw <- merge(raw, raw_match_dm1_age[,-c("hash", "ID.y", "ins_age")], by.x = "ID", by.y = "ID.x", all.x = T)
raw <- merge(raw, raw_match_w_orig[,-c("hash", "ID.y")], by.x = "ID", by.y = "ID.x", all.x = T)

# Replace extreme/invalid diabetes diagnosis ages using alternative sources
raw[a_an_metdm1_age>300 & !is.na(tab_age), a_an_metdm1_age := tab_age]
raw[a_an_metdm1_age>300 & is.na(tab_age) & !is.na(a_an_metdm2a_age),a_an_metdm1_age := a_an_metdm2a_age]

# Select and rename relevant columns
relevant_cols <- c("basis_age", "d_an_met_1", "d_an_metdm1a","a_an_metdm1_age", "d_an_metdm2", "a_an_metdm2a_age",
                   "basis_sex", "d_ff7", "d_ff1", "d_ff2", "d_ff12", "Geburtsjahr", "d_fa_v3_dm", "d_fa_m3_dm", "a_ses_mig_status",
                   "d_ff6", "d_ff8", "d_ff9", "d_gg2_18", "a_anthro_groe")

# Subset relevant columns
train <- raw[,..relevant_cols]

# Rename columns to analysis-friendly names
setnames(train, c("age", "reported_diabetes_diagnosis", "diabetes_diagnosis_pregnant", "diabetes_diagnosis_age", "diabetes_treatment", "insulin_treatment_first_age",
                       "sex", "caesarean_delivery", "siblings_no", "birth_rank", "attended_daycare", "birth_year", "paternal_diabetes", "maternal_diabetes", "migration_background",
                       "prematural_birth", "birth_weight", "ever_breastfed", "weight_18", "height"))

### Recode and clean variables

# Diabetes diagnosis
train[reported_diabetes_diagnosis %in% c(8888, 9999, NA), reported_diabetes_diagnosis := 9999]
train[,reported_diabetes_diagnosis := factor(reported_diabetes_diagnosis, labels = c("Yes", "No", "Unknown"))]

train[!(diabetes_diagnosis_pregnant %in% c(1,2)),diabetes_diagnosis_pregnant := 9]
train[,diabetes_diagnosis_pregnant := factor(diabetes_diagnosis_pregnant, labels = c("Yes", "No", "Not applicable / Unknown"))]

train[diabetes_diagnosis_age > 72,diabetes_diagnosis_age:= NA]

train[diabetes_treatment %in% c(8888, 9999, NA), diabetes_treatment := 9999]
train[,diabetes_treatment := factor(diabetes_treatment, labels = c("Only insulin", "Only oral medication", "Both insulin and oral medication", "Diet", "Other", "Not applicable / Unknown"))]

train[insulin_treatment_first_age > 72, insulin_treatment_first_age:= NA]

# Sex and early-life factors
train[, sex := factor(sex, labels = c("Male", "Female"))]

train[!(caesarean_delivery %in% c(1,2)),caesarean_delivery := 9]
train[,caesarean_delivery := factor(caesarean_delivery, levels = c(2,1,9), labels = c("No", "Yes", "Unknown"))]

train[siblings_no > 36, siblings_no:= NA]

train[birth_rank == 0 | birth_rank > 22, birth_rank:= NA]

train[!(attended_daycare %in% c(1,2)),attended_daycare := 9]
train[,attended_daycare := factor(attended_daycare, levels = c(2,1,9), labels = c("No", "Yes", "Unknown"))]

# Birth year corrections
train[birth_year == 958, birth_year := 1958]
train[birth_year == 76, birth_year := 1976]
train[birth_year == 69, birth_year := 1969]
train[birth_year == 195, birth_year := 1954] ### s. Tanoey Original
train[birth_year == 81, birth_year := 1981]
train[birth_year == 970, birth_year := 1970]
train[birth_year == 979, birth_year := 1979]
train[birth_year == 2014,birth_year:=NA]

# Parental diabetes
train[paternal_diabetes %in% c(11,13,14),paternal_diabetes := 13]
train[!(paternal_diabetes %in% c(2,12,13)),paternal_diabetes := 99]
train[,paternal_diabetes := factor(paternal_diabetes, labels = c("No", "Yes, age <40 years", "Yes, age >=40 years/unknown", "Unknown"))]

train[maternal_diabetes %in% c(11,13,14),maternal_diabetes := 13]
train[!(maternal_diabetes %in% c(2,12,13)),maternal_diabetes := 99]
train[,maternal_diabetes := factor(maternal_diabetes, labels = c("No", "Yes, age <40 years", "Yes, age >=40 years/unknown", "Unknown"))]

# Migration background
train[!migration_background %in% c("No","Yes"), migration_background:= 'Missing']
train[,migration_background := factor(migration_background, levels = c("No", "Yes", "Missing"))]

# Other early-life factors
train[!(prematural_birth %in% c(1,2)),prematural_birth := 9]
train[,prematural_birth := factor(prematural_birth, levels = c(2,1,9), labels = c("No", "Yes", "Unknown"))]

train[!(birth_weight %in% c(1,2,3)),birth_weight := 9]
train[,birth_weight := factor(birth_weight, levels = c(2,1,3,9), labels = c("Average", "Light", "Heavy", "Unkown"))]

train[!(ever_breastfed %in% c(2,11,12)),ever_breastfed := 99]
train[,ever_breastfed := factor(ever_breastfed, labels = c("No", "Yes, >4 months", "Yes, until 4 months", "Unkown"))]

# Weight at age 18 and height
train[weight_18 > 300, weight_18 := NA]
train[height >= 7777, height := NA]

# Number of siblings
train[siblings_no == 0, birth_rank := 0]
train[, siblings_no := NULL]

# Save full cleaned dataset
saveRDS(train, paste(data_folder, "train", "train.rds", sep = "/"))


# ------------------------------------------------------------------------------
# Task-specific Dataset Generation
# ------------------------------------------------------------------------------

### task-specific datesets using custom prep function from analysis_functions.R

# subset columns for table 1, derive variables and save as RDS
train_tab1_subset_cols <- c("sex", "caesarean_delivery", "birth_rank", "attended_daycare", "birth_year", "paternal_diabetes",
               "maternal_diabetes", "migration_background", "prematural_birth", "birth_weight",
               "ever_breastfed", "weight_18", "height", "diabetes_diagnosis_age", "diabetes_treatment", "reported_diabetes_diagnosis", "insulin_treatment_first_age", "diabetes_diagnosis_pregnant")
train_tab1_subset <- train[, ..train_tab1_subset_cols]
train_tab1_subset_derived <- prepare_analysis(train_tab1_subset)
saveRDS(train_tab1_subset_derived, paste(data_folder, "train", "tab1", "train_tab1_subset_derived.rds", sep = "/"))

# subset columns for figure 2, derive variables and save as RDS
train_fig2_subset_cols <- c("diabetes_diagnosis_age", "diabetes_treatment", "sex", "reported_diabetes_diagnosis", 
            "insulin_treatment_first_age", "diabetes_diagnosis_pregnant")
train_fig2_subset <- train[, ..train_fig2_subset_cols]
train_fig2_subset_derived <- prepare_analysis(cbind(train_fig2_subset, age = train[, age]))
saveRDS(train_fig2_subset_derived, paste(data_folder, "train", "fig2", "train_fig2_subset_derived.rds", sep = "/"))

# subset columns for table 2 univariable analysis, derive variables and save as RDS
train_tab2_univariable_subset_cols <- c("diabetes_diagnosis_age", "age", "sex", "birth_year", "reported_diabetes_diagnosis", "diabetes_diagnosis_pregnant",
                                       "birth_rank", "caesarean_delivery", "attended_daycare", "insulin_treatment_first_age", "diabetes_treatment")
train_tab2_univariable_subset <- train[, ..train_tab2_univariable_subset_cols]
train_tab2_univariable_subset_derived <- prepare_analysis(train_tab2_univariable_subset)
saveRDS(train_tab2_univariable_subset_derived, paste(data_folder, "train", "tab2_univariable", "train_tab2_univariable_subset_derived.rds", sep = "/"))
