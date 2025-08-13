# ==============================================================================
# Create training datasets for Berger et al. study on COVID-19 and mental health.
#
# This script performs the following:
# - Loads raw data from NAKO baseline and COVID-19 survey
# - Selects relevant variables and applies necessary data cleaning and recoding
# - Converts coded variables to factors with meaningful levels
# - Merges NAKO and COVID-19 survey datasets on participant ID
# - Derives summary scores for PHQ-9 and GAD-7 questionnaires at baseline
#   and follow-up
# - Creates full task-specific datasets for different manuscript tables
#   and saves them as RDS files
# ==============================================================================

# Load required libraries
library(data.table)

# Set the data folder path (masked)
data_folder <- "<...>/Berger_Corona"

# -----------------------------------------------------------------------------
# Full Dataset Generation
# ------------------------------------------------------------------------------

### Load and process NAKO baseline data

# Read raw NAKO participant data and coding dictionary
raw_nako <- fread(paste(data_folder, "raw", "exportfile464.csv", sep = "/"))
cat_codes_nako <- fread(paste(data_folder, "raw", "codierungen_nako.csv", sep = "/"))

# Define relevant columns from NAKO dataset
relevant_cols_nako <- c("ID", "StudZ", "basis_age", "basis_sex", "basis_udat_hour", "d_se_b1", "d_se_f2a", "d_phq91_1", 
                        "d_phq91_2", "d_phq91_3", "d_phq91_4", "d_phq91_5", "d_phq91_6", "d_phq91_7", "d_phq91_8",
                        "d_phq91_9", "d_gad7_1", "d_gad7_2", "d_gad7_3", "d_gad7_4", "d_gad7_5", "d_gad7_6", "d_gad7_7")

# Extract categorical columns and those to convert based on codebook
cat_cols_nako <- relevant_cols_nako[c(2,4,6:23)]
cat_cols_2convert_nako <- unique(cat_codes_nako[,var])[-1]

# Filter and clean NAKO data
train_nako <- raw_nako[!is.na(basis_age),..relevant_cols_nako]
train_nako <- train_nako[basis_age < 20, basis_age := 20]
train_nako[, (relevant_cols_nako) := lapply(.SD, \(x) replace(x, x %in% c(88, 99, 8888, 9999), NA))]

# Convert time variable to numeric
train_nako[,basis_udat_hour := as.numeric(basis_udat_hour)]

# Convert coded categorical variables to factor levels based on codebook
train_nako[,(cat_cols_2convert_nako) := {
  sd_long <- merge(melt(.SD,variable.name = "var", value.name = "code", measure.vars = cat_cols_2convert_nako, na.rm = F),cat_codes_nako, by = c("var","code"), sort = F, all.x = T)
  sd <- split(sd_long[,-"code"], by = "var", keep.by = F)
  as.data.table(sd)
}, .SDcols = cat_cols_2convert_nako]

# Recode PHQ9 and GAD7 item responses to numeric scores 0-3
phq9_gd7_cols <- 8:ncol(train_nako)
train_nako[, (phq9_gd7_cols) := lapply(.SD, \(x) c(0, 1, 2, 3)[match(x, c(2, 11, 12, 13))]), .SDcols = phq9_gd7_cols]

# Convert categorical columns to factors
train_nako[,(cat_cols_nako) := lapply(.SD, factor, exclude = NULL), .SDcols = cat_cols_nako]


### Load and preprocess NAKO COVID survey data

# Read raw COVID survey data
raw_covid <- fread(paste(data_folder, "raw", "exportfile464ls_mapped.csv", sep = "/"))

# Select relevant columns and filter complete cases
relevant_cols_covid <- names(raw_covid)[c(2,6,132:151,172,173,174,176)]
train_covid <- raw_covid[lastpage > 0,][, lastpage := NULL]
train_covid <- train_covid[,..relevant_cols_covid]

# Select latest observation per participant token
relevant_idx_covid <- setorder(train_covid, token, datestamp)[, .N, by = token][, cumsum(N)]
train_covid <- train_covid[relevant_idx_covid,]

# Clean COVID dataset
train_covid[, datestamp := as.numeric(datestamp)]
train_covid[, (relevant_cols_covid) := lapply(.SD, function(x) replace(x, x %in% c("","NULL"), NA))]

# Rename columns to meaningful names with suffix "_covid"
setnames(train_covid, 3:ncol(train_covid), sapply(names(train_covid)[3:ncol(train_covid)],function(x) paste(substr(x,14, nchar(x)),"_covid",sep="")))
setnames(train_covid, c("token","datestamp","_covid"), c("ID","date_stamp_covid","ISO2_covid"))

# Recode PHQ9 and GAD7 COVID questionnaire items to numeric 0-3
phq9_gad7_covid_cols <- 3:18
train_covid[, (phq9_gad7_covid_cols) := lapply(.SD, \(x) c(0, 1, 2, 3)[match(x, c("A1", "A2", "A3", "A4"))]), .SDcols = phq9_gad7_covid_cols]

# Recode ISO1 COVID questionnaire items to factor levels with labels
iso1_covid_cols <- 19:21
train_covid[, (iso1_covid_cols) := lapply(.SD, \(x) c("Oft", "Manchmal", "Selten", "Nie")[match(x, c("A2", "A3", "A4", "A5"))]), .SDcols = iso1_covid_cols]
train_covid[, ISO2_covid := c("Eher stärker", "Eher weniger", "Nicht zutreffend")[match(ISO2_covid, c("A1", "A2", "A3"))]]

# Recode anxiety related COVID questionnaire items to factors
ang_covid_cols <- 23:length(relevant_cols_covid)
train_covid[, (ang_covid_cols) := lapply(.SD, \(x) c("Ja", "Nein", "Ich weiß ncht")[match(x, c("A1", "A2", "A3"))]), .SDcols = ang_covid_cols]

# Convert COVID categorical columns to factors
cat_cols_covid <- 3:length(relevant_cols_covid)
train_covid[,(cat_cols_covid) := lapply(.SD, factor, exclude = NULL), .SDcols = cat_cols_covid]

### Merge NAKO baseline and COVID survey data

train <- merge(train_nako, train_covid, by = "ID", )[,-"ID"]

# Rename baseline variables to concise names for easier analysis
setnames(train, c("StudZ","basis_age","basis_sex","basis_udat_hour","d_se_b1","d_se_f2a",
                          "d_phq91_1","d_phq91_2","d_phq91_3","d_phq91_4","d_phq91_5","d_phq91_6",
                          "d_phq91_7","d_phq91_8","d_phq91_9","d_gad7_1","d_gad7_2","d_gad7_3",
                          "d_gad7_4","d_gad7_5","d_gad7_6","d_gad7_7"),
         c("untersuchungsort", "age", "sex", "untersuchungsdatum",
                  "bildungsniveau", "mit_partner_lebend", "PHQ911_basis",
           "PHQ912_basis","PHQ913_basis","PHQ914_basis","PHQ915_basis",
           "PHQ916_basis","PHQ917_basis","PHQ918_basis","PHQ919_basis",
           "GAD71_basis","GAD72_basis","GAD73_basis","GAD74_basis",
           "GAD75_basis","GAD76_basis","GAD77_basis")
         )

# Replace missing factor levels with "Missing"
cat_cols <- which(sapply(train, is.factor))
train[, (cat_cols) := lapply(.SD, \(x) {
  levels(x)[is.na(levels(x))] <- "Missing"
  x
  }), .SDcols = cat_cols]

# Save cleaned full training dataset
saveRDS(train, paste(data_folder, "train", "train.rds", sep = "/"))


# ------------------------------------------------------------------------------
# Task-specific Dataset Generation
# ------------------------------------------------------------------------------

### Derive variables

# Identify PHQ9 and GAD7 columns at baseline and follow-up
phq_basis_cols <- names(train)[intersect(grep("basis", names(train)), grep("PHQ9", names(train)))]
gad_basis_cols <- names(train)[intersect(grep("basis", names(train)), grep("GAD7", names(train)))]
phq_covid_cols <- names(train)[intersect(grep("covid", names(train)), grep("PHQ9", names(train)))]
gad_covid_cols <- names(train)[intersect(grep("covid", names(train)), grep("GAD7", names(train)))]
iso1_covid_cols <- names(train)[intersect(grep("covid", names(train)), grep("ISO1", names(train)))]

train_derived <- copy(train)

# Convert dates and compute follow-up time in years (adjusted by -0.5 for baseline offset)
train_derived[,untersuchungsdatum := as.Date.POSIXct(untersuchungsdatum)]
train_derived[,date_stamp_covid := as.Date.POSIXct(date_stamp_covid)]
train_derived[, follow_up_time := as.numeric(date_stamp_covid - untersuchungsdatum)/365 - 0.5]
train_derived[, `:=` (untersuchungsdatum = NULL, date_stamp_covid = NULL)]

# Calculate PHQ9 and GAD7 sum scores at baseline and COVID follow-up
train_derived[, phq9_basis_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = phq_basis_cols]
train_derived[, gad7_basis_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = gad_basis_cols]
train_derived[, phq9_covid_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = phq_covid_cols]
train_derived[, gad7_covid_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = gad_covid_cols]

# Clean ISO1 COVID questionnaire data and derive summary factor and sum score 
train_derived[, (iso1_covid_cols) := replace(.SD, .SD == "Selten", "Nie"), .SDcols = iso1_covid_cols]
train_derived[, (iso1_covid_cols) := replace(.SD, .SD == "Missing", NA), .SDcols = iso1_covid_cols]
train_derived[, (iso1_covid_cols) := lapply(.SD, factor, levels = c("Nie", "Manchmal", "Oft"), labels = c("Nie/Selten", "Manchmal", "Oft")),.SDcols = iso1_covid_cols]
train_derived[, iso1_covid_sum := rowSums(sapply(.SD, as.numeric), na.rm = F), .SDcols = iso1_covid_cols]

# Convert derived sum scores to factors with "Missing" level for NA values
aggregated_cols <- c("phq9_basis_sum", "gad7_basis_sum", "phq9_covid_sum", "gad7_covid_sum", "iso1_covid_sum")
train_derived[, (aggregated_cols) := lapply(.SD, \(col) {
  col_factor <- factor(col, levels = c(sort(unique(col)), "Missing"))
  col_factor[is.na(col_factor)] <- "Missing"
  col_factor
}), .SDcols = aggregated_cols]

# Remove original PHQ9, GAD7, and ISO1 COVID columns from derived dataset
train_derived[, c(phq_basis_cols, gad_basis_cols, phq_covid_cols, gad_covid_cols, iso1_covid_cols) := NULL]


### task-specific datesets

# subset columns for table 1 and save as RDS
train_tab1_subset_derived_cols <- c("age", "sex", "follow_up_time", "bildungsniveau", "mit_partner_lebend", "phq9_basis_sum", "gad7_basis_sum")
train_tab1_subset_derived <- train_derived[, ..train_tab1_subset_derived_cols]
saveRDS(train_tab1_subset_derived, paste(data_folder, "train", "tab1", "train_tab1_subset_derived.rds", sep = "/"))

# subset columns for table 3 and save as RDS
train_tab3_subset_derived_cols <- c("iso1_covid_sum", "untersuchungsort", "age", "sex", "bildungsniveau", "mit_partner_lebend", "ANG3_covid", "phq9_covid_sum", "gad7_covid_sum")
train_tab3_subset_derived <- train_derived[, ..train_tab3_subset_derived_cols]
saveRDS(train_tab3_subset_derived, paste(data_folder, "train", "tab3", "train_tab3_subset_derived.rds", sep = "/"))