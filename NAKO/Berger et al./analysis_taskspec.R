# ==============================================================================
# Replicate task-specific data synthesis analyses of Berger et al. on original
# and synthetic data
# Create tables and figures for task-specific data synthesis for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom analysis functions
source("NAKO/Berger et al./analysis_functions.R")

# Load utility function for saving tables and plots
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Berger_Corona"

# Load the training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# Full and task-specific synthetic datasets ("derived + subsetted")
syn_datasets <- c("full", "subset_derived")

# ------------------------------------------------------------------------------
# Table 3 (Task-specific data synthesis)
# ------------------------------------------------------------------------------

### Function to prepare data for Table 3
prepare_tab3 <- function(data_, derive = F) {
  data <- copy(data_)
  data[,untersuchungsort := as.numeric(as.factor(untersuchungsort))]
  
  data[, sex := factor(data$sex, levels = c("m","f"), labels = c("Male", "Female"))]
  
  data[bildungsniveau %in% c("Hauptschulabschluss", "Polytechnische Oberschule DDR Abschluss 8./9. Klasse") ,bildungsniveau:= "Hauptschulabschluss"]
  data[bildungsniveau %in% c("Allgemeine oder fachgebundene Hochschulreife/Abitur",
                             "Fachhochschulreife, Abschluss einer Fachoberschule",
                             "Abitur (zweiter Bildungsweg)")
       , bildungsniveau:= "Fach-/Abitur"]
  data[bildungsniveau %in% c("Realschulabschluss", "Polytechnische Oberschule DDR Abschluss 10. Klasse"), bildungsniveau:= "Realschulabschluss"]
  data[bildungsniveau %in% c("Ohne Schulabschluss", "Schueler/-in", "Einen anderen Schulabschluss", "Missing"), bildungsniveau:= "kein/anderer Abschluss"]
  data[, bildungsniveau := factor(bildungsniveau, levels = c("kein/anderer Abschluss","Hauptschulabschluss",
                                                             "Realschulabschluss", "Fach-/Abitur"), ordered = T)]
  data[, bildungsniveau_num := as.numeric(bildungsniveau)]
  
  data[mit_partner_lebend == "Missing", mit_partner_lebend := "Nein"]
  data[mit_partner_lebend != "Nein", mit_partner_lebend := "Ja"]
  data$mit_partner_lebend <- factor(data$mit_partner_lebend, levels = c("Nein","Ja"))
  
  data[, ANG3_covid := replace(ANG3_covid, ANG3_covid == "Missing", NA)]
  data[, ANG3_covid := replace(ANG3_covid, ANG3_covid != "Ja", "Nein")]
  data[, ANG3_covid := factor(ANG3_covid, levels = c("Nein", "Ja"))]
  
  if (derive) {
    
    phq_covid_cols <- names(data)[intersect(grep("covid", names(data)), grep("PHQ9", names(data)))]
    gad_covid_cols <- names(data)[intersect(grep("covid", names(data)), grep("GAD7", names(data)))]
    iso1_covid_cols <- names(data)[intersect(grep("covid", names(data)), grep("ISO1", names(data)))]
    
    data[, phq9_covid_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = phq_covid_cols]
    data[, gad7_covid_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = gad_covid_cols]
    
    data[, (iso1_covid_cols) := replace(.SD, .SD == "Selten", "Nie"), .SDcols = iso1_covid_cols]
    data[, (iso1_covid_cols) := replace(.SD, .SD == "Missing", NA), .SDcols = iso1_covid_cols]
    data[, (iso1_covid_cols) := lapply(.SD, factor, levels = c("Nie", "Manchmal", "Oft"), labels = c("Nie/Selten", "Manchmal", "Oft")),.SDcols = iso1_covid_cols]
    data[, iso1_covid_sum := rowSums(sapply(.SD, as.numeric), na.rm = F), .SDcols = iso1_covid_cols]
    data[, c(phq_covid_cols, gad_covid_cols, iso1_covid_cols) := NULL]
  } else {
    aggregated_cols <- c("phq9_covid_sum", "gad7_covid_sum", "iso1_covid_sum")
    data[, (aggregated_cols) := lapply(.SD, \(col) as.numeric(as.character(col))), .SDcols = aggregated_cols]
  }
  
  data[]
}

### Generate Table 3 for original data
tab3_real <- generate_tab3(prepare_tab3(train, derive = T))
tab3_real[, data := "Original"]

### Generate Table 3 for synthetic data
tab3_syn <- foreach(syn_dataset = syn_datasets) %dopar% {
  
  derived = ifelse(syn_dataset %in% c("derived", "subset_derived"), T, F)
  
  if (syn_dataset == "full") {
    syn_dataset_files <- dir(paste(data_folder, "syn/full" , sep = "/"), pattern = ".rds", full.names = T)
  } else if (syn_dataset == "derived") {
    syn_dataset_files <- dir(paste(data_folder, "syn/derived" , sep = "/"), pattern = ".rds", full.names = T)
  } else {
    syn_dataset_files <- dir(paste(data_folder, "syn/tab3", syn_dataset , sep = "/"), pattern = ".rds", full.names = T)
  }
  
  syn_dataset_stacked <- foreach(syn_file_no = seq_along(syn_dataset_files), .combine = rbind) %do% {
    syn <- readRDS(syn_dataset_files[syn_file_no])
    syn[ , dataset := paste0("Syn_",syn_file_no)]
    syn[]
  }
  
  tab3_syn_dataset_stacked <- syn_dataset_stacked[, generate_tab3(prepare_tab3(.SD, derive = !derived)), by = dataset]
  
  tab3_syn_dataset_table <- tab3_syn_dataset_stacked[, c(.(agg = c("median", "lo", "hi")), lapply(.SD, \(col) c(median(col), quantile(col, 0.025), quantile(col, 0.975)))),
                                                by = Variable, .SDcols = c("beta coefficient", "2.5%", "97.5%")]
  tab3_syn_dataset_table[, data := paste("Synthetic", syn_dataset)]
  
  tab3_syn_dataset_plot <- tab3_syn_dataset_stacked[, .(`beta coefficient` = median(`beta coefficient`), 
                                           lo = quantile(`beta coefficient`, 0.025),
                                           hi = quantile(`beta coefficient`, 0.975)) , by = Variable]
  tab3_syn_dataset_plot[, data := paste("Synthetic", syn_dataset)]
  
  list(table = tab3_syn_dataset_table[], plot = tab3_syn_dataset_plot[])
}
tab3_syn <- lapply(transpose(tab3_syn), rbindlist)
names(tab3_syn) <- c("table", "plot")

### Create Table 3 for Manuscript (Task-specific data synthesis)

# Use save_table function from utils.R to save tables
save_table(tab3_real[, -"data"], tab3_syn$table[agg == "median", -c("agg", "data")], 
           tab3_syn$table[agg == "lo", -c("agg", "data")], 
           tab3_syn$table[agg == "hi", -c("agg", "data")],
           save_path = "NAKO/Berger et al./tables/Berger_tab3_taskspec.tex",
           colours = c("orangered", "violetred4"))


### Create figure for Table 3 for Manuscript (Task-specific data synthesis)

# Prepare original data results
tab3_real_plot <- copy(tab3_real)
setnames(tab3_real_plot, c("2.5%", "97.5%"), c("lo", "hi"))

# Prepare synthetic data results
tab3_syn_plot <- tab3_syn$plot

# Combine real and synthetic data results
tab3_plot <- rbind(tab3_real_plot, tab3_syn_plot)[Variable != "(Intercept)"]
tab3_plot[, data := factor(data, levels = c("Original", "Synthetic full", "Synthetic subset_derived"),
                           labels = c("Original", "Synthetic (full)", "Synthetic (task-specific)"))]

# Create the figure
pd <- position_dodge(width = .5)
ggplot(tab3_plot[data %in% c("Original", "Synthetic (full)", "Synthetic (task-specific)")], aes(x = Variable, y = `beta coefficient`, col = data)) + 
  geom_point(position = pd) + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = .5) + 
  theme_bw() +
  labs(title = NULL,
       x = NULL,
       y = "beta",
       col = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("dodgerblue4", "orangered", "violetred4"))

# Save the figure
ggsave("NAKO/Berger et al./plots/Berger_tab3_linreg_taskspec.pdf", width = 10, height = 6)


# ------------------------------------------------------------------------------
# Table 1 (Task-specific data synthesis)
# ------------------------------------------------------------------------------

### Function to prepare data for Table 1
prepare_tab1 <- function(data_, derive = F) {
  data <- copy(data_)

  data[, sex := factor(data$sex, levels = c("m","f"), labels = c("Male", "Female"))]
  
  data[age >= 20 & age < 30, age_grp := "20-29"]
  data[age >= 30 & age < 60, age_grp := "30-59"]
  data[age >= 60, age_grp := "60+"]
  data[, age_grp := factor(age_grp, levels = c("20-29", "30-59", "60+"))]
  
  data[bildungsniveau %in% c("Hauptschulabschluss", "Polytechnische Oberschule DDR Abschluss 8./9. Klasse") ,bildungsniveau:= "Hauptschulabschluss"]
  data[bildungsniveau %in% c("Allgemeine oder fachgebundene Hochschulreife/Abitur",
                             "Fachhochschulreife, Abschluss einer Fachoberschule",
                             "Abitur (zweiter Bildungsweg)")
       , bildungsniveau:= "Fach-/Abitur"]
  data[bildungsniveau %in% c("Realschulabschluss", "Polytechnische Oberschule DDR Abschluss 10. Klasse"), bildungsniveau:= "Realschulabschluss"]
  data[bildungsniveau %in% c("Ohne Schulabschluss", "Schueler/-in", "Einen anderen Schulabschluss", "Missing"), bildungsniveau:= "kein/anderer Abschluss"]
  data[, bildungsniveau := factor(bildungsniveau, levels = c("kein/anderer Abschluss","Hauptschulabschluss",
                                                             "Realschulabschluss", "Fach-/Abitur"), ordered = T)]
  data[, bildungsniveau_num := as.numeric(bildungsniveau)]
  
  data[mit_partner_lebend == "Missing", mit_partner_lebend := "Nein"]
  data[mit_partner_lebend != "Nein", mit_partner_lebend := "Ja"]
  data$mit_partner_lebend <- factor(data$mit_partner_lebend, levels = c("Nein","Ja"))
  
  if (derive) {
    
    data[,untersuchungsdatum := as.Date.POSIXct(untersuchungsdatum)]
    data[,date_stamp_covid := as.Date.POSIXct(date_stamp_covid)]
    data[, follow_up_time := as.numeric(date_stamp_covid - untersuchungsdatum)/365 - 0.5]
    
    phq_basis_cols <- names(train)[intersect(grep("basis", names(train)), grep("PHQ9", names(train)))]
    gad_basis_cols <- names(train)[intersect(grep("basis", names(train)), grep("GAD7", names(train)))]
    
    data[, phq9_basis_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = phq_basis_cols]
    data[, gad7_basis_sum := rowSums(sapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), na.rm = F), .SDcols = gad_basis_cols]
    
    data[, c(phq_basis_cols, gad_basis_cols) := NULL]
  } else {
    aggregated_cols <- c("phq9_basis_sum", "gad7_basis_sum")
    data[, (aggregated_cols) := lapply(.SD, \(col) as.numeric(as.character(col))), .SDcols = aggregated_cols]
  }
  
  data[]
}

### Generate Table 1 for original data
tab1_real <- generate_tab1(prepare_tab1(train, derive = T))
tab1_real[, data := "Original"]

### Generate Table 1 for synthetic data
tab1_syn <- foreach(syn_dataset = syn_datasets, .combine = rbind) %dopar% {
  
  derived = ifelse(syn_dataset %in% c("derived", "subset_derived"), T, F)
  
  if (syn_dataset == "full") {
    syn_dataset_files <- dir(paste(data_folder, "syn/full" , sep = "/"), pattern = ".rds", full.names = T)
  } else if (syn_dataset == "derived") {
    syn_dataset_files <- dir(paste(data_folder, "syn/derived" , sep = "/"), pattern = ".rds", full.names = T)
  } else {
    syn_dataset_files <- dir(paste(data_folder, "syn/tab1", syn_dataset , sep = "/"), pattern = ".rds", full.names = T)
  }
  
  syn_dataset_stacked <- foreach(syn_file_no = seq_along(syn_dataset_files), .combine = rbind) %do% {
    syn <- readRDS(syn_dataset_files[syn_file_no])
    syn[ , dataset := paste0("Syn_",syn_file_no)]
    syn[]
  }
  
  tab1_syn_dataset_stacked <- syn_dataset_stacked[, generate_tab1(prepare_tab1(.SD, derive = !derived)), by = dataset]
  
  tab1_syn_dataset_table <- tab1_syn_dataset_stacked[, c(.(agg = c("median", "lo", "hi")), lapply(.SD, \(col) c(median(col), quantile(col, 0.025), quantile(col, 0.975)))),
                                                     by = Variable, .SDcols = "Value"]
  tab1_syn_dataset_table[, data := paste("Synthetic", syn_dataset)]
  
  tab1_syn_dataset_table
}

### Create Table 1 for Manuscript (Task-specific data synthesis)

# Use save_table function from utils.R to save tables
save_table(tab1_real[, -"data"], tab1_syn[(agg == "median") & (data %in% c("Original", "Synthetic full", "Synthetic subset_derived")), -c("agg", "data")], 
           tab1_syn[agg == "lo", -c("agg", "data")], 
           tab1_syn[agg == "hi", -c("agg", "data")],
           save_path = "NAKO/Berger et al./tables/Berger_tab1_taskspec.tex",
           colours = c("orangered", "violetred4"))