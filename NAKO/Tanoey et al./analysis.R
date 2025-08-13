# ==============================================================================
# Replicate full data synthesis analyses of Tanoey et al. on original and
# synthetic data
# Create tables and figures for full data synthesis for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(survival)
library(ggplot2)

# Load custom analysis functions also conducted for task-specific data synthesis
source("NAKO/Tanoey et al./analysis_functions.R")

# Load custom utility function for saving tables
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Tanoey_Diabetes"

# Load the training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# ------------------------------------------------------------------------------
# Perform Analyses (Full data synthesis)
# ------------------------------------------------------------------------------

### Function to perform analyses for original tables and figures (full data synthesis)
perform_analysis_tables <- function(data_input) {
  
  data <- copy(data_input)
  
  ### Data preparation (called from analysis_functions.R)
  data_analysis <- prepare_analysis(data)
  
  ### Analyses
  
  ## Cox regressoins as prepration for Table 2
  data_analysis[diabetes_type == 1, db1status := 1]
  data_analysis[is.na(db1status), db1status := 0]
  
  # Univariable Cox
  uni_birthorder <- coxph(formula = Surv(db1time_age, db1status) ~ birth_order + strata(birthyrgr), data = data_analysis)
  uni_csec <- coxph(formula = Surv(db1time_age, db1status) ~ caesarean_delivery + strata(birthyrgr), data = data_analysis)
  uni_daycare <- coxph(formula = Surv(db1time_age, db1status) ~ attended_daycare + strata(birthyrgr), data = data_analysis)
  uni_sex <- coxph(formula = Surv(db1time_age, db1status) ~ sex + strata(birthyrgr), data = data_analysis)
  
  univar <- data.table(rbind(summary(uni_birthorder)$conf.int,
                             summary(uni_csec)$conf.int,
                             summary(uni_daycare)$conf.int,
                             summary(uni_sex)$conf.int)
  )[,-2]
  
  # Multivariable full Cox
  mf <- coxph(formula = Surv(db1time_age, db1status) ~ strata(birthyrgr) + 
                birth_order + caesarean_delivery + attended_daycare + sex +
                paternal_diabetes + maternal_diabetes + migration_background + prematural_birth + birth_weight +
                ever_breastfed, data = data_analysis)
  multivar_full <- data.table(summary(mf)$conf.int)[1:9, -2]
  
  
  # Multivariable reduced Cox
  
  mr <- coxph(formula = Surv(db1time_age, db1status) ~ strata(birthyrgr) + 
                birth_order + caesarean_delivery + attended_daycare + sex +
                paternal_diabetes + maternal_diabetes + migration_background, data = data_analysis)
  multivar_reduced <- data.table(summary(mr)$conf.int)[1:9, -2]
  
  mr_younger <- coxph(formula = Surv(db1time_age, db1status) ~ strata(birthyrgr) + 
                        birth_order + caesarean_delivery + attended_daycare + sex +
                        paternal_diabetes + maternal_diabetes + migration_background, data = data_analysis[db1status == 0 | db1status == 1 & db1time_age <= 15,])
  multivar_reduced_younger <- data.table(summary(mr_younger)$conf.int)[1:9, -2]
  
  mr_older <- coxph(formula = Surv(db1time_age, db1status) ~ strata(birthyrgr) + 
                      birth_order + caesarean_delivery + attended_daycare + sex +
                      paternal_diabetes + maternal_diabetes + migration_background, data = data_analysis[db1status == 0 | db1status == 1 & db1time_age > 15,])
  multivar_reduced_older <- data.table(summary(mr_older)$conf.int)[1:9, -2]
  
  # Table 2
  tab2 <- rbind(cbind(Model = "Univariable", `Age at diagnosis` = "0–40", univar),
                cbind(Model = "Multivariable full", `Age at diagnosis` = "0–40", multivar_full),
                cbind(Model = "Multivariable reduced", `Age at diagnosis` = "0–40", multivar_reduced),
                cbind(Model = "Multivariable reduced", `Age at diagnosis` = "0–15", multivar_reduced_younger),
                cbind(Model = "Multivariable reduced", `Age at diagnosis` = "16–40", multivar_reduced_older)
  )
  tab2_names_col <- c("Birth order: First", "Birth order: Second", "Birth order: >=Third", "Birth order: Unknown",
                      "C-section delivery: Yes", "C-section delivery: Unknown",
                      "Attended daycare: Yes", "Attended Daycare: Unknown",
                      "Sex: Female")
  tab2 <- cbind(tab2_names_col, tab2)
  setnames(tab2, c("Variable", "Model", "Age at diagnosis", "Hazard ratio", "2.5%", "97.5%"))
  
  ### Return results as a list
  list(tab2 = tab2)
  
}

### Perform analyses on original data
analysis_tables_results_real <- perform_analysis_tables(train)

### Perform analyses on synthetic data
syn_files <- dir(paste(data_folder, "syn/full", sep = "/"), pattern = ".rds", full.names = T)
analysis_tables_results_syn <- as.data.table(foreach(syn_file = syn_files, .combine = "rbind") %dopar% {
  syn <- readRDS(syn_file)
  result_syn <- perform_analysis_tables(syn)
  results_syn_mod <- lapply(seq_along(result_syn), \(i) {
    result_syn_i <- result_syn[[i]]
    factor_cols <- sapply(analysis_tables_results_real[[i]], \(col) !is.numeric(col))
    result_real_i_factors <- analysis_tables_results_real[[i]][, .SD, .SDcols = factor_cols]
    result_syn_i_mod <- unique(rbind(result_syn_i, result_real_i_factors, fill = T), by = which(factor_cols==T))
    result_syn_i_mod[is.na(result_syn_i_mod)] <- 0
    result_syn_i_mod[result_real_i_factors, on = names(result_real_i_factors), ]
  })
  names(results_syn_mod) <- names(result_syn)
  results_syn_mod
})

# Define aggregation function for synthetic data results
aggregate_analysis_tables_results_syn <- function(analysis_tables_results_syn, aggregate_func, by_vars_list) {
  entries <- lapply(analysis_tables_results_syn, rbindlist)
  entries_agg <- foreach(entry_id = 1:(length(entries))) %do% {
    entry <- entries[[entry_id]]
    entry[,lapply(.SD,\(x) as.numeric(aggregate_func(x))), by = entry[,1:by_vars_list[[entry_id]]], .SDcols = -(1:by_vars_list[[entry_id]])]
  }
  names(entries_agg) <- names(analysis_tables_results_syn)
  entries_agg
}

# Aggregate synthetic data results (median and quantiles)
by_vars_list <- sapply(as.list(analysis_tables_results_syn[1]), \(dt) sum(sapply(dt[[1]], \(col) !is.numeric(col))))
analysis_tables_results_syn_median <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, median, by_vars_list)
analysis_tables_results_syn_lo <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.025, na.rm = T), by_vars_list)
analysis_tables_results_syn_hi <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.975, na.rm = T), by_vars_list)


# ------------------------------------------------------------------------------
# Create Tables for Manuscript (Full data synthesis)
# ------------------------------------------------------------------------------

# Use save_table function from utils.R to save tables
for(table_name in names(analysis_tables_results_real)) {
  save_table(analysis_tables_results_real[[table_name]],
             analysis_tables_results_syn_median[[table_name]],
             analysis_tables_results_syn_lo[[table_name]],
             analysis_tables_results_syn_hi[[table_name]],
             paste0("NAKO/Tanoey et al./tables/Tanoey_", table_name, ".tex"))
}