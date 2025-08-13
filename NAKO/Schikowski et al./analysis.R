# ==============================================================================
# Replicate analyses of Schikowski et al. on original and synthetic data
# Create tables and figures for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom utility function for saving tables
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Schikowski_Blutdruck"

# Load the training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# ------------------------------------------------------------------------------
# Perform Analyses
# ------------------------------------------------------------------------------

### Function to perform analyses for original tables and figures
perform_analysis_tables <- function(input_data) {
  
  data <- copy(input_data)
  
  ### Data preparation
  
  # Sex
  data[, sex := factor(sex, levels = c("m", "f"), labels= c("Male", "Female"))]
  
  # Age group
  data[age <25, agegroup := "20-24"]
  data[age >= 25 & age <35, agegroup := "25-34"]
  data[age >= 35 & age <45, agegroup := "35-44"]
  data[age >= 45 & age <55, agegroup := "45-54"]
  data[age >= 55 & age <65, agegroup := "55-64"]
  data[age >= 65 & age <70, agegroup := "65-69"]
  data[, agegroup := factor(agegroup, levels = c("20-24","25-34","35-44","45-54","55-64","65-69", ordered =T))]
  
  # Blood pressure measurements and hypertension status
  data[systole_mess2 <140 & diastole_mess2 < 90 & hypertonie_eigenangabe == 0, hypertonie_status_mess2 := "Normotension"]
  data[systole_mess2 <140 & diastole_mess2 < 90 & hypertonie_eigenangabe == 1, hypertonie_status_mess2 := "Hypertension known controlled"]
  data[!(systole_mess2 <140 & diastole_mess2 < 90) & hypertonie_eigenangabe == 1, hypertonie_status_mess2 := "Hypertension known uncontrolled"]
  data[!(systole_mess2 <140 & diastole_mess2 < 90) & hypertonie_eigenangabe == 0, hypertonie_status_mess2 := "Hypertension unknown uncontrolled"]
  data[, hypertonie_status_mess2 := factor(hypertonie_status_mess2, levels = c("Normotension", "Hypertension known controlled", "Hypertension known uncontrolled", "Hypertension unknown uncontrolled"))]
  
  data[, systole_mess12 := (systole_mess1 + systole_mess2)/2]
  data[, diastole_mess12 := (diastole_mess1 + diastole_mess2)/2]
  data[systole_mess12 <140 & diastole_mess12 < 90 & hypertonie_eigenangabe == 0, hypertonie_status_mess12 := "Normotension"]
  data[systole_mess12 <140 & diastole_mess12 < 90 & hypertonie_eigenangabe == 1, hypertonie_status_mess12 := "Hypertension known controlled"]
  data[!(systole_mess12 <140 & diastole_mess12 < 90) & hypertonie_eigenangabe == 1, hypertonie_status_mess12 := "Hypertension known uncontrolled"]
  data[!(systole_mess12 <140 & diastole_mess12 < 90) & hypertonie_eigenangabe == 0, hypertonie_status_mess12 := "Hypertension unknown uncontrolled"]
  data[, hypertonie_status_mess12 := factor(hypertonie_status_mess12, levels = c("Normotension", "Hypertension known controlled", "Hypertension known uncontrolled", "Hypertension unknown uncontrolled"))]
  
  # Calculate differences between measurements
  data[, diff_systole12_2 := systole_mess12 - systole_mess2]
  data[, diff_diastole12_2 := diastole_mess12 - diastole_mess2]
  
  ### Analyses
  
  # Figure 2
  fig2 <- setorder(data[, data.table(blood_pressure_measure = factor(c("Systolic", "Diastolic"), levels = c("Systolic", "Diastolic", ordered = T)), diff_mean12_2 = c(mean(diff_systole12_2),mean(diff_diastole12_2))), by = .(sex,agegroup)])[]
  setnames(fig2, c("Sex", "Age group", "Blood pressure measure", "Avg. meas. 1/2 - meas. 2 (mean)"))
  
  # Figure 3 and 4 (combined)
  fig3_4 <- setorder(data[, data.table(blood_pressure_measure = factor(c("Systolic", "Systolic", "Diastolic", "Diastolic"), levels = c("Systolic", "Diastolic", ordered = T)),
                                       aggregation = factor(c("Mean", "SD", "Mean", "SD"), levels = c("Mean", "SD", ordered = T)),
                                       value = c(mean(systole_mess2),sd(systole_mess2),
                                                 mean(diastole_mess2),sd(diastole_mess2))), by = .(sex,agegroup)])[]
  setnames(fig3_4, c("Sex", "Age group", "Blood pressure measure", "Aggregation", "Value"))
  
  # Table 5 and 6 (combined)
  data[, N_sex_agegroup := .N, by = .(sex, agegroup)]
  tab5_6 <- cbind(
      setorder(data[, .(hypertonie_status_mess12_share = unique(.N/N_sex_agegroup)), by = .(sex, agegroup, hypertonie_status_mess12)])[],
      setorder(data[, .(hypertonie_status_mess2_share = unique(.N/N_sex_agegroup)), by = .(sex, agegroup, hypertonie_status_mess2)])[, .(hypertonie_status_mess2_share)]
      )
  
  setnames(tab5_6, c("Sex", "Age group", "Hypertension status", "Proportion using avg. meas. 1/2", "Proportion using meas. 2"))
  
  ### Return results as a list
  list(fig2 = fig2, fig3_4 = fig3_4, tab5_6 = tab5_6) 
}

### Perform analyses on original data
analysis_tables_results_real <- perform_analysis_tables(train)

### Perform analyses on synthetic data
syn_files <- dir(paste(data_folder, "syn", sep = "/"), pattern = ".rds", full.names = T)
analysis_tables_results_syn <- as.data.table(foreach(syn_file = syn_files, .combine = "rbind") %dopar% {
  syn <- readRDS(syn_file)
  perform_analysis_tables(syn)
})

# Define aggregation function for synthetic data results
aggregate_analysis_tables_results_syn <- function(analysis_tables_results_syn, aggregate_func, by_vars_list) {
  entries <- lapply(analysis_tables_results_syn, rbindlist)
  entries_agg <- foreach(entry_id = 1:(length(entries))) %do% {
    entry <- entries[[entry_id]]
    entry[,lapply(.SD, aggregate_func), by = entry[,1:by_vars_list[[entry_id]]], .SDcols = -(1:by_vars_list[[entry_id]])]
  }
  names(entries_agg) <- names(analysis_tables_results_syn)
  entries_agg
}

# Aggregate synthetic data results (median and quantiles)
by_vars_list <- sapply(as.list(analysis_tables_results_syn[1]), \(dt) sum(sapply(dt[[1]], \(col) !is.numeric(col))))
analysis_tables_results_syn_median <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, median, list(3,4,3))
analysis_tables_results_syn_lo <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.025), list(3,4,3))
analysis_tables_results_syn_hi <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.975), list(3,4,3))


# ------------------------------------------------------------------------------
# Create Tables for Manuscript
# ------------------------------------------------------------------------------

# Use save_table function from utils.R to save tables
for(table_name in names(analysis_tables_results_real)) {
  save_table(analysis_tables_results_real[[table_name]],
             analysis_tables_results_syn_median[[table_name]],
             analysis_tables_results_syn_lo[[table_name]],
             analysis_tables_results_syn_hi[[table_name]],
             paste0("NAKO/Schikowski et al./tables/Schikowski_", table_name, ".tex"))
}


# ------------------------------------------------------------------------------
# Create Figures for Manuscript
# ---------------------------------------------------------------------

### Figure 2

# Prepare original data results
fig2_real <- analysis_tables_results_real$fig2[, data := "Original"]

# Prepare synthetic data results
fig2_syn <- cbind(analysis_tables_results_syn_median$fig2, analysis_tables_results_syn_lo$fig2[, .(lo = `Avg. meas. 1/2 - meas. 2 (mean)`)], 
                  analysis_tables_results_syn_hi$fig2[, .(hi = `Avg. meas. 1/2 - meas. 2 (mean)`)])[, data := "Synthetic"]
# Combine real and synthetic data results
fig2 <- rbind(fig2_real, fig2_syn, fill = T)

# Create the figure
ggplot(fig2, aes(x = `Age group`, y = `Avg. meas. 1/2 - meas. 2 (mean)`, color = data, group = data)) + 
  geom_point()+
  geom_line() + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  ggh4x::facet_grid2(`Blood pressure measure` ~ Sex, scales = "free_y", independent = "y") +
  ggh4x::facetted_pos_scales(
    y = list(
      scale_y_continuous(limits=c(0, 3)),
      scale_y_continuous(limits=c(0, 3)),
      scale_y_continuous(limits=c(0, 1.5)),
      scale_y_continuous(limits=c(0, 1.5))
    )
  ) +
  labs(title = NULL,
       x = "Age group",
       y = "Avg. meas. 1/2 - meas. 2 (mmHg)",
       color = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("dodgerblue4", "orangered"))

# Save the figure
ggsave(paste("NAKO/Schikowski et al./plots/Schikowski_fig2.pdf"), width = 10, height = 6)


### Figures 3 and 4 (combined)

# Prepare original data results
fig3_4_real <- analysis_tables_results_real$fig3_4[, data := "Original"]

# Prepare synthetic data results
fig3_4_syn <- cbind(analysis_tables_results_syn_median$fig3_4, analysis_tables_results_syn_lo$fig3_4[, .(lo = Value)], analysis_tables_results_syn_hi$fig3_4[, .(hi = Value)])[, data := "Synthetic"]

# Combine real and synthetic data results
fig3_4 <- rbind(fig3_4_real, fig3_4_syn, fill = T)

# Create the figure
ggplot(fig3_4, aes(x = `Age group`, y = Value, color = data, group = data)) + 
  geom_point()+
  geom_line() + 
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  ggh4x::facet_grid2(`Blood pressure measure` ~ Sex + Aggregation, scales = "free_y", independent = "y") +
  labs(title = NULL,
       x = "Age group",
       y = "Blood pressure (mmHg)",
       color = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("dodgerblue4", "orangered"))

# Save the figure
ggsave(paste("NAKO/Schikowski et al./plots/Schikowski_fig3_4.pdf"), width = 10, height = 6)