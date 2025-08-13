# ==============================================================================
# Replicate task-specific data synthesis analyses of Tanoey et al. on original
# and synthetic data
# Create tables and figures for task-specific data synthesis for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom analysis functions
source("NAKO/Tanoey et al./analysis_functions.R")

# Load utility function for saving tables and plots
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Tanoey_Diabetes"

# Load the training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# Full and task-specific synthetic datasets ("derived + subsetted")
syn_datasets <- c("full", "subset_derived")


# ------------------------------------------------------------------------------
# Table 2 univaraible Cox (Task-specific data synthesis)
# ------------------------------------------------------------------------------

### Generate Table 2 univariable Cox regression results for original data (calling functions from analysis_functions.R) 
tab2_univariable_real <- generate_tab2_univariable(prepare_analysis(train))
tab2_univariable_real[, data := "Original"]
tab2_univariable_real[, c("Model", "Age at diagnosis") := NULL]

### Generate Table 2 univariable Cox regression results for synthetic data (calling functions from analysis_functions.R) 
tab2_univariable_syn <- foreach(syn_dataset = syn_datasets) %dopar% {

  if (syn_dataset == "full") {
    syn_dataset_files <- dir(paste(data_folder, "syn/full" , sep = "/"), pattern = ".rds", full.names = T)
  } else if (syn_dataset == "derived") {
    syn_dataset_files <- dir(paste(data_folder, "syn/derived" , sep = "/"), pattern = ".rds", full.names = T)
  } else {
    syn_dataset_files <- dir(paste(data_folder, "syn/tab2_univariable", syn_dataset , sep = "/"), pattern = ".rds", full.names = T)
  }
  
  syn_dataset_stacked <- foreach(syn_file_no = seq_along(syn_dataset_files), .combine = rbind) %do% {
    syn <- readRDS(syn_dataset_files[syn_file_no])
    syn[ , dataset := paste0("Syn_",syn_file_no)]
    syn[]
  }
  
  tab2_univariable_syn_dataset_stacked <- syn_dataset_stacked[, generate_tab2_univariable(prepare_analysis(.SD)), by = dataset]
  
  tab2_univariable_syn_dataset_table <- tab2_univariable_syn_dataset_stacked[, c(.(agg = c("median", "lo", "hi")), lapply(.SD, \(col) c(median(col), quantile(col, 0.025), quantile(col, 0.975)))),
                                                     by = Variable, .SDcols = c("Hazard ratio", "2.5%", "97.5%")]
  tab2_univariable_syn_dataset_table[, data := paste("Synthetic", syn_dataset)]
  
  tab2_univariable_syn_dataset_plot <- tab2_univariable_syn_dataset_stacked[, .(`beta coefficient` = median(log(`Hazard ratio`)), 
                                                        lo = quantile(log(`Hazard ratio`), 0.025),
                                                        hi = quantile(log(`Hazard ratio`), 0.975)) , by = Variable]
  tab2_univariable_syn_dataset_plot[, data := paste("Synthetic", syn_dataset)]
  
  list(table = tab2_univariable_syn_dataset_table[], plot = tab2_univariable_syn_dataset_plot[])
}

tab2_univariable_syn <- lapply(transpose(tab2_univariable_syn), rbindlist)
names(tab2_univariable_syn) <- c("table", "plot")

### Create Table 2 univariable Cox regression for Manuscript (Task-specific data synthesis)

# Use save_table function from utils.R to save tables
save_table(tab2_univariable_real[, -"data"], tab2_univariable_syn$table[agg == "median", -c("agg", "data")], 
           tab2_univariable_syn$table[agg == "lo", -c("agg", "data")], 
           tab2_univariable_syn$table[agg == "hi", -c("agg", "data")],
           save_path = "NAKO/Tanoey et al./tables/Tanoey_tab2_univariable_taskspec.tex",
           colours = c("orangered", "violetred4"))

### Create Figure for Table 2 univariable Cox regression for Manuscript (Task-specific data synthesis)

# Prepare original data results
tab2_univariable_real_plot <- copy(tab2_univariable_real)[, c("Hazard ratio", "2.5%", "97.5%") := lapply(.SD, log), .SDcols = c("Hazard ratio", "2.5%", "97.5%")][]
setnames(tab2_univariable_real_plot, c("Hazard ratio", "2.5%", "97.5%"), c("beta coefficient", "lo", "hi"))

# Prepare synthetic data results
tab2_univariable_syn_plot <- tab2_univariable_syn$plot

# Combine real and synthetic data results
tab2_univariable_plot <- rbind(tab2_univariable_real_plot, tab2_univariable_syn_plot)[Variable != "(Intercept)"]
tab2_univariable_plot[, data := factor(data, levels = c("Original", "Synthetic full", "Synthetic derived", "Synthetic subset", "Synthetic subset_derived"),
                                       labels = c("Original", "Synthetic (full)", "Synthetic (full derived)", "Synthetic (subset)", "Synthetic (task-specific)"))]
tab2_univariable_plot[, Variable := factor(Variable, levels = tab2_univariable_real[, Variable])]

# Create the figure
pd <- position_dodge(width = .5)
ggplot(tab2_univariable_plot[data %in% c("Original", "Synthetic (full)", "Synthetic (task-specific)")], aes(x = Variable, y = `beta coefficient`, col = data)) + 
  geom_point(position = pd) + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = .5) + 
  theme_bw() +
  labs(title = NULL,
       x = NULL,
       y = "beta",
       col = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values=c("dodgerblue4", "orangered", "violetred4" ))

# Save the figure
ggsave("NAKO/Tanoey et al./plots/Tanoey_tab2_univariable_coxreg_taskspec.pdf", width = 10, height = 6)


# ------------------------------------------------------------------------------
# Table 1 (Task-specific data synthesis)
# ------------------------------------------------------------------------------

### Generate Table 1 results for original data (calling functions from analysis_functions.R)
tab1_real <- generate_tab1(prepare_analysis(train))
tab1_real[, data := "Original"]

### Generate Table 1 results for synthetic data (calling functions from analysis_functions.R)
tab1_syn <- foreach(syn_dataset = syn_datasets, .combine = rbind) %dopar% {
  
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
  
  tab1_syn_dataset_stacked <- syn_dataset_stacked[, generate_tab1(prepare_analysis(.SD)), by = dataset]
  
  tab1_syn_dataset_table <- tab1_syn_dataset_stacked[, c(.(agg = c("median", "lo", "hi")), lapply(.SD, \(col) c(median(col), quantile(col, 0.025), quantile(col, 0.975)))),
                                                     by = .(Variable, `Agg.`), .SDcols = c("No diabetes", "Diabetes type 1", "Diabetes type 2", "Total")]
  tab1_syn_dataset_table[, data := paste("Synthetic", syn_dataset)]
  
  tab1_syn_dataset_table
}

### Create Table 1 for Manuscript (Task-specific data synthesis)

# Use save_table function from utils.R to save tables
save_table(tab1_real[, -"data"], tab1_syn[agg == "median", -c("agg", "data")], 
           tab1_syn[agg == "lo", -c("agg", "data")], 
           tab1_syn[agg == "hi", -c("agg", "data")],
           save_path = "NAKO/Tanoey et al./tables/Tanoey_tab1_taskspec.tex",
           colours = c("orangered", "violetred4"))


# ------------------------------------------------------------------------------
# Figure 2 (Task-specific data synthesis)
# ------------------------------------------------------------------------------

### Generate Figure 2 results for original data (calling functions from analysis_functions.R)
fig2_real <- generate_fig2(prepare_analysis(train))
fig2_real[, data := "Original"]

### Generate Figure 2 results for synthetic data (calling functions from analysis_functions.R)
fig2_syn <- foreach(syn_dataset = syn_datasets, .combine = rbind) %dopar% {
  
  if (syn_dataset == "full") {
    syn_dataset_files <- dir(paste(data_folder, "syn/full" , sep = "/"), pattern = ".rds", full.names = T)
  } else if (syn_dataset == "derived") {
    syn_dataset_files <- dir(paste(data_folder, "syn/derived" , sep = "/"), pattern = ".rds", full.names = T)
  } else {
    syn_dataset_files <- dir(paste(data_folder, "syn/fig2", syn_dataset , sep = "/"), pattern = ".rds", full.names = T)
  }
  
  syn_dataset_stacked <- foreach(syn_file_no = seq_along(syn_dataset_files), .combine = rbind) %do% {
    syn <- readRDS(syn_dataset_files[syn_file_no])
    syn[ , dataset := paste0("Syn_",syn_file_no)]
    syn[]
  }
  
  fig2_syn_dataset_stacked <- syn_dataset_stacked[, generate_fig2(prepare_analysis(.SD)), by = dataset]
  
  fig2_syn_dataset_table <- fig2_syn_dataset_stacked[, c(.(agg = c("median", "lo", "hi")), lapply(.SD, \(col) c(median(col), quantile(col, 0.025), quantile(col, 0.975)))),
                                                     by = .(Sex, `Age at diagnosis`, `Therapy`), .SDcols = c("n")]
  fig2_syn_dataset_table[, data := paste("Synthetic", syn_dataset)]
  
  fig2_syn_dataset_table
}

### Create table for Figure 2 for Manuscript (Task-specific data synthesis)

# Use save_table function from utils.R to save tables
save_table(fig2_real[, -"data"], fig2_syn[(agg == "median") & (data %in% c("Original", "Synthetic full", "Synthetic subset_derived")), -c("agg", "data")], 
           fig2_syn[(agg == "lo") & (data %in% c("Original", "Synthetic full", "Synthetic subset_derived")), -c("agg", "data")], 
           fig2_syn[(agg == "hi") & (data %in% c("Original", "Synthetic full", "Synthetic subset_derived")), -c("agg", "data")],
           save_path = "NAKO/Tanoey et al./tables/Tanoey_fig2_taskspec.tex",
           colours = c("orangered", "violetred4"))

### Create Figure 2 for Manuscript (Task-specific data synthesis)

# prepare synthetic data results
fig2_syn_plot <- fig2_syn[, as.list(n), by = .(Sex, `Age at diagnosis`, Therapy, data)]
setnames(fig2_syn_plot, c("V1", "V2", "V3"), c("n", "lo", "hi"))

# combine real and synthetic data results
fig2_plot <- rbind(fig2_real, fig2_syn_plot, fill = T)
fig2_plot[, data := factor(data, levels = c("Original", "Synthetic full", "Synthetic derived", "Synthetic subset", "Synthetic subset_derived"),
                           labels = c("Original", "Synthetic (full)", "Synthetic (full derived)", "Synthetic (subset)", "Synthetic (task-specific)"))]

# Create the figure
ggplot(fig2_plot[data %in% c("Original", "Synthetic (full)", "Synthetic (task-specific)")], aes(x = `Age at diagnosis`, y = n, fill = `Therapy`, color = data)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = "dodge", width = 0.9) + 
  facet_wrap(~Sex) + 
  theme_bw() +
  labs(title = NULL,
       x = "Age at diagnosis",
       y = "Number of type 1 diabetes cases",
       fill = "Therapy",
       color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values=c("dodgerblue4", "lightsteelblue1")) +
  scale_color_manual(values=c("white", "orangered", "violetred4"))

# Save the figure
ggsave(paste0("NAKO/Tanoey et al./plots/Tanoey_fig2_taskspec.pdf"), width = 10, height = 6)