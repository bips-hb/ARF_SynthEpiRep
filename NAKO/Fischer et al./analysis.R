# ==============================================================================
# Replicate analyses of Fischer et al. on original and synthetic data
# Create tables and figures for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom utility function for saving tables
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Fischer_Anthopometrie"

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
  data[, sex := factor(sex, levels = c("m", "f"), labels = c("Male", "Female"))]
  
  # BMI
  data[, bmi := gewicht/(groesse/100)^2]
  data[, bmi_grp := factor(sapply(bmi, \(x) {
    if(!is.na(x)) {
      if(x >= 25 & x < 30) "Overweight"
      else if(x >= 30) "Obese"
      else "Not overweight"
    } else NA
  }), levels = c("Not overweight", "Overweight", "Obese"))]
  
  # Age group
  data[, age_grp := factor(sapply(age, \(x) {
    if(!is.na(x)) {
      if(x >= 20 & x < 30) "20-29"
      else if(x >= 30 & x < 40) "30-39"
      else if(x >= 40 & x < 50) "40-49"
      else if(x >= 50 & x < 60) "50-59"
      else if(x >= 60 & x < 70) "60-69"
      else "70+"
    } else NA
  }), levels = c("20-29", "30-39", "40-49", "50-59", "60-69", "70+"))]
  
  ### Analyses
  
  # Figure 2
  fig2 <- dcast(data[, .(mean = mean(groesse, na.rm = T)), by = .(untersuchungsort, sex)], untersuchungsort ~ sex, value.var = "mean")
  setnames(fig2, c("Study centre", "Height men (mean)", "Height women (mean)"))
  
  # Figure 3
  fig3 <- copy(data)
  fig3[!is.na(bmi), share_age_sex := .N, by = .(sex, age_grp)]
  fig3 <- setorder(fig3[!is.na(bmi),.(Proportion = unique(.N/share_age_sex)), by = .(sex, age_grp, bmi_grp)][bmi_grp %in% c("Overweight", "Obese")])[]
  setnames(fig3, c("Sex", "Age group", "BMI group", "Proportion"))
  
  # Figure 4
  fig4 <- dcast(data[,.(mean = mean(bmi, na.rm = T)), by = .(untersuchungsort, sex)], untersuchungsort ~ sex, value.var = "mean")
  setnames(fig4, c("Study centre", "BMI men (mean)", "BMI women (mean)"))
  
  # Figure 5
  fig5 <- copy(data)[, .(sex, sat1, sat2, vat1, vat2)]
  fig5[, Subcutaneous := rowMeans(.SD, na.rm = T), .SDcols = c("sat1", "sat2")]
  fig5[, Visceral := rowMeans(.SD, na.rm = T), .SDcols = c("vat1", "vat2")]
  fig5 <- fig5[, -c("sat1", "sat2", "vat1", "vat2")]
  fig5 <- melt(fig5, id.vars = "sex", variable.name = "at_type", variable.factor = T)
  
  bins <- data.table(binwidth = c(0.2, 0.5), min_binleft = c(-0.1, -0.25), at_type = c("Subcutaneous", "Visceral"))
  fig5 <- fig5[!is.na(value), .(value, min = min(value), max = max(value)), by = .(sex, at_type)]
  fig5 <- fig5[bins, on = "at_type"][, .(value, min_binleft_new = min - (min - min_binleft) %% binwidth, binwidth, max), by = .(sex, at_type)]
  fig5_0 <- fig5[, .(x = seq(min_binleft_new[1], max[1], binwidth[1]) + binwidth[1]/2, y = 0), by = .(sex, at_type)]
  fig5[, bin := (value - min_binleft_new)%/%binwidth * binwidth + min_binleft_new]
  fig5 <- unique(fig5[, .(binwidth, x = bin + binwidth/2, y = .N), by = .(sex, at_type, bin)])[, -c("bin", "binwidth")]
  fig5 <- unique(rbind(fig5, fig5_0), by = c("sex", "at_type", "x"))
  setorder(fig5, at_type, sex, x)
  setnames(fig5, c("Sex", "Adipose tissue type", "Value", "Count"))
  fig5[, Value := factor(Value)]
  
  # Table 1
  tab1 <- setnames(foreach(variable = c("groesse", "gewicht", "bmi", "taillenumfang", "hueftumfang"), .combine = rbind) %do% {
    cbind(variable, setorder(data[, isna := sapply(.SD, is.na), .SDcols = variable][isna == F, sapply(.SD, \(x) {
      list(.N, mean(x), sd(x), quantile(x, 0.1), median(x), quantile(x, 0.9))
    }), by = .(sex, age_grp), .SDcols = variable])[])
  }, c("Variable", "Sex", "Age group", "n", "Mean", "SD", "10th percentile", "50th perecentile", "90th percentile"))[]
  data[, isna := NULL]

  tab1[, Variable := factor(Variable,
                            levels = c("groesse", "gewicht", "bmi", "taillenumfang", "hueftumfang"),
                            labels = c("Height", "Weight", "BMI", "Waist circumference", "Hip circumference"))]
  

  ### Return results as a list
  list(fig2 = fig2, fig3 = fig3, fig4 = fig4, fig5 = fig5, tab1 = tab1)

}

### Perform analyses on original data
analysis_tables_results_real <- perform_analysis_tables(train)
sum_values_real <- analysis_tables_results_real$fig5[, .(sum_counts_real = sum(Count)), by = `Adipose tissue type`]

### Perform analyses on synethetic data
syn_files <- dir(paste(data_folder, "syn", sep = "/"), pattern = ".rds", full.names = T)
analysis_tables_results_syn <- as.data.table(foreach(syn_file = syn_files, .combine = "rbind") %dopar% {
  syn <- readRDS(syn_file)
  analysis_tables <- perform_analysis_tables(syn)
  analysis_tables$fig5 <- analysis_tables$fig5[sum_values_real, on = "Adipose tissue type"][,  `:=` (Count = Count/sum(Count)*sum_counts_real[[1]]), by = `Adipose tissue type`][, -"sum_counts_real"]
  analysis_tables
})

# Define aggregation function for synthetic data results
aggregate_analysis_tables_results_syn <- function(analysis_tables_results_syn, aggregate_func, by_vars_list) {
  entries <- lapply(analysis_tables_results_syn, rbindlist)
  entries_agg <- foreach(entry_id = 1:(length(entries))) %do% {
    entry <- entries[[entry_id]]
    entry[, (lapply(.SD, \(x) as.numeric(aggregate_func(x)))), by = entry[,1:by_vars_list[[entry_id]]], .SDcols = -(1:by_vars_list[[entry_id]])]
  }
  names(entries_agg) <- names(analysis_tables_results_syn)
  entries_agg
}

# Aggregate synthetic data results (median and quantiles)
by_vars_list <- sapply(as.list(analysis_tables_results_syn[1]), \(dt) sum(sapply(dt[[1]], \(col) !is.numeric(col))))
analysis_tables_results_syn_median <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, median, by_vars_list)
analysis_tables_results_syn_lo <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.025), by_vars_list)
analysis_tables_results_syn_hi <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.975), by_vars_list)


# ------------------------------------------------------------------------------
# Create Tables for Manuscript
# ------------------------------------------------------------------------------

# Use save_table function from utils.R to save tables
for(table_name in names(analysis_tables_results_real)) {
  save_table(analysis_tables_results_real[[table_name]],
             analysis_tables_results_syn_median[[table_name]],
             analysis_tables_results_syn_lo[[table_name]],
             analysis_tables_results_syn_hi[[table_name]],
             paste0("NAKO/Fischer et al./tables/Fischer_", table_name, ".tex"))
}


# ------------------------------------------------------------------------------
# Create Figures for Manuscript
# ------------------------------------------------------------------------------

### Figure 3

# Prepare original data results
fig3_real <- analysis_tables_results_real$fig3[, data := "Original"]

# Prepare synthetic data results
fig3_syn <- cbind(analysis_tables_results_syn_median$fig3,
                  analysis_tables_results_syn_lo$fig3[,.(lo = Proportion)],
                  analysis_tables_results_syn_hi$fig3[,.(hi = Proportion)])[, data := "Synthetic"]

# Combine real and synthetic data results
fig3 <- rbind(fig3_real, fig3_syn, fill = T)

# Create the figure
ggplot(fig3, aes(x = `Age group`, y = Proportion, fill = `BMI group`, color = data)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = "dodge", width = 0.9) + 
  facet_wrap(~Sex) + 
  theme_bw() +
  labs(title = NULL,
       x = "Age group",
       y = "Proportion",
       fill = "BMI group",
       color = NULL) +
  guides(color = guide_legend(order=1),
         fill = guide_legend(order=2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values=c("dodgerblue4", "lightsteelblue1")) +
  scale_color_manual(values=c("white", "orangered"))

# Save the figure
ggsave(paste("NAKO/Fischer et al./plots/Fischer_fig3.pdf"), width = 10, height = 6)


### Figure 5

# Prepare original data results
fig5_real <- analysis_tables_results_real$fig5[, data := "Original"]

# Prepare synthetic data results
fig5_syn <- cbind(analysis_tables_results_syn_median$fig5,
                  analysis_tables_results_syn_lo$fig5[,.(lo = Count)],
                  analysis_tables_results_syn_hi$fig5[,.(hi = Count)])[, data := "Synthetic"]

# Combine real and synthetic data results
fig5 <- rbind(fig5_real, fig5_syn, fill = T)

# Adjust binwidth with respective to adipose tissue type
fig5[, binwidth := fifelse(`Adipose tissue type` == "Subcutaneous", 0.2, 0.5)]
fig5[, Value := as.numeric(as.character(Value))]

# Create the figure
ggplot(fig5, aes(x = Value, y = Count, width = binwidth, col = data, fill = data)) + 
  geom_bar(fig5[data == "Original"],  mapping = aes(), stat = "identity") +
  geom_line(fig5[data != "Original"],  mapping = aes(), linewidth = 0.5) +
  geom_ribbon(fig5[data != "Original"], mapping = aes(ymin = lo, ymax = hi), linetype = 0, alpha = 0.5) + 
  theme(axis.title=element_blank()) + facet_grid(Sex ~ `Adipose tissue type`, scales = "free_x") + theme_bw() +
  ylim(0, 1400) + 
  labs(title = NULL,
       x = "Adipose tissue (cm)",
       y = "Count",
       color = NULL,
       fill = NULL) +
  ggh4x::facetted_pos_scales(
    x = list(
      scale_x_continuous(limits=c(0, 8)),
      scale_x_continuous(limits=c(0, 20))
    )
  ) +
  scale_fill_manual(values=c("dodgerblue4", "orangered")) +
  scale_color_manual(values=c("white", "orangered"))

# Save the figure
ggsave(paste("NAKO/Fischer et al./plots/Fischer_fig5.pdf"), width = 10, height = 6)
