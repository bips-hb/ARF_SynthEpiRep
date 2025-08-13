# ==============================================================================
# Replicate analyses of Wienbergen et al. on original and synthetic data
# Create tables and figures for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom utility function for saving tables
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Wienbergen_MI"

# Load the training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# ------------------------------------------------------------------------------
# Perform Analyses
# ------------------------------------------------------------------------------

### Function to perform analyses for original tables and figures
perform_analysis_tables <- function(data_input) {
  
  data <- copy(data_input)
  
  ### Data preparation
  
  # Case / Control variable
  data[, Case := factor(Case, levels = c("0","1"))]
  
  # Sex
  data[, Sex := factor(Sex, levels = c("m","f"))]
  
  # Born in Germany
  data[, Birth_Germany := factor(Birth_Germany, levels = c("1","0"))]
  
  # Smoking status
  data[, Smoking_status := factor(Smoking_status,
                                  levels = c("never", "former", "current"),
                                  labels = c("Never", "Former", "Current"))]
  
  # Alcohol consumption
  data[, Alcohol_consumption := factor(Alcohol_consumption,
                                       levels = c("never", "1x/month", "2-4x/month", "2-3x/week","4+x/week"),
                                       labels = c("Never", "1x/month", "2-4x/month", "2-3x/week","4+x/week"))]
  
  # Hypertension and diabetes mellitus
  data[, Hypertension := factor(Hypertension, levels = c("0","1"), labels = c("No","Yes"))]
  data[, Diabetes_mellitus := factor(Diabetes_mellitus, levels = c("0","1"), labels = c("No","Yes"))]
  data[, Hyp_or_Diab := as.numeric(as.numeric(Hypertension) - 1|as.numeric(Diabetes_mellitus) - 1)]
  
  # Family history of premature MI
  data[, Family_history_premature_MI := factor(Family_history_premature_MI, levels = c("0","1","unknown"), labels = c("No","Yes","Unknown"))]
  
  # Education
  data[, Education := factor(Education, levels = c("12+years","< 10years","10-11years"), labels = c("12+years","<10years","10-11years"))]
  
  # Waist-to-hip ratio categorized
  data[Waist_hip_ratio < 0.867, Waist_hip_ratio_cat := "<0.87"]
  data[Waist_hip_ratio >= 0.867 & round(Waist_hip_ratio, 2) < 0.932, Waist_hip_ratio_cat := "0.87–0.93"]
  data[Waist_hip_ratio >= 0.932, Waist_hip_ratio_cat := "0.93+"]
  data[, Waist_hip_ratio_cat := factor(Waist_hip_ratio_cat, levels = c("<0.87","0.87–0.93","0.93+"))]
  
  # Body mass index (BMI) per 5 units and categorized
  data[, BMIper5:= pmin(BMI %/% 5 - 2, 7)]
  data[BMI < 25, BMI_cat := "<25.0"]
  data[BMI >= 30, BMI_cat := "30.0+"]
  data[is.na(BMI_cat), BMI_cat := "25.0-29.9"]
  data[, BMI_cat := factor(BMI_cat, levels = c("<25.0", "25.0-29.9", "30.0+"))]
  data[, Ageper5:= pmin(Age %/% 5 -3, 5)]
  
  # Reorder columns for better readability
  setcolorder(data, c("Case", "Age", "Ageper5", "Sex", "Birth_Germany", "Education", "Smoking_status", "Alcohol_consumption",
                      "BMI", "BMI_cat", "BMIper5", "Waist_hip_ratio", "Waist_hip_ratio_cat", "Hypertension", "Diabetes_mellitus", "Hyp_or_Diab"))[]
  
  ### Analyses
  
  # Table 1
  data_case <- data[Case == 1]
  data_control <- data[Case == 0]
  
  create_tab1_part <- function(data_part) {
    
    tab1_part <- data.table(Variable = character(35), Value = NA_real_, Proportion = NA_real_)
    tab1_part_cols_cnt <- 1:2
    tab1_part_cols_cat <- 1:3
    tab1_part[1, tab1_part_cols_cnt] <- list("Number of participants (n)", data_part[, .N])
    #tab1_part[2, tab1_part_cols_cnt] <- list("Participants with missings" ,data_part[!complete.cases(data_part),.N])
    tab1_part[2:4, tab1_part_cols_cnt] <- data.table(c("Age (years) (median)", "Age (years) (1st quartile)", "Age (years) (3rd quartile)"),
                                                     data_part[, c(median(Age), quantile(Age, 0.25), quantile(Age, 0.75))])
    tab1_part[5, tab1_part_cols_cat] <- c("Sex: Male (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Sex)])[1,2:3])
    tab1_part[6, tab1_part_cols_cat] <- c("Sex: Female (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Sex)])[2,2:3])
    tab1_part[7, tab1_part_cols_cat] <- c("Country of birth: Other countries (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Birth_Germany)])[2,2:3])
    tab1_part[8, tab1_part_cols_cat] <- c("Country of birth: Germany (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Birth_Germany)])[1,2:3])
    tab1_part[9, tab1_part_cols_cat] <- c("School education (years): <10 (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Education)])[2,2:3])
    tab1_part[10, tab1_part_cols_cat] <- c("School education (years): 10-11 (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Education)])[3,2:3])
    tab1_part[11, tab1_part_cols_cat] <- c("School education (years): 12+ (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Education)])[1,2:3])
    tab1_part[12, tab1_part_cols_cat] <- c("Smoking status: Never (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Smoking_status)])[1,2:3])
    tab1_part[13, tab1_part_cols_cat] <- c("Smoking status: Former (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Smoking_status)])[2,2:3])
    tab1_part[14, tab1_part_cols_cat] <- c("Smoking status: Current (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Smoking_status)])[3,2:3])                                                                                             
    tab1_part[15, tab1_part_cols_cat] <- c("Alcohol consumption: Never (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Alcohol_consumption)])[1,2:3])
    tab1_part[16, tab1_part_cols_cat] <- c("Alcohol consumption: Once a month (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Alcohol_consumption)])[2,2:3])
    tab1_part[17, tab1_part_cols_cat] <- c("Alcohol consumption: 2-4 times a month (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Alcohol_consumption)])[3,2:3])
    tab1_part[18, tab1_part_cols_cat] <- c("Alcohol consumption: 2-3 times a week (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Alcohol_consumption)])[4,2:3])
    tab1_part[19, tab1_part_cols_cat] <- c("Alcohol consumption: 4+ times a week (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Alcohol_consumption)])[5,2:3])
    tab1_part[20:22, tab1_part_cols_cnt] <- data.table(c("Body mass index (kg/m²) (median)","Body mass index (kg/m²) (1st quartile)","Body mass index (kg/m²) (3rd quartile)"),
                                                       data_part[, c(median(BMI), quantile(BMI, 0.25), quantile(BMI, 0.75))])
    tab1_part[23, tab1_part_cols_cat] <- c("Body mass index: <25.0 (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(BMI_cat)])[1,2:3])
    tab1_part[24, tab1_part_cols_cat] <- c("Body mass index: 25.0-29.9 (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(BMI_cat)])[2,2:3])
    tab1_part[25, tab1_part_cols_cat] <- c("Body mass index: 30.0+ (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(BMI_cat)])[3,2:3])
    tab1_part[26:28, tab1_part_cols_cnt] <- data.table(c("Waist-to-hip ratio (median)", "Waist-to-hip ratio (1st quartile)", "Waist-to-hip ratio (3rd quartile)"),
                                                       data_part[, c(median(Waist_hip_ratio, na.rm = T), quantile(Waist_hip_ratio, 0.25, na.rm = T), quantile(Waist_hip_ratio, 0.75, na.rm = T))])
    tab1_part[29, tab1_part_cols_cat] <- c("Hypertension: No (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Hypertension)])[1,2:3])
    tab1_part[30, tab1_part_cols_cat] <- c("Hypertension: Yes (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Hypertension)])[2,2:3])
    tab1_part[31, tab1_part_cols_cat] <- c("Diabetes mellitus: No (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Diabetes_mellitus)])[1,2:3])
    tab1_part[32, tab1_part_cols_cat] <- c("Diabetes mellitus: Yes (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Diabetes_mellitus)])[2,2:3])
    tab1_part[33, tab1_part_cols_cat] <- c("Family history of premature MI: No (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Family_history_premature_MI)])[1,2:3])
    tab1_part[34, tab1_part_cols_cat] <- c("Family history of premature MI: Yes (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Family_history_premature_MI)])[2,2:3])
    tab1_part[35, tab1_part_cols_cat] <- c("Family history of premature MI: Unknown (n)", setorder(data_part[, .(.N, .N/nrow(data_part)), by = .(Family_history_premature_MI)])[3,2:3])
    
    tab1_part
    
  }
  
  tab1 <- cbind(create_tab1_part(data_case), create_tab1_part(data_control)[, -1])
  setnames(tab1, old = -1, new = c(paste(names(tab1)[2:3], "cases"), paste(names(tab1)[4:5], "controls")))
  
  # Helper function for logistic regression results (Table 2 and Table 3)
  perform_logreg <- function(data, adj_level, variable_of_interest, odds_ratios = F) {
    formula <- update(adj_levels[[adj_level]],  paste(". ~ . +", variable_of_interest))
    if (variable_of_interest %in% c("BMIper5", "Waist_hip_ratio_cat")) {
      formula <- update(formula, . ~ . - BMI_cat)
    } else if (variable_of_interest %in% c("Hypertension", "Diabetes_mellitus")) {
      formula <- update(formula,  . ~ . - Hyp_or_Diab)
    }
    logreg <- glm(formula, data = data, family = "binomial")
    result <- data.table(estimate = coef(logreg),
                         `2.5%` = summary(logreg)$coefficients[,1] - 1.96*summary(logreg)$coefficients[,2], 
                         `97.5%` = summary(logreg)$coefficients[,1] + 1.96*summary(logreg)$coefficients[,2])
    if (odds_ratios) {
      result <- exp(result)
      setnames(result, "estimate", "Odds ratio")
    } else {
      setnames(result, "estimate", "Coefficient")
    }
    result <- cbind(`Adj. level` = factor(adj_level), Variable = names(coef(logreg)), result)[stringr::str_detect(Variable, variable_of_interest)]
    result[, Variable := paste0(variable_of_interest, ": ", substr(Variable, nchar(variable_of_interest) + 1, nchar(Variable)))]
    result[substr(Variable, nchar(Variable) - 1, nchar(Variable)) == ": ", Variable := substr(Variable, 0, nchar(Variable) - 2)]
    
    # replace in Variables: "BMIper5" with "BMI per 5", "_cat" with "", "_" with " ", "number" with "Number"
    result[Variable == "BMIper5", Variable := "BMI 5 units increase"]
    result[, Variable := gsub("_cat", "", Variable)]
    result[, Variable := gsub("_", " ", Variable)]
    result[, Variable := gsub("number risk factors: ", "", Variable)]
    result[Variable == "number risk factors", Variable := "1 unit increase"]
    result[, Variable := gsub("Waist hip ratio", "Waist-to-hip ratio", Variable)]
    
    
    result
  }
  
  # Table 2
  model1_basic_formula <- as.formula("Case ~ Ageper5 + Sex + Birth_Germany + Education")
  model2_basic_formula <- update(model1_basic_formula, . ~ . + BMI_cat + Smoking_status + Alcohol_consumption)
  model3_basic_formula <- update(model2_basic_formula, . ~ . + Hyp_or_Diab)
  adj_levels <- list(AdjLvl1 = model1_basic_formula, AdjLvl2 = model2_basic_formula, AdjLvl3 = model3_basic_formula)
  variables_of_interest <- names(data)[7:17][-c(3, 6, 10)]
  tab2 <- foreach(adj_level = 1:3, .combine = "rbind") %do% {
    foreach(variable_of_interest = variables_of_interest, .combine = "rbind") %do% perform_logreg(data, adj_level, variable_of_interest, odds_ratios = T)
  }
  
  # Table 3
  data[, number_risk_factors := (Smoking_status == "Current") + (Hypertension == "Yes") + (Diabetes_mellitus == "Yes") + (BMI >= 25) ]
  data[, number_risk_factors_cat := as.character(number_risk_factors)][number_risk_factors >= 2, number_risk_factors_cat := "2+"]

  model1_basic_formula <- as.formula("Case ~ Ageper5 + Sex + Birth_Germany + Education")
  model2_basic_formula <- update(model1_basic_formula, . ~ . + Family_history_premature_MI)
  adj_levels_ <- list(AdjLvl1 = model1_basic_formula, AdjLvl2 = model2_basic_formula)
  data_sets <- list(All = data,
                    `Family history of premature MI: No` = data[Family_history_premature_MI == "No",],
                    `Family history of premature MI: Yes` = data[Family_history_premature_MI == "Yes",],
                    `Sex: Male` = data[Sex == "m",],
                    `Sex: Female` = data[Sex == "f",])

  variables_of_interest <- c("number_risk_factors_cat", "number_risk_factors")
  tab3 <- foreach(data_set_no = 1:5, .combine = "rbind") %do% {
    data_set <- data_sets[[data_set_no]]
    if (data_set_no %in% 2:3) {
      adj_levels <- lapply(adj_levels_, update,  . ~ . - Family_history_premature_MI)
    } else if (data_set_no %in% 4:5) {
      adj_levels <- lapply(adj_levels_, update,  . ~ . - Sex)
    }
    results_data_set <- foreach(adj_level = 1:2, .combine = "rbind") %do% {
      foreach(variable_of_interest = variables_of_interest, .combine = "rbind") %do% perform_logreg(data_set, adj_level, variable_of_interest, odds_ratios = T)
    }
    cbind(Subset = names(data_sets)[data_set_no], results_data_set)
  }
  setnames(tab3, "Variable", "No. risk factors")
  
  # Return results as a list
  list(tab1 = tab1, tab2 = tab2, tab3 = tab3)
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
by_vars_list <- sapply(analysis_tables_results_real, \(dt) sum(sapply(dt, \(col) !is.numeric(col))))
analysis_tables_results_syn_median <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, median, by_vars_list)
analysis_tables_results_syn_lo <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.025, na.rm = T), by_vars_list)
analysis_tables_results_syn_hi <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.975, na.rm = T), by_vars_list)


# ------------------------------------------------------------------------------
# Create Tables for Manuscript
# ------------------------------------------------------------------------------

# Use save_table function from utils.R to save tables
for(table_name in names(analysis_tables_results_real)) {
  save_table(analysis_tables_results_real[[table_name]],
             analysis_tables_results_syn_median[[table_name]],
             analysis_tables_results_syn_lo[[table_name]],
             analysis_tables_results_syn_hi[[table_name]],
             paste0("NAKO/Wienbergen et al./tables/Wienbergen_", table_name, ".tex"))
}


# ------------------------------------------------------------------------------
# Create Figures for Manuscript
# ------------------------------------------------------------------------------

### Table 2 - Logistic Regression (Adjustment Level 1)

# Prepare original data results
logm_results_real <- analysis_tables_results_real$tab2[`Adj. level` == 1, .(Variable, beta = log(`Odds ratio`),
                                                          data = "Original",
                                                          lo = log(`2.5%`),
                                                          hi = log(`97.5%`))]

# Prepare synthetic data results
logm_results_syn <- cbind(analysis_tables_results_syn_median$tab2[`Adj. level` == 1, .(Variable, beta = log(`Odds ratio`))],
                        analysis_tables_results_syn_lo$tab2[`Adj. level` == 1, .(lo = log(`Odds ratio`))],
                        analysis_tables_results_syn_hi$tab2[`Adj. level` == 1, .(hi = log(`Odds ratio`))])[, data := "Synthetic"]

# Exlude variables that are not relevant for the plot
variables_2exlude <- logm_results_real[7:8, Variable]
logm_results_real <- logm_results_real[!Variable %in% variables_2exlude]
logm_results_syn <- logm_results_syn[!Variable %in% variables_2exlude]

# Combine real and synthetic data results
logm_results <- rbind(logm_results_real, logm_results_syn)

# Adjust factor levels for plotting
logm_results[, Variable := factor(Variable, levels = logm_results_real[, Variable])]

# Create the plot
pd <- position_dodge(width = .5)
ggplot(logm_results, aes(x = Variable, y = beta, col = data)) + 
  geom_point(position = pd) + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = .5) + 
  theme_bw() +
  labs(title = NULL,
       x = NULL,
       y = "beta",
       col = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = margin(l = 25)) +
  scale_color_manual(values=c("dodgerblue4", "orangered"))

# Save the plot
ggsave(paste("NAKO/Wienbergen et al./plots/Wienbergen_tab2_logreg_adjlvl1.pdf"), width = 10, height = 6)