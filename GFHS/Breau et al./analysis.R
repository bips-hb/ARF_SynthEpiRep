# ==============================================================================
# Replicate analyses of Breau et al. on original and synthetic data
# Create tables and figures for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom utility function for saving tables
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Breau_PA"

# Load training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# Define cutpoint names for movement intensity classification
cutpoints <- c("Puyau", "Evenson", "Pate", "Trost", "Butte_VM", "Butte_VA", "VanCauwenberghe", "Pate_Pfeiffer", "Costa_VA", "Costa_VM")
cutpoints_nice <- c("Puyau", "Evenson", "Pate", "Trost", "Butte (VM)", "Butte (VA)", "van Cauwenberghe", "Pate & Pfeiffer", "Costa (VA)", "Costa (VM)")

# ------------------------------------------------------------------------------
# Perform Analyses
# ------------------------------------------------------------------------------

### Function to perform analyses for original tables and figures
perform_analysis_tables <- function(data_input){
  
  data <- copy(data_input)
  
  ### Data preparation
  
  # Sex
  data[, Sex := factor(Sex, levels = c("M", "F"))]
  
  # Age group: <3: Toddler, 3<=x<5: Preschooler, >=5 School aged
  data[Age<3, Age_grp := "Toddler"][Age>=3 & Age<5, Age_grp := "Preschooler"][Age>=5, Age_grp:="School aged"]
  data[, Age_grp := factor(Age_grp, levels = c("Toddler", "Preschooler", "School aged"))]
  
  # Ethnicity
  data[Ethnicity != "White", Ethnicity := "Other"]
  data[, Ethnicity := factor(Ethnicity, levels = c("White", "Other"))]
  
  # Household Income
  data[Income_Household %in% c("I don’t know","I am not comfortable answering this question"), Income_Household := "Did not answer"]
  data[Income_Household %in% c("$100,000 to $149,999", "$150,000 or more"), Income_Household := "$100,000+"]
  data[!Income_Household %in% c("Did not answer", "$100,000+"), Income_Household := "<100,000$"]
  data[,Income_Household := factor(Income_Household, levels = c("Did not answer", "<100,000$", "$100,000+"))]
  
  # BMI
  data[,Sex_num := (Sex == "F") + 1]
  data[, Age_days := Age*365]
  
  capture.output(data[, BMI := zscorer::addWGSR(data = data,
                        sex = "Sex_num",
                        firstPart = "Weight",
                        secondPart = "Height",
                        thirdPart = "Age_days",
                        index = "bfa")[,bfaz]
  ][,c("Sex_num", "Age_days") := NULL])
  
  # SED, TPA, Guideline
  for(cutpoint in cutpoints) {
    
    LPA_colname <- paste(cutpoint, "_LPA_mean", sep = "")
    MVPA_colname <- paste(cutpoint, "_MVPA_mean", sep = "")
    
    Age_grp <- data[,Age_grp]
    LPA <- data[, ..LPA_colname]
    MVPA <- data[, ..MVPA_colname]
    TPA <- LPA + MVPA
    SED <- data[,Wear_Time_mean] - TPA
    guideline <- as.logical((Age_grp == "Toddler")*(TPA >= 180)*(MVPA >= 1) + (Age_grp == "Preschooler")*(TPA >= 180)*(MVPA >= 60) +
                              (Age_grp == "School aged")*(MVPA >= 60))
    
    data[, (paste(cutpoint, "_SED_mean", sep = "")) := SED]
    data[, (paste(cutpoint, "_TPA_mean", sep = "")) := TPA]
    data[, (paste(cutpoint, "_guideline", sep = "")) := guideline]
  }
  
  ### Analyses
  
  # Table 3
  calc_tab3_part <- function(data_part) {
    rbind(
      data_part[, .N],
      transpose(data_part[,lapply(.SD, \(x) c(mean = mean(x, na.rm = T), sd = sd(x, na.rm = T))), .SDcols = 
             c("Age", "Height", "Weight", "BMI", "Valid_days", "Wear_Time_mean")]),
      setkey(data_part, Sex)[levels(Sex), .N, by = .EACHI][,2],
      setkey(data_part, Ethnicity)[levels(Ethnicity), .N, by = .EACHI][,2],
      setkey(data_part, Income_Household)[levels(Income_Household), .N, by = .EACHI][,2],
      use.names = F, fill = T
      )
  }

  tab3 <- foreach(data_part = list(data,
                        data[Age_grp == "Toddler", ],
                        data[Age_grp == "Preschooler", ],
                        data[Age_grp %in% c("Preschooler", "School aged"), ],
                        data[Age_grp == "School aged", ]),
          .combine = 'cbind') %do% calc_tab3_part(data_part)
  
  setnames(tab3, c("Everyone (mean)", "Everyone (SD)", "Toddlers 1.5-2 years (mean)", "Toddlers 1.5-2 years (SD)", "Preschoolers 3-4 years (mean)",
           "Preschoolers 3-4 years (SD)", "Preschoolers 3-6 years (mean)", "Preschoolers 3-6 years (SD)",
           "School aged 5-6 years (mean)", "School aged 5-6 years (SD)"))
  
  tab3_variables <- c("Number of participants (n)", "Age (years)", "Height (cm)", "Weight (kg)", "BMI z score", "Valid days",
                      "Valid min/day", "Sex: Male (n)", "Sex: Female (n)", "Ethnicity: White (n)",
                      "Ethnicity: Other (n)", "HHI: No answer (n)", "HHI: <$100,000 (n)",
                      "HHI: $100,000+ (n)")
  tab3 <- cbind(Variable = factor(tab3_variables, levels = tab3_variables), tab3)
  
  tab3 <- melt(tab3, id.vars = "Variable", measure.vars = list(Everyone = 2:3, `Toddlers 1.5-2 yrs` = 4:5, 
                                                               `Preschoolers 3-4 yrs` = 6:7, `Preschoolers 3-6 yrs` = 8:9, 
                                                               `School aged 5-6 yrs` = 10:11),
               variable.name = "Aggregation", na.rm = T)[, Aggregation := factor(Aggregation, labels = c("mean", "SD"))][]
  setorder(tab3, Variable)
  tab3[, Var_N := .N , by = Variable][Var_N == 2, Variable := paste0(Variable, " (", Aggregation, ")")][, `:=` (Var_N = NULL, Aggregation = NULL)]
  
  # Figure 1
  movement_intensities <- c("_SED_", "_LPA_", "_MVPA_")
  movement_intensities_cols <- lapply(movement_intensities, \(x) which(grepl(x,names(data))))
  cutpoint_results <- melt(data, measure = movement_intensities_cols, variable.name = "Set of cutpoints", value.name = substr(movement_intensities, 2, nchar(movement_intensities) - 1), id.vars = "Age_grp")[, `Set of cutpoints` := cutpoints[`Set of cutpoints`]][]
  
  fig1 <- cutpoint_results[, lapply(.SD, mean), by = `Set of cutpoints`, .SDcols = c("SED","LPA","MVPA")]
  setkey(fig1, `Set of cutpoints`)
  fig1 <- fig1[c("VanCauwenberghe", "Pate", "Puyau", "Trost", "Butte_VA", "Pate_Pfeiffer", "Evenson", "Costa_VA", "Costa_VM", "Butte_VM"), ]
  fig1[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = cutpoints, labels = cutpoints_nice)]
  
  
  # Figure 2
  fig2 <- rbind(
    cutpoint_results[Age_grp == "Toddler" & `Set of cutpoints` %in% c("Trost", "Costa_VA", "Costa_VM"), lapply(.SD, mean), by = .(Age_grp, `Set of cutpoints`), .SDcols = c("SED","LPA","MVPA")],
    cutpoint_results[Age_grp != "Toddler" & `Set of cutpoints` %in% c("VanCauwenberghe", "Pate", "Butte_VA","Pate_Pfeiffer","Butte_VM"), lapply(.SD, mean), by = `Set of cutpoints`, .SDcols = c("SED","LPA","MVPA")][, Age_grp := "Preschoolers (3-6 years)"],
    cutpoint_results[Age_grp == "School aged" & `Set of cutpoints` %in% c("Puyau", "Evenson"), lapply(.SD,mean), by = .(Age_grp, `Set of cutpoints`), .SDcols = c("SED","LPA","MVPA")]
  )[,Age_grp := factor(Age_grp, levels = c("Toddler", "Preschoolers (3-6 years)", "School aged"),
                       labels = c("Toddlers (1.5-2 years)", "Preschoolers (3-6 years)", "School aged (5-6 years)"))]
  setkey(fig2, `Set of cutpoints`)
  fig2 <- fig2[c("Trost", "Costa_VA", "Costa_VM", "VanCauwenberghe", "Pate", "Butte_VA", "Pate_Pfeiffer", "Butte_VM", "Puyau", "Evenson")]
  fig2[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = cutpoints, labels = cutpoints_nice)]
  setnames(fig2, "Age_grp", "Age group")
  
  # Figure 3
  guidelines <- which(grepl("_guideline", names(data)))
  guidelines_results <- melt(data, measure = guidelines, variable.name = "Set of cutpoints", value.name = "guideline", id.vars = "Age_grp")[, `Set of cutpoints` := gsub("_guideline", "", `Set of cutpoints`)]
  fig3 <- setnames(rbind(
    guidelines_results[Age_grp == "Toddler" & `Set of cutpoints` %in% c("Trost", "Costa_VA", "Costa_VM"), mean(guideline), by = .(Age_grp, `Set of cutpoints`)],
    guidelines_results[Age_grp != "Toddler" & `Set of cutpoints` %in% c("VanCauwenberghe", "Pate", "Butte_VA","Pate_Pfeiffer","Butte_VM"), mean(guideline), by = `Set of cutpoints`][,Age_grp := "Preschoolers (3-6 years)"],
    guidelines_results[Age_grp == "School aged" & `Set of cutpoints` %in% c("Puyau", "Evenson"), mean(guideline), by = .(Age_grp, `Set of cutpoints`)]
  )[,Age_grp := factor(Age_grp, levels = c("Toddler", "Preschoolers (3-6 years)", "School aged"),
                       labels = c("Toddlers (1.5-2 years)", "Preschoolers (3-6 years)", "School aged (5-6 years)"))],old = "V1", new = "guideline")
  setkey(fig3, `Set of cutpoints`)
  fig3 <- fig3[c("Trost", "Costa_VA", "Costa_VM", "VanCauwenberghe", "Pate", "Butte_VA", "Pate_Pfeiffer", "Butte_VM", "Puyau", "Evenson")]
  fig3[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = cutpoints, labels = cutpoints_nice)]
  setnames(fig3, c("Age_grp", "guideline"), c("Age group", "Proportion meeting PA guideline"))
  
  ### Return results as a list
  list(
    tab3 = tab3,
    fig1 = fig1,
    fig2 = fig2,
    fig3 = fig3
  )
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
    entry[, lapply(.SD, aggregate_func), by = entry[,1:by_vars_list[[entry_id]]], .SDcols = -(1:by_vars_list[[entry_id]])]
  }
  names(entries_agg) <- names(analysis_tables_results_syn)
  entries_agg
}

# Aggregate synthetic data results (median and quantiles)
by_vars_list <- sapply(analysis_tables_results_real, \(dt) sum(sapply(dt, \(col) !is.numeric(col))))
analysis_tables_results_syn_median <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, median, by_vars_list)
analysis_tables_results_syn_lo <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x,0.025, na.rm =T), by_vars_list)
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
             paste0("GFHS/Breau et al./tables/Breau_", table_name, ".tex"))
}


# ------------------------------------------------------------------------------
# Create Figures for Manuscript
# ------------------------------------------------------------------------------

### Figure 1

# Prepare original data results
fig1_real <- analysis_tables_results_real$fig1[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = `Set of cutpoints`)]
fig1_real <- melt(fig1_real, id.vars = "Set of cutpoints")[, variable := factor(variable, levels = c("SED","LPA","MVPA"))]
fig1_real[, data := "Original"]

# Prepare synthetic data results
fig1_syn <- analysis_tables_results_syn_median$fig1[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = `Set of cutpoints`)]
fig1_syn<- cbind(
  melt(fig1_syn, id.vars = "Set of cutpoints")[,variable := factor(variable, levels = c("SED","LPA","MVPA"))],
  melt(analysis_tables_results_syn_lo$fig1, id.vars = "Set of cutpoints", value.name = "lo")[, .(lo)],
  melt(analysis_tables_results_syn_hi$fig1, id.vars = "Set of cutpoints", value.name = "hi")[, .(hi)])
fig1_syn[,data := "Synthetic"]

# Combine real and synthetic data results
fig1 <- rbind(fig1_real, fig1_syn, fill = T)

# Create the figure
pd <- position_dodge(0.9)
ggplot(fig1, aes(x = `Set of cutpoints`, y = value, fill = variable, col = data, label = round(value,1))) +
  geom_bar(stat = "identity", position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = position_dodge(0.9), width = .5) + 
  labs(title = NULL, x = "Set of cutpoints", y = "Average time (min/day)", color = NULL, fill = NULL) +
  scale_y_continuous(
    breaks = seq(0, max(fig1$value, fig1$hi, na.rm = TRUE), by = 100),
    minor_breaks = seq(0, max(fig1$value, fig1$hi, na.rm = TRUE), by = 20)
  ) +
  scale_fill_manual(values = c("dodgerblue4", "lightsteelblue1", "lightsteelblue")) +
  scale_color_manual(values = c("white", "orangered")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.3)
  )

# Save the figure
ggsave("GFHS/Breau et al./plots/Breau_fig1.pdf", width = 10, height = 6)


### Figure 2

# Prepare original data results
fig2_real <- analysis_tables_results_real$fig2[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = `Set of cutpoints`)]
fig2_real <- melt(fig2_real, id.vars = c("Age group", "Set of cutpoints"))[,variable := factor(variable, levels = c("SED","LPA","MVPA"))]
fig2_real[,data := "Original"]

# Prepare synthetic data results
fig2_syn <- analysis_tables_results_syn_median$fig2[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = `Set of cutpoints`)]
fig2_syn<- cbind(
  melt(fig2_syn, id.vars = c("Age group", "Set of cutpoints"))[,variable := factor(variable, levels = c("SED","LPA","MVPA"))],
  melt(analysis_tables_results_syn_lo$fig2, id.vars = c("Age group", "Set of cutpoints"), value.name = "lo")[, .(lo)],
  melt(analysis_tables_results_syn_hi$fig2, id.vars = c("Age group", "Set of cutpoints"), value.name = "hi")[, .(hi)])
fig2_syn[,data := "Synthetic"]

# Combine real and synthetic data results
fig2 <- rbind(fig2_real, fig2_syn, fill = T)

# Create the figure
pd <- position_dodge(0.9)
ggplot(fig2, aes(x = `Set of cutpoints`, y = value, fill = variable, col = data, label = round(value,1))) +
  geom_bar(stat = "identity", position = pd) +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = .5) + 
  facet_grid(~`Age group`, scales = "free_x", space = "free") +
  scale_y_continuous(
    breaks = seq(0, max(fig1$value, fig1$hi, na.rm = TRUE), by = 100),
    minor_breaks = seq(0, max(fig1$value, fig1$hi, na.rm = TRUE), by = 20)
  ) +
  labs(title = NULL, x = "Set of cutpoints", y = "Average time (min/day)", color = NULL, fill = NULL) +
  scale_fill_manual(values = c("dodgerblue4", "lightsteelblue1", "lightsteelblue")) +
  scale_color_manual(values=c("white", "orangered"))+
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.3)
  )

# Save the figure
ggsave("GFHS/Breau et al./plots/Breau_fig2.pdf", width = 10, height = 6)


### Figure 3

# Prepare original data results
fig3_real <- analysis_tables_results_real$fig3[, `Set of cutpoints` := factor(`Set of cutpoints`, levels = `Set of cutpoints`)]
fig3_real[,data := "Original"]

# Prepare synthetic data results
fig3_syn <- cbind(
  analysis_tables_results_syn_median$fig3[, variable := factor(`Set of cutpoints`, levels = `Set of cutpoints`)],
  analysis_tables_results_syn_lo$fig3[, .(lo = `Proportion meeting PA guideline`)],
  analysis_tables_results_syn_hi$fig3[, .(hi = `Proportion meeting PA guideline`)])
fig3_syn[, data := "Synthetic"]

# Combine real and synthetic data results
fig3 <- rbind(fig3_real, fig3_syn, fill = T)

# Create the figure
pd <- position_dodge(0.9)
ggplot(fig3, aes(x = `Set of cutpoints`, y = `Proportion meeting PA guideline`, color = data, label = round(`Proportion meeting PA guideline`,1))) +
  geom_bar(stat = "identity", position = pd, fill = "dodgerblue4") +
  geom_errorbar(aes(ymin = lo, ymax = hi), position = pd, width = .5) + 
  facet_grid(~`Age group`, scales = "free_x", space = "free") +
  labs(title = NULL, x = "Set of cutpoints", y = "Proportion meeting PA guideline", color = NULL, fill = NULL) +
  scale_fill_manual(values = c("dodgerblue4")) +
  scale_color_manual(values=c("white", "orangered"))+
  scale_y_continuous(labels = scales::percent, breaks = seq(0, max(fig1$value, fig1$hi, na.rm = TRUE), by = 0.25),
                     minor_breaks = seq(0, max(fig1$value, fig1$hi, na.rm = TRUE), by = 0.05)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(linewidth = 0.5),
    panel.grid.minor.y = element_line(linetype = "dashed", linewidth = 0.3)
  )

# Save the figure
ggsave("GFHS/Breau et al./plots/Breau_fig3.pdf", width = 10, height = 6)
