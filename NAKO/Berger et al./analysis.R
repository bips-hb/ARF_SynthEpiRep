# ==============================================================================
# Replicate full data synthesis analyses of Berger et al. on original and
# synthetic data
# Create tables and figures for full data synthesis for manuscript
# ==============================================================================

# Load required libraries
library(data.table)
library(foreach)
library(ggplot2)

# Load custom utility function for saving tables
source("utils.R")

# Set the data folder path (masked)
data_folder <- "<...>/Berger_Corona"

# Load the training dataset
train <- readRDS(paste(data_folder, "train", "train.rds", sep="/"))

# ------------------------------------------------------------------------------
# Perform Analyses (Full data synthesis)
# ------------------------------------------------------------------------------

### Function to perform analyses for original tables and figures (full data synthesis)
perform_analysis_tables <- function(data_input) {
  
  data <- copy(data_input)
  
  ### Data preparation
  
  # Study center
  data[,untersuchungsort := as.numeric(as.factor(untersuchungsort))]
  
  # Sex
  data[, sex := factor(data$sex, levels = c("m","f"), labels = c("Male", "Female"))]
  
  # Age group
  data[age >= 20 & age < 30, age_grp := "20-29"]
  data[age >= 30 & age < 60, age_grp := "30-59"]
  data[age >= 60, age_grp := "60+"]
  data[, age_grp := factor(age_grp, levels = c("20-29", "30-59", "60+"))]
  
  # Alternative age grouping
  data[age >= 20 & age < 30, age_grp2 := "20-29"]
  data[age >= 30 & age < 40, age_grp2 := "30-39"]
  data[age >= 40 & age < 50, age_grp2 := "40-59"]
  data[age >= 50 & age < 60, age_grp2 := "50-59"]
  data[age >= 60 & age < 70, age_grp2 := "60-69"]
  data[age >= 70, age_grp2 := "70+"]
  data[, age_grp2 := factor(age_grp2, levels = sort(data[, unique(age_grp2)]))]
  
  # Convert date columns to Date format
  data[, untersuchungsdatum := as.Date.POSIXct(untersuchungsdatum)]
  data[, date_stamp_covid := as.Date.POSIXct(date_stamp_covid)]
  
  # Calculate follow-up time in years
  data[, follow_up_time := as.numeric(date_stamp_covid - untersuchungsdatum)/365 - 0.5]
  
  # Education level
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
  
  # Living with partner
  data[mit_partner_lebend == "Missing", mit_partner_lebend := "Nein"]
  data[mit_partner_lebend != "Nein", mit_partner_lebend := "Ja"]
  data$mit_partner_lebend <- factor(data$mit_partner_lebend, levels = c("Nein","Ja"))
  
  # PHQ-9 and GAD-7 scores
  phq9_basis_cols <- names(data)[7:15]
  gad7_basis_cols <- names(data)[16:22]
  phq9_gad7_basis_cols <- c(phq9_basis_cols, gad7_basis_cols)
  phq9_covid_cols <- names(data)[24:32]
  gad7_covid_cols <- names(data)[33:39]
  phq9_gad7_covid_cols <- c(phq9_covid_cols, gad7_covid_cols)
  phq9_gad7_cols <- c(phq9_gad7_basis_cols, phq9_gad7_covid_cols)
  data[, (phq9_gad7_cols) := lapply(.SD, \(x) as.numeric(replace(x, x == "Missing", NA)) - 1), .SDcols = phq9_gad7_cols]
  data[,phq9_basis_sum := apply(.SD,1,sum), .SDcols = phq9_basis_cols]
  data[,gad7_basis_sum := apply(.SD,1,sum), .SDcols = gad7_basis_cols]
  data[,phq9_covid_sum := apply(.SD,1,sum), .SDcols = phq9_covid_cols]
  data[,gad7_covid_sum := apply(.SD,1,sum), .SDcols = gad7_covid_cols]
  
  # Isolation
  iso1_covid_cols <- names(data)[40:42]
  data[, (iso1_covid_cols) := replace(.SD, .SD == "Selten", "Nie"), .SDcols = iso1_covid_cols]
  data[, (iso1_covid_cols) := replace(.SD, .SD == "Missing", NA), .SDcols = iso1_covid_cols]
  data[,(iso1_covid_cols) := lapply(.SD, factor, levels = c("Nie", "Manchmal", "Oft"), labels = c("Nie/Selten", "Manchmal", "Oft")),.SDcols = iso1_covid_cols]
  data[, (iso1_covid_cols) := lapply(.SD, as.numeric), .SDcols = iso1_covid_cols]
  data[,iso1_covid_sum := apply(.SD,1,sum), .SDcols = iso1_covid_cols]
  data[, feels_isolated := factor(fifelse(iso1_covid_sum >= 6, "Yes", "No"), levels = c("Yes", "No"))]
  
  # Anxiety
  ang_covid_cols <- names(data[,44:47])
  data[, (ang_covid_cols) := replace(.SD, .SD == "Missing", NA), .SDcols = ang_covid_cols]
  data[, (ang_covid_cols) := replace(.SD, .SD != "Ja", "Nein"), .SDcols = ang_covid_cols]
  data[,(ang_covid_cols) := lapply(.SD, factor, levels = c("Nein", "Ja")),.SDcols = ang_covid_cols]
  
  ### Analyses
  
  # Table2
  data_m <- data[sex == "Male"]
  data_f <- data[sex == "Female"]
  
  createTab2part <- function(dataset_part) {
    
    tab2_part <- data.table(Variable = character(15), Value = numeric(15))
    
    tab2_part[1,1:2] <- list("Feeling of missing company of others (%)",dataset_part[ISO11_covid >= 2, .N/dataset_part[!is.na(ISO11_covid),.N]])
    tab2_part[2,1:2] <- list("Feeling of being left out (%)", dataset_part[ISO12_covid >= 2, .N/dataset_part[!is.na(ISO12_covid),.N]])
    tab2_part[3,1:2] <- list("Feeling of being socially isolated (%)", dataset_part[ISO13_covid >= 2, .N/dataset_part[!is.na(ISO12_covid),.N]])
    tab2_part[4:5,1:2] <- data.table(c("Sum score loneliness (mean)", "Sum score loneliness (SD)"), dataset_part[, c(mean(iso1_covid_sum, na.rm = T), sd(iso1_covid_sum, na.rm = T))])
    tab2_part[6,1:2] <- list("Proportion of lonely participants (%)", dataset_part[iso1_covid_sum >= 6, .N/dataset_part[!is.na(iso1_covid_sum),.N]])
    tab2_part[7,1:2] <- list("Subjective increase of loneliness during pandemic (%)", dataset_part[ISO2_covid == "Eher stärker",.N/dataset_part[ISO2_covid != "Missing",.N], by = ISO2_covid][, V1])
    tab2_part[8,1:2] <- list("Fear of natural disasters (%)", dataset_part[ANG2_covid == "Ja", .N/dataset_part[!is.na(ANG2_covid),.N]])
    tab2_part[9,1:2] <- list("Fear of cancer (%)", dataset_part[ANG4_covid == "Ja", .N/dataset_part[!is.na(ANG4_covid),.N]])
    tab2_part[10,1:2] <- list("Fear of cardiac infarction (%)", dataset_part[ANG6_covid == "Ja", .N/dataset_part[!is.na(ANG6_covid),.N]])
    tab2_part[11,1:2] <- list("Fear of COVID infection (%)", dataset_part[ANG3_covid == "Ja", .N/dataset_part[!is.na(ANG3_covid),.N]])
    tab2_part[12:13,1:2] <- data.table(c("PHQ-9 score COVID survey (mean)","PHQ-9 score COVID survey (SD)"), dataset_part[, c(mean(phq9_covid_sum, na.rm = T), sd(phq9_covid_sum, na.rm = T))])
    tab2_part[14:15,1:2] <- data.table(c("GAD-7 score COVID survey (mean)", "GAD-7 score COVID survey (SD)"), dataset_part[, c(mean(gad7_covid_sum, na.rm = T), sd(gad7_covid_sum, na.rm = T))])
    
    tab2_part[!c(4:5, 12:15), Value := Value * 100]
    
    tab2_part
  }
  
  tab2 <- cbind(createTab2part(data), 
                createTab2part(data_f)[, -1],
                createTab2part(data_m)[, -1])
  setnames(tab2, c("Variable", "Value", "Value woman", "Value men"))
  
  # Figure 1
  fig1 <- setorder(data[,.(`Lonely participants (proportion)` = sum(iso1_covid_sum >= 6, na.rm = T) / sum(!is.na(iso1_covid_sum)),
                           `Participants with increased loneliness (proportion)` = sum(ISO2_covid == "Eher stärker", na.rm = T) / sum(ISO2_covid != "Missing")), by = .(sex,age_grp2)])[]
  setnames(fig1, c("sex", "age_grp2"), c("Sex", "Age group"))
  
  # Figure 2
  fig2 <- setorder(data[!is.na(iso1_covid_sum), .(`PHQ-9 score COVID survey (mean)` = mean(phq9_covid_sum, na.rm = T),
                                                  `GAD-7 score COVID survey (mean)` = mean(gad7_covid_sum, na.rm = T)), by = .(sex, iso1_covid_sum)])[, iso1_covid_sum := as.factor(iso1_covid_sum)][]
  setnames(fig2, c("sex", "iso1_covid_sum"), c("Sex", "Loneliness score"))
  
  # Figure 3
  fig3 <- setorder(data[!is.na(feels_isolated), .(`PHQ-9 score baseline examination (mean)` = mean(phq9_basis_sum, na.rm = T),
                                                  `GAD-7 score baseline examination (mean)` = mean(gad7_basis_sum, na.rm = T)), by = .(sex, feels_isolated)])[]
  setnames(fig3, c("sex", "feels_isolated"), c("Sex", "Lonely"))
  
  ### Return results as a list
  list(tab2 = tab2, fig1 = fig1, fig2 = fig2, fig3 = fig3)
  
}

### Perform analyses on original data
analysis_tables_results_real <- perform_analysis_tables(train)

### Perform analyses on synthetic data
syn_files <- dir(paste(data_folder, "syn/full", sep = "/"), pattern = ".rds", full.names = T)
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
analysis_tables_results_syn_lo <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x, 0.025, na.rm = T), by_vars_list)
analysis_tables_results_syn_hi <- aggregate_analysis_tables_results_syn(analysis_tables_results_syn, \(x) quantile(x, 0.975, na.rm = T), by_vars_list)


# ------------------------------------------------------------------------------
# Create Tables for Manuscript (Full data synthesis)
# ------------------------------------------------------------------------------

# Use save_table function from utils.R to save tables
for(table_name in names(analysis_tables_results_real)) {
  save_table(analysis_tables_results_real[[table_name]],
             analysis_tables_results_syn_median[[table_name]],
             analysis_tables_results_syn_lo[[table_name]],
             analysis_tables_results_syn_hi[[table_name]],
             paste0("NAKO/Berger et al./tables/Berger_", table_name, ".tex"))
}



# ------------------------------------------------------------------------------
# Create Figures for Manuscript (Full data synthesis)
# ------------------------------------------------------------------------------

### Figure 1

# Prepare original data results
fig1_real <- melt(analysis_tables_results_real$fig1, id.vars = c("Sex", "Age group"))[, data := "Original"]

# Prepare synthetic data results
fig1_syn <- cbind(melt(analysis_tables_results_syn_median$fig1, id.vars =c("Sex", "Age group")),
                  melt(analysis_tables_results_syn_lo$fig1, id.vars = c("Sex", "Age group"))[, .(lo = value)],
                  melt(analysis_tables_results_syn_hi$fig1, id.vars = c("Sex", "Age group"))[, .(hi = value)])[, data := "Synthetic"]
# Combine real and synthetic data results
fig1 <- rbind(fig1_real, fig1_syn, fill = T)

# Create the figure
ggplot(fig1, aes(x = `Age group`, y =value, fill = variable, color = data)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = "dodge", width = 0.9) + 
  facet_wrap(~Sex) + 
  theme_bw() +
  labs(title = NULL,
       x = "Age group",
       y = "Proportion",
       fill = NULL,
       color = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(labels = c("Feels lonely", "Increased loneliness\nsince COVID"), values=c("dodgerblue4", "lightsteelblue1")) +
  scale_color_manual(values=c("white", "orangered"))

# Save the figure
ggsave("NAKO/Berger et al./plots/Berger_fig1.pdf", width = 10, height = 6)


### Figure 2

# Prepare original data results
fig2_real <- melt(analysis_tables_results_real$fig2, id.vars = c("Sex", "Loneliness score"))[, data := "Original"]

# Prepare synthetic data results
fig2_syn <- cbind(melt(analysis_tables_results_syn_median$fig2, id.vars = c("Sex", "Loneliness score")),
                  melt(analysis_tables_results_syn_lo$fig2, id.vars = c("Sex", "Loneliness score"))[, .(lo = value)],
                  melt(analysis_tables_results_syn_hi$fig2, id.vars = c("Sex", "Loneliness score"))[, .(hi = value)])[, data := "Synthetic"]

# Combine real and synthetic data results
fig2 <- rbind(fig2_real, fig2_syn, fill = T)

# Create the figure
ggplot(fig2, aes(x = `Loneliness score` , y = value, fill = variable, color = data)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = "dodge", width = 0.9) + 
  facet_wrap(~Sex) + 
  theme_bw() +
  labs(title = NULL,
       x = "Loneliness score",
       y = "Sum score",
       fill = NULL,
       color = NULL) +
  scale_fill_manual(labels = c("PHQ-9", "GAD-7"), values=c("dodgerblue4", "lightsteelblue1")) +
  scale_color_manual(values=c("white", "orangered"))

# Save the figure
ggsave("NAKO/Berger et al./plots/Berger_fig2.pdf", width = 10, height = 6)


### Figure 3

# Prepare original data results
fig3_real <- melt(analysis_tables_results_real$fig3, id.vars = c("Sex", "Lonely"))[, data := "Original"]

# Prepare synthetic data results
fig3_syn <- cbind(melt(analysis_tables_results_syn_median$fig3, id.vars = c("Sex", "Lonely")),
                  melt(analysis_tables_results_syn_lo$fig3, id.vars = c("Sex", "Lonely"))[, .(lo = value)],
                  melt(analysis_tables_results_syn_hi$fig3, id.vars = c("Sex", "Lonely"))[, .(hi = value)])[, data := "Synthetic"]

# Combine real and synthetic data results
fig3 <- rbind(fig3_real, fig3_syn, fill = T)

# Create the figure
ggplot(fig3, aes(x = Lonely , y = value, fill = variable, color = data)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = lo, ymax = hi), position = "dodge", width = 0.9) + 
  facet_wrap(~Sex) + 
  theme_bw() +
  labs(title = NULL,
       x = "Feels lonely",
       y = "Sum score",
       fill = NULL,
       color = NULL) +
  scale_fill_manual(labels = c("PHQ-9", "GAD-7"), values=c("dodgerblue4", "lightsteelblue1")) +
  scale_color_manual(values=c("white", "orangered"))

# Save the figure
ggsave("NAKO/Berger et al./plots/Berger_fig3.pdf", width = 10, height = 6)