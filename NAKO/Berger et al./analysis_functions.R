# ==============================================================================
# Analysis functions for Berger et al. performed for both full and synthetic
# data
# Called by analysis.R and analysis_taskspec.R
# ==============================================================================

# Load required libraries
library(data.table)

# Function to generate Table 1
generate_tab1 <- function(data_) {
  data <- copy(data_)
  tab1 <- data.table(Variable = character(19), Value = numeric(19))
  
  tab1[1,1:2] <- list("Number of participants (n)", data[,.N])
  tab1[2,1:2] <- list("Sex: Female (%)", data[, .N/nrow(data), by = sex][sex == "Female", V1])
  tab1[3,1:2] <- list("Age (mean years)", data[, mean(age)])
  tab1[4,1:2] <- list("Age: 20-29 years (%)", setorder(data[, .N/nrow(data), by = age_grp][age_grp %in% c("20-29", "60+")])[1,V1])
  tab1[5,1:2] <- list("Age: 60+ years (%)", setorder(data[, .N/nrow(data), by = age_grp][age_grp %in% c("20-29", "60+")])[2,V1])
  tab1[6,1:2] <- list("Follow up time (mean years)", data[, mean(follow_up_time)])
  tab1[7:10,1] <- c("Education: Hauptschule (%)", "Education: Realschule (%)", "Education: (Fach-)Abitur (%)", "Education: none/other (%)")
  tab1[7:10,2] <- setorder(data[, .N/nrow(data), by = bildungsniveau])[c(2,3,4,1), V1]
  tab1[11,1:2] <- list("Living in relationship: Yes (%)", data[,.N/nrow(data), by = mit_partner_lebend][mit_partner_lebend == "Ja", V1])
  tab1[12:19,1] <- c("PHQ-9 score baseline examination woman (mean)",
                     "PHQ-9 score baseline examination woman (SD)",
                     "GAD-7 score baseline examination woman (mean)",
                     "GAD-7 score baseline examination woman (SD)",
                     "PHQ-9 score baseline examination man (mean)",
                     "PHQ-9 score baseline examination man (SD)",
                     "GAD-7 score baseline examination man (mean)",
                     "GAD-7 score baseline examination man (SD)")
  tab1[12:15,2] <- data[sex == "Female", c(mean(phq9_basis_sum, na.rm = T),
                                           sd(phq9_basis_sum, na.rm = T),
                                           mean(gad7_basis_sum, na.rm = T),
                                           sd(gad7_basis_sum, na.rm = T))]
  tab1[16:19,2] <- data[sex == "Male", c(mean(phq9_basis_sum, na.rm = T),
                                         sd(phq9_basis_sum, na.rm = T),
                                         mean(gad7_basis_sum, na.rm = T),
                                         sd(gad7_basis_sum, na.rm = T))]
  tab1[c(2,4,5,7:11), Value := Value * 100]
  tab1[]
}

# Function to generate Table 3
generate_tab3 <- function(data_) {
  data <- copy(data_)
  data_lm <- data[, .(iso1_covid_sum, untersuchungsort, age, sex, bildungsniveau_num, mit_partner_lebend, phq9_covid_sum, gad7_covid_sum, ANG3_covid)]
  lin_reg <- lm(formula = iso1_covid_sum ~ age + sex + bildungsniveau_num + mit_partner_lebend + phq9_covid_sum + gad7_covid_sum + ANG3_covid + untersuchungsort, data_lm)
  
  lin_reg_vars <- names(lin_reg$coefficients)
  tab3_ <- data.table(Variable = lin_reg_vars, `beta coefficient` = lin_reg$coefficients[lin_reg_vars], confint(lin_reg, lin_reg_vars, 0.95))
  tab3_[, Variable := factor(Variable, levels= lin_reg_vars, ordered = T)]
  
  nice_variable_names <- c("(Intercept)", "Age", "Sex: Female", "Education level", "In relationship: Yes", "PHP-9 score COVID survey", "GAD-7 score COVID survey", "Fear of COVID infection: Yes", "Study center")
  nice_variable_names <- factor(nice_variable_names, levels = nice_variable_names)
  
  tab3_[, Variable := factor(Variable, levels = Variable, labels = nice_variable_names)]
  setnames(tab3_, c("Variable", "beta coefficient", "2.5%", "97.5%"))
  tab3_[]
}