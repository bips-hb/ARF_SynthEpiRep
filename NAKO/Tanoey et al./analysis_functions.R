# ==============================================================================
# Data preparation and analysis functions for Tanoey et al. performed for both
# full and synthetic data
# Called by analysis.R and analysis_taskspec.R
# ==============================================================================

# Load required libraries
library(data.table)

# Function to prepare data for analysis
prepare_analysis <- function(data_) {
  
  data <- copy(data_)
  
  #birth_order
  if ("birth_rank" %in% names(data)) {

    data[,birth_rank := round(birth_rank)]
    data[birth_rank == 0,birth_order := "Only child"]
    data[birth_rank == 1,birth_order := "First"]
    data[birth_rank == 2,birth_order := "Second"]
    data[birth_rank >= 3 &  birth_rank < 14 ,birth_order := "Third or more"] # ab 14 abgeschnitten, s. Original
    data[is.na(birth_order), birth_order := "Missing"]
    data[,birth_order := factor(birth_order, levels = c("Only child", "First", "Second", "Third or more", "Missing"))]
    data[, birth_rank := NULL]
  }
  
  # birth year group
  if ("birth_year" %in% names(data)) {

  data[,birthyrgr := sapply(birth_year, \(x){
    if(!is.na(x)) {
      if(x <= 1955) "<=1955"
      else if(x >= 1956 & x <= 1965) "1956-1965"
      else if(x >= 1966 & x <= 1975) "1966-1975"
      else if(x >= 1976 & x <= 1985) "1976-1985"
      else ">=1986"
    }
    else "Unknown"
  })]
  data[, birthyrgr := factor(birthyrgr, levels = c("<=1955", "1956-1965", "1966-1975", "1976-1985", ">=1986", "Unknown"))]
  data[, birth_year := NULL]
  }
  
  # BMI
  if (all(c("weight_18", "height") %in% names(data))) {
    data[, bmi_18 := weight_18/(height/100)^2]
    bmi18_lowercut <- data[,quantile(bmi_18,0.25,na.rm=T)-2*sd(bmi_18,na.rm=T)]
    bmi18_uppercut <- data[,quantile(bmi_18,0.75,na.rm=T)+2*sd(bmi_18,na.rm=T)]
    data[bmi_18 < 18.5, bmi_18_cat := "Underweight"]
    data[bmi_18 >= 18.5 & bmi_18 < 25, bmi_18_cat := "Normal weight"]
    data[bmi_18 >= 25 & bmi_18 < 30, bmi_18_cat := "Overweight"]
    data[bmi_18 >= 30, bmi_18_cat := "Obese"]
    data[bmi_18 <= bmi18_lowercut, bmi_18_cat := NA] ### s. Tanoey Original
    data[bmi_18 >= bmi18_uppercut, bmi_18_cat := NA] ### s. Tanoey Original
    data[is.na(bmi_18_cat), bmi_18_cat := "Missing"]
    data[,bmi_18_cat := factor(bmi_18_cat, levels = c("Underweight", "Normal weight", "Overweight", "Obese", "Missing"))]
    data[, c("weight_18", "height", "bmi_18") := NULL]
  }

  # classify diabetes
  if (all(c("reported_diabetes_diagnosis", "diabetes_diagnosis_age", "diabetes_treatment", 
            "insulin_treatment_first_age", "diabetes_diagnosis_pregnant") %in% names(data))) {
    
    data[,ID := .I]
    
    # Exclude1
    data[reported_diabetes_diagnosis == 'Unknown', .N]
    data_base <- data[reported_diabetes_diagnosis != 'Unknown',]
    
    # Exclude2
    data_base[(is.na(diabetes_diagnosis_age) & reported_diabetes_diagnosis == 'Yes'),.N]
    data_base <- data_base[!(is.na(diabetes_diagnosis_age) & reported_diabetes_diagnosis == 'Yes'),]
    
    # classify diabetes
    data_diab <- data_base[reported_diabetes_diagnosis == 'Yes',]
    data_diab[,ins_time := insulin_treatment_first_age - diabetes_diagnosis_age]
    data_diab[ins_time < -3,ins_time :=NA]
    data_pot1 <- data_diab[diabetes_diagnosis_age <=40 & ins_time %in% c(-3,-2,-1,0,1,NA),]
    data_pot1[(!(sex == 'Female' & diabetes_diagnosis_pregnant == 'Yes') & diabetes_treatment %in% c('Only insulin', 'Both insulin and oral medication', 'Not applicable / Unknown')) |
                (sex == 'Female' & diabetes_diagnosis_pregnant == 'Yes' & (diabetes_treatment == 'Only insulin' | (diabetes_treatment == 'Both insulin and oral medication' & (diabetes_diagnosis_age < 20 | !is.na(ins_time))))), diabetes_type := 1]
    data_diab <- merge(data_diab, data_pot1[,.(ID,diabetes_type)], by= "ID", all.x = T)[is.na(diabetes_type), diabetes_type := 2]
    data_analysis <- merge(data_base, data_diab[,.(ID, diabetes_type)], by = "ID", all.x = T)
    data_analysis[is.na(diabetes_type), diabetes_type := 0]
    data_analysis[,diabetes_type := factor(diabetes_type)]
    data_analysis[,ID := NULL]
    data_analysis[, c("reported_diabetes_diagnosis", "insulin_treatment_first_age", "diabetes_diagnosis_pregnant") := NULL]
  } else {
    data_analysis <- copy(data)
  }
  
  if (all(c("diabetes_diagnosis_age", "age") %in% names(data_analysis))) {
    data_analysis[diabetes_type == 1, db1status := 1]
    data_analysis[is.na(db1status), db1status := 0]
    data_analysis[db1status == 1, db1time_age := diabetes_diagnosis_age]
    data_analysis[db1status == 0, db1time_age := (age <= 40)*age + (age > 40)*40]
    data_analysis[, c("diabetes_diagnosis_age", "age", "db1status") := NULL]
  }
  data_analysis[]
}

# Function to generate Table 1
generate_tab1 <- function(data_analysis_) {
  
  data_analysis <- copy(data_analysis_)
  
  tab1_vars <- c("sex", "caesarean_delivery", "birth_order", "attended_daycare", "birthyrgr", "paternal_diabetes",
                 "maternal_diabetes", "migration_background", "prematural_birth", "birth_weight",
                 "ever_breastfed", "bmi_18_cat")
  tab1_vars_nice <- c("Sex", "Caesarean delivery", "Birth order", "Attended daycare", "Birth year group", "Paternal diabetes",
                      "Maternal diabetes", "Migration background", "Prematural birth", "Birth weight",
                      "Ever breastfed", "BMI at age 18")
  
  tab1_names <- lapply(data_analysis[, ..tab1_vars], \(x) names(summary(x)))
  names(tab1_names) <- tab1_vars_nice
  tab1_names <- unlist(lapply(seq_along(tab1_names), \(i) paste(names(tab1_names)[i], ": ", unlist(tab1_names[i]), sep = "")))
  tab1 <- data.table(Variable = character(length(tab1_names)))
  tab1[, Variable := factor(tab1_names, levels = tab1_names)]
  tab1[, `No diabetes` := unlist(lapply(data_analysis[diabetes_type == 0, .SD, .SDcols = tab1_vars], summary), use.names = F)]
  tab1[, `No diabetes (%)` := unlist(lapply(data_analysis[diabetes_type == 0, .SD, .SDcols = tab1_vars], \(x) summary(x)/sum(summary(x))), use.names = F)]
  tab1[, `Diabetes type 1` := unlist(lapply(data_analysis[diabetes_type == 1, .SD, .SDcols = tab1_vars], summary), use.names = F)]
  tab1[, `Diabetes type 1 (%)` := unlist(lapply(data_analysis[diabetes_type == 1, .SD, .SDcols = tab1_vars], \(x) summary(x)/sum(summary(x))), use.names = F)]
  tab1[, `Diabetes type 2` := unlist(lapply(data_analysis[diabetes_type == 2, .SD, .SDcols = tab1_vars], summary), use.names = F)]
  tab1[, `Diabetes type 2(%)` := unlist(lapply(data_analysis[diabetes_type == 2, .SD, .SDcols = tab1_vars], \(x) summary(x)/sum(summary(x))), use.names = F)]
  tab1[, Total := unlist(lapply(data_analysis[, .SD, .SDcols = tab1_vars], summary), use.names = F)]
  tab1[, `Total (%)` := unlist(lapply(data_analysis[, .SD, .SDcols = tab1_vars], \(x) summary(x)/sum(summary(x))), use.names = F)]
  
  tab1 <- rbind(data.table(Variable = "Number of participants (n)") , tab1, fill = T)
  tab1[1, c(2,4,6) := as.list(setorder(data_analysis[, .N, by = diabetes_type])[, N])]
  tab1[1, Total := data_analysis[, .N]]
  
  suppressWarnings({
    tab1 <- melt(tab1, id.vars = "Variable", measure.vars = patterns("No diabetes", "Diabetes type 1", "Diabetes type 2", "Total"),
                 variable.name = "Agg.", value.name = c("No diabetes", "Diabetes type 1", "Diabetes type 2", "Total"), na.rm = T)[, `Agg.` := factor(`Agg.`, levels = c(1,2), labels = c("n", "Prop."))][]
  })
  setorder(tab1, Variable)
  tab1[1, `Agg.` := NA]
  
  # replace in Variable: years with yrs, >=40 with 40+
  tab1[grepl("years", Variable), Variable := gsub(" years", "", Variable)]
  tab1[grepl(">=40", Variable), Variable := gsub(">=40", "40+", Variable)]
  tab1[grepl("/unkown", Variable), Variable := gsub("/unkown", "/NA", Variable)]
  tab1[grepl("/unknown", Variable), Variable := gsub("/unknown", "/NA", Variable)]
  tab1[grepl("Unkown", Variable), Variable := gsub("Unkown", "Unknown", Variable)]
  tab1[]
}

# Function to generate Figure 2
generate_fig2 <- function(data_analysis_) {
  
  data_analysis <- copy(data_analysis_)
  
  if ("db1time_age" %in% names(data_analysis)) {
    setnames(data_analysis, "db1time_age", "diabetes_diagnosis_age")
  }
  
  if(!("diabetes_type" %in% names(data_analysis))) {
    data_analysis[, diabetes_type := 1]
  }
  
  data_analysis[diabetes_diagnosis_age  <= 15, diabetes_diagnosis_age_grp := "0-15"]
  data_analysis[diabetes_diagnosis_age  %between% c(16, 30), diabetes_diagnosis_age_grp := "16-30"]
  data_analysis[diabetes_diagnosis_age  > 30, diabetes_diagnosis_age_grp := "31-40"]
  data_analysis[, diabetes_diagnosis_age_grp := factor(diabetes_diagnosis_age_grp, levels = c("0-15", "16-30", "31-40"))]
  
  data_analysis[diabetes_treatment %in% c("Only insulin", "Both insulin and oral medication", "With insulin"), diabetes_treatment_2 := "With insulin"]
  data_analysis[diabetes_treatment %in% c("Not applicable / Unknown", "Unknown"), diabetes_treatment_2 := "Unknown"]
  data_analysis[, diabetes_treatment_2 := factor(diabetes_treatment_2, levels = c("With insulin", "Unknown"))]
  
  fig2 <- setorder(data_analysis[diabetes_type == 1 & !is.na(diabetes_treatment_2), .N, by = .(sex, diabetes_diagnosis_age_grp, diabetes_treatment_2)])[]
  
  setnames(fig2, c("Sex", "Age at diagnosis", "Therapy", "n"))
  fig2[]
}

# Function to generate Table 2 (univariable analysis) - only task-specific synthesis
generate_tab2_univariable <- function(data_analysis) {
  
  data_analysis <- copy(data_analysis)
  
  data_analysis[diabetes_type == 1, db1status := 1]
  data_analysis[is.na(db1status), db1status := 0]
  
  # univariable cox
  
  uni_birthorder <- coxph(formula = Surv(db1time_age, db1status) ~ birth_order + strata(birthyrgr), data = data_analysis)
  uni_csec <- coxph(formula = Surv(db1time_age, db1status) ~ caesarean_delivery + strata(birthyrgr), data = data_analysis)
  uni_daycare <- coxph(formula = Surv(db1time_age, db1status) ~ attended_daycare + strata(birthyrgr), data = data_analysis)
  uni_sex <- coxph(formula = Surv(db1time_age, db1status) ~ sex + strata(birthyrgr), data = data_analysis)
  
  univar <- data.table(rbind(summary(uni_birthorder)$conf.int,
                             summary(uni_csec)$conf.int,
                             summary(uni_daycare)$conf.int,
                             summary(uni_sex)$conf.int)
  )[,-2]
  tab2_names_col <- c("Birth order: First", "Birth order: Second", "Birth order: >=Third", "Birth order: Unknown",
                      "C-section delivery: Yes", "C-section delivery: Unknown",
                      "Attended daycare: Yes", "Attended Daycare: Unknown",
                      "Sex: Female")
  tab2_univar <- cbind(tab2_names_col, Model = "Univariable", `Age at diagnosis` = "0–40", univar)
  setnames(tab2_univar, c("Variable", "Model", "Age at diagnosis", "Hazard ratio", "2.5%", "97.5%"))[]
}
