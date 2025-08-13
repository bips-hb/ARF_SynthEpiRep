# =============================================================================
# Generate Table 1: Dataset Overview
# This script summarizes key properties of each dataset (sample size, variables,
# number of categorical/numerical features, missingness, etc.).
# The resulting table is used as Table 1 in the manuscript.
# =============================================================================

# Load required libraries
library(data.table)

# Define a named list of dataset folders (masked) for each publication and corresponding dataset
datasets <- list(
  `Fischer et al.` = "<...>/Fischer_Anthopometrie/train/train.rds",
  `Schikowski et al.` = "<...>/Schikowski_Blutdruck/train/train.rds",
  `Tanoey et al.` = "<...>/Tanoey_Diabetes/train/train.rds",
  `Wienbergen et al.` = "<...>/Wienbergen_MI/train/train.rds",
  `Berger et al.` = "<...>/Berger_Corona/train/train.rds",
  `Breau et al.` = "<...>/Breau_PA/train/train.rds"
)

# Create a dataset overview by reading and summarizing each dataset
dataset_overview <- foreach(dataset_name = names(datasets), .combine = rbind) %do% {
  data <- readRDS(datasets[[dataset_name]])
  n <- nrow(data)
  d <- ncol(data)
  n_missings <- sum(apply(data, 1, \(row) any(is.na(row))))
  d_missings <- sum(sapply(data, \(col) any(is.na(col))))
  missings <- sum(is.na(data))/(n*d)
  d_num <- sum(sapply(data, is.numeric))
  d_cat <- sum(sapply(data, \(col) is.factor(col) | is.character(col)))
  classes <- data[, sum(sapply(.SD, uniqueN)), .SDcols = !sapply(data, is.numeric)]
  # if nako part of dataset name, then study_data is NAKO, otherwise GFHS
  study_data <- ifelse(grepl("NAKO", datasets[dataset_name]), "NAKO", "GFHS")
  data.table(dataset_name = dataset_name, n = n, d = d, d_num = d_num, d_cat = d_cat, classes = classes,
             n_missings = n_missings, d_missings = d_missings, missings = missings, study_data = study_data)
}

# Print overview table
dataset_overview
