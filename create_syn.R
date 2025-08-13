# =========================================
# Create synthetic data using ARF
# Applies to all 6 epidemiological studies
# =========================================

# Load required libraries
library(data.table)
library(doParallel)
library(foreach)
library(arf)

# Register parallel backend if desired
registerDoParallel(20)

# Define a named list of dataset folders (masked) for each publication and corresponding dataset
data_folders <- list(
  Berger = "<...>/Berger_Corona",
  Fischer = "<...>/Fischer_Anthopometrie",
  Schikowski = "<...>/Schikowski_Blutdruck",
  Tanoey = "<...>/Tanoey_Diabetes",
  Wienbergen = "<...>/Wienbergen_MI",
  Breau = "<...>/Breau_PA"
  )

# Function to perform synthetic data generation using ARF pipeline with controllable parameters
# for training and sampling repetitions
synthesize <- function(dataset_name, reps_arf_training = 20, reps_sampling = 5) {
  
  # Locate the dataset directory
  data_folder <- data_folders[[dataset_name]]
  
  # Identify all task-specific subdirectories inside the training folder (if any)
  taskspec_folders <- list.dirs(paste0(data_folder, "/train"), full.names = F, recursive = F)
  
  # Construct a named list of training file paths:
  # - First element: full dataset
  # - Subsequent elements: task-specific subsets
  train_files <- as.list(c(paste(data_folder, "train", "train.rds", sep = "/"),
                      sapply(taskspec_folders, \(x) list.files(paste(data_folder, "train", x, sep = "/") , full.names = T))))
  names(train_files) <- c("full", sapply(taskspec_folders, \(x) paste0(x, "/",
                                         stringr::str_remove_all(list.files(paste(data_folder, "train", x, sep = "/")),
                                                         paste("train_", paste0(x,"_"), ".rds", sep = "|")))))
  
  # Loop over all training datasets (full and task-specific)                        
  for(train_file_name in names(train_files)) {
    
    # Load training data
    train <- readRDS(train_files[[train_file_name]])
    
    # Ensure all integer columns are treated as numeric for ARF modeling
    integer_cols <- which(sapply(train, is.integer))
    train[,(integer_cols) := lapply(.SD,as.numeric), .SDcols = integer_cols]
    
    # Set reproducible random seed for parallel processing
    set.seed(2025, kind = "L'Ecuyer-CMRG")
    
    # Outer loop: ARF model training and FORDE parameter estimation (parallelized)
    void <- foreach(rep_arf_training = 1: reps_arf_training) %dopar% {
      arf <- adversarial_rf(train, parallel = F)
      params <- forde(arf, train, finite_bounds = "local", parallel = F)
      
      # Inner loop: generate multiple synthetic datasets using FORGE
      for(rep_sampling in 1:reps_sampling) {
        synth <- forge(params, nrow(train), sample_NAs = T, parallel = F)
        synth[,(integer_cols) := lapply(.SD, \(x) as.integer(x)), .SDcols = integer_cols]
        save_path <- ifelse(length(train_files) == 1, 
                            paste0(data_folder, "/syn/syn_", rep_arf_training, "_", rep_sampling, ".rds"),
                            paste0(data_folder, "/syn/", train_file_name, "/syn_", rep_arf_training, "_", rep_sampling, ".rds"))
        
        # Save synthetic dataset to disk
        saveRDS(synth, save_path)
      }
    }
    
    # Progress update
    print(paste0("Synthesis for ", dataset_name, " (", train_file_name,") completed."))
  }
}

# Apply synthesis function to all datasets
for(dataset_name in names(data_folders)) {
  synthesize(dataset_name)
}