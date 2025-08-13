# ==============================================================================
# Create training dataset for Breau et al. study on physical activity in children
#
# This script performs the following:
# - Loads raw GFHS data with physical activity and anthropometry measures
# - Excludes unnecessary or redundant columns, including derived z-scores,
#   age groupings, and accelerometer summaries from multiple algorithms
# - Saves the reduced dataset as an RDS file for further analysis
# ==============================================================================

# Load required libraries
library(data.table)

# Set the data folder path (masked)
data_folder <- "<...>/Breau_PA"

# Load raw data
raw <- fread(paste(data_folder, "raw", "raw.csv", sep = "/"),stringsAsFactors = T,dec = ",")

# Remove unnecessary columns
train <- raw[,-c("PID","Birthdate","Age_Category","Age_Group_first","bmi.zscore.age.ha","bmi.zscore.age.asa",
      "Puyau_SED_mean","Puyau_TPA_mean","Puyau_Guide_180_1","Puyau_Guide_180_60","Puyau_Guide_60","Puyau_Guide_age",
      "Evenson_SED_mean","Evenson_TPA_mean","Evenson_Guide_180_1","Evenson_Guide_180_60","Evenson_Guide_60","Evenson_Guide_age",
      "Pate_SED_mean","Pate_TPA_mean","Pate_Guide_180_1","Pate_Guide_180_60","Pate_Guide_60","Pate_Guide_age",
      "Trost_SED_mean","Trost_TPA_mean","Trost_Guide_180_1","Trost_Guide_180_60","Trost_Guide_60","Trost_Guide_age",
      "Butte_VM_SED_mean","Butte_VM_TPA_mean","Butte_VM_Guide_180_1","Butte_VM_Guide_180_60","Butte_VM_Guide_60","Butte_VM_Guide_age",
      "Butte_VA_SED_mean","Butte_VA_TPA_mean","Butte_VA_Guide_180_1","Butte_VA_Guide_180_60","Butte_VA_Guide_60","Butte_VA_Guide_age",
      "VanCauwenberghe_SED_mean","VanCauwenberghe_TPA_mean","VC_Guide_180_1","VC_Guide_180_60","VC_Guide_60","VC_Guide_age",
      "Pate_Pfeiffer_SED_mean","Pate_Pfeiffer_MPA_mean","Pate_Pfeiffer_VPA_mean","Pate_Pfeiffer_TPA_mean","PP_Guide_180_1","PP_Guide_180_60","PP_Guide_60","PP_Guide_age",
      "Costa_VA_SED_mean","Costa_VA_TPA_mean","Costa_VA_Guide_180_1","Costa_VA_Guide_180_60","Costa_VA_Guide_60","Costa_VA_Guide_age",
      "Costa_VM_SED_mean","Costa_VM_TPA_mean","Costa_VM_Guide_180_1","Costa_VM_Guide_180_60","Costa_VM_Guide_60","Costa_VM_Guide_age"
      )]

# Save cleaned training dataset
saveRDS(train,paste(data_folder, "train", "train.rds",sep="/"))
