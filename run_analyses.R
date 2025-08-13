# ==========================================================================
# Run dataset-specific analysis scripts (analysis.R and analysis_taskspec.R)
# These scripts replicate original statistical analyses and compare results
# from real and synthetic datasets for each study.
# ==========================================================================

# Load required libraries
library(doParallel)

# Register parallel workers if desired
registerDoParallel(32)

# Full dataset analysis scripts
source("NAKO/Berger et al./analysis.R")
source("NAKO/Fischer et al./analysis.R")
source("NAKO/Schikowski et al./analysis.R")
source("NAKO/Tanoey et al./analysis.R")
source("NAKO/Wienbergen et al./analysis.R")
source("GFHS/Breau et al./analysis.R")

# Task-specific analysis scripts
source("NAKO/Berger et al./analysis_taskspec.R")
source("NAKO/Tanoey et al./analysis_taskspec.R")
