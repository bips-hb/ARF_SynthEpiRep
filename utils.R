# ==============================================================================
# Utility function `save_table()` for formatting and exporting replication
# tables of real and synthetic dataset results.
# The function combines median, lower, and upper quantile synthetic
# (empirical percentile-based 95% CI) results into concise interval strings,
# applies LaTeX color formatting for clarity, and writes output tables suitable
# for manuscript inclusion.
# ==============================================================================

# Load required libraries
library(xtable)
library(data.table)

# Main function to format and save combined real and synthetic results as a LaTeX table
# Inputs:
# - table_real: data.table with original dataset results
# - table_syn_median: Median synthetic results (same structure as table_real)
# - table_syn_lo: Lower bounds of synthetic 95% CI (same structure as table_real)
# - table_syn_hi: Upper bounds of synthetic 95% CI (same structure as table_real)
# - save_path: File path to save the LaTeX table
# - digits: Number of decimal places for rounding (default 2)
# - colours: Vector of LaTeX color names for synthetic result highlighting
save_table <- function(table_real, table_syn_median, table_syn_lo, table_syn_hi, save_path, digits = 2, colours = c("orangered", "orange", "salmon3", "violetred4" )) {
  
  table_combine_syn <- function(table_syn_median, table_syn_lo, table_syn_hi) {
    # identify factor or character columns
    num_cols <- names(which(sapply(table_syn_median, is.numeric)))
    
    keep_identical <- (sapply(num_cols, \(col) {
      which(is.na(table_syn_median[[col]]))
    }))
    names(keep_identical) <- num_cols
    
    # combine tables
    table_syn <- copy(table_syn_median)
    table_syn[, (num_cols) := lapply(num_cols, \(col) {
      combined <- paste0(round(table_syn_median[[col]], digits), " (", 
                         round(table_syn_lo[[col]], digits), "-",
                         round(table_syn_hi[[col]], digits), ")"
      )
      combined[keep_identical[[col]]] <- round(table_syn_median[[col]][keep_identical[[col]]], digits)
      combined
    })]
    
    table_syn[]
  }
  
  table_syn <- table_combine_syn(table_syn_median, table_syn_lo, table_syn_hi)
  
  format_syn_colour <- function(x, colour_idx, colours) {
    ifelse(is.na(x), NA, paste0("\\textcolor{", colours[[colour_idx]] ,"}{", x, "}"))
  }
  
  table_combine_realsyn <- function(table_real, table_syn, format_syn = identity) {
    
    table_real_ <- copy(table_real)
    
    num_cols <- names(which(sapply(table_real, is.numeric)))
    factor_cols <- setdiff(names(table_real), num_cols)
    
    table_real_[, (num_cols) := lapply(num_cols, \(col) round(table_real[[col]], digits))] 
    
    # apply format_syn to table_syn num_cols
    table_syn[, colour_idx := 1:.N, by = factor_cols]
    table_syn[, (num_cols) := lapply(num_cols, \(col) format_syn(.SD[[col]], unique(colour_idx))), by = colour_idx]
    table_syn[, colour_idx := NULL]
    
    table_real_[, I := .GRP, by = factor_cols]
    table_syn[, I := .GRP, by = factor_cols]
    table <- rbind(table_real_, table_syn)
    table <- table[order(I)]
    table[, I := NULL]
    table[, (factor_cols) := lapply(factor_cols, \(col) xtable::sanitize(table[[col]]))]
    table[, (factor_cols) := lapply(.SD, \(col) {
      match <- c(FALSE, (col[-1] == col[-.N]))
      col[match] <- NA
      col
    }), .SDcols = factor_cols]
    table[]
  }
  
  tbl <- table_combine_realsyn(table_real, table_syn, format_syn = \(x, colour_idx) format_syn_colour(x, colour_idx, colours))
  
  print(xtable(tbl),
        only.contents = T,
        floating = FALSE,
        include.rownames=FALSE,
        sanitize.text.function = identity,
        sanitize.colnames.function = sanitize,
        file = save_path)
}