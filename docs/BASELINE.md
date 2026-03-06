# BASELINE - Dataset Summaries

This document provides code snippets and a checklist to establish baseline metrics for key datasets in the SIAE 2010-2020 analysis pipeline.

## Checklist of Baseline Outputs

- [ ] Record baseline dimensions and summaries for `df_final`
- [ ] Record baseline dimensions and summaries for `df_completo_actualizado`
- [ ] Record baseline dimensions and summaries for `df_seleccion`
- [ ] Record baseline statistics for `mix` variable
- [ ] Store baseline outputs in `outputs/baseline_summary.txt`
- [ ] Archive baseline R session info and package versions
- [ ] Compare across pipeline stages for data integrity

---

## R Code Snippets for Baseline Recording

### 1. Setup and Loading

```r
# Source configuration
source("scripts/00_config.R")
library(tidyverse)
library(dplyr)

# Create output file for baseline summary
baseline_file <- file.path(OUT_DIR, "baseline_summary.txt")
sink(baseline_file, append = TRUE)

cat("\n========================================\n")
cat("BASELINE SUMMARY - ", Sys.time(), "\n")
cat("========================================\n\n")
```

### 2. df_final Baseline

Load and record baseline metrics for `df_final`:

```r
# Load df_final
df_final <- read.table(file.path(INT_DIR, "df_final.txt"), 
                       sep = ";", dec = ",", header = TRUE)

cat("--- df_final ---\n")
cat("Dimensions: ", nrow(df_final), " rows x ", ncol(df_final), " columns\n\n")

cat("Variables:\n")
cat(paste(names(df_final), collapse = ", "), "\n\n")

cat("Data types:\n")
print(str(df_final))

cat("\nMissing values by column:\n")
missing_summary <- colSums(is.na(df_final))
print(missing_summary[missing_summary > 0])

cat("\nKey summaries:\n")
cat("Years covered: ", min(df_final$anyo, na.rm = TRUE), 
    " to ", max(df_final$anyo, na.rm = TRUE), "\n")
cat("Unique hospitals (NCODI): ", n_distinct(df_final$NCODI), "\n")
cat("Unique regions (ccaa): ", n_distinct(df_final$ccaa), "\n")

cat("\nSample of first rows:\n")
print(head(df_final, 3))

cat("\n")
```

### 3. df_completo_actualizado Baseline

Load and record baseline metrics for `df_completo_actualizado`:

```r
# Load df_completo_actualizado
load(file.path(INT_DIR, "df_completo_actualizado.RData"))

cat("--- df_completo_actualizado ---\n")
cat("Dimensions: ", nrow(df_completo_actualizado), " rows x ", 
    ncol(df_completo_actualizado), " columns\n\n")

cat("Variables:\n")
cat(paste(names(df_completo_actualizado), collapse = ", "), "\n\n")

cat("Data types summary:\n")
type_summary <- table(sapply(df_completo_actualizado, class))
print(type_summary)

cat("\nMissing values (showing columns with missing data):\n")
missing_summary <- colSums(is.na(df_completo_actualizado))
print(missing_summary[missing_summary > 0])

cat("\nKey summaries:\n")
cat("Total rows: ", nrow(df_completo_actualizado), "\n")
cat("Total columns: ", ncol(df_completo_actualizado), "\n")
cat("Years covered: ", min(df_completo_actualizado$anyo, na.rm = TRUE),
    " to ", max(df_completo_actualizado$anyo, na.rm = TRUE), "\n")
cat("Unique hospitals (NCODI): ", n_distinct(df_completo_actualizado$NCODI), "\n")

cat("\nObservations per year:\n")
print(table(df_completo_actualizado$anyo))

cat("\nObservations per region (ccaa):\n")
print(table(df_completo_actualizado$ccaa))

cat("\nDuplicate check (NCODI + anyo):\n")
cat("Duplicates: ", sum(duplicated(df_completo_actualizado[, c("NCODI", "anyo")])), "\n")

cat("\n")
```

### 4. df_seleccion Baseline

Load and record baseline metrics for `df_seleccion`:

```r
# Load df_seleccion
load(file.path(INT_DIR, "df_seleccion.RData"))

cat("--- df_seleccion ---\n")
cat("Dimensions: ", nrow(df_seleccion), " rows x ", ncol(df_seleccion), " columns\n\n")

cat("Variables:\n")
cat(paste(names(df_seleccion), collapse = ", "), "\n\n")

cat("Data types summary:\n")
type_summary <- table(sapply(df_seleccion, class))
print(type_summary)

cat("\nMissing values (showing columns with missing data):\n")
missing_summary <- colSums(is.na(df_seleccion))
if(length(missing_summary[missing_summary > 0]) > 0) {
  print(missing_summary[missing_summary > 0])
} else {
  cat("No missing values detected.\n")
}

cat("\nKey summaries:\n")
cat("Total rows: ", nrow(df_seleccion), "\n")
cat("Total columns: ", ncol(df_seleccion), "\n")
cat("Years covered: ", min(df_seleccion$anyo, na.rm = TRUE),
    " to ", max(df_seleccion$anyo, na.rm = TRUE), "\n")
cat("Unique hospitals (NCODI): ", n_distinct(df_seleccion$NCODI), "\n")

cat("\nDuplicate check (NCODI + anyo):\n")
cat("Duplicates: ", sum(duplicated(df_seleccion[, c("NCODI", "anyo")])), "\n")

cat("\nNumeric variables summary:\n")
numeric_cols <- df_seleccion %>% select(where(is.numeric)) %>% names()
for(col in numeric_cols[1:min(5, length(numeric_cols))]) {
  cat("\n", col, ":\n")
  print(summary(df_seleccion[[col]]))
}

cat("\n")
```

### 5. Mix Variable Baseline

Record baseline statistics for the `mix` variable across datasets:

```r
# Assuming 'mix' is in df_seleccion
cat("--- mix Variable ---\n")

if("mix" %in% names(df_seleccion)) {
  cat("Mix variable found in df_seleccion\n\n")
  
  cat("Summary statistics:\n")
  print(summary(df_seleccion$mix))
  
  cat("\nMissing values: ", sum(is.na(df_seleccion$mix)), "\n")
  cat("Non-missing values: ", sum(!is.na(df_seleccion$mix)), "\n")
  
  cat("\nDistribution by year:\n")
  print(df_seleccion %>% 
          group_by(anyo) %>%
          summarise(
            n = n(),
            mean_mix = mean(mix, na.rm = TRUE),
            sd_mix = sd(mix, na.rm = TRUE),
            min_mix = min(mix, na.rm = TRUE),
            max_mix = max(mix, na.rm = TRUE),
            .groups = 'drop'
          ))
  
  cat("\nDistribution by region (first 10 regions):\n")
  print(df_seleccion %>% 
          group_by(ccaa) %>%
          summarise(
            n = n(),
            mean_mix = mean(mix, na.rm = TRUE),
            sd_mix = sd(mix, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
        head(10))
  
} else {
  cat("Mix variable NOT found in df_seleccion.\n")
}

cat("\n")
```

### 6. Closing and Session Info

```r
# Record session and package information
cat("\n========================================\n")
cat("SESSION INFO\n")
cat("========================================\n")
print(sessionInfo())

# Close sink
sink()

cat("\nBaseline summary saved to: ", baseline_file, "\n")
```

---

## Usage Instructions

1. **Load Configuration**: Source `00_config.R` at the beginning of your session
2. **Run in Sequence**: Execute the code snippets above in order (Setup → df_final → df_completo_actualizado → df_seleccion → mix → Session Info)
3. **Output Location**: Results are saved to `outputs/baseline_summary.txt`
4. **Check Results**: Review `baseline_summary.txt` to verify dimensions, missing values, and key distributions
5. **Archive**: Save a timestamped copy of `baseline_summary.txt` for future comparison

---

## Key Metrics to Monitor

| Dataset | Key Metrics |
|---------|------------|
| **df_final** | Rows, cols, years, hospitals, missing values |
| **df_completo_actualizado** | Rows, cols, years, hospitals, duplicates, missing by column |
| **df_seleccion** | Rows, cols, unique hospitals, duplicates, numeric distributions |
| **mix** | Mean, SD, range, distribution by year/region, missing count |

---

## Notes

- All paths use configuration variables from `00_config.R`
- Baseline comparisons help detect pipeline changes
- Keep timestamped versions for audit trail
- Run after loading major datasets or after pipeline modifications
