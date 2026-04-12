## =============================================================
## _pipeline_fix.R — Comprehensive pipeline fix
## Phases 1-5: Fix cod_depend_agrupada, build all SFA variables
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")

library(dplyr)
library(readr)

CLEAN_DIR <- "data_clean"
dir.create(CLEAN_DIR, showWarnings = FALSE, recursive = TRUE)

to_num <- function(x) suppressWarnings(as.numeric(x))
safe_log <- function(x) ifelse(!is.na(x) & x > 0, log(x), NA_real_)

## =============================================================
## PHASE 1 — Load df_final and fix cod_depend_agrupada
## =============================================================
cat("\n========== PHASE 1: Fix cod_depend_agrupada ==========\n")

load("data_legacy_outputs/df_final.RData")
cat("Loaded df_final:", nrow(df_final), "x", ncol(df_final), "\n")
df_final$NCODI <- as.integer(df_final$NCODI)

# The XML column is Cód_Pertenencia_SNS (with accent) for 2016-2023
# TXT files already have cod_depend_agrupada for 2010-2015
if ("Cód_Pertenencia_SNS" %in% names(df_final)) {
  xml_val <- to_num(df_final[["Cód_Pertenencia_SNS"]])
  siae_val <- to_num(df_final$cod_depend_agrupada)

  # Fill in missing cod_depend_agrupada from XML column
  mask <- is.na(siae_val) & !is.na(xml_val)
  cat("Filling cod_depend_agrupada from Cód_Pertenencia_SNS:", sum(mask), "values\n")
  df_final$cod_depend_agrupada[mask] <- xml_val[mask]
} else {
  cat("WARNING: Cód_Pertenencia_SNS not found. Checking all column names...\n")
  hits <- grep("Pertenencia|depend", names(df_final), ignore.case = TRUE, value = TRUE)
  cat("  Matches:", paste(hits, collapse = ", "), "\n")
}

# Verify
cat("\ncod_depend_agrupada by year after fix:\n")
print(table(df_final$anyo, df_final$cod_depend_agrupada, useNA = "always"))

# Compute % cod=2 (private) by year
pct_priv <- df_final %>%
  group_by(anyo) %>%
  summarise(
    n = n(),
    n_pub = sum(cod_depend_agrupada == 1, na.rm = TRUE),
    n_priv = sum(cod_depend_agrupada == 2, na.rm = TRUE),
    n_na = sum(is.na(cod_depend_agrupada)),
    pct_priv = round(100 * n_priv / n, 1),
    .groups = "drop"
  )
cat("\n% private by year (target ~40-45%):\n")
print(as.data.frame(pct_priv))

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))
cat("\nPhase 1 complete. Saved to", file.path(CLEAN_DIR, "df_final.RData"), "\n")
cat("Total rows:", nrow(df_final), "\n")
