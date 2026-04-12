setwd("G:/Mi unidad/SIAE 2010-2020")
suppressPackageStartupMessages({
  library(frontier)
})

files <- list(
  "A_Total"        = c("sfa_A_Total.rds",        0.719),
  "A_I_winsorized" = c("sfa_A_I_winsorized.rds", 0.666),
  "A3g_Total"      = c("sfa_A3g_Total.rds",      0.718),
  "A3g_I"          = c("sfa_A3g_I.rds",          0.665),
  "B_Q_v2"         = c("sfa_B_Q_v2.rds",         0.278),
  "B_M_v2"         = c("sfa_B_M_v2.rds",         0.598),
  "C_panel"        = c("sfa_C_panel.rds",        0.291),
  "D_odf"          = c("sfa_D_odf.rds",          0.585)
)

for (label in names(files)) {
  fn <- files[[label]][1]
  expected <- as.numeric(files[[label]][2])
  path <- file.path("data_intermediate", fn)
  obj <- readRDS(path)
  te <- tryCatch(
    round(mean(frontier::efficiencies(obj), na.rm = TRUE), 3),
    error = function(e) NA
  )
  flag <- if (is.na(te)) "ERROR" else if (abs(te - expected) > 0.05) "*** FLAG ***" else "OK"
  cat(sprintf("%-20s TE=%-6s  expected=%-6s  %s\n", fn, te, expected, flag))
}
