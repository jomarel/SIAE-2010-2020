setwd("G:/Mi unidad/SIAE 2010-2020")
suppressPackageStartupMessages({
  library(frontier)
  library(dplyr)
  library(moments)
})
writeLines(
  capture.output(sessionInfo()),
  "data_clean/session_info.txt"
)
cat("session_info.txt written\n")
