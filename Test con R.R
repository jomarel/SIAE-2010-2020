setwd("G:/Mi unidad/SIAE 2010-2020")
load("data_clean/df_final.RData")

to_num <- function(x) suppressWarnings(as.numeric(x))

vars_2021 <- c(
  "totbiopsias","totangio","totdensiometrias","totgamma","totmamo",
  "totpet","totresonancia","totrx","totspect","tottac",
  "Col_hosp","Bron_hosp","ERCP_hosp","Col_Amb","Bron_Amb","ERCP_Amb"
)

sapply(vars_2021, function(v) v %in% names(df_final))

diag_2021 <- do.call(rbind, lapply(vars_2021, function(v) {
  x <- to_num(df_final[df_final$anyo == 2021, v])
  data.frame(
    variable = v,
    n_pos = sum(!is.na(x) & x > 0),
    median = median(x, na.rm = TRUE),
    mean = mean(x, na.rm = TRUE),
    pct_na = 100 * mean(is.na(x))
  )
}))

diag_2021