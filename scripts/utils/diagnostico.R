source("G:/Mi unidad/SIAE 2010-2020/scripts/00_config.R")
library(sfaR); library(dplyr)
load(file.path(INT_DIR, "df_sfa.RData"))

# Muestra principal: excluye años COVID
df_est_main <- df_sfa %>%
  filter(
    !anyo %in% 2020:2022,
    altQ_bruto >= 200, altM_bruto >= 200,
    !is.na(ln_altQ_pond) & is.finite(ln_altQ_pond),
    !is.na(ln_altM_pond) & is.finite(ln_altM_pond),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(D_desc), !is.na(pct_sns),
    altTotal_bruto > 100
  )

cat("Muestra main (sin COVID):", nrow(df_est_main), "obs |",
    n_distinct(df_est_main$NCODI), "hospitales\n")
cat("Años incluidos:", paste(sort(unique(df_est_main$anyo)), collapse=", "), "\n")

# Submuestra Diseño A sin COVID
df_est_A_main <- df_sfa %>%
  filter(
    !anyo %in% 2020:2022,
    !is.na(ln_altTotal_pond) & is.finite(ln_altTotal_pond),
    !is.na(ln_i_diag) & is.finite(ln_i_diag),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(D_desc), !is.na(pct_sns),
    altTotal_bruto > 100
  )
cat("Muestra A main (sin COVID, con i_diag):", nrow(df_est_A_main), "obs\n")

formula_Q <- ln_altQ_pond ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2
formula_M <- ln_altM_pond ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2
formula_Tot <- ln_altTotal_pond ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2
formula_I <- ln_i_diag ~ ln_L_total_c + ln_K_camas_c +
  ln_K_tech_c + ln_L_total_c2 + ln_K_camas_c2 + trend + trend2
uhet_f <- ~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ

estimar <- function(formula, data, label) {
  for (cfg in list(
    list(ud="tnormal", mt="bfgs", ht=1L),
    list(ud="tnormal", mt="bhhh", ht=1L),
    list(ud="hnormal", mt="bfgs", ht=2L),
    list(ud="hnormal", mt="bhhh", ht=2L)
  )) {
    cat(sprintf("  [%s] %s/%s\n", label, cfg$ud, cfg$mt))
    m <- tryCatch(
      sfaR::sfacross(formula, uhet=uhet_f, data=data,
                     S=1L, udist=cfg$ud, method=cfg$mt,
                     hessianType=cfg$ht),
      error=function(e){cat("  Error:",e$message,"\n"); NULL}
    )
    if (is.null(m)) next
    ll <- as.numeric(m$mlLoglik)
    cf <- coef(m)
    if (!is.finite(ll)||abs(ll)>1e10||any(abs(cf)>100,na.rm=TRUE)){
      cat("  Degenerado\n"); next
    }
    cat(sprintf("  OK: ll=%.2f [%s]\n", ll, cfg$ud))
    return(m)
  }
  NULL
}

cat("\n--- Diseño B sin COVID ---\n")
mQ_main <- estimar(formula_Q, df_est_main, "Q_main")
mM_main <- estimar(formula_M, df_est_main, "M_main")

cat("\n--- Diseño A sin COVID ---\n")
mTot_main <- estimar(formula_Tot, df_est_A_main, "Tot_main")
mI_main   <- estimar(formula_I,   df_est_A_main, "I_main")

# Guardar y mostrar tabla comparativa para cada diseño
mostrar_tabla <- function(m1, m2, label1, label2, titulo) {
  if (is.null(m1) || is.null(m2)) {
    cat("Tabla", titulo, "no disponible\n"); return()
  }
  cf1 <- coef(m1); se1 <- sqrt(diag(vcov(m1)))
  cf2 <- coef(m2); se2 <- sqrt(diag(vcov(m2)))
  i1 <- grep("^Zu_", names(cf1))
  i2 <- grep("^Zu_", names(cf2))
  tab <- data.frame(
    param  = sub("^Zu_","", names(cf1)[i1]),
    coef_1 = round(cf1[i1],4), t_1 = round(cf1[i1]/se1[i1],3),
    coef_2 = round(cf2[i2],4), t_2 = round(cf2[i2]/se2[i2],3)
  )
  tab$z <- round((tab$coef_1-tab$coef_2)/
                   sqrt(se1[i1]^2+se2[i2]^2), 3)
  tab$p <- round(2*(1-pnorm(abs(tab$z))),4)
  names(tab)[2:3] <- c(label1, paste0("t_",label1))
  names(tab)[4:5] <- c(label2, paste0("t_",label2))
  cat("\n════════", titulo, "════════\n")
  print(tab)
}

mostrar_tabla(mQ_main, mM_main, "Q_main", "M_main",
              "DISEÑO B sin COVID (>=200 altas)")
mostrar_tabla(mTot_main, mI_main, "Tot_main", "I_main",
              "DISEÑO A sin COVID")

# Guardar modelos
for (nm in c("mQ_main","mM_main","mTot_main","mI_main")) {
  m <- get(nm)
  if (!is.null(m))
    saveRDS(m, file.path(INT_DIR, paste0("sfa_", nm, ".rds")))
}
cat("\nModelos guardados.\n")