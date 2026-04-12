# Salida completa todos los modelos
source("scripts/00_config.R")
library(sfaR); library(dplyr)

rds_exists <- function(f) file.exists(file.path(INT_DIR, f))

modelos_todos <- list(
  "D_ODF"       = if (rds_exists("sfa_modeloD_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloD_definitivo.rds")) else NULL,
  "D_ODF_enr"   = if (rds_exists("sfa_modeloD_enriquecido.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloD_enriquecido.rds")) else NULL,
  "B_Q"         = if (rds_exists("sfa_modeloQ_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloQ_definitivo.rds")) else NULL,
  "B_M"         = if (rds_exists("sfa_modeloM_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloM_definitivo.rds")) else NULL,
  "A_Tot"       = if (rds_exists("sfa_modeloTotal_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloTotal_definitivo.rds")) else NULL,
  "A_I"         = if (rds_exists("sfa_modeloI_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloI_definitivo.rds")) else NULL,
  "A_Tot_Ddesc" = if (rds_exists("sfa_mTot_Ddesc_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_mTot_Ddesc_definitivo.rds")) else NULL,
  "A_I_Ddesc"   = if (rds_exists("sfa_mI_Ddesc_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_mI_Ddesc_definitivo.rds")) else NULL,
  "C_panel"     = if (rds_exists("sfa_modeloC_definitivo.rds"))
                    readRDS(file.path(INT_DIR, "sfa_modeloC_definitivo.rds")) else NULL
)

# SECCION 1: Resumen
cat("\n")
cat("=================================================================\n")
cat("  RESUMEN GENERAL - TODOS LOS MODELOS\n")
cat("=================================================================\n")
cat(sprintf("%-20s %10s %8s %8s %s\n", "Modelo","ll","TE_media","TE_sd","dist"))
cat(paste(rep("-", 60), collapse=""), "\n")
for (nm in names(modelos_todos)) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) { cat(sprintf("%-20s  no disponible\n", nm)); next }
  ll   <- round(as.numeric(m$mlLoglik), 2)
  dist <- tryCatch(m$udist, error = function(e) "?")
  te   <- tryCatch(sfaR::efficiencies(m)$teJLMS, error = function(e) NULL)
  med  <- if (!is.null(te)) round(mean(te, na.rm=TRUE), 3) else NA
  sdte <- if (!is.null(te)) round(sd(te,   na.rm=TRUE), 3) else NA
  cat(sprintf("%-20s %10.2f %8.3f %8.3f [%s]\n", nm, ll, med, sdte, dist))
}

# SECCION 2: Ecuacion de ineficiencia (sin CCAA)
cat("\n")
cat("=================================================================\n")
cat("  ECUACION DE INEFICIENCIA - determinantes (sin dummies CCAA)\n")
cat("=================================================================\n")
for (nm in names(modelos_todos)) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) next
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA, length(cf)))
  idx <- grep("^Zu_", names(cf))
  idx <- idx[!grepl("ccaa", names(cf)[idx])]
  if (length(idx) == 0) next
  cat(sprintf("\n[%s]\n", nm))
  cat(sprintf("  %-25s %8s %6s %8s\n", "Parametro","coef","t","p"))
  for (i in idx) {
    t_val <- cf[i] / se[i]
    p_val <- 2 * (1 - pnorm(abs(t_val)))
    sig <- if (is.na(p_val)) "" else
           if (p_val < 0.001) "***" else
           if (p_val < 0.01)  "**"  else
           if (p_val < 0.05)  "*"   else
           if (p_val < 0.10)  "."   else ""
    cat(sprintf("  %-25s %8.4f %6.2f %8.4f %s\n",
      sub("^Zu_", "", names(cf)[i]), cf[i], t_val, p_val, sig))
  }
}

# SECCION 3: Contrastes P1-P4
cat("\n")
cat("=================================================================\n")
cat("  CONTRASTES P1-P4 - COMPARACION DE PARES\n")
cat("=================================================================\n")

contrastar_par <- function(m1, m2, nm1, nm2, pars_interes, titulo) {
  if (is.null(m1) || is.null(m2)) {
    cat(titulo, ": no disponible\n"); return(invisible(NULL))
  }
  cf1 <- coef(m1); se1 <- tryCatch(sqrt(diag(vcov(m1))), error=function(e) rep(NA,length(cf1)))
  cf2 <- coef(m2); se2 <- tryCatch(sqrt(diag(vcov(m2))), error=function(e) rep(NA,length(cf2)))
  cat(sprintf("\n%s\n", titulo))
  cat(sprintf("  %-22s %8s %6s %8s %6s %7s %7s\n",
    "Parametro", nm1,"t1", nm2,"t2","z","p"))
  cat(paste(rep("-", 74), collapse=""), "\n")
  for (p in pars_interes) {
    nm_p <- paste0("Zu_", p)
    if (!nm_p %in% names(cf1) || !nm_p %in% names(cf2)) next
    dif  <- cf1[nm_p] - cf2[nm_p]
    se_d <- sqrt(se1[nm_p]^2 + se2[nm_p]^2)
    z    <- dif / se_d
    pv   <- 2 * (1 - pnorm(abs(z)))
    sig  <- if (is.na(pv)) "" else
            if (pv < 0.001) "***" else
            if (pv < 0.01)  "**"  else
            if (pv < 0.05)  "*"   else
            if (pv < 0.10)  "."   else ""
    cat(sprintf("  %-22s %8.3f %6.2f %8.3f %6.2f %7.3f %7.4f %s\n",
      p, cf1[nm_p], cf1[nm_p]/se1[nm_p],
         cf2[nm_p], cf2[nm_p]/se2[nm_p],
         z, pv, sig))
  }
}

contrastar_par(modelos_todos$A_Tot, modelos_todos$A_I,
  "alpha_Tot", "delta_I",
  c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ"),
  "DISENO A (grupos pago): Cantidad vs Intensidad")

contrastar_par(modelos_todos$A_Tot_Ddesc, modelos_todos$A_I_Ddesc,
  "alpha_Tot", "delta_I",
  c("D_desc","pct_sns","desc_pago","ShareQ","desc_shareQ"),
  "DISENO A (D_desc): Cantidad vs Intensidad")

contrastar_par(modelos_todos$B_Q, modelos_todos$B_M,
  "alpha_Q", "delta_M",
  c("d_Priv_Conc","d_Priv_Merc","ShareQ","Conc_shareQ","Merc_shareQ"),
  "DISENO B: Quirurgico vs Medico")

# SECCION 4: Eficiencias por grupo
cat("\n")
cat("=================================================================\n")
cat("  EFICIENCIAS TECNICAS POR GRUPO\n")
cat("=================================================================\n")

load(file.path(INT_DIR, "df_sfa.RData"))

for (nm in c("B_Q","B_M","A_Tot","A_I","A_Tot_Ddesc","A_I_Ddesc")) {
  m <- modelos_todos[[nm]]
  if (is.null(m)) next
  te <- tryCatch(sfaR::efficiencies(m)$teJLMS, error = function(e) NULL)
  if (is.null(te)) next

  is_A <- grepl("^A", nm)
  df_te <- if (is_A) {
    df_sfa %>% filter(
      es_agudo == 1, !anyo %in% 2020:2022, altTotal_bruto >= 200,
      is.finite(ln_altTotal_pond), is.finite(ln_i_diag),
      !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
      !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
      !is.na(grupo_pago), !is.na(ccaa_cod))
  } else {
    df_sfa %>% filter(
      es_agudo == 1, !anyo %in% 2020:2022,
      altQ_bruto >= 200, altM_bruto >= 200,
      is.finite(ln_altQ_pond), is.finite(ln_altM_pond),
      !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
      !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
      !is.na(grupo_pago), !is.na(ccaa_cod))
  }

  if (length(te) != nrow(df_te)) {
    cat(sprintf("\n[%s] — longitud TE (%d) != muestra (%d), omitido\n",
        nm, length(te), nrow(df_te)))
    next
  }
  df_te$TE <- te
  cat(sprintf("\n[%s]\n", nm))
  res <- df_te %>%
    group_by(D_desc, grupo_cluster) %>%
    summarise(n = n(),
              TE_media = round(mean(TE, na.rm=TRUE), 3),
              TE_sd    = round(sd(TE,   na.rm=TRUE), 3),
              .groups = "drop") %>%
    arrange(D_desc, grupo_cluster)
  print(as.data.frame(res))
}

# SECCION 5: Tabla P1-P4
cat("\n")
cat("=================================================================\n")
cat("  TABLA RESUMEN - PREDICCIONES DEL MODELO TEORICO\n")
cat("=================================================================\n")
cat("\nReferencia: Pub_Retro = hospitales publicos SNS\n")
cat("(+) = confirma prediccion  (-) = no confirma\n\n")
cat(sprintf("%-8s %-30s %-20s %-20s %-8s\n",
    "Pred.","Descripcion","Diseno A (D_desc)","Diseno B","Veredicto"))
cat(paste(rep("-", 88), collapse=""), "\n")

get_c <- function(m, param) {
  if (is.null(m)) return(NA_real_)
  cf <- coef(m); nm_p <- paste0("Zu_", param)
  if (!nm_p %in% names(cf)) return(NA_real_)
  cf[nm_p]
}

a_p1 <- get_c(modelos_todos$A_Tot_Ddesc, "D_desc")
b_p1_c <- get_c(modelos_todos$B_Q, "d_Priv_Conc")
b_p1_m <- get_c(modelos_todos$B_Q, "d_Priv_Merc")
cat(sprintf("%-8s %-30s %-20s %-20s %-8s\n", "P1",
  "Desc -> menor inef. cantidad",
  ifelse(!is.na(a_p1), sprintf("%.3f %s", a_p1, ifelse(a_p1<0,"(+)","(-)")), "n/d"),
  ifelse(!is.na(b_p1_c), sprintf("Conc:%.2f Merc:%.2f", b_p1_c, b_p1_m), "n/d"),
  ifelse(!is.na(a_p1) && a_p1 < 0, "OK", "NO")))

a_p2 <- get_c(modelos_todos$A_I_Ddesc, "D_desc")
b_p2_c <- get_c(modelos_todos$B_M, "d_Priv_Conc")
cat(sprintf("%-8s %-30s %-20s %-20s %-8s\n", "P2",
  "Desc -> mayor inef. intensidad",
  ifelse(!is.na(a_p2), sprintf("%.3f %s", a_p2, ifelse(a_p2>0,"(+)","(-)")), "n/d"),
  ifelse(!is.na(b_p2_c), sprintf("Conc:%.2f", b_p2_c), "n/d"),
  ifelse(!is.na(a_p2) && a_p2 > 0, "OK", "NO")))

a_p3_t <- get_c(modelos_todos$A_Tot_Ddesc, "pct_sns")
a_p3_i <- get_c(modelos_todos$A_I_Ddesc,   "pct_sns")
b_p3_q <- get_c(modelos_todos$B_Q, "d_Priv_Merc")
b_p3_m <- get_c(modelos_todos$B_M, "d_Priv_Merc")
cat(sprintf("%-8s %-30s %-20s %-20s %-8s\n", "P3",
  "Pago difiere entre dims.",
  ifelse(!is.na(a_p3_t)&&!is.na(a_p3_i),
    sprintf("T:%.2f I:%.2f", a_p3_t, a_p3_i), "n/d"),
  ifelse(!is.na(b_p3_q)&&!is.na(b_p3_m),
    sprintf("Q:%.2f M:%.2f", b_p3_q, b_p3_m), "n/d"),
  ifelse(!is.na(a_p3_t)&&!is.na(a_p3_i)&&sign(a_p3_t)!=sign(a_p3_i), "OK","NO")))

a_p4_t <- get_c(modelos_todos$A_Tot_Ddesc,"ShareQ")
a_p4_i <- get_c(modelos_todos$A_I_Ddesc,  "ShareQ")
b_p4_q <- get_c(modelos_todos$B_Q,"ShareQ")
b_p4_m <- get_c(modelos_todos$B_M,"ShareQ")
cat(sprintf("%-8s %-30s %-20s %-20s %-8s\n", "P4",
  "ShareQ modera asimetr.",
  ifelse(!is.na(a_p4_t)&&!is.na(a_p4_i),
    sprintf("T:%.2f I:%.2f %s", a_p4_t, a_p4_i,
      ifelse(sign(a_p4_t)!=sign(a_p4_i),"(+)","(-)")), "n/d"),
  ifelse(!is.na(b_p4_q)&&!is.na(b_p4_m),
    sprintf("Q:%.2f M:%.2f %s", b_p4_q, b_p4_m,
      ifelse(sign(b_p4_q)!=sign(b_p4_m),"(+)","(-)")), "n/d"),
  ifelse((!is.na(a_p4_t)&&!is.na(a_p4_i)&&sign(a_p4_t)!=sign(a_p4_i))||
         (!is.na(b_p4_q)&&!is.na(b_p4_m)&&sign(b_p4_q)!=sign(b_p4_m)), "OK","NO")))

cat("\n=== Salida completa finalizada ===\n")
