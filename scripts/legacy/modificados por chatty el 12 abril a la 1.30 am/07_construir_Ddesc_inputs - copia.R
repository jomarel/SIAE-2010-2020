## =============================================================
## Phase 3 — D_desc, Payment, Inputs (L, K), Log-centered,
##           Trend, Clusters, Interactions, CCAA, es_agudo, grupo_pago
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_final.RData"))
cat("Loaded df_final:", nrow(df_final), "x", ncol(df_final), "\n")

to_num <- function(x) suppressWarnings(as.numeric(x))
df <- df_final

## --- A. D_desc ---
cat("\n========== A. D_desc ==========\n")
df$D_desc <- dplyr::case_when(
  to_num(df$cod_depend_agrupada) == 1 ~ 0L,   # public SNS
  to_num(df$cod_depend_agrupada) == 2 ~ 1L,   # private/decentralised
  TRUE ~ NA_integer_
)

ddesc_yr <- df %>%
  group_by(anyo) %>%
  summarise(
    n = n(),
    pct_D1 = round(100 * sum(D_desc == 1, na.rm = TRUE) / n(), 1),
    n_na = sum(is.na(D_desc)),
    .groups = "drop"
  )
cat("D_desc by year (% private, target 40-47%):\n")
print(as.data.frame(ddesc_yr))

## --- B. Payment ---
cat("\n========== B. Payment ==========\n")
denom <- to_num(df$altTotal_bruto)
denom[!is.na(denom) & denom <= 0] <- NA_real_

df$pct_sns <- pmin(1, pmax(0, to_num(df$altas_sns) / denom, na.rm = FALSE))
cat("pct_sns: mean =", round(mean(df$pct_sns, na.rm = TRUE), 3),
    "| NA% =", round(100 * mean(is.na(df$pct_sns)), 1), "\n")

## --- C. Labor inputs (NO _colab variables) ---
cat("\n========== C. Labor inputs ==========\n")
safe0 <- function(x) { x <- to_num(x); x[is.na(x)] <- 0; x }

total_cMed <- to_num(df$total_cMedicos)
total_pMed <- safe0(df$total_pMedicos)
due_cT     <- to_num(df$due_cTotal)
due_pT     <- safe0(df$due_pTotal)
subMed_cT  <- to_num(df$subMedicas_cTotal)
subMed_pT  <- safe0(df$subMedicas_pTotal)
subQui_cT  <- to_num(df$subQuirurgicas_cTotal)
subQui_pT  <- safe0(df$subQuirurgicas_pTotal)

df$L_total  <- (total_cMed + 0.5 * total_pMed) + (due_cT + 0.5 * due_pT)
df$L_quirur <- subQui_cT + 0.5 * subQui_pT
df$L_medico <- subMed_cT + 0.5 * subMed_pT

# Set NA where both components are NA
all_na_L <- is.na(to_num(df$total_cMedicos)) & is.na(to_num(df$due_cTotal))
df$L_total[all_na_L]  <- NA_real_
df$L_medico[is.na(to_num(df$subMedicas_cTotal))] <- NA_real_
df$L_quirur[is.na(to_num(df$subQuirurgicas_cTotal))] <- NA_real_

cat("L_total:  mean =", round(mean(df$L_total, na.rm = TRUE), 1),
    "| NA% =", round(100 * mean(is.na(df$L_total)), 1), "\n")

## --- D. Capital inputs (thesis weights) ---
cat("\n========== D. Capital inputs (thesis weights) ==========\n")
df$K_camas <- to_num(df$camas_funcionamiento)

TAC  <- safe0(df$TAC_hospital)
RNM  <- safe0(df$RNM_hospital)
PET  <- safe0(df$PET_hospital)
acel <- safe0(df$acelerador_hospital)
angi <- safe0(df$angiografo_hospital)
gam  <- safe0(df$gammacamara_hospital)
spe  <- safe0(df$spect_hospital)

df$K_tech_index <- TAC * 1.0 + RNM * 2.5 + PET * 5.0 +
                   acel * 4.0 + angi * 1.5 + gam * 1.8 + spe * 1.8

# Set NA where ALL tech components are NA in original
all_tech_na <- is.na(to_num(df$TAC_hospital)) &
               is.na(to_num(df$RNM_hospital)) &
               is.na(to_num(df$PET_hospital))
df$K_tech_index[all_tech_na] <- NA_real_

cat("K_camas:      mean =", round(mean(df$K_camas, na.rm = TRUE), 1),
    "| NA% =", round(100 * mean(is.na(df$K_camas)), 1), "\n")
cat("K_tech_index: mean =", round(mean(df$K_tech_index, na.rm = TRUE), 1),
    "| NA% =", round(100 * mean(is.na(df$K_tech_index)), 1), "\n")

## --- E. Log-centered inputs ---
cat("\n========== E. Log-centered inputs ==========\n")

df$ln_L_total  <- log(pmax(df$L_total, 0, na.rm = FALSE) + 1)
df$ln_K_camas  <- log(pmax(df$K_camas, 0, na.rm = FALSE) + 1)
df$ln_K_tech   <- log(pmax(df$K_tech_index, 0, na.rm = FALSE) + 1)

# Centered at sample mean
mu_L <- mean(df$ln_L_total, na.rm = TRUE)
mu_K <- mean(df$ln_K_camas, na.rm = TRUE)
mu_T <- mean(df$ln_K_tech, na.rm = TRUE)

df$ln_L_total_c  <- df$ln_L_total - mu_L
df$ln_K_camas_c  <- df$ln_K_camas - mu_K
df$ln_K_tech_c   <- df$ln_K_tech - mu_T

df$ln_L_total_c2 <- 0.5 * df$ln_L_total_c^2
df$ln_K_camas_c2 <- 0.5 * df$ln_K_camas_c^2
# NO ln_LK_c (dropped due to collinearity)

cat(sprintf("Centers: ln_L=%.4f, ln_K_camas=%.4f, ln_K_tech=%.4f\n",
            mu_L, mu_K, mu_T))

## --- F. Trend ---
cat("\n========== F. Trend ==========\n")
df$trend  <- as.integer(df$anyo) - 2010L
df$trend2 <- 0.5 * df$trend^2
cat("trend range:", range(df$trend), "\n")

## --- G. Interactions ---
cat("\n========== G. Interactions ==========\n")
df$desc_pago   <- df$D_desc * df$pct_sns
df$desc_shareQ <- df$D_desc * df$ShareQ
cat("desc_pago:   NA% =", round(100 * mean(is.na(df$desc_pago)), 1), "\n")
cat("desc_shareQ: NA% =", round(100 * mean(is.na(df$desc_shareQ)), 1), "\n")

## --- H. CCAA code ---
cat("\n========== H. CCAA code ==========\n")
cat("ccaa_codigo column present:", "ccaa_codigo" %in% names(df), "\n")
cat("ccaa_cnh column present:", "ccaa_cnh" %in% names(df), "\n")

# Verify non-NA
if ("ccaa_codigo" %in% names(df)) {
  ccaa_tab <- table(df$anyo, is.na(df$ccaa_codigo))
  cat("ccaa_codigo non-NA by year (FALSE=non-NA):\n")
  print(ccaa_tab)
  cat("\nUnique ccaa_codigo values:", sort(unique(to_num(df$ccaa_codigo))), "\n")
}

## --- I. es_agudo ---
cat("\n========== I. es_agudo ==========\n")
if ("cod_finalidad_agrupada" %in% names(df)) {
  df$es_agudo <- ifelse(to_num(df$cod_finalidad_agrupada) == 1, 1L, 0L)
} else if ("finalidad_cnh" %in% names(df)) {
  df$es_agudo <- ifelse(toupper(trimws(df$finalidad_cnh)) == "AGUDOS", 1L, 0L)
} else {
  df$es_agudo <- NA_integer_
}
cat("es_agudo: n_agudo =", sum(df$es_agudo == 1, na.rm = TRUE),
    "| n_other =", sum(df$es_agudo == 0, na.rm = TRUE),
    "| NA =", sum(is.na(df$es_agudo)), "\n")

## --- J. Hospital cluster (5 groups) ---
cat("\n========== J. Hospital cluster ==========\n")
hosp_stats <- df %>%
  group_by(NCODI) %>%
  summarise(
    med_camas = median(K_camas, na.rm = TRUE),
    med_tech  = median(K_tech_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    grupo_cluster = case_when(
      med_camas < 100 & med_tech <= 1 ~ 1L,
      med_camas < 200 & med_tech <= 2 ~ 2L,
      med_camas < 400 & med_tech <= 4 ~ 3L,
      med_camas < 700                 ~ 4L,
      TRUE                            ~ 5L
    )
  )
df <- left_join(df, hosp_stats %>% select(NCODI, grupo_cluster), by = "NCODI")
for (g in 2:5) df[[paste0("d_cluster", g)]] <- as.integer(!is.na(df$grupo_cluster) & df$grupo_cluster == g)

cat("Cluster distribution:\n")
print(table(df$grupo_cluster, useNA = "always"))

## --- K. grupo_pago (Design B) ---
cat("\n========== K. grupo_pago ==========\n")
df$grupo_pago <- case_when(
  df$D_desc == 0                      ~ "Pub_Retro",
  df$D_desc == 1 & df$pct_sns >= 0.50 ~ "Priv_Conc",
  df$D_desc == 1 & df$pct_sns <  0.50 ~ "Priv_Merc",
  TRUE ~ NA_character_
)
df$d_Priv_Conc  <- as.integer(!is.na(df$grupo_pago) & df$grupo_pago == "Priv_Conc")
df$d_Priv_Merc  <- as.integer(!is.na(df$grupo_pago) & df$grupo_pago == "Priv_Merc")
df$Conc_shareQ  <- df$d_Priv_Conc * df$ShareQ
df$Merc_shareQ  <- df$d_Priv_Merc * df$ShareQ
cat("grupo_pago:\n")
print(table(df$grupo_pago, useNA = "always"))

## --- L. Update df_final and save ---
cat("\n========== Saving ==========\n")
VARS_NEW <- c("D_desc", "pct_sns", "L_total", "L_quirur", "L_medico",
              "K_camas", "K_tech_index",
              "ln_L_total", "ln_K_camas", "ln_K_tech",
              "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
              "ln_L_total_c2", "ln_K_camas_c2",
              "trend", "trend2",
              "desc_pago", "desc_shareQ",
              "es_agudo", "grupo_cluster",
              "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5",
              "grupo_pago", "d_Priv_Conc", "d_Priv_Merc",
              "Conc_shareQ", "Merc_shareQ")

for (v in VARS_NEW) {
  if (v %in% names(df)) df_final[[v]] <- df[[v]]
}

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))
cat("Phase 3 complete. Saved", nrow(df_final), "x", ncol(df_final), "\n")

# Variable coverage report
cat("\nVariable coverage (% NA):\n")
for (v in VARS_NEW) {
  if (v %in% names(df_final)) {
    pna <- round(100 * mean(is.na(df_final[[v]])), 1)
    cat(sprintf("  %-18s %5.1f%%\n", v, pna))
  }
}
