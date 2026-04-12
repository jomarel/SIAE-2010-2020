## =============================================================
## Phase 3 — D_desc, Payment, Inputs (L, K), Log-centered,
##           Trend, Clusters, Interactions, CCAA, es_agudo, grupo_pago
## v5:
##   - integrates the old 07b fixes directly
##   - rebuilds es_agudo and ccaa_codigo before using them
##   - avoids left_join collision on grupo_cluster / d_cluster*
##   - adds K_rx_salas (module 3) to technology indexes
##   - adds K_quirofanos from module 3 for Design B/C
##   - stores K_hemo_salas and K_tech_diag_hemo for robustness
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_final.RData"))
cat("Loaded df_final:", nrow(df_final), "x", ncol(df_final), "\n")

to_num <- function(x) suppressWarnings(as.numeric(x))
safe0  <- function(x) { x <- to_num(x); x[is.na(x)] <- 0; x }

get_num_col <- function(data, candidates) {
  for (nm in candidates) {
    if (nm %in% names(data)) return(to_num(data[[nm]]))
  }
  rep(NA_real_, nrow(data))
}

df <- df_final
yr <- to_num(df$anyo)
pre21  <- !is.na(yr) & yr <= 2020
post21 <- !is.na(yr) & yr >= 2021

## =============================================================
## 0. Integrate old 07b fixes here
## =============================================================
cat("\n========== 0. Integrated fixes: finalidad + ccaa ==========" , "\n")

if ("Cód_Grupo_Finalidad" %in% names(df)) {
  xml_fin <- to_num(df[["Cód_Grupo_Finalidad"]])
  siae_fin <- if ("cod_finalidad_agrupada" %in% names(df)) to_num(df$cod_finalidad_agrupada) else rep(NA_real_, nrow(df))
  mask_fin <- is.na(siae_fin) & !is.na(xml_fin)
  cat("Filling cod_finalidad_agrupada from Cód_Grupo_Finalidad:", sum(mask_fin), "values\n")
  if (!("cod_finalidad_agrupada" %in% names(df))) df$cod_finalidad_agrupada <- NA_real_
  df$cod_finalidad_agrupada[mask_fin] <- xml_fin[mask_fin]
}

if ("Cód_CCAA_Todas" %in% names(df)) {
  xml_ccaa <- to_num(df[["Cód_CCAA_Todas"]])
  siae_ccaa <- if ("ccaa_codigo" %in% names(df)) to_num(df$ccaa_codigo) else rep(NA_real_, nrow(df))
  mask_ccaa <- is.na(siae_ccaa) & !is.na(xml_ccaa)
  cat("Filling ccaa_codigo from Cód_CCAA_Todas:", sum(mask_ccaa), "values\n")
  if (!("ccaa_codigo" %in% names(df))) df$ccaa_codigo <- NA_real_
  df$ccaa_codigo[mask_ccaa] <- xml_ccaa[mask_ccaa]
}

## =============================================================
## A. D_desc
## =============================================================
cat("\n========== A. D_desc ==========\n")
df$D_desc <- dplyr::case_when(
  to_num(df$cod_depend_agrupada) == 1 ~ 0L,
  to_num(df$cod_depend_agrupada) == 2 ~ 1L,
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

## =============================================================
## B. Payment
## =============================================================
cat("\n========== B. Payment ==========\n")
denom <- to_num(df$altTotal_bruto)
denom[!is.na(denom) & denom <= 0] <- NA_real_

pct_sns_raw <- to_num(df$altas_sns) / denom
df$pct_sns <- pmin(1, pmax(0, pct_sns_raw))
cat("pct_sns: mean =", round(mean(df$pct_sns, na.rm = TRUE), 3),
    "| NA% =", round(100 * mean(is.na(df$pct_sns)), 1), "\n")

## =============================================================
## C. Labor inputs
## =============================================================
cat("\n========== C. Labor inputs ==========\n")

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

all_na_L <- is.na(to_num(df$total_cMedicos)) & is.na(to_num(df$due_cTotal))
df$L_total[all_na_L] <- NA_real_
df$L_medico[is.na(to_num(df$subMedicas_cTotal))] <- NA_real_
df$L_quirur[is.na(to_num(df$subQuirurgicas_cTotal))] <- NA_real_

cat("L_total: mean =", round(mean(df$L_total, na.rm = TRUE), 1),
    "| NA% =", round(100 * mean(is.na(df$L_total)), 1), "\n")

## =============================================================
## D. Capital inputs (general and diagnostic technology)
## =============================================================
cat("\n========== D. Capital inputs (technology bridged) ==========\n")

df$K_camas <- to_num(df$camas_funcionamiento)

# Module 3: operating rooms in use (continuous series across years)
df$K_quirofanos <- get_num_col(df, c(
  "quirofanos_funcionamiento",
  "quirofanos_Funcionamiento",
  "quirofanos funcionamiento"
))


# ----- module 4 technology: <=2020 classic names; >=2021 Tot* names -----
bridge_stock <- function(pre_hosp, pre_cep = NULL, post_tot = NULL) {
  hosp <- get_num_col(df, pre_hosp)
  cep  <- if (!is.null(pre_cep)) get_num_col(df, pre_cep) else rep(NA_real_, nrow(df))
  tot  <- if (!is.null(post_tot)) get_num_col(df, post_tot) else rep(NA_real_, nrow(df))

  out <- rep(NA_real_, nrow(df))

  pre_sum <- safe0(hosp) + safe0(cep)
  pre_all_na <- is.na(hosp) & is.na(cep)
  out[pre21] <- pre_sum[pre21]
  out[pre21 & pre_all_na] <- NA_real_

  out[post21] <- tot[post21]
  out
}

# Main stocks from module 4
TAC_total         <- bridge_stock(c("TAC_hospital"),          c("TAC_CEP"),          c("Tottac"))
RNM_total         <- bridge_stock(c("RNM_hospital"),          c("RNM_CEP"),          c("Totrnm"))
PET_total         <- bridge_stock(c("PET_hospital"),          c("PET_CEP"),          c("Totpet"))
acelerador_total  <- bridge_stock(c("acelerador_hospital"),   c("acelerador_CEP"),   c("Totacelera"))
angiografo_total  <- bridge_stock(c("angiografo_hospital"),   c("angiografo_CEP"),   c("Totangio"))
gammacamara_total <- bridge_stock(c("gammacamara_hospital"),  c("gammacamara_CEP"),  c("Totgammaca"))
spect_total       <- bridge_stock(c("spect_hospital"),        c("spect_CEP"),        c("Totspect"))
mamografos_total  <- bridge_stock(c("mamografos_hospital"),   c("mamografos_CEP"),   c("Totmamos"))
densiometros_total<- bridge_stock(c("densiometros_hospital"), c("densiometros_CEP"), c("Totdensi"))
litotriptor_total <- bridge_stock(c("litotriptor_hospital"),  c("litotriptor_CEP"),  c("Totlito"))
hemodialisis_total<- bridge_stock(c("hemodialisis_hospital"), c("hemodialisis_CEP"), c("Tothemodial"))
bombas_total      <- bridge_stock(c("bombas_hospital"),       c("bombas_CEP"),       c("Totbombas"))

# ----- module 3 extra stocks: salas RX and salas Hemo -----
rx_hosp <- get_num_col(df, c("salas_rx_Hospital", "salas_rx_hospital"))
rx_cep  <- get_num_col(df, c("salas_rx_CEP", "salas_rx_cep"))
rx_tot  <- get_num_col(df, c("Totalsalas_rx", "totalsalas_rx"))

df$K_rx_salas <- NA_real_
df$K_rx_salas[pre21] <- safe0(rx_hosp[pre21]) + safe0(rx_cep[pre21])
df$K_rx_salas[post21] <- rx_tot[post21]
all_rx_na_pre  <- is.na(rx_hosp) & is.na(rx_cep)
all_rx_na_post <- is.na(rx_tot)
df$K_rx_salas[pre21 & all_rx_na_pre] <- NA_real_
df$K_rx_salas[post21 & all_rx_na_post] <- NA_real_

hemo_fun <- get_num_col(df, c("salasHemo_funcionamiento", "salashemo_funcionamiento"))
df$K_hemo_salas <- hemo_fun

# Expose bridged components
components_to_store <- list(
  TAC_total_tech = TAC_total,
  RNM_total_tech = RNM_total,
  PET_total_tech = PET_total,
  acelerador_total_tech = acelerador_total,
  angiografo_total_tech = angiografo_total,
  gammacamara_total_tech = gammacamara_total,
  spect_total_tech = spect_total,
  mamografos_total_tech = mamografos_total,
  densiometros_total_tech = densiometros_total,
  litotriptor_total_tech = litotriptor_total,
  hemodialisis_total_tech = hemodialisis_total,
  bombas_total_tech = bombas_total
)
for (nm in names(components_to_store)) df[[nm]] <- components_to_store[[nm]]

# General technology index (keeps old name for compatibility with script 10)
df$K_tech_index <-
  1.0 * df$TAC_total_tech +
  2.5 * df$RNM_total_tech +
  5.0 * df$PET_total_tech +
  4.0 * df$acelerador_total_tech +
  1.5 * df$angiografo_total_tech +
  1.8 * df$gammacamara_total_tech +
  1.8 * df$spect_total_tech +
  0.8 * df$mamografos_total_tech +
  0.5 * df$densiometros_total_tech +
  0.2 * df$K_rx_salas

# Diagnostic technology index (for intensity frontier)
df$K_tech_diag <-
  1.0 * df$TAC_total_tech +
  2.5 * df$RNM_total_tech +
  5.0 * df$PET_total_tech +
  1.5 * df$angiografo_total_tech +
  1.8 * df$gammacamara_total_tech +
  1.8 * df$spect_total_tech +
  0.3 * df$mamografos_total_tech +
  0.2 * df$densiometros_total_tech +
  0.2 * df$K_rx_salas

# Optional robustness including hemo rooms
df$K_tech_diag_hemo <- df$K_tech_diag + 1.0 * safe0(df$K_hemo_salas)

# Set NA only if all main diagnostic stocks are NA
main_diag_mat <- cbind(
  df$TAC_total_tech, df$RNM_total_tech, df$PET_total_tech,
  df$angiografo_total_tech, df$gammacamara_total_tech, df$spect_total_tech,
  df$mamografos_total_tech, df$densiometros_total_tech, df$K_rx_salas
)
all_diag_na <- apply(is.na(main_diag_mat), 1, all)
df$K_tech_diag[all_diag_na] <- NA_real_
df$K_tech_diag_hemo[all_diag_na & is.na(df$K_hemo_salas)] <- NA_real_

main_gen_mat <- cbind(
  df$TAC_total_tech, df$RNM_total_tech, df$PET_total_tech,
  df$acelerador_total_tech, df$angiografo_total_tech,
  df$gammacamara_total_tech, df$spect_total_tech,
  df$mamografos_total_tech, df$densiometros_total_tech, df$K_rx_salas
)
all_gen_na <- apply(is.na(main_gen_mat), 1, all)
df$K_tech_index[all_gen_na] <- NA_real_

cat("K_camas:      mean =", round(mean(df$K_camas, na.rm = TRUE), 1),
    "| NA% =", round(100 * mean(is.na(df$K_camas)), 1), "\n")
cat("K_quirofanos: mean =", round(mean(df$K_quirofanos, na.rm = TRUE), 2),
    "| NA% =", round(100 * mean(is.na(df$K_quirofanos)), 1), "\n")
cat("K_rx_salas:   mean =", round(mean(df$K_rx_salas, na.rm = TRUE), 2),
    "| NA% =", round(100 * mean(is.na(df$K_rx_salas)), 1), "\n")
cat("K_hemo_salas: mean =", round(mean(df$K_hemo_salas, na.rm = TRUE), 2),
    "| NA% =", round(100 * mean(is.na(df$K_hemo_salas)), 1), "\n")
cat("K_tech_index: mean =", round(mean(df$K_tech_index, na.rm = TRUE), 2),
    "| NA% =", round(100 * mean(is.na(df$K_tech_index)), 1), "\n")
cat("K_tech_diag:  mean =", round(mean(df$K_tech_diag, na.rm = TRUE), 2),
    "| NA% =", round(100 * mean(is.na(df$K_tech_diag)), 1), "\n")

## =============================================================
## E. Log-centered inputs
## =============================================================
cat("\n========== E. Log-centered inputs ==========\n")

df$ln_L_total   <- ifelse(!is.na(df$L_total),      log(df$L_total + 1), NA_real_)
df$ln_K_camas   <- ifelse(!is.na(df$K_camas),      log(df$K_camas + 1), NA_real_)
df$ln_K_tech    <- ifelse(!is.na(df$K_tech_index), log(df$K_tech_index + 1), NA_real_)
df$ln_K_tech_diag <- ifelse(!is.na(df$K_tech_diag), log(df$K_tech_diag + 1), NA_real_)

mu_L  <- mean(df$ln_L_total, na.rm = TRUE)
mu_K  <- mean(df$ln_K_camas, na.rm = TRUE)
mu_T  <- mean(df$ln_K_tech, na.rm = TRUE)
mu_Td <- mean(df$ln_K_tech_diag, na.rm = TRUE)

df$ln_L_total_c   <- df$ln_L_total - mu_L
df$ln_K_camas_c   <- df$ln_K_camas - mu_K
df$ln_K_tech_c    <- df$ln_K_tech - mu_T
df$ln_K_tech_diag_c <- df$ln_K_tech_diag - mu_Td

df$ln_L_total_c2  <- 0.5 * df$ln_L_total_c^2
df$ln_K_camas_c2  <- 0.5 * df$ln_K_camas_c^2
cat(sprintf("Centers: ln_L=%.4f, ln_K_camas=%.4f, ln_K_tech=%.4f, ln_K_tech_diag=%.4f\n",
            mu_L, mu_K, mu_T, mu_Td))

df$ln_K_tech_diag_c2 <- 0.5 * df$ln_K_tech_diag_c^2
# Y añadir a VARS_NEW en la sección L:
# "ln_K_tech_diag_c2"

## =============================================================
## F. Trend
## =============================================================
cat("\n========== F. Trend ==========\n")
df$trend  <- as.integer(df$anyo) - 2010L
df$trend2 <- 0.5 * df$trend^2
cat("trend range:", range(df$trend, na.rm = TRUE), "\n")

## =============================================================
## G. Interactions
## =============================================================
cat("\n========== G. Interactions ==========\n")
df$desc_pago   <- df$D_desc * df$pct_sns
df$desc_shareQ <- df$D_desc * df$ShareQ
cat("desc_pago:   NA% =", round(100 * mean(is.na(df$desc_pago)), 1), "\n")
cat("desc_shareQ: NA% =", round(100 * mean(is.na(df$desc_shareQ)), 1), "\n")

## =============================================================
## H. CCAA code
## =============================================================
cat("\n========== H. CCAA code ==========\n")
cat("ccaa_codigo column present:", "ccaa_codigo" %in% names(df), "\n")
cat("ccaa_cnh column present:", "ccaa_cnh" %in% names(df), "\n")
if ("ccaa_codigo" %in% names(df)) {
  ccaa_tab <- table(df$anyo, is.na(df$ccaa_codigo))
  cat("ccaa_codigo non-NA by year (FALSE = non-NA):\n")
  print(ccaa_tab)
  cat("\nUnique ccaa_codigo values:", sort(unique(to_num(df$ccaa_codigo))), "\n")
}

## =============================================================
## I. es_agudo
## =============================================================
cat("\n========== I. es_agudo ==========\n")
df$es_agudo <- case_when(
  to_num(df$cod_finalidad_agrupada) == 1 ~ 1L,
  !is.na(to_num(df$cod_finalidad_agrupada)) ~ 0L,
  toupper(trimws(as.character(df$finalidad_cnh))) == "AGUDOS" ~ 1L,
  !is.na(df$finalidad_cnh) & nchar(trimws(df$finalidad_cnh)) > 0 ~ 0L,
  TRUE ~ NA_integer_
)
cat("es_agudo: n_agudo =", sum(df$es_agudo == 1, na.rm = TRUE),
    "| n_other =", sum(df$es_agudo == 0, na.rm = TRUE),
    "| NA =", sum(is.na(df$es_agudo)), "\n")

## =============================================================
## J. Hospital cluster (5 groups)
## =============================================================
cat("\n========== J. Hospital cluster ==========\n")

# Remove previous derived columns if they exist, to avoid left_join suffix collisions
old_cluster_vars <- c("grupo_cluster", paste0("d_cluster", 2:5))
for (v in old_cluster_vars) {
  if (v %in% names(df)) df[[v]] <- NULL
}

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

# Safer than left_join for reruns: direct match by NCODI
match_idx <- match(df$NCODI, hosp_stats$NCODI)
df$grupo_cluster <- hosp_stats$grupo_cluster[match_idx]

for (g in 2:5) {
  df[[paste0("d_cluster", g)]] <- as.integer(!is.na(df$grupo_cluster) & df$grupo_cluster == g)
}

cat("Cluster distribution:\n")
print(table(df$grupo_cluster, useNA = "always"))

## =============================================================
## K. grupo_pago
## =============================================================
cat("\n========== K. grupo_pago ==========\n")
df$grupo_pago <- case_when(
  df$D_desc == 0                      ~ "Pub_Retro",
  df$D_desc == 1 & df$pct_sns >= 0.50 ~ "Priv_Conc",
  df$D_desc == 1 & df$pct_sns <  0.50 ~ "Priv_Merc",
  TRUE ~ NA_character_
)
df$d_Priv_Conc <- as.integer(!is.na(df$grupo_pago) & df$grupo_pago == "Priv_Conc")
df$d_Priv_Merc <- as.integer(!is.na(df$grupo_pago) & df$grupo_pago == "Priv_Merc")
df$Conc_shareQ <- df$d_Priv_Conc * df$ShareQ
df$Merc_shareQ <- df$d_Priv_Merc * df$ShareQ
cat("grupo_pago:\n")
print(table(df$grupo_pago, useNA = "always"))

## =============================================================
## L. Save back to df_final
## =============================================================
cat("\n========== Saving ==========\n")

VARS_NEW <- c(
  "cod_finalidad_agrupada", "ccaa_codigo",
  "D_desc", "pct_sns",
  "L_total", "L_quirur", "L_medico",
  "K_camas", "K_quirofanos", "K_rx_salas", "K_hemo_salas",
  "TAC_total_tech", "RNM_total_tech", "PET_total_tech",
  "acelerador_total_tech", "angiografo_total_tech",
  "gammacamara_total_tech", "spect_total_tech",
  "mamografos_total_tech", "densiometros_total_tech",
  "litotriptor_total_tech", "hemodialisis_total_tech", "bombas_total_tech",
  "K_tech_index", "K_tech_diag", "K_tech_diag_hemo",
  "ln_L_total", "ln_K_camas", "ln_K_tech", "ln_K_tech_diag",
  "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c", "ln_K_tech_diag_c",
  "ln_L_total_c2", "ln_K_camas_c2",
  "trend", "trend2",
  "desc_pago", "desc_shareQ",
  "es_agudo", "grupo_cluster",
  "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5",
  "grupo_pago", "d_Priv_Conc", "d_Priv_Merc",
  "Conc_shareQ", "Merc_shareQ"
)

for (v in VARS_NEW) {
  if (v %in% names(df)) df_final[[v]] <- df[[v]]
}

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))
cat("Phase 3 complete. Saved", nrow(df_final), "x", ncol(df_final), "\n")

cat("\nVariable coverage (% NA):\n")
for (v in VARS_NEW) {
  if (v %in% names(df_final)) {
    pna <- round(100 * mean(is.na(df_final[[v]])), 1)
    cat(sprintf("  %-24s %5.1f%%\n", v, pna))
  }
}
