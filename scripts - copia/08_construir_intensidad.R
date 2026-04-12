## =============================================================
## Phase 4 — Build intensity index i_diag from C1_16
## Bridge: <=2020 hosp+CEP, >=2021 tot*
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)
CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_final.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))
safe0 <- function(x) { x <- to_num(x); x[is.na(x)] <- 0; x }

cat("========== Phase 4: Intensity index i_diag ==========\n")
cat("df_final:", nrow(df_final), "x", ncol(df_final), "\n")

df <- df_final
yr <- df$anyo
pre21 <- yr <= 2020
post21 <- yr >= 2021

## --- Build bridged procedure counts ---
# Each procedure: for <=2020 sum hosp+CEP, for >=2021 use tot*

bridge <- function(hosp_col, cep_col, tot_col) {
  hosp <- safe0(df[[hosp_col]])
  cep  <- if (!is.null(cep_col) && cep_col %in% names(df)) safe0(df[[cep_col]]) else rep(0, nrow(df))
  tot  <- if (!is.null(tot_col) && tot_col %in% names(df)) safe0(df[[tot_col]]) else rep(0, nrow(df))

  result <- rep(NA_real_, nrow(df))
  result[pre21]  <- hosp[pre21] + cep[pre21]
  result[post21] <- tot[post21]
  result
}

# Endoscopies have different structure: single var pre-2021, hosp+Amb post-2021
bridge_endo <- function(single_col, hosp_col, amb_col) {
  single <- safe0(df[[single_col]])
  hosp   <- if (hosp_col %in% names(df)) safe0(df[[hosp_col]]) else rep(0, nrow(df))
  amb    <- if (amb_col %in% names(df)) safe0(df[[amb_col]]) else rep(0, nrow(df))

  result <- rep(NA_real_, nrow(df))
  result[pre21]  <- single[pre21]
  result[post21] <- hosp[post21] + amb[post21]
  result
}

proc_biopsias     <- bridge("biopsias_hosp",     "biopsias_CEP",     "totbiopsias")
proc_tac          <- bridge("tac_hosp",          "tac_CEP",          "tottac")
proc_resonancia   <- bridge("resonancia_hosp",   "resonancia_CEP",   "totresonancia")
proc_pet          <- bridge("pet_hosp",          "pet_CEP",          "totpet")
proc_rx           <- bridge("rx_hosp",           "rx_CEP",           "totrx")
proc_spect        <- bridge("spect_hosp",        "spect_CEP",        "totspect")
proc_angio        <- bridge("angio_hosp",        "angio_CEP",        "totangio")
proc_gamma        <- bridge("gamma_hosp",        "gamma_CEP",        "totgamma")
proc_mamo         <- bridge("mamo_hosp",         "mamo_CEP",         "totmamo")
proc_densiom      <- bridge("densiometrias_hosp","densiometrias_CEP","totdensiometrias")

proc_colonoscopia <- bridge_endo("Colonosopia", "Col_hosp", "Col_Amb")
proc_broncoscopia <- bridge_endo("Broncoscopia","Bron_hosp","Bron_Amb")
proc_ercp         <- bridge_endo("ERCP",        "ERCP_hosp","ERCP_Amb")

## --- Weighted sum ---
# Weights from thesis specification
w_tac   <- 1.0
w_rnm   <- 2.0
w_pet   <- 5.0
w_angio <- 2.5
w_gamma <- 2.0
w_spect <- 2.0
w_colon <- 0.8
w_bronco<- 0.7
w_ercp  <- 1.5
w_biop  <- 0.5
w_rx    <- 0.2
w_mamo  <- 0.3
w_densi <- 0.2

weighted_procs <- w_tac * proc_tac +
                  w_rnm * proc_resonancia +
                  w_pet * proc_pet +
                  w_angio * proc_angio +
                  w_gamma * proc_gamma +
                  w_spect * proc_spect +
                  w_colon * proc_colonoscopia +
                  w_bronco * proc_broncoscopia +
                  w_ercp * proc_ercp +
                  w_biop * proc_biopsias +
                  w_rx * proc_rx +
                  w_mamo * proc_mamo +
                  w_densi * proc_densiom

altTotal_pond <- to_num(df$altTotal_pond)
altTotal_bruto <- to_num(df$altTotal_bruto)

df$i_diag <- ifelse(!is.na(altTotal_pond) & altTotal_pond > 0,
                    weighted_procs / altTotal_pond, NA_real_)

df$ln_i_diag <- ifelse(!is.na(df$i_diag) & df$i_diag > 0,
                       log(df$i_diag), NA_real_)

# i_simple
consultas <- safe0(df$total_consulta)
sesiones  <- safe0(df$total_sesionHdia)
df$i_simple <- ifelse(!is.na(altTotal_bruto) & altTotal_bruto > 0,
                      (consultas + sesiones) / altTotal_bruto, NA_real_)
df$ln_i_simple <- log(df$i_simple + 1)

## --- Validation: median i_diag by year ---
cat("\n=== Median i_diag by year (thesis targets: 7.9-9.5) ===\n")
idiag_yr <- df %>%
  filter(!is.na(i_diag) & i_diag > 0) %>%
  group_by(anyo) %>%
  summarise(
    n = n(),
    median_i = round(median(i_diag, na.rm = TRUE), 2),
    mean_i = round(mean(i_diag, na.rm = TRUE), 2),
    pct_na_total = round(100 * sum(is.na(i_diag)) / n(), 1),
    .groups = "drop"
  )
print(as.data.frame(idiag_yr))

# Check year-over-year jumps
meds <- idiag_yr$median_i
jumps <- diff(meds) / head(meds, -1)
cat("\nYear-over-year jumps (max 15%):\n")
for (i in seq_along(jumps)) {
  flag <- if (abs(jumps[i]) > 0.15) " *** FLAG" else ""
  cat(sprintf("  %d->%d: %+.1f%%%s\n",
              idiag_yr$anyo[i], idiag_yr$anyo[i+1],
              100*jumps[i], flag))
}

# Also store individual proc_ variables for diagnostics
df$proc_biopsias     <- proc_biopsias
df$proc_tac          <- proc_tac
df$proc_resonancia   <- proc_resonancia
df$proc_pet          <- proc_pet
df$proc_rx           <- proc_rx
df$proc_spect        <- proc_spect
df$proc_angio        <- proc_angio
df$proc_gamma        <- proc_gamma
df$proc_mamo         <- proc_mamo
df$proc_densiom      <- proc_densiom
df$proc_colonoscopia <- proc_colonoscopia
df$proc_broncoscopia <- proc_broncoscopia
df$proc_ercp         <- proc_ercp

## --- Save ---
VARS_NEW <- c("i_diag","ln_i_diag","i_simple","ln_i_simple",
              "proc_biopsias","proc_tac","proc_resonancia","proc_pet",
              "proc_rx","proc_spect","proc_angio","proc_gamma",
              "proc_mamo","proc_densiom","proc_colonoscopia",
              "proc_broncoscopia","proc_ercp")

for (v in VARS_NEW) df_final[[v]] <- df[[v]]

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))
cat("\nPhase 4 complete. i_diag NA% =",
    round(100 * mean(is.na(df_final$i_diag)), 1), "\n")
cat("ln_i_diag NA% =",
    round(100 * mean(is.na(df_final$ln_i_diag)), 1), "\n")
