## =============================================================
## Phase 4 — Build intensity index i_diag from C1_16
## v3:
##   - principal series uses the validated bridge
##   - <=2020: hosp + CEP
##   - >=2021: tot*
##   - endoscopies post-2021: hosp + Amb
##   - denominator = altTotal_bruto
##   - raw numerator stored as i_diag_sum
## =============================================================

setwd("G:/Mi unidad/SIAE 2010-2020")
library(dplyr)

CLEAN_DIR <- "data_clean"
load(file.path(CLEAN_DIR, "df_final.RData"))

to_num <- function(x) suppressWarnings(as.numeric(x))
safe0  <- function(x) { x <- to_num(x); x[is.na(x)] <- 0; x }

cat("========== Phase 4: Intensity index i_diag (bridged) ==========\n")
cat("df_final:", nrow(df_final), "x", ncol(df_final), "\n")

df <- df_final
yr <- to_num(df$anyo)
pre21  <- !is.na(yr) & yr <= 2020
post21 <- !is.na(yr) & yr >= 2021

## =============================================================
## Helpers
## =============================================================

get_col <- function(var) {
  if (var %in% names(df)) to_num(df[[var]]) else rep(NA_real_, nrow(df))
}

bridge <- function(hosp_col, cep_col, tot_col) {
  hosp <- get_col(hosp_col)
  cep  <- if (!is.null(cep_col)) get_col(cep_col) else rep(NA_real_, nrow(df))
  tot  <- if (!is.null(tot_col)) get_col(tot_col) else rep(NA_real_, nrow(df))

  res <- rep(NA_real_, nrow(df))

  # <=2020 : hosp + CEP
  pre_sum <- safe0(hosp) + safe0(cep)
  pre_all_na <- is.na(hosp) & is.na(cep)
  res[pre21] <- pre_sum[pre21]
  res[pre21 & pre_all_na] <- NA_real_

  # >=2021 : tot*
  res[post21] <- tot[post21]

  res
}

bridge_endo <- function(single_col, hosp_col, amb_col) {
  single <- get_col(single_col)
  hosp   <- get_col(hosp_col)
  amb    <- get_col(amb_col)

  res <- rep(NA_real_, nrow(df))

  # <=2020 : variable única
  res[pre21] <- single[pre21]

  # >=2021 : hosp + Amb
  post_sum <- safe0(hosp) + safe0(amb)
  post_all_na <- is.na(hosp) & is.na(amb)
  res[post21] <- post_sum[post21]
  res[post21 & post_all_na] <- NA_real_

  res
}

## =============================================================
## Bridged procedure counts
## =============================================================

proc_biopsias     <- bridge("biopsias_hosp",      "biopsias_CEP",      "totbiopsias")
proc_tac          <- bridge("tac_hosp",           "tac_CEP",           "tottac")
proc_resonancia   <- bridge("resonancia_hosp",    "resonancia_CEP",    "totresonancia")
proc_pet          <- bridge("pet_hosp",           "pet_CEP",           "totpet")
proc_rx           <- bridge("rx_hosp",            "rx_CEP",            "totrx")
proc_spect        <- bridge("spect_hosp",         "spect_CEP",         "totspect")
proc_angio        <- bridge("angio_hosp",         "angio_CEP",         "totangio")
proc_gamma        <- bridge("gamma_hosp",         "gamma_CEP",         "totgamma")
proc_mamo         <- bridge("mamo_hosp",          "mamo_CEP",          "totmamo")
proc_densiom      <- bridge("densiometrias_hosp", "densiometrias_CEP", "totdensiometrias")

proc_colonoscopia <- bridge_endo("Colonosopia",  "Col_hosp",  "Col_Amb")
proc_broncoscopia <- bridge_endo("Broncoscopia", "Bron_hosp", "Bron_Amb")
proc_ercp         <- bridge_endo("ERCP",         "ERCP_hosp", "ERCP_Amb")

## =============================================================
## Weights
## =============================================================

w_tac    <- 1.0
w_rnm    <- 2.0
w_pet    <- 5.0
w_angio  <- 2.5
w_gamma  <- 2.0
w_spect  <- 2.0
w_colon  <- 0.8
w_bronco <- 0.7
w_ercp   <- 1.5
w_biop   <- 0.5
w_rx     <- 0.2
w_mamo   <- 0.3
w_densi  <- 0.2

## =============================================================
## Raw numerator and ratio
## =============================================================

proc_mat <- cbind(
  proc_tac, proc_resonancia, proc_pet, proc_angio, proc_gamma, proc_spect,
  proc_colonoscopia, proc_broncoscopia, proc_ercp,
  proc_biopsias, proc_rx, proc_mamo, proc_densiom
)

weighted_mat <- cbind(
  w_tac    * proc_tac,
  w_rnm    * proc_resonancia,
  w_pet    * proc_pet,
  w_angio  * proc_angio,
  w_gamma  * proc_gamma,
  w_spect  * proc_spect,
  w_colon  * proc_colonoscopia,
  w_bronco * proc_broncoscopia,
  w_ercp   * proc_ercp,
  w_biop   * proc_biopsias,
  w_rx     * proc_rx,
  w_mamo   * proc_mamo,
  w_densi  * proc_densiom
)

all_proc_na <- apply(is.na(proc_mat), 1, all)

df$i_diag_sum <- rowSums(weighted_mat, na.rm = TRUE)
df$i_diag_sum[all_proc_na] <- NA_real_

altTotal_bruto <- to_num(df$altTotal_bruto)
altTotal_bruto[!is.na(altTotal_bruto) & altTotal_bruto <= 0] <- NA_real_

df$i_diag <- ifelse(!is.na(altTotal_bruto) & !is.na(df$i_diag_sum),
                    df$i_diag_sum / altTotal_bruto,
                    NA_real_)

df$ln_i_diag <- ifelse(!is.na(df$i_diag) & df$i_diag > 0,
                       log(df$i_diag), NA_real_)

## =============================================================
## Simple intensity (kept for robustness / diagnostics)
## =============================================================

consultas <- safe0(df$total_consulta)
sesiones  <- safe0(df$total_sesionHdia)

df$i_simple <- ifelse(!is.na(altTotal_bruto),
                      (consultas + sesiones) / altTotal_bruto,
                      NA_real_)

df$ln_i_simple <- log(df$i_simple + 1)

## =============================================================
## Validation by year
## =============================================================

cat("\n=== Intensity diagnostics by year ===\n")
idiag_yr <- df %>%
  group_by(anyo) %>%
  summarise(
    n = n(),
    n_num_non_na = sum(!is.na(i_diag_sum)),
    pct_num_na   = round(100 * mean(is.na(i_diag_sum)), 1),
    median_num   = round(median(i_diag_sum, na.rm = TRUE), 2),
    median_i     = round(median(i_diag, na.rm = TRUE), 2),
    mean_i       = round(mean(i_diag, na.rm = TRUE), 2),
    pct_i_na     = round(100 * mean(is.na(i_diag)), 1),
    .groups = "drop"
  )
print(as.data.frame(idiag_yr))

# Check year-over-year jumps in median_i among positive observations
idiag_pos <- df %>%
  filter(!is.na(i_diag) & i_diag > 0) %>%
  group_by(anyo) %>%
  summarise(
    median_i = round(median(i_diag, na.rm = TRUE), 2),
    .groups = "drop"
  )

if (nrow(idiag_pos) > 1) {
  meds <- idiag_pos$median_i
  jumps <- diff(meds) / head(meds, -1)
  cat("\nYear-over-year jumps in median_i (positive obs only):\n")
  for (i in seq_along(jumps)) {
    flag <- if (abs(jumps[i]) > 0.15) " *** FLAG" else ""
    cat(sprintf("  %d->%d: %+.1f%%%s\n",
                idiag_pos$anyo[i], idiag_pos$anyo[i + 1],
                100 * jumps[i], flag))
  }
}

## =============================================================
## Store individual proc variables for diagnostics
## =============================================================

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

## =============================================================
## Save
## =============================================================

VARS_NEW <- c(
  "i_diag_sum", "i_diag", "ln_i_diag",
  "i_simple", "ln_i_simple",
  "proc_biopsias", "proc_tac", "proc_resonancia", "proc_pet",
  "proc_rx", "proc_spect", "proc_angio", "proc_gamma",
  "proc_mamo", "proc_densiom", "proc_colonoscopia",
  "proc_broncoscopia", "proc_ercp"
)

for (v in VARS_NEW) df_final[[v]] <- df[[v]]

save(df_final, file = file.path(CLEAN_DIR, "df_final.RData"))

cat("\nPhase 4 complete.\n")
cat("i_diag_sum NA% =", round(100 * mean(is.na(df_final$i_diag_sum)), 1), "\n")
cat("i_diag NA%     =", round(100 * mean(is.na(df_final$i_diag)), 1), "\n")
cat("ln_i_diag NA%  =", round(100 * mean(is.na(df_final$ln_i_diag)), 1), "\n")