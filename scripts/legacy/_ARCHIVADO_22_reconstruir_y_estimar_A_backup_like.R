config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "sfaR")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(sfaR)
})

message("\n=== Reconstruccion backup-like + estimacion A/A1 ===")

backup_root <- file.path(BASE_DIR, "SIAE-2010-2020-main_backup")
backup_scripts <- file.path(backup_root, "scripts")
backup_script_09 <- file.path(backup_scripts, "09_crear_df_sfa.R")
current_script_09 <- file.path(BASE_DIR, "scripts", "09_crear_df_sfa.R")
if (!file.exists(backup_script_09)) {
  stop("No existe script backup 09: ", backup_script_09, call. = FALSE)
}

tmp_root <- file.path(INT_DIR, "backup_like_recon")
tmp_legacy <- file.path(tmp_root, "legacy")
tmp_int <- file.path(tmp_root, "int")
tmp_out <- file.path(tmp_root, "out")
dir.create(tmp_legacy, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_int, recursive = TRUE, showWarnings = FALSE)
dir.create(tmp_out, recursive = TRUE, showWarnings = FALSE)

load(file.path(LEGACY_BASE_DIR, "df_final.RData"))
message("df_final base: ", nrow(df_final), " obs x ", ncol(df_final), " vars")

to_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA")] <- NA_character_
  x
}

df_final <- df_final %>%
  mutate(
    NCODI = to_chr(NCODI),
    D_desc_siae = dplyr::case_when(
      suppressWarnings(as.integer(cod_depend_agrupada)) == 1L ~ 0L,
      suppressWarnings(as.integer(cod_depend_agrupada)) == 2L ~ 1L,
      TRUE ~ NA_integer_
    )
  )

map_cnh <- read.csv(file.path(INT_DIR, "ncodi_hospital_map.csv"), stringsAsFactors = FALSE)
map_cnh$NCODI <- to_chr(map_cnh$NCODI)
map_cnh$dependencia_cnh <- to_chr(map_cnh$dependencia_cnh)

public_vals <- c(
  "Servicios e Institutos de Salud de Las Comunidades Autónomas",
  "Servicios e Institutos de Salud de Las Comunidades Autnomas",
  "Otras Entidades u o rganismos Públicos",
  "Otras Entidades u o rganismos Pblicos",
  "Otros Centros o Establecimientos Públicos de Dependencia Autonómica",
  "Otros Centros o Establecimientos Pblicos de Dependencia Autonmica",
  "Otros Centros o Establecimientos Públicos de Dependencia Estatal",
  "Otros Centros o Establecimientos Pblicos de Dependencia Estatal",
  "Instituto de Gestión Sanitaria-Ingesa",
  "Instituto de Gestin Sanitaria-Ingesa",
  "Ministerio de Defensa",
  "Municipio",
  "Diputación o Cabildo",
  "Diputacin o Cabildo"
)

private_vals <- c(
  "Privados",
  "Organizaciones No Gubernamentales"
)

cnh_map <- map_cnh %>%
  transmute(
    NCODI,
    D_desc_cnh = dplyr::case_when(
      dependencia_cnh %in% public_vals ~ 0L,
      dependencia_cnh %in% private_vals ~ 1L,
      TRUE ~ NA_integer_
    )
  ) %>%
  distinct(NCODI, .keep_all = TRUE)

df_final <- df_final %>%
  select(-any_of("D_desc_cnh")) %>%
  left_join(cnh_map, by = "NCODI") %>%
  mutate(D_desc = dplyr::coalesce(D_desc_cnh, D_desc_siae))

write.csv(
  df_final %>%
    count(D_desc_siae, D_desc_cnh, D_desc, sort = TRUE),
  file.path(tmp_out, "tabla_D_desc_backup_like.csv"),
  row.names = FALSE
)

save(df_final, file = file.path(tmp_legacy, "df_final.RData"))

# El script backup 09 está fijado a data_intermediate del proyecto principal.
# Lo ejecutamos, copiamos el resultado a la rama temporal y luego restauramos
# el df_sfa actual al final para no dejar el workspace alterado.
main_df_sfa_rdata <- file.path(INT_DIR, "df_sfa.RData")
main_df_sfa_csv <- file.path(INT_DIR, "df_sfa.csv")

had_main_df_sfa <- file.exists(main_df_sfa_rdata)
main_df_sfa_bak <- if (had_main_df_sfa) file.path(tmp_out, "df_sfa_main_pre_backup_like.RData") else NULL
main_df_sfa_csv_bak <- if (file.exists(main_df_sfa_csv)) file.path(tmp_out, "df_sfa_main_pre_backup_like.csv") else NULL

if (!is.null(main_df_sfa_bak)) file.copy(main_df_sfa_rdata, main_df_sfa_bak, overwrite = TRUE)
if (!is.null(main_df_sfa_csv_bak)) file.copy(main_df_sfa_csv, main_df_sfa_csv_bak, overwrite = TRUE)

orig_df_final <- NULL
if (file.exists(DF_FINAL_RDATA_PATH)) {
  load(DF_FINAL_RDATA_PATH)
  if (exists("df_final")) orig_df_final <- df_final
}

save(df_final, file = DF_FINAL_RDATA_PATH)
sys.source(backup_script_09, envir = globalenv())

file.copy(main_df_sfa_rdata, file.path(tmp_int, "df_sfa.RData"), overwrite = TRUE)
if (file.exists(main_df_sfa_csv)) {
  file.copy(main_df_sfa_csv, file.path(tmp_int, "df_sfa.csv"), overwrite = TRUE)
}

load(file.path(tmp_int, "df_sfa.RData"))
message("df_sfa backup-like: ", nrow(df_sfa), " obs x ", ncol(df_sfa), " vars")

ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(df_sfa), value = TRUE)

df_base <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    !is.na(D_desc), !is.na(pct_sns),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod)
  )

df_est_A <- df_base %>%
  filter(
    altTotal_bruto >= 200,
    !is.na(ShareQ),
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE)
  )

write.csv(
  data.frame(
    n_obs = nrow(df_est_A),
    n_hospitales = dplyr::n_distinct(df_est_A$NCODI),
    D0 = sum(df_est_A$D_desc == 0, na.rm = TRUE),
    D1 = sum(df_est_A$D_desc == 1, na.rm = TRUE)
  ),
  file.path(tmp_out, "muestra_A_backup_like.csv"),
  row.names = FALSE
)

frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)

uhet_A <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago + ShareQ + desc_shareQ +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A1 <- as.formula(paste(
  "~ D_desc + pct_sns + desc_pago +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_simple_A <- ~ D_desc + pct_sns + ShareQ
uhet_simple_A1 <- ~ D_desc + pct_sns

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || ll > 0 || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

estimar_sfa_backup <- function(lhs, uhet_full, uhet_simple, data, label) {
  frontier <- as.formula(paste(lhs, frontier_str))
  frontier_cd <- as.formula(paste(lhs, "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"))
  configs <- list(
    list(form = frontier,    uhet = uhet_full,   ud = "tnormal", mt = "bfgs", ht = 1L, tag = "TL"),
    list(form = frontier,    uhet = uhet_full,   ud = "tnormal", mt = "bhhh", ht = 1L, tag = "TL"),
    list(form = frontier,    uhet = uhet_full,   ud = "tnormal", mt = "nm",   ht = 2L, tag = "TL"),
    list(form = frontier_cd, uhet = uhet_simple, ud = "tnormal", mt = "bfgs", ht = 1L, tag = "CD"),
    list(form = frontier,    uhet = uhet_full,   ud = "hnormal", mt = "bfgs", ht = 1L, tag = "TL"),
    list(form = frontier_cd, uhet = uhet_simple, ud = "hnormal", mt = "bfgs", ht = 1L, tag = "CD"),
    list(form = frontier_cd, uhet = uhet_simple, ud = "hnormal", mt = "nm",   ht = 2L, tag = "CD")
  )

  for (cfg in configs) {
    message(sprintf("  [%s] %s/%s/%s", label, cfg$tag, cfg$ud, cfg$mt))
    fit <- NULL
    capture.output({
      fit <- tryCatch(
        suppressWarnings(sfaR::sfacross(
          formula = cfg$form,
          uhet = cfg$uhet,
          data = data,
          S = 1L,
          udist = cfg$ud,
          method = cfg$mt,
          hessianType = cfg$ht
        )),
        error = function(e) NULL
      )
    })
    if (!is.null(fit) && !es_degenerado(fit)) {
      message(sprintf("  OK [%s]: ll=%.3f", label, as.numeric(fit$mlLoglik)))
      return(fit)
    }
  }
  NULL
}

extraer_tab <- function(m, modelo) {
  if (is.null(m)) return(NULL)
  cf <- coef(m)
  se <- tryCatch(sqrt(diag(vcov(m))), error = function(e) rep(NA_real_, length(cf)))
  z <- cf / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  data.frame(
    modelo = modelo,
    term = names(cf),
    coef = as.numeric(cf),
    se = as.numeric(se),
    p = as.numeric(p),
    sig = dplyr::case_when(
      is.na(p) ~ "na",
      p < 0.001 ~ "***",
      p < 0.01 ~ "**",
      p < 0.05 ~ "*",
      p < 0.10 ~ ".",
      TRUE ~ "ns"
    ),
    stringsAsFactors = FALSE
  )
}

modelo_A_total <- estimar_sfa_backup("ln_altTotal_pond", uhet_A, uhet_simple_A, df_est_A, "A_Total_backup_like")
modelo_A_int <- estimar_sfa_backup("ln_i_diag", uhet_A, uhet_simple_A, df_est_A, "A_I_backup_like")
modelo_A1_total <- estimar_sfa_backup("ln_altTotal_pond", uhet_A1, uhet_simple_A1, df_est_A, "A1_Total_backup_like")
modelo_A1_int <- estimar_sfa_backup("ln_i_diag", uhet_A1, uhet_simple_A1, df_est_A, "A1_I_backup_like")

coefs <- bind_rows(
  extraer_tab(modelo_A_total, "A_Total_backup_like"),
  extraer_tab(modelo_A_int, "A_I_backup_like"),
  extraer_tab(modelo_A1_total, "A1_Total_backup_like"),
  extraer_tab(modelo_A1_int, "A1_I_backup_like")
) %>%
  filter(term %in% c("Zu_D_desc", "Zu_pct_sns", "Zu_desc_pago", "Zu_ShareQ", "Zu_desc_shareQ"))

resumen <- bind_rows(
  data.frame(modelo = "A_Total_backup_like", n = if (is.null(modelo_A_total)) NA else nrow(modelo_A_total$dataTable), logLik = if (is.null(modelo_A_total)) NA else as.numeric(modelo_A_total$mlLoglik), TE = if (is.null(modelo_A_total)) NA else mean(sfaR::efficiencies(modelo_A_total)$teJLMS, na.rm = TRUE)),
  data.frame(modelo = "A_I_backup_like", n = if (is.null(modelo_A_int)) NA else nrow(modelo_A_int$dataTable), logLik = if (is.null(modelo_A_int)) NA else as.numeric(modelo_A_int$mlLoglik), TE = if (is.null(modelo_A_int)) NA else mean(sfaR::efficiencies(modelo_A_int)$teJLMS, na.rm = TRUE)),
  data.frame(modelo = "A1_Total_backup_like", n = if (is.null(modelo_A1_total)) NA else nrow(modelo_A1_total$dataTable), logLik = if (is.null(modelo_A1_total)) NA else as.numeric(modelo_A1_total$mlLoglik), TE = if (is.null(modelo_A1_total)) NA else mean(sfaR::efficiencies(modelo_A1_total)$teJLMS, na.rm = TRUE)),
  data.frame(modelo = "A1_I_backup_like", n = if (is.null(modelo_A1_int)) NA else nrow(modelo_A1_int$dataTable), logLik = if (is.null(modelo_A1_int)) NA else as.numeric(modelo_A1_int$mlLoglik), TE = if (is.null(modelo_A1_int)) NA else mean(sfaR::efficiencies(modelo_A1_int)$teJLMS, na.rm = TRUE))
)

write.csv(coefs, file.path(tmp_out, "coeficientes_A_A1_backup_like.csv"), row.names = FALSE)
write.csv(resumen, file.path(tmp_out, "resumen_A_A1_backup_like.csv"), row.names = FALSE)
save(modelo_A_total, modelo_A_int, modelo_A1_total, modelo_A1_int,
     file = file.path(tmp_out, "modelos_A_A1_backup_like.RData"))

if (!is.null(orig_df_final)) {
  df_final <- orig_df_final
  save(df_final, file = DF_FINAL_RDATA_PATH)
  sys.source(current_script_09, envir = globalenv())
}

message("Salidas guardadas en: ", tmp_out)
