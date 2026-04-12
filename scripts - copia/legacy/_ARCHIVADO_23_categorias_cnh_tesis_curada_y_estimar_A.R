config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

required_pkgs <- c("dplyr", "readr", "sfaR")
missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Faltan paquetes: ", paste(missing_pkgs, collapse = ", "), call. = FALSE)
}

library(dplyr)
library(readr)
library(sfaR)

message("\n=== Reconstruccion tesis-curada: categorias CNH + A/A1 ===")

backup_like_path <- file.path(INT_DIR, "backup_like_recon", "int", "df_sfa.RData")
if (!file.exists(backup_like_path)) {
  stop("No existe df_sfa backup_like en: ", backup_like_path, call. = FALSE)
}

load(backup_like_path)

out_dir <- file.path(INT_DIR, "tesis_curada_recon")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

num <- function(x) suppressWarnings(as.numeric(as.character(x)))
norm_txt <- function(x) {
  y <- as.character(x)
  y <- trimws(y)
  y[y %in% c("", "NA")] <- NA_character_
  y <- iconv(y, from = "", to = "ASCII//TRANSLIT")
  y <- toupper(y)
  y <- gsub("[[:space:]]+", " ", y)
  y
}
nvl_chr <- function(x) ifelse(is.na(x), "", x)

map <- read_csv(file.path(INT_DIR, "ncodi_hospital_map.csv"), show_col_types = FALSE) %>%
  transmute(
    NCODI = as.character(NCODI),
    CODCNH = as.character(CODCNH),
    dependencia_cnh_map = as.character(dependencia_cnh),
    pertenencia_sns_cnh = as.character(pertenencia_sns_cnh)
  )

raw_cnh <- read_csv(
  file.path(INT_DIR, "cnh_raw_compare", "cnh_raw_dependencia_panel_2010_2023.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    anyo = num(anyo),
    CODCNH = as.character(CODCNH),
    cod_dep_cnh_raw = num(cod_dep_cnh_raw),
    dep_desc_raw = as.character(dep_desc_raw),
    fuente_cnh_raw = as.character(fuente_cnh_raw)
  ) %>%
  arrange(anyo, CODCNH, desc(!is.na(dep_desc_raw))) %>%
  distinct(anyo, CODCNH, .keep_all = TRUE)

df <- df_sfa
for (v in c("anyo", "es_agudo", "altTotal_bruto", "pct_sns")) df[[v]] <- num(df[[v]])
df$NCODI <- as.character(df$NCODI)

df <- df %>%
  left_join(map, by = "NCODI") %>%
  left_join(raw_cnh, by = c("anyo", "CODCNH")) %>%
  mutate(
    dep_raw_norm = norm_txt(dep_desc_raw),
    dep_map_norm = norm_txt(dependencia_cnh_map),
    dep_any_norm = coalesce(dep_raw_norm, dep_map_norm),
    cat_cnh_tesis = case_when(
      grepl("PENITEN|INTERNACIONAL|DEFENSA", nvl_chr(dep_any_norm)) ~ "Otros",
      grepl("DIPUT|MUNICIP|CABILDO", nvl_chr(dep_any_norm)) ~ "Diputaciones/ayuntamientos",
      grepl("MATEP|MUTUA", nvl_chr(dep_any_norm)) ~ "MATEPSS",
      grepl("CRUZ ROJA|IGLESIA|ORGANIZACIONES NO GUBERNAMENTALES|CARITAS|AECC", paste(nvl_chr(dep_raw_norm), nvl_chr(dep_map_norm))) ~ "Cruz Roja / iglesia",
      cod_dep_cnh_raw == 22 ~ "Fundaciones públicas auts.",
      grepl("OTROS PUBLICOS|OTRAS ENTIDADES U ORGANISMOS PUBLICOS|OTROS CENTROS O ESTABLECIMIENTOS PUBLICOS DE DEPENDENCIA AUTONOMICA", nvl_chr(dep_any_norm)) ~ "Fundaciones públicas auts.",
      grepl("^PRIVADOS?$|PRIVADO NO BENEFICO", nvl_chr(dep_any_norm)) & pct_sns >= 0.50 ~ "Soc. mercantil con concierto",
      grepl("^PRIVADOS?$|PRIVADO NO BENEFICO", nvl_chr(dep_any_norm)) & pct_sns < 0.50 ~ "Soc. mercantil de mercado",
      grepl("SERVICIO|INSTITUTO CATALAN DE LA SALUD|INGESA|COMUNIDAD AUTONOMA", nvl_chr(dep_any_norm)) ~ "SAS / integrado SNS",
      TRUE ~ NA_character_
    ),
    D_desc_cnh_curada = case_when(
      cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ 0L,
      cat_cnh_tesis %in% c(
        "MATEPSS", "Cruz Roja / iglesia",
        "Fundaciones públicas auts.",
        "Soc. mercantil con concierto",
        "Soc. mercantil de mercado"
      ) ~ 1L,
      TRUE ~ NA_integer_
    ),
    D_desc_curada = dplyr::coalesce(D_desc_cnh_curada, D_desc_siae),
    desc_pago_curada = D_desc_curada * pct_sns,
    desc_shareQ_curada = D_desc_curada * ShareQ
  )

`%notin%` <- Negate(`%in%`)

sample_texto <- df %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    !is.na(ln_altTotal_pond), is.finite(ln_altTotal_pond),
    !is.na(ln_i_diag), is.finite(ln_i_diag),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ccaa_cod),
    !is.na(D_desc_curada),
    !is.na(pct_sns),
    !is.na(ShareQ)
  )

sample_backup <- sample_texto %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))

cat_order <- c(
  "SAS / integrado SNS",
  "Diputaciones/ayuntamientos",
  "MATEPSS",
  "Cruz Roja / iglesia",
  "Fundaciones públicas auts.",
  "Soc. mercantil con concierto",
  "Soc. mercantil de mercado",
  "Otros"
)

make_cuadro53 <- function(dat) {
  dat %>%
    count(cat_cnh_tesis, name = "N_obs") %>%
    mutate(
      pct_muestra = round(100 * N_obs / sum(N_obs), 1),
      Ddesc = case_when(
        cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ "0",
        cat_cnh_tesis == "Fundaciones públicas auts." ~ "1 (A) / 0 (R2b)",
        cat_cnh_tesis %in% c("MATEPSS", "Cruz Roja / iglesia",
                             "Soc. mercantil con concierto", "Soc. mercantil de mercado") ~ "1",
        cat_cnh_tesis == "Otros" ~ "Excluye",
        TRUE ~ "Sin clasificar"
      ),
      Robustez = case_when(
        cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos",
                             "Soc. mercantil con concierto", "Soc. mercantil de mercado") ~ "Estable",
        cat_cnh_tesis == "MATEPSS" ~ "R2b: excluye",
        cat_cnh_tesis == "Cruz Roja / iglesia" ~ "R5: excluye",
        cat_cnh_tesis == "Fundaciones públicas auts." ~ "Ambiguo",
        cat_cnh_tesis == "Otros" ~ "Excluye",
        TRUE ~ "Revisar"
      )
    ) %>%
    arrange(match(cat_cnh_tesis, cat_order))
}

write.csv(make_cuadro53(sample_texto),
          file.path(out_dir, "cuadro53_tesis_curada_sample_texto.csv"),
          row.names = FALSE, na = "")
write.csv(make_cuadro53(sample_backup),
          file.path(out_dir, "cuadro53_tesis_curada_sample_backup.csv"),
          row.names = FALSE, na = "")

write.csv(
  data.frame(
    muestra = c("texto_tesis", "backup_script"),
    n_obs = c(nrow(sample_texto), nrow(sample_backup)),
    n_hospitales = c(n_distinct(sample_texto$NCODI), n_distinct(sample_backup$NCODI)),
    D1_obs = c(sum(sample_texto$D_desc_curada == 1, na.rm = TRUE),
               sum(sample_backup$D_desc_curada == 1, na.rm = TRUE)),
    D1_pct = c(round(100 * mean(sample_texto$D_desc_curada == 1, na.rm = TRUE), 1),
               round(100 * mean(sample_backup$D_desc_curada == 1, na.rm = TRUE), 1))
  ),
  file.path(out_dir, "resumen_muestras_tesis_curada.csv"),
  row.names = FALSE
)

frontier_str <- paste(
  "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c +",
  "ln_L_total_c2 + ln_K_camas_c2 + trend + trend2 +",
  "d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"
)
ccaa_vars <- grep("^d_ccaa_[0-9]+$", names(sample_backup), value = TRUE)

uhet_A <- as.formula(paste(
  "~ D_desc_curada + pct_sns + desc_pago_curada + ShareQ + desc_shareQ_curada +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A1 <- as.formula(paste(
  "~ D_desc_curada + pct_sns + desc_pago_curada +",
  paste(ccaa_vars, collapse = " + ")
))
uhet_A_simple <- ~ D_desc_curada + pct_sns + ShareQ
uhet_A1_simple <- ~ D_desc_curada + pct_sns

frontier_tl_total <- as.formula(paste("ln_altTotal_pond", frontier_str))
frontier_tl_int <- as.formula(paste("ln_i_diag", frontier_str))
frontier_cd_total <- as.formula(paste("ln_altTotal_pond", "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"))
frontier_cd_int <- as.formula(paste("ln_i_diag", "~ ln_L_total_c + ln_K_camas_c + ln_K_tech_c + trend + trend2 + d_cluster2 + d_cluster3 + d_cluster4 + d_cluster5"))

es_degenerado <- function(m) {
  if (is.null(m)) return(TRUE)
  ll <- tryCatch(as.numeric(m$mlLoglik), error = function(e) NA_real_)
  if (!is.finite(ll) || ll > 0 || abs(ll) > 1e10) return(TRUE)
  cf <- tryCatch(coef(m), error = function(e) NULL)
  if (is.null(cf) || any(!is.finite(cf))) return(TRUE)
  if (any(abs(cf) > 100, na.rm = TRUE)) return(TRUE)
  FALSE
}

fit_sfa <- function(form_tl, form_cd, uhet_full, uhet_simple, data, label) {
  configs <- list(
    list(form = form_tl, uhet = uhet_full, ud = "tnormal", mt = "bfgs", ht = 1L, tag = "TL"),
    list(form = form_tl, uhet = uhet_full, ud = "tnormal", mt = "bhhh", ht = 1L, tag = "TL"),
    list(form = form_tl, uhet = uhet_full, ud = "tnormal", mt = "nm",   ht = 2L, tag = "TL"),
    list(form = form_cd, uhet = uhet_simple, ud = "tnormal", mt = "bfgs", ht = 1L, tag = "CD"),
    list(form = form_tl, uhet = uhet_full, ud = "hnormal", mt = "bfgs", ht = 1L, tag = "TL"),
    list(form = form_cd, uhet = uhet_simple, ud = "hnormal", mt = "bfgs", ht = 1L, tag = "CD")
  )

  for (cfg in configs) {
    message(sprintf("  [%s] %s/%s/%s", label, cfg$tag, cfg$ud, cfg$mt))
    fit <- NULL
    capture.output({
      fit <- tryCatch(
        suppressWarnings(sfacross(
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

extract_terms <- function(model, model_name) {
  if (is.null(model)) return(NULL)
  cf <- coef(model)
  se <- tryCatch(sqrt(diag(vcov(model))), error = function(e) rep(NA_real_, length(cf)))
  z <- cf / se
  p <- 2 * pnorm(abs(z), lower.tail = FALSE)
  keep <- c("Zu_D_desc_curada", "Zu_pct_sns", "Zu_desc_pago_curada", "Zu_ShareQ", "Zu_desc_shareQ_curada")
  out <- data.frame(
    modelo = model_name,
    term = names(cf),
    coef = as.numeric(cf),
    p = as.numeric(p),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ ".",
      TRUE ~ "ns"
    )
  )
  out[out$term %in% keep, , drop = FALSE]
}

message("\n--- Estimando modelos A y A1 sobre muestra backup-script ---")
m_A_total <- fit_sfa(frontier_tl_total, frontier_cd_total, uhet_A, uhet_A_simple, sample_backup, "A_Total_curada")
m_A_int   <- fit_sfa(frontier_tl_int,   frontier_cd_int,   uhet_A, uhet_A_simple, sample_backup, "A_I_curada")
m_A1_total <- fit_sfa(frontier_tl_total, frontier_cd_total, uhet_A1, uhet_A1_simple, sample_backup, "A1_Total_curada")
m_A1_int   <- fit_sfa(frontier_tl_int,   frontier_cd_int,   uhet_A1, uhet_A1_simple, sample_backup, "A1_I_curada")

coef_tab <- bind_rows(
  extract_terms(m_A_total, "A_Total_curada"),
  extract_terms(m_A_int, "A_I_curada"),
  extract_terms(m_A1_total, "A1_Total_curada"),
  extract_terms(m_A1_int, "A1_I_curada")
)

write.csv(coef_tab, file.path(out_dir, "coeficientes_A_A1_tesis_curada.csv"), row.names = FALSE)

summary_tab <- data.frame(
  modelo = c("A_Total_curada", "A_I_curada", "A1_Total_curada", "A1_I_curada"),
  n_obs = nrow(sample_backup),
  n_hospitales = n_distinct(sample_backup$NCODI),
  logLik = c(
    if (!is.null(m_A_total)) as.numeric(m_A_total$mlLoglik) else NA_real_,
    if (!is.null(m_A_int)) as.numeric(m_A_int$mlLoglik) else NA_real_,
    if (!is.null(m_A1_total)) as.numeric(m_A1_total$mlLoglik) else NA_real_,
    if (!is.null(m_A1_int)) as.numeric(m_A1_int$mlLoglik) else NA_real_
  ),
  TE_media = c(
    if (!is.null(m_A_total)) mean(efficiencies(m_A_total), na.rm = TRUE) else NA_real_,
    if (!is.null(m_A_int)) mean(efficiencies(m_A_int), na.rm = TRUE) else NA_real_,
    if (!is.null(m_A1_total)) mean(efficiencies(m_A1_total), na.rm = TRUE) else NA_real_,
    if (!is.null(m_A1_int)) mean(efficiencies(m_A1_int), na.rm = TRUE) else NA_real_
  )
)
write.csv(summary_tab, file.path(out_dir, "resumen_A_A1_tesis_curada.csv"), row.names = FALSE)

save(
  sample_texto, sample_backup, m_A_total, m_A_int, m_A1_total, m_A1_int,
  file = file.path(out_dir, "modelos_A_A1_tesis_curada.RData")
)

message("\n=== Tesis-curada completado ===")
message("Resultados en: ", out_dir)
