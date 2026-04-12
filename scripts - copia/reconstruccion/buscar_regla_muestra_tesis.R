options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc")
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

target_pdf <- tribble(
  ~anyo, ~n_target, ~pct_target,
  2010, 463, 47.1,
  2011, 471, 47.3,
  2012, 468, 47.2,
  2013, 455, 47.0,
  2014, 452, 46.9,
  2015, 461, 46.8,
  2016, 458, 47.4,
  2017, 455, 47.9,
  2018, 449, 48.1,
  2019, 444, 47.3,
  2023, 389, 48.7
)

load(file.path(project_dir, "data_intermediate", "df_sfa.RData"))
df_current <- df_sfa
load(file.path(project_dir, "data_intermediate", "backup_like_recon", "int", "df_sfa.RData"))
df_backup <- df_sfa
load(file.path(project_dir, "data_intermediate", "legacy_tesis_recon", "int", "df_sfa.RData"))
df_legacy <- df_sfa

for (nm in c("df_current", "df_backup", "df_legacy")) {
  x <- get(nm)
  for (v in intersect(c("anyo", "es_agudo", "altTotal_bruto", "pct_sns", "ShareQ", "D_desc", "D_desc_tesis", "D_desc_siae"), names(x))) {
    x[[v]] <- num(x[[v]])
  }
  if ("NCODI" %in% names(x)) x$NCODI <- as.character(x$NCODI)
  assign(nm, x)
}

map <- read_csv(file.path(project_dir, "data_intermediate", "ncodi_hospital_map.csv"), show_col_types = FALSE) %>%
  transmute(
    NCODI = as.character(NCODI),
    CODCNH = as.character(CODCNH),
    dependencia_cnh_map = as.character(dependencia_cnh),
    pertenencia_sns_cnh = as.character(pertenencia_sns_cnh)
  )

raw_cnh <- read_csv(
  file.path(project_dir, "data_intermediate", "cnh_raw_compare", "cnh_raw_dependencia_panel_2010_2023.csv"),
  show_col_types = FALSE
) %>%
  transmute(
    anyo = num(anyo),
    CODCNH = as.character(CODCNH),
    cod_dep_cnh_raw = num(cod_dep_cnh_raw),
    dep_desc_raw = as.character(dep_desc_raw)
  ) %>%
  arrange(anyo, CODCNH, desc(!is.na(dep_desc_raw))) %>%
  distinct(anyo, CODCNH, .keep_all = TRUE)

build_curada <- function(df) {
  df %>%
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
        cod_dep_cnh_raw == 22 ~ "Fundaciones publicas auts.",
        grepl("OTROS PUBLICOS|OTRAS ENTIDADES U ORGANISMOS PUBLICOS|OTROS CENTROS O ESTABLECIMIENTOS PUBLICOS DE DEPENDENCIA AUTONOMICA", nvl_chr(dep_any_norm)) ~ "Fundaciones publicas auts.",
        grepl("^PRIVADOS?$|PRIVADO NO BENEFICO", nvl_chr(dep_any_norm)) & pct_sns >= 0.50 ~ "Soc. mercantil con concierto",
        grepl("^PRIVADOS?$|PRIVADO NO BENEFICO", nvl_chr(dep_any_norm)) & pct_sns < 0.50 ~ "Soc. mercantil de mercado",
        grepl("SERVICIO|INSTITUTO CATALAN DE LA SALUD|INGESA|COMUNIDAD AUTONOMA", nvl_chr(dep_any_norm)) ~ "SAS / integrado SNS",
        TRUE ~ NA_character_
      ),
      D_desc_curada = case_when(
        cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ 0,
        cat_cnh_tesis %in% c("MATEPSS", "Cruz Roja / iglesia", "Fundaciones publicas auts.", "Soc. mercantil con concierto", "Soc. mercantil de mercado") ~ 1,
        TRUE ~ NA_real_
      ),
      D_desc_curada = coalesce(D_desc_curada, D_desc_siae)
    )
}

df_backup_recovered <- df_backup %>%
  left_join(
    df_current %>%
      select(NCODI, anyo, es_agudo_current = es_agudo) %>%
      distinct(),
    by = c("NCODI", "anyo")
  ) %>%
  mutate(es_agudo = coalesce(es_agudo, es_agudo_current))

datasets <- list(
  current_D_desc = df_current,
  current_D_desc_tesis = df_current,
  legacy_D_desc = df_legacy,
  legacy_D_desc_tesis = df_legacy,
  backup_curada = build_curada(df_backup),
  backup_curada_esag = build_curada(df_backup_recovered)
)

dvar_map <- c(
  current_D_desc = "D_desc",
  current_D_desc_tesis = "D_desc_tesis",
  legacy_D_desc = "D_desc",
  legacy_D_desc_tesis = "D_desc_tesis",
  backup_curada = "D_desc_curada",
  backup_curada_esag = "D_desc_curada"
)

quantile_rules <- c("none", "q005", "q01", "q02")
alt_rules <- c("gt100", "ge150", "ge200")
share_rules <- c(TRUE, FALSE)
lnktech_rules <- c(TRUE, FALSE)
grp_pago_rules <- c(TRUE, FALSE)

apply_rule <- function(df, dvar, alt_rule, require_shareQ, require_lnktech, require_grupo_pago, q_rule) {
  if (!all(c("anyo", "es_agudo", "altTotal_bruto", dvar, "pct_sns", "ln_L_total_c", "ln_K_camas_c", "ccaa_cod", "ln_i_diag", "ln_altTotal_pond") %in% names(df))) {
    return(NULL)
  }
  dat <- df %>%
    filter(
      es_agudo == 1,
      !anyo %in% 2020:2022,
      !is.na(.data[[dvar]]),
      !is.na(pct_sns),
      !is.na(ln_L_total_c),
      !is.na(ln_K_camas_c),
      !is.na(ccaa_cod),
      is.finite(ln_i_diag),
      is.finite(ln_altTotal_pond)
    )
  if (alt_rule == "gt100") dat <- dat %>% filter(altTotal_bruto > 100)
  if (alt_rule == "ge150") dat <- dat %>% filter(altTotal_bruto >= 150)
  if (alt_rule == "ge200") dat <- dat %>% filter(altTotal_bruto >= 200)
  if (require_shareQ) {
    if (!"ShareQ" %in% names(dat)) return(NULL)
    dat <- dat %>% filter(!is.na(ShareQ))
  }
  if (require_lnktech) {
    if (!"ln_K_tech_c" %in% names(dat)) return(NULL)
    dat <- dat %>% filter(!is.na(ln_K_tech_c))
  }
  if (require_grupo_pago) {
    if (!"grupo_pago" %in% names(dat)) return(NULL)
    dat <- dat %>% filter(!is.na(grupo_pago))
  }
  if (q_rule != "none") {
    p <- switch(q_rule, q005 = 0.005, q01 = 0.01, q02 = 0.02)
    cut <- suppressWarnings(quantile(dat$ln_i_diag, probs = p, na.rm = TRUE))
    dat <- dat %>% filter(ln_i_diag > cut)
  }
  dat
}

score_rule <- function(dat, dvar, dataset_name, alt_rule, require_shareQ, require_lnktech, require_grupo_pago, q_rule) {
  if (is.null(dat) || nrow(dat) == 0) return(NULL)
  yearly <- dat %>%
    group_by(anyo) %>%
    summarise(
      n_obs = n(),
      n_hospitales = n_distinct(NCODI),
      pct_desc = round(100 * mean(.data[[dvar]] == 1, na.rm = TRUE), 3),
      .groups = "drop"
    )
  cmp <- target_pdf %>%
    left_join(yearly, by = "anyo") %>%
    mutate(
      abs_diff_n = abs(n_obs - n_target),
      abs_diff_pct = abs(pct_desc - pct_target)
    )
  if (any(is.na(cmp$n_obs))) return(NULL)
  tibble(
    dataset = dataset_name,
    dvar = dvar,
    alt_rule = alt_rule,
    require_shareQ = require_shareQ,
    require_lnktech = require_lnktech,
    require_grupo_pago = require_grupo_pago,
    i_diag_rule = q_rule,
    total_obs = nrow(dat),
    total_hospitales = n_distinct(dat$NCODI),
    total_pct_desc = round(100 * mean(dat[[dvar]] == 1, na.rm = TRUE), 3),
    diff_total_obs = abs(nrow(dat) - 4765),
    mae_year_n = mean(cmp$abs_diff_n, na.rm = TRUE),
    mae_year_pct = mean(cmp$abs_diff_pct, na.rm = TRUE),
    score = abs(nrow(dat) - 4765) + mean(cmp$abs_diff_n, na.rm = TRUE) + 10 * mean(cmp$abs_diff_pct, na.rm = TRUE)
  )
}

results <- list()
for (dataset_name in names(datasets)) {
  df <- datasets[[dataset_name]]
  dvar <- dvar_map[[dataset_name]]
  if (!dvar %in% names(df)) next
  for (alt_rule in alt_rules) {
    for (require_shareQ in share_rules) {
      for (require_lnktech in lnktech_rules) {
        for (require_grupo_pago in grp_pago_rules) {
          for (q_rule in quantile_rules) {
            dat <- apply_rule(df, dvar, alt_rule, require_shareQ, require_lnktech, require_grupo_pago, q_rule)
            sc <- score_rule(dat, dvar, dataset_name, alt_rule, require_shareQ, require_lnktech, require_grupo_pago, q_rule)
            if (!is.null(sc)) results[[length(results) + 1L]] <- sc
          }
        }
      }
    }
  }
}

results_df <- bind_rows(results) %>% arrange(score, diff_total_obs, mae_year_n, mae_year_pct)

top_rules <- head(results_df, 25)

detail_rows <- list()
for (i in seq_len(nrow(top_rules))) {
  row <- top_rules[i, ]
  dat <- apply_rule(
    datasets[[row$dataset]],
    row$dvar,
    row$alt_rule,
    row$require_shareQ,
    row$require_lnktech,
    row$require_grupo_pago,
    row$i_diag_rule
  )
  yearly <- dat %>%
    group_by(anyo) %>%
    summarise(
      n_obs = n(),
      pct_desc = round(100 * mean(.data[[row$dvar]] == 1, na.rm = TRUE), 3),
      .groups = "drop"
    ) %>%
    right_join(target_pdf, by = "anyo") %>%
    mutate(
      abs_diff_n = abs(n_obs - n_target),
      abs_diff_pct = abs(pct_desc - pct_target),
      dataset = row$dataset,
      dvar = row$dvar,
      alt_rule = row$alt_rule,
      require_shareQ = row$require_shareQ,
      require_lnktech = row$require_lnktech,
      require_grupo_pago = row$require_grupo_pago,
      i_diag_rule = row$i_diag_rule
    )
  detail_rows[[length(detail_rows) + 1L]] <- yearly
}

details_df <- bind_rows(detail_rows)

write_csv(results_df, file.path(out_dir, "busqueda_regla_muestra_tesis_todas.csv"), na = "")
write_csv(top_rules, file.path(out_dir, "busqueda_regla_muestra_tesis_top25.csv"), na = "")
write_csv(details_df, file.path(out_dir, "busqueda_regla_muestra_tesis_top25_detalle.csv"), na = "")

txt <- c(
  "Busqueda sistematica de reglas de muestra tesis",
  "",
  sprintf("Se evaluaron %s reglas candidatas validas.", nrow(results_df)),
  "",
  "Top 10 reglas por score:",
  paste(
    sprintf(
      "- %s / %s / alt=%s / ShareQ=%s / lnKtech=%s / grupo_pago=%s / i_diag=%s / n=%s / pct=%.3f / score=%.3f",
      head(results_df$dataset, 10),
      head(results_df$dvar, 10),
      head(results_df$alt_rule, 10),
      head(results_df$require_shareQ, 10),
      head(results_df$require_lnktech, 10),
      head(results_df$require_grupo_pago, 10),
      head(results_df$i_diag_rule, 10),
      head(results_df$total_obs, 10),
      head(results_df$total_pct_desc, 10),
      head(results_df$score, 10)
    ),
    collapse = "\n"
  )
)

writeLines(txt, file.path(out_dir, "busqueda_regla_muestra_tesis_NOTA.txt"), useBytes = TRUE)

message("Busqueda de regla exportada a: ", out_dir)
