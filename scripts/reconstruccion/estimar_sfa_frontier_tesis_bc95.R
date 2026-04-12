options(stringsAsFactors = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(plm)
  library(frontier)
})

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
out_dir <- file.path(project_dir, "outputs", "audit_Ddesc", "frontier_bc95_tesis")
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

load(file.path(project_dir, "data_intermediate", "legacy_tesis_recon", "int", "df_sfa.RData"))
df_legacy <- df_sfa
load(file.path(project_dir, "data_intermediate", "backup_like_recon", "int", "df_sfa.RData"))
df_backup <- df_sfa
load(file.path(project_dir, "data_intermediate", "df_sfa.RData"))
df_current <- df_sfa

for (nm in c("df_legacy", "df_backup", "df_current")) {
  x <- get(nm)
  for (v in intersect(c("anyo", "es_agudo", "altTotal_bruto", "pct_sns", "ShareQ", "D_desc", "D_desc_siae"), names(x))) {
    x[[v]] <- num(x[[v]])
  }
  x$NCODI <- as.character(x$NCODI)
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

build_sample_texto <- function(df) {
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
        cat_cnh_tesis %in% c("SAS / integrado SNS", "Diputaciones/ayuntamientos") ~ 0L,
        cat_cnh_tesis %in% c("MATEPSS", "Cruz Roja / iglesia", "Fundaciones publicas auts.", "Soc. mercantil con concierto", "Soc. mercantil de mercado") ~ 1L,
        TRUE ~ NA_integer_
      ),
      D_desc_curada = dplyr::coalesce(D_desc_curada, D_desc_siae)
    ) %>%
    filter(
      es_agudo == 1,
      !anyo %in% 2020:2022,
      altTotal_bruto >= 200,
      is.finite(ln_altTotal_pond),
      is.finite(ln_i_diag),
      !is.na(ln_L_total_c),
      !is.na(ln_K_camas_c),
      !is.na(ln_K_tech_c),
      !is.na(ShareQ),
      !is.na(pct_sns),
      !is.na(ccaa_cod),
      !is.na(D_desc_curada)
    )
}

sample_texto <- build_sample_texto(df_backup)
sample_backup <- sample_texto %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))

legacy_A_q01 <- df_legacy %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(ln_K_tech_c),
    !is.na(ShareQ),
    !is.na(pct_sns),
    !is.na(ccaa_cod),
    !is.na(D_desc)
  ) %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.01, na.rm = TRUE))

legacy_A_q02 <- df_legacy %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto >= 200,
    is.finite(ln_altTotal_pond),
    is.finite(ln_i_diag),
    !is.na(ln_L_total_c),
    !is.na(ln_K_camas_c),
    !is.na(ln_K_tech_c),
    !is.na(ShareQ),
    !is.na(pct_sns),
    !is.na(ccaa_cod),
    !is.na(D_desc)
  ) %>%
  filter(ln_i_diag > quantile(ln_i_diag, 0.02, na.rm = TRUE))

candidates <- list(
  sample_texto_4716 = list(data = sample_texto, dvar = "D_desc_curada"),
  sample_backup_4667 = list(data = sample_backup, dvar = "D_desc_curada"),
  legacy_q01_5474 = list(data = legacy_A_q01, dvar = "D_desc"),
  legacy_q02_5419 = list(data = legacy_A_q02, dvar = "D_desc")
)

target <- tibble(
  model = c("A_total", "A_intensity"),
  thesis_n = c(4765, 4765),
  thesis_logLik = c(-2349.6, -3924.0),
  thesis_te = c(0.731, 0.683),
  thesis_D_desc = c(-2.530, -1.477),
  thesis_pct_sns = c(-2.473, -2.439),
  thesis_ShareQ = c(-1.888, -1.473)
)

frontier_rhs <- paste(
  c(
    "ln_L_total_c", "ln_K_camas_c", "ln_K_tech_c",
    "ln_L_total_c2", "ln_K_camas_c2", "trend", "trend2",
    "d_cluster2", "d_cluster3", "d_cluster4", "d_cluster5"
  ),
  collapse = " + "
)

fit_bc95 <- function(dat, yvar, dvar) {
  work <- dat %>%
    transmute(
      NCODI = as.character(NCODI),
      anyo = as.integer(anyo),
      y = .data[[yvar]],
      D_desc = .data[[dvar]],
      pct_sns = pct_sns,
      ShareQ = ShareQ,
      ln_L_total_c = ln_L_total_c,
      ln_K_camas_c = ln_K_camas_c,
      ln_K_tech_c = ln_K_tech_c,
      ln_L_total_c2 = ln_L_total_c2,
      ln_K_camas_c2 = ln_K_camas_c2,
      trend = trend,
      trend2 = trend2,
      d_cluster2 = d_cluster2,
      d_cluster3 = d_cluster3,
      d_cluster4 = d_cluster4,
      d_cluster5 = d_cluster5
    ) %>%
    filter(complete.cases(.))

  pdat <- pdata.frame(work, index = c("NCODI", "anyo"), drop.index = FALSE, row.names = FALSE)
  fml <- as.formula(paste("y ~", frontier_rhs, "| D_desc + pct_sns + ShareQ"))
  fit <- frontier::sfa(
    formula = fml,
    data = pdat,
    truncNorm = TRUE,
    ineffDecrease = TRUE,
    timeEffect = FALSE
  )
  list(model = fit, data = work, formula = fml)
}

extract_frontier_summary <- function(obj, sample_name, model_name) {
  sm <- summary(obj$model)
  cf <- as.data.frame(sm$mleParam)
  cf$term <- rownames(cf)
  rownames(cf) <- NULL
  names(cf) <- make.names(names(cf), unique = TRUE)
  eff <- frontier::efficiencies(obj$model, asInData = TRUE)
  te_mean <- mean(as.numeric(eff), na.rm = TRUE)

  pick_coef <- function(pattern) {
    hit <- cf$Estimate[grepl(pattern, cf$term, ignore.case = TRUE)]
    if (length(hit) == 0) NA_real_ else hit[1]
  }
  pick_t <- function(pattern) {
    hit <- cf$t.value[grepl(pattern, cf$term, ignore.case = TRUE)]
    if (length(hit) == 0) NA_real_ else hit[1]
  }

  tibble(
    sample = sample_name,
    model = model_name,
    n_obs = nrow(obj$data),
    n_hospitals = n_distinct(obj$data$NCODI),
    pct_private = round(100 * mean(obj$data$D_desc == 1, na.rm = TRUE), 3),
    logLik = as.numeric(logLik(obj$model)),
    te_mean = te_mean,
    coef_D_desc = pick_coef("D_desc"),
    t_D_desc = pick_t("D_desc"),
    coef_pct_sns = pick_coef("pct_sns"),
    t_pct_sns = pick_t("pct_sns"),
    coef_ShareQ = pick_coef("ShareQ"),
    t_ShareQ = pick_t("ShareQ")
  )
}

results <- list()
coef_tables <- list()
te_tables <- list()

for (sample_name in names(candidates)) {
  info <- candidates[[sample_name]]
  message("Estimando frontier BC95 para ", sample_name)
  fit_total <- tryCatch(fit_bc95(info$data, "ln_altTotal_pond", info$dvar), error = function(e) e)
  fit_int <- tryCatch(fit_bc95(info$data, "ln_i_diag", info$dvar), error = function(e) e)

  if (!inherits(fit_total, "error")) {
    results[[length(results) + 1L]] <- extract_frontier_summary(fit_total, sample_name, "A_total")
    sm <- summary(fit_total$model)
    ct <- as.data.frame(sm$mleParam)
    ct$term <- rownames(ct)
    ct$sample <- sample_name
    ct$model <- "A_total"
    rownames(ct) <- NULL
    coef_tables[[length(coef_tables) + 1L]] <- as_tibble(ct)
    te <- frontier::efficiencies(fit_total$model, asInData = TRUE)
    te_tables[[length(te_tables) + 1L]] <- tibble(sample = sample_name, model = "A_total", TE = as.numeric(te))
    saveRDS(fit_total$model, file.path(out_dir, paste0(sample_name, "_A_total_frontier.rds")))
  }

  if (!inherits(fit_int, "error")) {
    results[[length(results) + 1L]] <- extract_frontier_summary(fit_int, sample_name, "A_intensity")
    sm <- summary(fit_int$model)
    ct <- as.data.frame(sm$mleParam)
    ct$term <- rownames(ct)
    ct$sample <- sample_name
    ct$model <- "A_intensity"
    rownames(ct) <- NULL
    coef_tables[[length(coef_tables) + 1L]] <- as_tibble(ct)
    te <- frontier::efficiencies(fit_int$model, asInData = TRUE)
    te_tables[[length(te_tables) + 1L]] <- tibble(sample = sample_name, model = "A_intensity", TE = as.numeric(te))
    saveRDS(fit_int$model, file.path(out_dir, paste0(sample_name, "_A_intensity_frontier.rds")))
  }
}

results_df <- bind_rows(results) %>%
  left_join(target, by = "model") %>%
  mutate(
    diff_n = n_obs - thesis_n,
    diff_logLik = logLik - thesis_logLik,
    diff_te = te_mean - thesis_te,
    diff_coef_D_desc = coef_D_desc - thesis_D_desc,
    diff_coef_pct_sns = coef_pct_sns - thesis_pct_sns,
    diff_coef_ShareQ = coef_ShareQ - thesis_ShareQ
  ) %>%
  arrange(model, abs(diff_logLik), abs(diff_te))

coef_df <- bind_rows(coef_tables)
te_df <- bind_rows(te_tables)

write_csv(results_df, file.path(out_dir, "frontier_bc95_comparacion_tesis.csv"), na = "")
write_csv(coef_df, file.path(out_dir, "frontier_bc95_coeficientes.csv"), na = "")
write_csv(te_df, file.path(out_dir, "frontier_bc95_te.csv"), na = "")

txt <- c(
  "Frontier BC95 vs tesis",
  "",
  "Resumen de muestras estimadas:",
  paste(
    sprintf(
      "- %s / %s / n=%s / pct_private=%.3f / logLik=%.3f / TE=%.3f / D_desc=%.3f / pct_sns=%.3f / ShareQ=%.3f",
      results_df$sample,
      results_df$model,
      results_df$n_obs,
      results_df$pct_private,
      results_df$logLik,
      results_df$te_mean,
      results_df$coef_D_desc,
      results_df$coef_pct_sns,
      results_df$coef_ShareQ
    ),
    collapse = "\n"
  )
)

writeLines(txt, file.path(out_dir, "frontier_bc95_NOTA.txt"), useBytes = TRUE)

message("Estimacion frontier BC95 exportada a: ", out_dir)
