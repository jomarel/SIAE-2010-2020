# 11b_estimar_sfa_frontier.R
# ------------------------------------------------------------
# Benchmark con {frontier}
# - Muy útil como especificación clásica Battese-Coelli (1995)
# - Más cercana al texto actual de la tesis si quieres mantener
#   u_it ~ N+(mu_it, sigma_u^2) con media heterogénea.
# ------------------------------------------------------------

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Faltan paquetes: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

ensure_packages(c("dplyr", "tibble", "tidyr", "readr", "plm", "frontier"))

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(readr)
  library(plm)
  library(frontier)
})

source_if_exists <- function(path) if (file.exists(path)) source(path, local = FALSE)
source_if_exists("00_config.R")

BASE_DIR <- get0("BASE_DIR", ifnotfound = getwd())
INT_DIR  <- get0("INT_DIR",  ifnotfound = file.path(BASE_DIR, "data_intermediate"))
OUT_DIR  <- get0("RESULT_DIR", ifnotfound = file.path(BASE_DIR, "outputs", "sfa_panel_frontier"))

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "models"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(OUT_DIR, "tables"), recursive = TRUE, showWarnings = FALSE)

locate_df_sfa <- function() {
  candidates <- c(
    file.path(INT_DIR, "df_sfa.RData"),
    file.path(INT_DIR, "df_sfa.rds"),
    file.path(INT_DIR, "df_sfa.csv"),
    file.path(BASE_DIR, "df_sfa.RData"),
    file.path(BASE_DIR, "df_sfa.rds"),
    file.path(BASE_DIR, "df_sfa.csv")
  )
  hit <- candidates[file.exists(candidates)]
  if (length(hit) == 0) stop("No encuentro df_sfa.", call. = FALSE)
  hit[1]
}

read_df_generic <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") return(readRDS(path))
  if (ext == "csv") return(readr::read_csv(path, show_col_types = FALSE))
  if (ext == "rdata") {
    env <- new.env(parent = emptyenv())
    nm <- load(path, envir = env)
    if ("df_sfa" %in% nm) return(get("df_sfa", envir = env))
    for (obj in nm) {
      val <- get(obj, envir = env)
      if (is.data.frame(val)) return(val)
    }
    stop("RData sin data.frame utilizable.", call. = FALSE)
  }
  stop("Formato no soportado.", call. = FALSE)
}

pick_one <- function(df, candidates, default = NULL) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(default)
  hit[1]
}

add_centered <- function(df, new_name, source_name) {
  if (!new_name %in% names(df)) {
    df[[new_name]] <- df[[source_name]] - mean(df[[source_name]], na.rm = TRUE)
  }
  df
}

safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

df <- read_df_generic(locate_df_sfa())
df <- as_tibble(df) %>%
  mutate(NCODI = as.character(NCODI), anyo = as.integer(anyo), .row_id = row_number())

var_y_total <- pick_one(df, c("ln_altTotal_pond", "ln_altTotalpond", "ln_altTotal"))
var_y_idiag <- pick_one(df, c("ln_i_diag", "ln_idiag", "ln_idiag_pond"))
var_y_q     <- pick_one(df, c("ln_altQ_pond", "ln_altQpond"))
var_y_m     <- pick_one(df, c("ln_altM_pond", "ln_altMpond"))
var_altQ    <- pick_one(df, c("altQ_pond", "altQpond"))
var_altM    <- pick_one(df, c("altM_pond", "altMpond"))

var_lnL_raw  <- pick_one(df, c("ln_L_total", "ln_L", "ln_L_medico", "lnL"))
var_lnKC_raw <- pick_one(df, c("ln_K_camas", "ln_KC", "ln_K", "lnK_camas"))
var_lnKT_raw <- pick_one(df, c("ln_K_tech_index", "ln_KT", "lnKT"))

var_lnL_c  <- pick_one(df, c("ln_L_c", "lnL_c"))
var_lnKC_c <- pick_one(df, c("ln_KC_c", "ln_K_c", "lnKC_c"))
var_lnKT_c <- pick_one(df, c("ln_KT_c", "lnKT_c"))

if (is.null(var_lnL_c) && !is.null(var_lnL_raw))  { df <- add_centered(df, "lnL_c",  var_lnL_raw);  var_lnL_c  <- "lnL_c"  }
if (is.null(var_lnKC_c) && !is.null(var_lnKC_raw)) { df <- add_centered(df, "lnKC_c", var_lnKC_raw); var_lnKC_c <- "lnKC_c" }
if (is.null(var_lnKT_c) && !is.null(var_lnKT_raw)) { df <- add_centered(df, "lnKT_c", var_lnKT_raw); var_lnKT_c <- "lnKT_c" }


if (is.null(var_lnL_c) || is.null(var_lnKC_c) || is.null(var_lnKT_c)) {
  stop(
    "No he podido identificar las tres variables logarítmicas base del translog ",
    "(trabajo, capital-camas y capital tecnológico).",
    call. = FALSE
  )
}

if (!"tt" %in% names(df))  df <- df %>% mutate(tt = anyo - min(anyo, na.rm = TRUE))
if (!"tt2" %in% names(df)) df <- df %>% mutate(tt2 = tt^2)

if (!"lnL_sq" %in% names(df))   df <- df %>% mutate(lnL_sq = 0.5 * .data[[var_lnL_c]]^2)
if (!"lnKC_sq" %in% names(df))  df <- df %>% mutate(lnKC_sq = 0.5 * .data[[var_lnKC_c]]^2)
if (!"lnKT_sq" %in% names(df))  df <- df %>% mutate(lnKT_sq = 0.5 * .data[[var_lnKT_c]]^2)
if (!"lnL_lnKC" %in% names(df)) df <- df %>% mutate(lnL_lnKC = .data[[var_lnL_c]]  * .data[[var_lnKC_c]])
if (!"lnL_lnKT" %in% names(df)) df <- df %>% mutate(lnL_lnKT = .data[[var_lnL_c]]  * .data[[var_lnKT_c]])
if (!"lnKC_lnKT" %in% names(df)) df <- df %>% mutate(lnKC_lnKT = .data[[var_lnKC_c]] * .data[[var_lnKT_c]])

var_ddesc  <- pick_one(df, c("D_desc", "Ddesc"))
var_pctsns <- pick_one(df, c("pct_sns", "pctSNS"))
var_shareq <- pick_one(df, c("ShareQ", "Q_share"))
var_ccaa   <- pick_one(df, c("ccaa", "ccaa_nombre"))


if (is.null(var_ddesc) || is.null(var_pctsns)) {
  stop("Faltan D_desc/Ddesc o pct_sns/pctSNS en df_sfa.", call. = FALSE)
}

df$z_Ddesc <- df[[var_ddesc]]
df$z_pctSNS <- df[[var_pctsns]]
df$z_DxSNS  <- df[[var_ddesc]] * df[[var_pctsns]]
if (!is.null(var_shareq)) {
  df$z_ShareQ   <- df[[var_shareq]]
  df$z_DxShareQ <- df[[var_ddesc]] * df[[var_shareq]]
}

x_terms <- c(var_lnL_c, var_lnKC_c, var_lnKT_c, "lnL_sq", "lnKC_sq", "lnKT_sq",
             "lnL_lnKC", "lnL_lnKT", "lnKC_lnKT", "tt", "tt2")
x_terms <- x_terms[x_terms %in% names(df)]

z_terms <- c("z_Ddesc", "z_pctSNS", "z_DxSNS")
if ("z_ShareQ" %in% names(df)) z_terms <- c(z_terms, "z_ShareQ")
if ("z_DxShareQ" %in% names(df)) z_terms <- c(z_terms, "z_DxShareQ")

x_rhs <- paste(x_terms, collapse = " + ")
if (!is.null(var_ccaa)) {
  z_rhs <- paste(c(z_terms, paste0("factor(", var_ccaa, ")")), collapse = " + ")
} else {
  z_rhs <- paste(z_terms, collapse = " + ")
}

make_formula <- function(lhs, x_rhs, z_rhs) as.formula(paste(lhs, "~", x_rhs, "|", z_rhs))

coerce_panel <- function(dat, id_var = "NCODI", time_var = "anyo") {
  plm::pdata.frame(dat, index = c(id_var, time_var), drop.index = FALSE, row.names = FALSE)
}

fit_frontier_safe <- function(dat, lhs, z_rhs = z_rhs, id_var = "NCODI", time_var = "anyo") {
  use_vars <- unique(c(lhs, x_terms, z_terms, var_ccaa, id_var, time_var, ".row_id"))
  use_vars <- use_vars[use_vars %in% names(dat)]

  work <- dat %>%
    select(all_of(use_vars)) %>%
    filter(is.finite(.data[[lhs]])) %>%
    filter(complete.cases(across(all_of(intersect(x_terms, names(.)))))) %>%
    filter(complete.cases(across(all_of(intersect(z_terms, names(.))))))

  pdat <- coerce_panel(work, id_var = id_var, time_var = time_var)
  fml  <- make_formula(lhs, x_rhs, z_rhs)

  fit <- frontier::sfa(
    formula = fml,
    data = pdat,
    truncNorm = TRUE,
    ineffDecrease = TRUE,
    timeEffect = FALSE
  )

  list(model = fit, data = work, formula = fml)
}

coef_table_frontier <- function(obj) {
  sm <- summary(obj$model)
  co <- as.data.frame(sm$mleParam)
  co$term <- rownames(co)
  rownames(co) <- NULL
  as_tibble(co)
}

te_table_frontier <- function(obj, label) {
  eff <- frontier::efficiencies(obj$model, asInData = TRUE)
  tibble(
    modelo = label,
    NCODI = obj$data$NCODI,
    anyo = obj$data$anyo,
    .row_id = obj$data$.row_id,
    TE = as.numeric(eff)
  )
}

write_model_bundle <- function(obj, label) {
  saveRDS(obj$model, file.path(OUT_DIR, "models", paste0(label, ".rds")))
  readr::write_csv(coef_table_frontier(obj), file.path(OUT_DIR, "tables", paste0(label, "_coef.csv")))
  readr::write_csv(te_table_frontier(obj, label), file.path(OUT_DIR, "tables", paste0(label, "_te.csv")))
}

results <- list()

if (!is.null(var_y_total)) {
  results$A_cantidad <- fit_frontier_safe(df, var_y_total)
  write_model_bundle(results$A_cantidad, "A_cantidad_frontier")
}
if (!is.null(var_y_idiag)) {
  results$A_intensidad <- fit_frontier_safe(df, var_y_idiag)
  write_model_bundle(results$A_intensidad, "A_intensidad_frontier")
}
if (!is.null(var_y_q)) {
  results$B_quirurgico <- fit_frontier_safe(df, var_y_q)
  write_model_bundle(results$B_quirurgico, "B_quirurgico_frontier")
}
if (!is.null(var_y_m)) {
  results$B_medico <- fit_frontier_safe(df, var_y_m)
  write_model_bundle(results$B_medico, "B_medico_frontier")
}

# ODF
if (!is.null(var_altQ) && !is.null(var_altM) && !is.null(var_y_q)) {
  dfD <- df %>%
    filter(is.finite(.data[[var_altQ]]), is.finite(.data[[var_altM]]),
           .data[[var_altQ]] > 0, .data[[var_altM]] > 0) %>%
    mutate(
      y_odf = - .data[[var_y_q]],
      ln_ratio_outputs = safe_log(.data[[var_altM]] / .data[[var_altQ]])
    )

  x_terms_D <- c("ln_ratio_outputs", x_terms)
  x_terms_D <- x_terms_D[x_terms_D %in% names(dfD)]
  x_rhs_D <- paste(x_terms_D, collapse = " + ")

  fit_frontier_odf <- function(dat) {
    use_vars <- unique(c("y_odf", x_terms_D, z_terms, var_ccaa, "NCODI", "anyo", ".row_id"))
    use_vars <- use_vars[use_vars %in% names(dat)]

    work <- dat %>%
      select(all_of(use_vars)) %>%
      filter(is.finite(y_odf)) %>%
      filter(complete.cases(across(all_of(intersect(x_terms_D, names(.)))))) %>%
      filter(complete.cases(across(all_of(intersect(z_terms, names(.))))))

    pdat <- coerce_panel(work)
    fml <- make_formula("y_odf", x_rhs_D, z_rhs)

    fit <- frontier::sfa(
      formula = fml,
      data = pdat,
      truncNorm = TRUE,
      ineffDecrease = TRUE,
      timeEffect = FALSE
    )
    list(model = fit, data = work, formula = fml)
  }

  results$D_odf <- fit_frontier_odf(dfD)
  write_model_bundle(results$D_odf, "D_odf_frontier")
}

# Diseño C
if (!is.null(var_y_q) && !is.null(var_y_m)) {
  dfC <- df %>%
    select(any_of(c("NCODI", "anyo", ".row_id", var_ccaa, var_y_q, var_y_m,
                    x_terms, z_terms, var_ddesc, var_pctsns, var_shareq))) %>%
    pivot_longer(cols = all_of(c(var_y_q, var_y_m)),
                 names_to = "servicio",
                 values_to = "ln_output") %>%
    mutate(
      Cs = ifelse(servicio == var_y_q, 1, 0),
      panel_id = paste(NCODI, servicio, sep = "__"),
      z_Cs = Cs,
      z_DxCs = .data[[var_ddesc]] * Cs
    )

  if (!is.null(var_shareq)) dfC$z_ShareQxCs <- dfC[[var_shareq]] * dfC$Cs

  z_terms_C <- c("z_Ddesc", "z_Cs", "z_DxCs", "z_pctSNS")
  if ("z_ShareQ" %in% names(dfC)) z_terms_C <- c(z_terms_C, "z_ShareQ")
  if ("z_ShareQxCs" %in% names(dfC)) z_terms_C <- c(z_terms_C, "z_ShareQxCs")

  if (!is.null(var_ccaa) && var_ccaa %in% names(dfC)) {
    z_rhs_C <- paste(c(z_terms_C, paste0("factor(", var_ccaa, ")")), collapse = " + ")
  } else {
    z_rhs_C <- paste(z_terms_C, collapse = " + ")
  }

  use_vars <- unique(c("ln_output", x_terms, z_terms_C, var_ccaa, "panel_id", "anyo", ".row_id", "NCODI"))
  use_vars <- use_vars[use_vars %in% names(dfC)]

  work <- dfC %>%
    select(all_of(use_vars)) %>%
    filter(is.finite(ln_output)) %>%
    filter(complete.cases(across(all_of(intersect(x_terms, names(.)))))) %>%
    filter(complete.cases(across(all_of(intersect(z_terms_C, names(.))))))

  pdat <- coerce_panel(work, id_var = "panel_id", time_var = "anyo")
  fml <- make_formula("ln_output", x_rhs, z_rhs_C)

  fit <- frontier::sfa(
    formula = fml,
    data = pdat,
    truncNorm = TRUE,
    ineffDecrease = TRUE,
    timeEffect = FALSE
  )

  results$C_panel <- list(model = fit, data = work, formula = fml)
  write_model_bundle(results$C_panel, "C_panel_frontier")
}

summary_tbl <- purrr::imap_dfr(results, function(obj, nm) {
  eff <- te_table_frontier(obj, nm)$TE
  tibble(
    modelo = nm,
    paquete = "frontier::sfa",
    formula = paste(deparse(obj$formula), collapse = " "),
    n = nrow(obj$data),
    te_media = mean(eff, na.rm = TRUE),
    logLik = as.numeric(logLik(obj$model))
  )
})
readr::write_csv(summary_tbl, file.path(OUT_DIR, "tables", "00_resumen_modelos_frontier.csv"))

cat("Benchmark frontier finalizado.\n")
cat("Salida en: ", OUT_DIR, "\n")
