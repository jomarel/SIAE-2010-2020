# 11_estimar_sfa.R
# ------------------------------------------------------------
# Estimación SFA en panel con el paquete {sfa} usando psfm()
# Diseñado para el pipeline SIAE 2010-2023.
#
# Idea:
# - Versión "panel real" y flexible.
# - Por defecto usa TRE con determinantes de ineficiencia (formula y ~ x | z),
#   que el propio paquete admite también como TRE_Z.
# - Si se quiere una extensión más rica que separe heterogeneidad persistente,
#   cambiar MODEL_NAME a "GTRE" o "GTRE_Z".
#
# Referencia práctica:
#   psfm(formula, model_name = c("TRE_Z","GTRE_Z","TRE","GTRE","TFE","FD"), ...)
# ------------------------------------------------------------

options(stringsAsFactors = FALSE)

`%||%` <- function(x, y) if (is.null(x)) y else x

ensure_packages <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      "Faltan paquetes: ", paste(missing, collapse = ", "),
      ". Instálalos antes de continuar.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

ensure_packages(c("dplyr", "tibble", "tidyr", "purrr", "readr", "plm", "Formula", "sfa"))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(readr)
  library(plm)
  library(sfa)
})

source_if_exists <- function(path) {
  if (file.exists(path)) source(path, local = FALSE)
}

# ------------------------------------------------------------------
# 0) Configuración flexible
# ------------------------------------------------------------------
source_if_exists("00_config.R")

BASE_DIR <- get0("BASE_DIR", ifnotfound = getwd())
INT_DIR  <- get0("INT_DIR",  ifnotfound = file.path(BASE_DIR, "data_intermediate"))
OUT_DIR  <- get0("RESULT_DIR", ifnotfound = file.path(BASE_DIR, "outputs", "sfa_panel"))

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
  if (length(hit) == 0) {
    stop("No encuentro df_sfa en data_intermediate/ ni en el directorio de trabajo.", call. = FALSE)
  }
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
    # si no existe df_sfa, devolver el primer objeto data.frame
    for (obj in nm) {
      val <- get(obj, envir = env)
      if (is.data.frame(val)) return(val)
    }
    stop("El archivo RData no contiene ningún data.frame utilizable.", call. = FALSE)
  }
  stop("Formato no soportado para df_sfa: ", ext, call. = FALSE)
}

pick_one <- function(df, candidates, default = NULL) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(default)
  hit[1]
}

pick_existing <- function(df, candidates) intersect(candidates, names(df))

safe_log <- function(x) ifelse(is.finite(x) & x > 0, log(x), NA_real_)

add_centered <- function(df, new_name, source_name) {
  if (!new_name %in% names(df)) {
    df[[new_name]] <- df[[source_name]] - mean(df[[source_name]], na.rm = TRUE)
  }
  df
}

# ------------------------------------------------------------------
# 1) Cargar df_sfa y armonizar nombres mínimos
# ------------------------------------------------------------------
df_path <- locate_df_sfa()
df <- read_df_generic(df_path)

if (!is.data.frame(df)) stop("df_sfa no se ha leído como data.frame.", call. = FALSE)

if (!"NCODI" %in% names(df)) stop("df_sfa debe contener NCODI.", call. = FALSE)
if (!"anyo" %in% names(df))  stop("df_sfa debe contener anyo.", call. = FALSE)

df <- as_tibble(df) %>%
  mutate(
    NCODI = as.character(NCODI),
    anyo  = as.integer(anyo),
    .row_id = row_number()
  )

# Variables de salida
var_y_total <- pick_one(df, c("ln_altTotal_pond", "ln_altTotalpond", "ln_altTotal"))
var_y_idiag <- pick_one(df, c("ln_i_diag", "ln_idiag", "ln_idiag_pond"))
var_y_q     <- pick_one(df, c("ln_altQ_pond", "ln_altQpond"))
var_y_m     <- pick_one(df, c("ln_altM_pond", "ln_altMpond"))
var_altQ    <- pick_one(df, c("altQ_pond", "altQpond"))
var_altM    <- pick_one(df, c("altM_pond", "altMpond"))

# Inputs del translog: intentar primero logs ya creados; si no, construirlos desde niveles
var_lnL_raw  <- pick_one(df, c("ln_L_total", "lnL_total", "ln_L", "ln_L_medico", "lnL", "ln_personal_total"))
var_lnKC_raw <- pick_one(df, c("ln_K_camas", "lnK_camas", "ln_KC", "ln_K", "lnKC", "ln_capital_camas"))
var_lnKT_raw <- pick_one(df, c("ln_K_tech_index", "lnK_tech_index", "ln_KT", "lnKT", "ln_capital_tech", "ln_capital_tecnologico"))

var_L_level  <- pick_one(df, c("L_total", "L", "personal_total", "L_medico"))
var_KC_level <- pick_one(df, c("K_camas", "capital_camas", "camas_funcionamiento"))
var_KT_level <- pick_one(df, c("K_tech_index", "capital_tech", "capital_tecnologico", "KT", "tecno", "altatecno"))

if (is.null(var_lnL_raw) && !is.null(var_L_level)) {
  df <- df %>% mutate(lnL_auto = safe_log(.data[[var_L_level]]))
  var_lnL_raw <- "lnL_auto"
}
if (is.null(var_lnKC_raw) && !is.null(var_KC_level)) {
  df <- df %>% mutate(lnKC_auto = safe_log(.data[[var_KC_level]]))
  var_lnKC_raw <- "lnKC_auto"
}
if (is.null(var_lnKT_raw) && !is.null(var_KT_level)) {
  df <- df %>% mutate(lnKT_auto = safe_log(.data[[var_KT_level]]))
  var_lnKT_raw <- "lnKT_auto"
}

# Si el dataset ya trae centrados, usarlos; si no, crearlos a partir de logs brutos
var_lnL_c  <- pick_one(df, c("ln_L_c", "lnL_c", "ln_Ltotal_c"))
var_lnKC_c <- pick_one(df, c("ln_KC_c", "ln_K_c", "lnKC_c", "ln_Kcamas_c"))
var_lnKT_c <- pick_one(df, c("ln_KT_c", "lnKT_c", "ln_Ktech_c"))

if (is.null(var_lnL_c) && !is.null(var_lnL_raw)) {
  df <- add_centered(df, "lnL_c", var_lnL_raw)
  var_lnL_c <- "lnL_c"
}
if (is.null(var_lnKC_c) && !is.null(var_lnKC_raw)) {
  df <- add_centered(df, "lnKC_c", var_lnKC_raw)
  var_lnKC_c <- "lnKC_c"
}
if (is.null(var_lnKT_c) && !is.null(var_lnKT_raw)) {
  df <- add_centered(df, "lnKT_c", var_lnKT_raw)
  var_lnKT_c <- "lnKT_c"
}

if (is.null(var_lnL_c) || is.null(var_lnKC_c) || is.null(var_lnKT_c)) {
  msg <- paste0(
    "No he podido identificar las tres variables logarítmicas base del translog.\n",
    "Trabajo: log=", var_lnL_raw %||% "<NA>", ", nivel=", var_L_level %||% "<NA>", ", centrada=", var_lnL_c %||% "<NA>", "\n",
    "Capital-camas: log=", var_lnKC_raw %||% "<NA>", ", nivel=", var_KC_level %||% "<NA>", ", centrada=", var_lnKC_c %||% "<NA>", "\n",
    "Capital-tech: log=", var_lnKT_raw %||% "<NA>", ", nivel=", var_KT_level %||% "<NA>", ", centrada=", var_lnKT_c %||% "<NA>
",
    "Revisa nombres en df_sfa o crea estas variables antes de estimar."
  )
  stop(msg, call. = FALSE)
}

# Tendencia temporal
if (!"tt" %in% names(df)) {
  df <- df %>% mutate(tt = anyo - min(anyo, na.rm = TRUE))
}
if (!"tt2" %in% names(df)) {
  df <- df %>% mutate(tt2 = tt^2)
}

# Crear cuadrados e interacciones si faltan
if (!"lnL_sq" %in% names(df))  df <- df %>% mutate(lnL_sq  = 0.5 * .data[[var_lnL_c]]^2)
if (!"lnKC_sq" %in% names(df)) df <- df %>% mutate(lnKC_sq = 0.5 * .data[[var_lnKC_c]]^2)
if (!"lnKT_sq" %in% names(df)) df <- df %>% mutate(lnKT_sq = 0.5 * .data[[var_lnKT_c]]^2)

if (!"lnL_lnKC" %in% names(df)) df <- df %>% mutate(lnL_lnKC = .data[[var_lnL_c]]  * .data[[var_lnKC_c]])
if (!"lnL_lnKT" %in% names(df)) df <- df %>% mutate(lnL_lnKT = .data[[var_lnL_c]]  * .data[[var_lnKT_c]])
if (!"lnKC_lnKT" %in% names(df)) df <- df %>% mutate(lnKC_lnKT = .data[[var_lnKC_c]] * .data[[var_lnKT_c]])

# Variables Z de ineficiencia
var_ddesc  <- pick_one(df, c("D_desc", "Ddesc"))
var_pctsns <- pick_one(df, c("pct_sns", "pctSNS"))
var_shareq <- pick_one(df, c("ShareQ", "Q_share"))
var_ccaa   <- pick_one(df, c("ccaa", "ccaa_nombre"))

if (is.null(var_ddesc) || is.null(var_pctsns)) {
  stop("Faltan D_desc/Ddesc o pct_sns/pctSNS en df_sfa.", call. = FALSE)
}

if (!"z_Ddesc" %in% names(df))    df$z_Ddesc    <- df[[var_ddesc]]
if (!"z_pctSNS" %in% names(df))   df$z_pctSNS   <- df[[var_pctsns]]
if (!"z_DxSNS" %in% names(df))    df$z_DxSNS    <- df[[var_ddesc]] * df[[var_pctsns]]

if (!is.null(var_shareq)) {
  if (!"z_ShareQ" %in% names(df))   df$z_ShareQ <- df[[var_shareq]]
  if (!"z_DxShareQ" %in% names(df)) df$z_DxShareQ <- df[[var_ddesc]] * df[[var_shareq]]
}

# Fórmulas base
frontier_terms <- c(
  var_lnL_c, var_lnKC_c, var_lnKT_c,
  "lnL_sq", "lnKC_sq", "lnKT_sq",
  "lnL_lnKC", "lnL_lnKT", "lnKC_lnKT",
  "tt", "tt2"
)

frontier_terms <- frontier_terms[frontier_terms %in% names(df)]

z_terms <- c("z_Ddesc", "z_pctSNS", "z_DxSNS")
if ("z_ShareQ" %in% names(df))   z_terms <- c(z_terms, "z_ShareQ")
if ("z_DxShareQ" %in% names(df)) z_terms <- c(z_terms, "z_DxShareQ")

# Incluir dummies regionales si existen
if (!is.null(var_ccaa)) {
  z_formula_rhs <- paste(c(z_terms, paste0("factor(", var_ccaa, ")")), collapse = " + ")
} else {
  z_formula_rhs <- paste(z_terms, collapse = " + ")
}
x_formula_rhs <- paste(frontier_terms, collapse = " + ")

# ------------------------------------------------------------------
# 2) Helpers de estimación
# ------------------------------------------------------------------
MODEL_NAME <- Sys.getenv("SFA_MAIN_MODEL", unset = "TRE")
requested_models <- toupper(trimws(strsplit(
  Sys.getenv(
    "SFA_ONLY_MODELS",
    "A_cantidad,A_intensidad,A1_cantidad,A1_intensidad,B_quirurgico,B_medico,D_odf,C_panel"
  ),
  ","
)[[1]]))
# Alternativas razonables: TRE, TRE_Z, GTRE, GTRE_Z, TFE, FD
# Para ser coherente con el texto de la tesis actual:
# - el benchmark más "fiel" sigue siendo frontier (script 11b)
# - aquí dejamos TRE/TRE_Z como panel real y flexible

make_formula <- function(lhs, x_rhs, z_rhs = NULL) {
  if (!is.null(z_rhs) && nzchar(z_rhs)) {
    as.formula(paste(lhs, "~", x_rhs, "|", z_rhs))
  } else {
    as.formula(paste(lhs, "~", x_rhs))
  }
}

coerce_panel <- function(dat, id_var = "NCODI", time_var = "anyo") {
  plm::pdata.frame(dat, index = c(id_var, time_var), drop.index = FALSE, row.names = FALSE)
}

fit_psfm_safe <- function(dat, lhs, model_name = MODEL_NAME, id_var = "NCODI", time_var = "anyo",
                          z_rhs = z_formula_rhs, z_vars = z_terms, pso = FALSE) {
  use_vars <- unique(c(lhs, frontier_terms, z_vars, var_ccaa, id_var, time_var, ".row_id"))
  use_vars <- use_vars[!is.na(use_vars) & nzchar(use_vars)]
  use_vars <- intersect(use_vars, names(dat))

  work <- dat %>%
    select(all_of(use_vars)) %>%
    filter(!is.na(.data[[id_var]]), !is.na(.data[[time_var]])) %>%
    filter(is.finite(.data[[lhs]])) %>%
    filter(complete.cases(across(all_of(intersect(frontier_terms, names(.)))))) %>%
    filter(complete.cases(across(all_of(intersect(z_vars, names(.))))))

  if (nrow(work) == 0) {
    stop("No hay observaciones utilizables para ", lhs, call. = FALSE)
  }

  pdat <- coerce_panel(work, id_var = id_var, time_var = time_var)
  fml  <- make_formula(lhs, x_formula_rhs, z_rhs)

  fit <- sfa::psfm(
    formula    = fml,
    model_name = model_name,
    data       = pdat,
    individual = id_var,
    PSopt      = pso,
    verbose    = FALSE
  )

  list(model = fit, data = work, formula = fml, model_name = model_name)
}

coef_table_psfm <- function(obj) {
  fit <- obj$model
  out <- fit$out %||% NULL
  if (!is.null(out)) {
    out <- as.data.frame(out)
    out$term <- rownames(out)
    rownames(out) <- NULL
    return(as_tibble(out))
  }
  tibble(term = names(fit$coefficients), estimate = unname(fit$coefficients))
}

te_table_psfm <- function(obj, label) {
  fit <- obj$model
  dat <- obj$data

  te <- NULL
  if (!is.null(fit$U)) {
    te <- as.numeric(fit$U)
  } else if (!is.null(fit$exp_u_hat)) {
    te <- as.numeric(fit$exp_u_hat)
  } else if (!is.null(fit$u_hat)) {
    te <- exp(-as.numeric(fit$u_hat))
  }

  if (is.null(te)) {
    return(dat %>%
      transmute(modelo = label, NCODI, anyo, .row_id, TE = NA_real_))
  }

  n <- min(length(te), nrow(dat))
  dat %>%
    slice(seq_len(n)) %>%
    transmute(modelo = label, NCODI, anyo, .row_id, TE = te[seq_len(n)])
}

write_model_bundle <- function(obj, label) {
  saveRDS(obj$model, file.path(OUT_DIR, "models", paste0(label, ".rds")))
  readr::write_csv(coef_table_psfm(obj), file.path(OUT_DIR, "tables", paste0(label, "_coef.csv")))
  readr::write_csv(te_table_psfm(obj, label), file.path(OUT_DIR, "tables", paste0(label, "_te.csv")))
}

# ------------------------------------------------------------------
# 3) Diseño A: cantidad total e intensidad diagnóstica
# ------------------------------------------------------------------
results <- list()

if ("A_CANTIDAD" %in% requested_models && !is.null(var_y_total)) {
  results$A_cantidad <- fit_psfm_safe(df, lhs = var_y_total)
  write_model_bundle(results$A_cantidad, "A_cantidad_sfa")
}
if ("A_INTENSIDAD" %in% requested_models && !is.null(var_y_idiag)) {
  results$A_intensidad <- fit_psfm_safe(df, lhs = var_y_idiag)
  write_model_bundle(results$A_intensidad, "A_intensidad_sfa")
}

z_terms_A1 <- c("z_Ddesc", "z_pctSNS", "z_DxSNS")
if (!is.null(var_ccaa)) {
  z_formula_rhs_A1 <- paste(c(z_terms_A1, paste0("factor(", var_ccaa, ")")), collapse = " + ")
} else {
  z_formula_rhs_A1 <- paste(z_terms_A1, collapse = " + ")
}

if ("A1_CANTIDAD" %in% requested_models && !is.null(var_y_total)) {
  results$A1_cantidad <- fit_psfm_safe(df, lhs = var_y_total, z_rhs = z_formula_rhs_A1, z_vars = z_terms_A1)
  write_model_bundle(results$A1_cantidad, "A1_cantidad_sfa")
}
if ("A1_INTENSIDAD" %in% requested_models && !is.null(var_y_idiag)) {
  results$A1_intensidad <- fit_psfm_safe(df, lhs = var_y_idiag, z_rhs = z_formula_rhs_A1, z_vars = z_terms_A1)
  write_model_bundle(results$A1_intensidad, "A1_intensidad_sfa")
}

# ------------------------------------------------------------------
# 4) Diseño B: quirúrgico vs médico
# ------------------------------------------------------------------
if ("B_QUIRURGICO" %in% requested_models && !is.null(var_y_q)) {
  results$B_quirurgico <- fit_psfm_safe(df, lhs = var_y_q)
  write_model_bundle(results$B_quirurgico, "B_quirurgico_sfa")
}
if ("B_MEDICO" %in% requested_models && !is.null(var_y_m)) {
  results$B_medico <- fit_psfm_safe(df, lhs = var_y_m)
  write_model_bundle(results$B_medico, "B_medico_sfa")
}

# ------------------------------------------------------------------
# 5) Diseño D: ODF
#    - ln(altQ) = normalizador
#    y_odf = - ln(altQ)
#    ratio = altM / altQ
# ------------------------------------------------------------------
if ("D_ODF" %in% requested_models && !is.null(var_altQ) && !is.null(var_altM) && !is.null(var_y_q)) {
  dfD <- df %>%
    filter(is.finite(.data[[var_altQ]]), is.finite(.data[[var_altM]]),
           .data[[var_altQ]] > 0, .data[[var_altM]] > 0) %>%
    mutate(
      y_odf = - .data[[var_y_q]],
      ln_ratio_outputs = safe_log(.data[[var_altM]] / .data[[var_altQ]])
    )

  if (!"ln_ratio_outputs" %in% frontier_terms) {
    frontier_terms_D <- c("ln_ratio_outputs", frontier_terms)
  } else {
    frontier_terms_D <- frontier_terms
  }

  x_formula_rhs_D <- paste(frontier_terms_D[frontier_terms_D %in% names(dfD)], collapse = " + ")

  fit_psfm_odf <- function(dat) {
    use_vars <- unique(c("y_odf", frontier_terms_D, z_terms, var_ccaa, "NCODI", "anyo", ".row_id"))
    use_vars <- use_vars[use_vars %in% names(dat)]

    work <- dat %>%
      select(all_of(use_vars)) %>%
      filter(!is.na(NCODI), !is.na(anyo)) %>%
      filter(is.finite(y_odf)) %>%
      filter(complete.cases(across(all_of(intersect(frontier_terms_D, names(.)))))) %>%
      filter(complete.cases(across(all_of(intersect(z_terms, names(.))))))

    pdat <- coerce_panel(work)
    fml <- make_formula("y_odf", x_formula_rhs_D, z_formula_rhs)

    fit <- sfa::psfm(
      formula    = fml,
      model_name = MODEL_NAME,
      data       = pdat,
      individual = "NCODI",
      PSopt      = FALSE,
      verbose    = FALSE
    )
    list(model = fit, data = work, formula = fml, model_name = MODEL_NAME)
  }

  results$D_odf <- fit_psfm_odf(dfD)
  write_model_bundle(results$D_odf, "D_odf_sfa")
}

# ------------------------------------------------------------------
# 6) Diseño C: panel largo hospital × servicio
#    Se crean dos paneles por hospital: quirúrgico y médico.
#    La unidad panel es NCODI_servicio.
# ------------------------------------------------------------------
if ("C_PANEL" %in% requested_models && !is.null(var_y_q) && !is.null(var_y_m)) {
  dfC <- df %>%
    select(any_of(c("NCODI", "anyo", ".row_id", var_ccaa, var_y_q, var_y_m,
                    frontier_terms, z_terms, var_ddesc, var_pctsns, var_shareq))) %>%
    pivot_longer(cols = all_of(c(var_y_q, var_y_m)),
                 names_to = "servicio",
                 values_to = "ln_output") %>%
    mutate(
      Cs = ifelse(servicio == var_y_q, 1, 0),
      panel_id = paste(NCODI, servicio, sep = "__")
    )

  if (!"z_Cs" %in% names(dfC))      dfC$z_Cs <- dfC$Cs
  if (!"z_DxCs" %in% names(dfC))    dfC$z_DxCs <- dfC[[var_ddesc]] * dfC$Cs
  if (!"z_ShareQxCs" %in% names(dfC) && !is.null(var_shareq)) {
    dfC$z_ShareQxCs <- dfC[[var_shareq]] * dfC$Cs
  }

  z_terms_C <- c("z_Ddesc", "z_Cs", "z_DxCs", "z_pctSNS")
  if ("z_ShareQ" %in% names(dfC))    z_terms_C <- c(z_terms_C, "z_ShareQ")
  if ("z_ShareQxCs" %in% names(dfC)) z_terms_C <- c(z_terms_C, "z_ShareQxCs")

  if (!is.null(var_ccaa) && var_ccaa %in% names(dfC)) {
    z_formula_rhs_C <- paste(c(z_terms_C, paste0("factor(", var_ccaa, ")")), collapse = " + ")
  } else {
    z_formula_rhs_C <- paste(z_terms_C, collapse = " + ")
  }

  use_vars <- unique(c("ln_output", frontier_terms, z_terms_C, var_ccaa, "panel_id", "anyo", ".row_id", "NCODI"))
  use_vars <- use_vars[use_vars %in% names(dfC)]

  work <- dfC %>%
    select(all_of(use_vars)) %>%
    filter(!is.na(panel_id), !is.na(anyo)) %>%
    filter(is.finite(ln_output)) %>%
    filter(complete.cases(across(all_of(intersect(frontier_terms, names(.)))))) %>%
    filter(complete.cases(across(all_of(intersect(z_terms_C, names(.))))))

  pdat <- coerce_panel(work, id_var = "panel_id", time_var = "anyo")
  fml  <- make_formula("ln_output", x_formula_rhs, z_formula_rhs_C)

  fitC <- sfa::psfm(
    formula    = fml,
    model_name = MODEL_NAME,
    data       = pdat,
    individual = "panel_id",
    PSopt      = FALSE,
    verbose    = FALSE
  )

  results$C_panel <- list(model = fitC, data = work, formula = fml, model_name = MODEL_NAME)
  write_model_bundle(results$C_panel, "C_panel_sfa")
}

# ------------------------------------------------------------------
# 7) Tabla resumen
# ------------------------------------------------------------------
summary_tbl <- imap_dfr(results, function(obj, nm) {
  fit <- obj$model
  tibble(
    modelo = nm,
    paquete = "sfa::psfm",
    model_name = fit$model_name %||% obj$model_name %||% NA_character_,
    formula = paste(deparse(obj$formula), collapse = " "),
    n = nrow(obj$data),
    te_media = mean((te_table_psfm(obj, nm)$TE), na.rm = TRUE)
  )
})

readr::write_csv(summary_tbl, file.path(OUT_DIR, "tables", "00_resumen_modelos_sfa.csv"))

cat("Estimación panel con {sfa} finalizada.\n")
cat("Salida en: ", OUT_DIR, "\n")
