panel_model_sequence <- function() {
  seq <- unique(c(
    Sys.getenv("PSFM_MODEL", "TRE"),
    Sys.getenv("PSFM_FALLBACK_MODEL", "GTRE")
  ))
  seq[nzchar(seq)]
}

panel_fit_psfm <- function(formula, data, label, individual = "NCODI") {
  stopifnot(requireNamespace("sfa", quietly = TRUE))

  if (!individual %in% names(data)) {
    stop(sprintf("[%s] No existe la variable panel '%s'", label, individual))
  }

  if ("anyo" %in% names(data)) {
    data <- data[order(data[[individual]], data[["anyo"]]), , drop = FALSE]
  } else {
    data <- data[order(data[[individual]]), , drop = FALSE]
  }

  maxit_bobyqa <- as.integer(Sys.getenv("PSFM_MAXIT_BOBYQA", "250"))
  maxit_psoptim <- as.integer(Sys.getenv("PSFM_MAXIT_PSOPTIM", "8"))
  maxit_optim <- as.integer(Sys.getenv("PSFM_MAXIT_OPTIM", "20"))
  use_hessian <- identical(
    tolower(Sys.getenv("PSFM_OPT_HESSIAN", "true")),
    "true"
  )

  err <- character(0)
  for (model_name in panel_model_sequence()) {
    message(sprintf("  [%s] Probando psfm model_name=%s", label, model_name))
    fit <- tryCatch(
      suppressWarnings(
        sfa::psfm(
          formula = formula,
          data = data,
          model_name = model_name,
          individual = individual,
          PSopt = FALSE,
          verbose = FALSE,
          maxit.bobyqa = maxit_bobyqa,
          maxit.psoptim = maxit_psoptim,
          maxit.optim = maxit_optim,
          optHessian = use_hessian
        )
      ),
      error = function(e) {
        err <<- c(err, sprintf("[%s/%s] %s", label, model_name, e$message))
        NULL
      }
    )

    if (!is.null(fit)) {
      fval <- tryCatch(as.numeric(fit[["opt"]][["fval"]]), error = function(e) NA_real_)
      message(sprintf("  [%s] OK model_name=%s fval=%.3f", label, model_name, fval))
      return(fit)
    }
  }

  stop(sprintf("[%s] Todos los intentos psfm fallaron:\n%s", label, paste(err, collapse = "\n")))
}

panel_fit_stats <- function(fit) {
  k <- nrow(fit[["out"]])
  n <- nrow(fit[["data"]])
  negloglik <- as.numeric(fit[["opt"]][["fval"]])
  loglik <- -negloglik
  aic <- 2 * k - 2 * loglik
  bic <- log(n) * k - 2 * loglik
  te_mean <- tryCatch(mean(exp(-fit[["U"]]), na.rm = TRUE), error = function(e) NA_real_)
  data.frame(
    logLik = loglik,
    AIC = aic,
    BIC = bic,
    n = n,
    TE_mean = te_mean,
    stringsAsFactors = FALSE
  )
}

panel_extract_ineff <- function(fit, z_vars = NULL, label) {
  out <- as.data.frame(fit[["out"]], stringsAsFactors = FALSE)
  rn <- rownames(out)
  intercept_idx <- which(rn %in% c("(Intercept)", "X.Intercept.", "X.Intercept..1"))
  if (length(intercept_idx) >= 2L) {
    block <- out[seq(from = intercept_idx[length(intercept_idx)], to = nrow(out)), , drop = FALSE]
    block$param <- rownames(block)
  } else {
    nz <- if (is.null(z_vars)) 1L else length(z_vars) + 1L
    block <- tail(out, nz)
    block$param <- c("(Intercept)", z_vars)
  }
  block$modelo <- label
  block$p_value <- 2 * (1 - pnorm(abs(block[["t-val"]])))
  block$sig <- ifelse(
    is.na(block$p_value), "NA",
    ifelse(block$p_value < 0.01, "***",
      ifelse(block$p_value < 0.05, "**",
        ifelse(block$p_value < 0.10, "*", "ns")
      )
    )
  )
  names(block)[names(block) == "par"] <- "coef"
  names(block)[names(block) == "st_err"] <- "se"
  names(block)[names(block) == "t-val"] <- "t_stat"
  block[, c("modelo", "param", "coef", "se", "t_stat", "p_value", "sig")]
}

panel_export_bundle <- function(fits, z_map, out_prefix) {
  tabs <- list()
  stats <- list()

  for (nm in names(fits)) {
    fit <- fits[[nm]]
    if (is.null(fit)) next
    tabs[[nm]] <- panel_extract_ineff(fit, z_map[[nm]], nm)
    s <- panel_fit_stats(fit)
    s$modelo <- nm
    stats[[nm]] <- s[, c("modelo", "logLik", "AIC", "BIC", "n", "TE_mean")]
  }

  coef_tab <- do.call(rbind, tabs)
  stats_tab <- do.call(rbind, stats)
  merged <- merge(coef_tab, stats_tab, by = "modelo", all.x = TRUE)

  write.table(
    merged,
    paste0(out_prefix, "_coeficientes.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )
  write.table(
    stats_tab,
    paste0(out_prefix, "_resumen.tsv"),
    sep = "\t",
    row.names = FALSE,
    quote = FALSE,
    fileEncoding = "UTF-8"
  )

  invisible(list(coef = coef_tab, stats = stats_tab, merged = merged))
}
