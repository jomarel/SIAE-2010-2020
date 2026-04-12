# ============================================================
# 10_descriptivos_sfa.R
# Estimation sample descriptives (Table 5.2 equivalent)
# ============================================================

config_path <- if (file.exists("scripts/00_config.R")) "scripts/00_config.R" else "00_config.R"
source(config_path)

library(dplyr)

message("\n=== Script 10: Descriptivos SFA (muestra estimación) ===")

load(file.path(INT_DIR, "df_sfa.RData"))
message("df_sfa: ", nrow(df_sfa), " x ", ncol(df_sfa))

# Use D_desc (coalesce CNH -> SIAE, excluding SIAE-public-without-CNH)
# as per existing construction in script 06/09

# Estimation sample: Design A filters
df_est <- df_sfa %>%
  filter(
    es_agudo == 1,
    !anyo %in% 2020:2022,
    altTotal_bruto > 100,
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ln_L_total_c), !is.na(ln_K_camas_c),
    !is.na(ln_i_diag)
  )

message(sprintf("Estimation sample: %d obs, %d hospitals", nrow(df_est), n_distinct(df_est$NCODI)))
message(sprintf("  D_desc=0 (centralised): %d", sum(df_est$D_desc == 0)))
message(sprintf("  D_desc=1 (decentralised): %d", sum(df_est$D_desc == 1)))

# Table 5.2: by D_desc
desc_fn <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  c(mean = mean(x), sd = sd(x), median = median(x), n = length(x))
}

vars_desc <- c("altTotal_pond", "i_diag", "L_total", "K_camas", "pct_sns", "ShareQ")
labels <- c("altTotal_pond/1000", "idiag", "L_total", "K_camas", "pct_sns", "ShareQ")

rows <- list()
for (i in seq_along(vars_desc)) {
  v <- vars_desc[i]
  divisor <- if (v == "altTotal_pond") 1000 else 1

  d0 <- df_est[[v]][df_est$D_desc == 0] / divisor
  d1 <- df_est[[v]][df_est$D_desc == 1] / divisor

  s0 <- desc_fn(d0)
  s1 <- desc_fn(d1)

  tt <- tryCatch(t.test(d0, d1), error = function(e) list(p.value = NA))

  rows[[i]] <- data.frame(
    variable = labels[i],
    mean_D0 = round(s0["mean"], 3),
    sd_D0 = round(s0["sd"], 3),
    median_D0 = round(s0["median"], 3),
    n_D0 = s0["n"],
    mean_D1 = round(s1["mean"], 3),
    sd_D1 = round(s1["sd"], 3),
    median_D1 = round(s1["median"], 3),
    n_D1 = s1["n"],
    t_pvalue = round(tt$p.value, 4),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

tabla <- do.call(rbind, rows)
cat("\n=== TABLE 5.2 EQUIVALENT ===\n")
print(tabla)

write.csv(tabla, file.path(INT_DIR, "tabla_descriptivos_ddesc.csv"), row.names = FALSE)
message("\nSaved: tabla_descriptivos_ddesc.csv")

message("\n=== Script 10 completado ===")
