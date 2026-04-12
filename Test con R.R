## =============================================================
## DESIGN B — diagnostico y fix
## =============================================================

# Probar filtros más bajos para restaurar heterogeneidad
for (umbral in c(200, 300, 500)) {
  sb <- df_sfa %>% filter(
    es_agudo==1, !anyo %in% EXCL_YEARS,
    altTotal_bruto >= MIN_ALT_TOTAL,
    altQ_bruto >= umbral, altM_bruto >= umbral,
    is.finite(ln_altQ_pond), is.finite(ln_altM_pond),
    !is.na(D_desc), !is.na(pct_sns), !is.na(ShareQ),
    !is.na(ccaa_cod), !is.na(L_quirur), !is.na(L_medico),
    !is.na(K_camas), !is.na(K_tech_index), !is.na(K_quirofanos)
  ) %>% mutate(
    ln_L_quirur_raw = safe_log1(to_num(L_quirur)),
    ln_L_medico_raw = safe_log1(to_num(L_medico)),
    ln_K_qx_raw     = safe_log1(to_num(K_quirofanos)),
    ln_K_camas_raw  = safe_log1(to_num(K_camas)),
    ln_K_tech_raw   = safe_log1(to_num(K_tech_index))
  )
  # Centrar
  sb <- sb %>% mutate(
    ln_L_quirur_c  = ln_L_quirur_raw - mean(ln_L_quirur_raw),
    ln_K_qx_c      = ln_K_qx_raw    - mean(ln_K_qx_raw),
    ln_L_medico_c  = ln_L_medico_raw - mean(ln_L_medico_raw),
    ln_K_camas_c   = ln_K_camas_raw  - mean(ln_K_camas_raw),
    ln_K_tech_c    = ln_K_tech_raw   - mean(ln_K_tech_raw),
    ln_L_quirur_c2 = 0.5*ln_L_quirur_c^2,
    ln_K_qx_c2     = 0.5*ln_K_qx_c^2,
    ln_L_medico_c2 = 0.5*ln_L_medico_c^2,
    ln_K_camas_c2  = 0.5*ln_K_camas_c^2
  )
  
  # OLS sin términos cuadráticos de K_tech (simplificado)
  ols_BQ_s <- lm(ln_altQ_pond ~ ln_L_quirur_c + ln_K_qx_c +
    ln_K_tech_c + ln_L_quirur_c2 + ln_K_qx_c2 +
    trend + trend2 + d_cluster2+d_cluster3+d_cluster4+d_cluster5,
    data=sb)
  ols_BM_s <- lm(ln_altM_pond ~ ln_L_medico_c + ln_K_camas_c +
    ln_K_tech_c + ln_L_medico_c2 + ln_K_camas_c2 +
    trend + trend2 + d_cluster2+d_cluster3+d_cluster4+d_cluster5,
    data=sb)
  
  cat(sprintf(
    "umbral=%-3d N=%-5d skew_BQ=%6.3f skew_BM=%6.3f\n",
    umbral, nrow(sb),
    moments::skewness(residuals(ols_BQ_s)),
    moments::skewness(residuals(ols_BM_s))))
}

## =============================================================
## DESIGN D — fix: usar i_diag per alta (normalizado)
## =============================================================

sample_D2 <- df_sfa %>%
  filter(
    es_agudo==1, !anyo %in% EXCL_YEARS,
    altTotal_bruto >= MIN_ALT_TOTAL,
    !is.na(D_desc), !is.na(pct_sns), !is.na(desc_pago),
    !is.na(ccaa_cod), !is.na(L_total),
    !is.na(K_camas), !is.na(K_tech_index),
    !is.na(altTotal_pond), altTotal_pond > 0,
    !is.na(i_diag_sum), i_diag_sum > 0
  ) %>%
  mutate(
    # i_diag per alta ponderada (comparable entre hospitales)
    i_diag_per_alta = to_num(i_diag_sum) / to_num(altTotal_pond),
    ln_i_per_alta   = log(i_diag_per_alta),
    ln_Q_total      = log(to_num(altTotal_pond)),
    # ratio normalizado
    ln_ratio_IQ     = ln_i_per_alta - ln_Q_total,
    neg_ln_Q_total  = -ln_Q_total
  ) %>%
  filter(is.finite(ln_i_per_alta), is.finite(ln_Q_total)) %>%
  mutate(
    # winsorizar ratio
    ln_ratio_IQ_w = pmin(pmax(ln_ratio_IQ,
      quantile(ln_ratio_IQ, WINSOR_P, na.rm=TRUE)),
      quantile(ln_ratio_IQ, 1-WINSOR_P, na.rm=TRUE)),
    ln_L_total_raw = safe_log1(to_num(L_total)),
    ln_K_camas_raw = safe_log1(to_num(K_camas)),
    ln_K_tech_raw  = safe_log1(to_num(K_tech_index))
  )

sample_D2 <- sample_D2 %>% mutate(
  ln_L_total_c  = ln_L_total_raw - mean(ln_L_total_raw, na.rm=TRUE),
  ln_K_camas_c  = ln_K_camas_raw - mean(ln_K_camas_raw, na.rm=TRUE),
  ln_K_tech_c   = ln_K_tech_raw  - mean(ln_K_tech_raw,  na.rm=TRUE),
  ln_L_total_c2 = 0.5*ln_L_total_c^2,
  ln_K_camas_c2 = 0.5*ln_K_camas_c^2
)

ols_D2 <- lm(neg_ln_Q_total ~ ln_ratio_IQ_w +
  ln_L_total_c + ln_K_camas_c + ln_K_tech_c +
  ln_L_total_c2 + ln_K_camas_c2 +
  trend + trend2 + d_cluster2+d_cluster3+d_cluster4+d_cluster5,
  data=sample_D2)

cat("D2 N:", nrow(sample_D2), "\n")
cat("D2 skewness OLS:", round(moments::skewness(residuals(ols_D2)), 4), "\n")
cat("i_diag_per_alta stats:\n")
print(summary(sample_D2$i_diag_per_alta))