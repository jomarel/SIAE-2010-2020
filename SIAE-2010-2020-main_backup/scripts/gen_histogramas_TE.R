## gen_histogramas_TE.R  (v2 — usa teJLMS de sfaR)
suppressPackageStartupMessages(library(sfaR))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))

dir_data   <- "G:/Mi unidad/SIAE 2010-2020/data_intermediate"
dir_output <- "G:/Mi unidad/SIAE 2010-2020/docs/Textos/Integrado"

# ── Cargar datos ──────────────────────────────────────────────────────────────
load(file.path(dir_data, "df_sfa.RData"))

mA_Q <- readRDS(file.path(dir_data, "sfa_modeloTotal_disenioA.rds"))
mA_I <- readRDS(file.path(dir_data, "sfa_modeloI_disenioA.rds"))

# ── Extraer teJLMS ────────────────────────────────────────────────────────────
TE_Q <- sfaR::efficiencies(mA_Q)$teJLMS
TE_I <- sfaR::efficiencies(mA_I)$teJLMS

cat(sprintf("TE_Q: n=%d | media=%.4f | sd=%.4f | min=%.4f | max=%.4f\n",
            length(TE_Q), mean(TE_Q,na.rm=T), sd(TE_Q,na.rm=T),
            min(TE_Q,na.rm=T), max(TE_Q,na.rm=T)))
cat(sprintf("TE_I: n=%d | media=%.4f | sd=%.4f | min=%.4f | max=%.4f\n",
            length(TE_I), mean(TE_I,na.rm=T), sd(TE_I,na.rm=T),
            min(TE_I,na.rm=T), max(TE_I,na.rm=T)))

# ── Construir data.frame ──────────────────────────────────────────────────────
n_obs <- min(length(TE_Q), nrow(df_sfa))

df_plot <- data.frame(
  TE_Q   = TE_Q[seq_len(n_obs)],
  TE_I   = TE_I[seq_len(n_obs)],
  D_desc = df_sfa$D_desc[seq_len(n_obs)],
  anyo   = df_sfa$anyo[seq_len(n_obs)]
) |> filter(!is.na(TE_Q) & !is.na(TE_I))

# Etiqueta D_desc (0/1 → texto)
df_plot$estructura <- factor(
  ifelse(df_plot$D_desc == 0, "Centralizado", "Descentralizado"),
  levels = c("Centralizado", "Descentralizado")
)

col_c <- "#2166ac"
col_d <- "#d6604d"

# ── [1] Histograma TE cantidad — muestra completa ─────────────────────────────
p1 <- ggplot(df_plot, aes(x = TE_Q)) +
  geom_histogram(binwidth = 0.03, fill = col_c, color = "white", alpha = 0.85) +
  scale_x_continuous("Eficiencia técnica", limits = c(0, 1)) +
  ylab("Frecuencia") +
  theme_bw(base_size = 11)

ggsave(file.path(dir_output, "hist_TE_cantidad_global.pdf"),
       p1, width = 5, height = 3.5, device = "pdf")
cat("Guardado: hist_TE_cantidad_global.pdf\n")

# ── [2] Histograma TE intensidad — muestra completa ───────────────────────────
p2 <- ggplot(df_plot, aes(x = TE_I)) +
  geom_histogram(binwidth = 0.03, fill = col_d, color = "white", alpha = 0.85) +
  scale_x_continuous("Eficiencia técnica", limits = c(0, 1)) +
  ylab("Frecuencia") +
  theme_bw(base_size = 11)

ggsave(file.path(dir_output, "hist_TE_intensidad_global.pdf"),
       p2, width = 5, height = 3.5, device = "pdf")
cat("Guardado: hist_TE_intensidad_global.pdf\n")

# ── [3] Densidades TE cantidad por estructura ─────────────────────────────────
p3 <- ggplot(df_plot |> filter(!is.na(estructura)),
             aes(x = TE_Q, fill = estructura, color = estructura)) +
  geom_density(alpha = 0.35, bw = "SJ") +
  scale_x_continuous("Eficiencia técnica", limits = c(0, 1)) +
  ylab("Densidad") +
  scale_fill_manual(values  = c(col_c, col_d)) +
  scale_color_manual(values = c(col_c, col_d)) +
  labs(fill = "Estructura", color = "Estructura") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(dir_output, "hist_TE_cantidad_desc.pdf"),
       p3, width = 5, height = 3.5, device = "pdf")
cat("Guardado: hist_TE_cantidad_desc.pdf\n")

# ── [4] Densidades TE intensidad por estructura ───────────────────────────────
p4 <- ggplot(df_plot |> filter(!is.na(estructura)),
             aes(x = TE_I, fill = estructura, color = estructura)) +
  geom_density(alpha = 0.35, bw = "SJ") +
  scale_x_continuous("Eficiencia técnica", limits = c(0, 1)) +
  ylab("Densidad") +
  scale_fill_manual(values  = c(col_c, col_d)) +
  scale_color_manual(values = c(col_c, col_d)) +
  labs(fill = "Estructura", color = "Estructura") +
  theme_bw(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(file.path(dir_output, "hist_TE_intensidad_desc.pdf"),
       p4, width = 5, height = 3.5, device = "pdf")
cat("Guardado: hist_TE_intensidad_desc.pdf\n")

# ── [5] Evolución temporal de TE media por año ────────────────────────────────
df_anyo <- df_plot |>
  group_by(anyo) |>
  summarise(media = mean(TE_Q, na.rm=TRUE),
            sd    = sd(TE_Q,   na.rm=TRUE),
            .groups = "drop") |>
  mutate(covid = anyo %in% 2020:2022,
         lo = pmax(media - sd, 0),
         hi = pmin(media + sd, 1))

df_main  <- filter(df_anyo, !covid)
df_covid <- filter(df_anyo,  covid)

# Unir extremos para que la línea no quede discontinua visualmente
# (conectar 2019-2020 y 2022-2023 con segmento gris)
df_puente1 <- bind_rows(
  filter(df_main,  anyo == 2019),
  filter(df_covid, anyo == 2020)
)
df_puente2 <- bind_rows(
  filter(df_covid, anyo == 2022),
  filter(df_main,  anyo == 2023)
)

p5 <- ggplot() +
  geom_ribbon(data = df_main,
              aes(x = anyo, ymin = lo, ymax = hi),
              fill = col_c, alpha = 0.15) +
  geom_line(data = df_main,
            aes(x = anyo, y = media), color = col_c, linewidth = 0.8) +
  geom_point(data = df_main,
             aes(x = anyo, y = media), color = col_c, size = 1.8) +
  geom_line(data = df_covid,
            aes(x = anyo, y = media),
            color = "grey55", linetype = "dashed", linewidth = 0.7) +
  geom_point(data = df_covid,
             aes(x = anyo, y = media), color = "grey55", size = 1.5) +
  geom_line(data = df_puente1,
            aes(x = anyo, y = media), color = "grey55", linetype = "dashed", linewidth = 0.5) +
  geom_line(data = df_puente2,
            aes(x = anyo, y = media), color = "grey55", linetype = "dashed", linewidth = 0.5) +
  scale_x_continuous("Año", breaks = seq(2010, 2023, 2)) +
  scale_y_continuous("Eficiencia técnica media", limits = c(0, 1)) +
  theme_bw(base_size = 11)

ggsave(file.path(dir_output, "hist_TE_anyo.pdf"),
       p5, width = 6, height = 3.5, device = "pdf")
cat("Guardado: hist_TE_anyo.pdf\n")

# ── Estadísticos para el reporte ──────────────────────────────────────────────
cat("\n── t-test TE cantidad: centralizado vs. descentralizado ──────\n")
tQ <- t.test(TE_Q ~ estructura, data = df_plot |> filter(!is.na(estructura)))
cat(sprintf("  Centralizado: %.4f  Descentralizado: %.4f  p=%.4f\n",
            tQ$estimate[1], tQ$estimate[2], tQ$p.value))

cat("── t-test TE intensidad: centralizado vs. descentralizado ────\n")
tI <- t.test(TE_I ~ estructura, data = df_plot |> filter(!is.na(estructura)))
cat(sprintf("  Centralizado: %.4f  Descentralizado: %.4f  p=%.4f\n",
            tI$estimate[1], tI$estimate[2], tI$p.value))

cat("\n── Medias por grupo ──────────────────────────────────────────\n")
df_plot |> filter(!is.na(estructura)) |>
  group_by(estructura) |>
  summarise(n=n(), TE_Q=round(mean(TE_Q),4), sd_Q=round(sd(TE_Q),4),
            TE_I=round(mean(TE_I),4), sd_I=round(sd(TE_I),4)) |>
  print()

cat("\n── Evolución temporal (medias anuales TE_Q) ──────────────────\n")
print(df_anyo[, c("anyo","media","sd","covid")])

cat("\nFin.\n")
