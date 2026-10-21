
# Carga de datos
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)
str(datos)
summary(datos)



# ============================================================
# ANÁLISIS DESCRIPTIVO COMPLETO - DATOS DE BIOCIDAS vs Leuconostoc
# ============================================================

# ============================================================
# 0. PAQUETES NECESARIOS
# ============================================================
paquetes <- c("tidyverse", "psych", "ggpubr", "patchwork",
              "skimr", "car", "nortest", "moments")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# Asegurar que Tratamiento sea factor
datos$Tratamiento <- as.factor(datos$Tratamiento)
cat("\nNiveles de Tratamiento:\n"); print(levels(datos$Tratamiento))
cat("\nTabla de frecuencias:\n"); print(table(datos$Tratamiento, useNA = "ifany"))

# ============================================================
# 1. VARIABLES DE INTERÉS
# ============================================================
vars <- c("Brix", "Pol", "POL...JUGO", "Pza",
          "X..Az.Reductor", "Dextrana.PPM", "temperatura")

# Subconjunto de trabajo
datos_sub <- datos[, c("Tratamiento", vars)]

# ============================================================
# 2. ESTADÍSTICOS DESCRIPTIVOS GLOBALES
# ============================================================
cat("\n===== RESUMEN GLOBAL =====\n")
print(psych::describe(datos_sub[, vars],
                      fast = TRUE,
                      skew = TRUE,
                      ranges = TRUE))

# Con skimr (más visual)
print(skim(datos_sub))

# ============================================================
# 3. ESTADÍSTICOS POR TRATAMIENTO
# ============================================================
resumen_por_trat <- datos_sub %>%
  pivot_longer(-Tratamiento, names_to = "Variable", values_to = "Valor") %>%
  group_by(Tratamiento, Variable) %>%
  summarise(
    n       = sum(!is.na(Valor)),
    media   = mean(Valor, na.rm = TRUE),
    sd      = sd(Valor, na.rm = TRUE),
    se      = sd / sqrt(n),
    min     = min(Valor, na.rm = TRUE),
    Q1      = quantile(Valor, 0.25, na.rm = TRUE),
    mediana = median(Valor, na.rm = TRUE),
    Q3      = quantile(Valor, 0.75, na.rm = TRUE),
    max     = max(Valor, na.rm = TRUE),
    CV      = 100 * sd / media,
    .groups = "drop"
  )

cat("\n===== DESCRIPTIVOS POR TRATAMIENTO =====\n")
print(as.data.frame(resumen_por_trat), digits = 3)

# ============================================================
# 4. HISTOGRAMAS POR VARIABLE
# ============================================================
graficar_hist <- function(var) {
  ggplot(datos_sub, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density), fill = Tratamiento),
                   bins = 15, alpha = 0.6, position = "identity",
                   color = "white") +
    geom_density(aes(color = Tratamiento), linewidth = 1) +
    facet_wrap(~ Tratamiento, ncol = 1) +
    labs(title = paste("Histograma -", var),
         x = var, y = "Densidad") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
}

# Genera y guarda cada histograma
for (v in vars) {
  p <- graficar_hist(v)
  print(p)
  ggsave(paste0("hist_", gsub("[^A-Za-z]", "_", v), ".png"),
         p, width = 7, height = 7, dpi = 150)
}

# ============================================================
# 5. BOXPLOTS POR TRATAMIENTO (comparación)
# ============================================================
for (v in vars) {
  p <- ggplot(datos_sub, aes(x = Tratamiento, y = .data[[v]], fill = Tratamiento)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21) +
    geom_jitter(width = 0.15, alpha = 0.5, size = 1.8) +
    stat_summary(fun = mean, geom = "point", shape = 23,
                 size = 3, fill = "white", color = "black") +
    labs(title = paste("Boxplot -", v), y = v, x = "Tratamiento") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  print(p)
  ggsave(paste0("box_", gsub("[^A-Za-z]", "_", v), ".png"),
         p, width = 7, height = 5, dpi = 150)
}

# ============================================================
# 6. GRÁFICO DE DENSIDAD SUPERPUESTO (comparación directa)
# ============================================================
for (v in vars) {
  p <- ggplot(datos_sub, aes(x = .data[[v]], fill = Tratamiento, color = Tratamiento)) +
    geom_density(alpha = 0.4, linewidth = 1) +
    labs(title = paste("Densidad -", v), x = v, y = "Densidad") +
    theme_minimal(base_size = 12)
  print(p)
}

# ============================================================
# 7. MATRIZ DE CORRELACIÓN
# ============================================================
# 7a. Correlaciones globales
M <- cor(datos_sub[, vars], use = "pairwise.complete.obs")
cat("\n===== MATRIZ DE CORRELACIONES (Pearson) =====\n")
print(round(M, 3))

# 7b. Heatmap
corr_long <- as.data.frame(as.table(M))
p_corr <- ggplot(corr_long, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Freq, 2)), size = 3.5) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Matriz de correlación", x = "", y = "", fill = "r") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(p_corr)

# 7c. Por tratamiento (opcional)
for (t in levels(datos_sub$Tratamiento)) {
   cat("\n-- Correlación para", t, "--\n")
   print(round(cor(subset(datos_sub, Tratamiento == t)[, vars],
                   use = "pairwise.complete.obs"), 3))
 }

# ============================================================
# 8. PRUEBAS DE NORMALIDAD POR GRUPO
# ============================================================
cat("\n===== SHAPIRO-WILK POR TRATAMIENTO =====\n")
normalidad <- datos_sub %>%
  pivot_longer(-Tratamiento, names_to = "Variable", values_to = "Valor") %>%
  group_by(Tratamiento, Variable) %>%
  summarise(
    n       = sum(!is.na(Valor)),
    W       = shapiro.test(Valor)$statistic,
    p_value = shapiro.test(Valor)$p.value,
    .groups = "drop"
  ) %>%
  mutate(Normal = ifelse(p_value > 0.05, "Sí", "No"))

print(as.data.frame(normalidad), digits = 4)

# ============================================================
# 9. QQ-PLOTS POR TRATAMIENTO
# ============================================================
for (v in vars) {
  p <- ggplot(datos_sub, aes(sample = .data[[v]], color = Tratamiento)) +
    stat_qq(size = 1.5, alpha = 0.7) +
    stat_qq_line(linewidth = 0.8) +
    facet_wrap(~ Tratamiento, scales = "free") +
    labs(title = paste("QQ-Plot -", v),
         x = "Cuantiles teóricos", y = "Cuantiles muestrales") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "none")
  print(p)
}

# ============================================================
# 10. RESUMEN TEXTUAL AUTOMÁTICO
# ============================================================
cat("\n==================================================\n")
cat("RESUMEN GENERAL\n")
cat("==================================================\n")
cat("Total observaciones:", nrow(datos_sub), "\n")
cat("Tratamientos:", nlevels(datos_sub$Tratamiento), "\n")
cat("Variables analizadas:", length(vars), "\n\n")

for (v in vars) {
  cat("----", v, "----\n")
  cat("Global: media =", round(mean(datos_sub[[v]], na.rm = TRUE), 3),
      "| sd =", round(sd(datos_sub[[v]], na.rm = TRUE), 3),
      "| CV =", round(100 * sd(datos_sub[[v]], na.rm = TRUE) /
                        mean(datos_sub[[v]], na.rm = TRUE), 2), "%\n")
  print(tapply(datos_sub[[v]], datos_sub$Tratamiento,
               function(x) round(c(media = mean(x, na.rm = TRUE),
                                   sd = sd(x, na.rm = TRUE)), 3)))
  cat("\n")
}



# ============================================================
# 11. TABLA RESUMEN (MEDIA ± SD, CV) Y GRÁFICOS DE LÍNEAS DE ERROR
# ============================================================

# --- 11a. Tabla resumen larga (base para todo lo demás) ---
tabla_resumen <- datos_sub %>%
  pivot_longer(-Tratamiento, names_to = "Variable", values_to = "Valor") %>%
  group_by(Tratamiento, Variable) %>%
  summarise(
    n     = sum(!is.na(Valor)),
    media = mean(Valor, na.rm = TRUE),
    sd    = sd(Valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CV       = 100 * sd / media,
    media_sd = sprintf("%.2f ± %.2f", media, sd),
    CV_pct   = sprintf("%.2f%%", CV)
  )

# --- 11b. Tabla ancha: media ± sd por tratamiento y variable ---
tabla_media_sd <- tabla_resumen %>%
  select(Tratamiento, Variable, media_sd) %>%
  pivot_wider(names_from = Tratamiento, values_from = media_sd) %>%
  arrange(Variable)

# --- 11c. Tabla ancha: CV (%) por tratamiento y variable ---
tabla_cv <- tabla_resumen %>%
  select(Tratamiento, Variable, CV_pct) %>%
  pivot_wider(names_from = Tratamiento, values_from = CV_pct) %>%
  arrange(Variable)

cat("\n==================================================\n")
cat("TABLA RESUMEN: MEDIA ± SD POR TRATAMIENTO\n")
cat("==================================================\n")
print(as.data.frame(tabla_media_sd))

cat("\n==================================================\n")
cat("TABLA RESUMEN: CV (%) POR TRATAMIENTO\n")
cat("==================================================\n")
print(as.data.frame(tabla_cv))


# --- 11d. Gráficos individuales: líneas de error verticales negras ---
for (v in vars) {
  p <- ggplot(tabla_resumen %>% filter(Variable == v),
              aes(x = Tratamiento, y = media)) +
    geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                  width = 0.15, color = "black", linewidth = 1) +
    geom_point(size = 3.5, color = "black") +
    labs(title = paste("Media ± SD -", v),
         x = "Tratamiento", y = v) +
    theme_minimal(base_size = 12)
  print(p)
  ggsave(paste0("errorbar_", gsub("[^A-Za-z]", "_", v), ".png"),
         p, width = 7, height = 5, dpi = 150)
}

# --- 11e. Versión facetada: todas las variables en un solo panel ---
p_errorbar_all <- ggplot(tabla_resumen,
                         aes(x = Tratamiento, y = media)) +
  geom_errorbar(aes(ymin = media - sd, ymax = media + sd),
                width = 0.15, color = "black", linewidth = 0.9) +
  geom_point(size = 2.8, color = "black") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  labs(title = "Media ± SD por tratamiento para cada variable",
       x = "Tratamiento", y = "Media ± SD") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_errorbar_all)
ggsave("errorbar_facetado.png", p_errorbar_all,
       width = 12, height = 7, dpi = 150)


# ============================================================
# 12. TENDENCIAS Y RELACIONES ENTRE VARIABLES
# ============================================================

# ------------------------------------------------------------
# 12.0. Preparar eje temporal
# ------------------------------------------------------------
# Convertir a numérico si es necesario (ajusta según el formato de tus datos)
datos$Hora.num <- suppressWarnings(as.numeric(datos$Hora.de.Analisis))
datos$Patio.num <- suppressWarnings(as.numeric(datos$Tiempo.Muestreo.Patio))

# Detectar cuál usar como eje temporal
if (all(is.na(datos$Hora.num))) {
  cat("Hora.de.Analisis no es numérica. Se usará Tiempo.Muestreo.Patio como eje X.\n")
  eje_x <- "Tiempo.Muestreo.Patio"
} else {
  eje_x <- "Hora.de.Analisis"
}
cat("Eje X para tendencias:", eje_x, "\n")

# Variable de agrupación temporal para promedios
datos$EjeTiempo <- datos[[eje_x]]

# ------------------------------------------------------------
# 12.1. TENDENCIA DE CADA VARIABLE EN EL TIEMPO
#       (promedio por tratamiento ± SE, líneas)
# ------------------------------------------------------------
for (v in vars) {
  resumen_t <- datos %>%
    filter(!is.na(EjeTiempo), !is.na(.data[[v]])) %>%
    group_by(Tratamiento, EjeTiempo) %>%
    summarise(
      media = mean(.data[[v]], na.rm = TRUE),
      sd    = sd(.data[[v]], na.rm = TRUE),
      n     = sum(!is.na(.data[[v]])),
      .groups = "drop"
    ) %>%
    mutate(se = sd / sqrt(n))
  
  p <- ggplot(resumen_t, aes(x = EjeTiempo, y = media,
                             color = Tratamiento, group = Tratamiento)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2.5) +
    geom_errorbar(aes(ymin = media - se, ymax = media + se),
                  width = 0.2, linewidth = 0.6) +
    labs(title = paste("Tendencia -", v),
         x = eje_x, y = v, color = "Tratamiento") +
    theme_minimal(base_size = 12)
  
  print(p)
  ggsave(paste0("tendencia_", gsub("[^A-Za-z]", "_", v), ".png"),
         p, width = 8, height = 5, dpi = 150)
}

# ------------------------------------------------------------
# 12.2. PANEL FACETADO CON TODAS LAS TENDENCIAS
# ------------------------------------------------------------
resumen_todo <- datos %>%
  filter(!is.na(EjeTiempo)) %>%
  pivot_longer(all_of(vars), names_to = "Variable", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  group_by(Tratamiento, EjeTiempo, Variable) %>%
  summarise(
    media = mean(Valor, na.rm = TRUE),
    sd    = sd(Valor, na.rm = TRUE),
    n     = sum(!is.na(Valor)),
    .groups = "drop"
  ) %>%
  mutate(se = sd / sqrt(n))

p_tend_all <- ggplot(resumen_todo,
                     aes(x = EjeTiempo, y = media,
                         color = Tratamiento, group = Tratamiento)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.15, linewidth = 0.5) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  labs(title = "Tendencias temporales por tratamiento",
       x = eje_x, y = "Media ± SE", color = "Tratamiento") +
  theme_minimal(base_size = 11)

print(p_tend_all)


# ------------------------------------------------------------
# 12.3. RELACIÓN ENTRE VARIABLES - GLOBAL Y POR TRATAMIENTO
#       (matriz de dispersión con correlación)
# ------------------------------------------------------------
# Global
pairs_global <- GGally::ggpairs(
  datos_sub %>% select(all_of(vars)),
  title = "Matriz de dispersión - Global"
)
print(pairs_global)


# Por tratamiento
for (t in levels(datos_sub$Tratamiento)) {
  sub_t <- datos_sub %>% filter(Tratamiento == t) %>% select(all_of(vars))
  p <- GGally::ggpairs(sub_t, title = paste("Matriz de dispersión -", t))
  print(p)
  ggsave(paste0("pairs_", gsub("[^A-Za-z0-9]", "_", t), ".png"),
         p, width = 10, height = 10, dpi = 130)
}

# ------------------------------------------------------------
# 12.4. SCATTERPLOTS CLAVE COMPARANDO TRATAMIENTOS
#       (pares más relevantes: Dextrana vs otras, Brix vs Pol, etc.)
# ------------------------------------------------------------
pares_clave <- list(
  c("Dextrana.PPM", "Brix"),
  c("Dextrana.PPM", "Pol"),
  c("Dextrana.PPM", "POL...JUGO"),
  c("Dextrana.PPM", "X..Az.Reductor"),
  c("Brix", "Pol"),
  c("Brix", "POL...JUGO"),
  c("Pol", "POL...JUGO")
)

for (par in pares_clave) {
  xv <- par[1]; yv <- par[2]
  p <- ggplot(datos, aes(x = .data[[xv]], y = .data[[yv]],
                         color = Tratamiento)) +
    geom_point(size = 2, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
    labs(title = paste(yv, "vs", xv, "por tratamiento"),
         x = xv, y = yv, color = "Tratamiento") +
    theme_minimal(base_size = 12)
  print(p)
  ggsave(paste0("scatter_", gsub("[^A-Za-z]", "_", xv), "_vs_",
                gsub("[^A-Za-z]", "_", yv), ".png"),
         p, width = 7, height = 5, dpi = 150)
}

# ------------------------------------------------------------
# 12.5. CORRELACIONES POR TRATAMIENTO (tabla resumida)
# ------------------------------------------------------------
cat("\n==================================================\n")
cat("CORRELACIONES (Pearson) POR TRATAMIENTO\n")
cat("==================================================\n")
for (t in levels(datos_sub$Tratamiento)) {
  cat("\n---- Tratamiento:", t, "----\n")
  M_t <- cor(subset(datos_sub, Tratamiento == t)[, vars],
             use = "pairwise.complete.obs")
  print(round(M_t, 3))
}

# ------------------------------------------------------------
# 12.6. HEATMAP DE CORRELACIONES POR TRATAMIENTO
# ------------------------------------------------------------
for (t in levels(datos_sub$Tratamiento)) {
  M_t <- cor(subset(datos_sub, Tratamiento == t)[, vars],
             use = "pairwise.complete.obs")
  df_m <- as.data.frame(as.table(M_t))
  p <- ggplot(df_m, aes(Var1, Var2, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Freq, 2)), size = 3.2) +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                         midpoint = 0, limits = c(-1, 1)) +
    labs(title = paste("Correlaciones -", t),
         x = "", y = "", fill = "r") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  print(p)
}

# ------------------------------------------------------------
# 12.7. INTERPRETACIÓN DESCRIPTIVA AUTOMÁTICA
#       (tendencia general + correlaciones fuertes)
# ------------------------------------------------------------
cat("\n==================================================\n")
cat("INTERPRETACIÓN DESCRIPTIVA\n")
cat("==================================================\n")

# Tendencia temporal: pendiente por tratamiento (regresión simple)
cat("\n-- Tendencia temporal (pendiente media por unidad de", eje_x, ") --\n")
for (v in vars) {
  cat("\nVariable:", v, "\n")
  for (t in levels(datos$Tratamiento)) {
    sub_t <- datos %>% filter(Tratamiento == t, !is.na(EjeTiempo), !is.na(.data[[v]]))
    if (nrow(sub_t) > 2) {
      mod <- lm(sub_t[[v]] ~ sub_t$EjeTiempo)
      pend <- coef(mod)[2]
      dir  <- ifelse(pend > 0, "↑ creciente", ifelse(pend < 0, "↓ decreciente", "→ estable"))
      cat(sprintf("  %-15s pendiente = %+.4f  %s\n", t, pend, dir))
    }
  }
}

# Correlaciones fuertes globales (|r| > 0.6)
cat("\n-- Correlaciones fuertes (|r| > 0.6) globales --\n")
M <- cor(datos_sub[, vars], use = "pairwise.complete.obs")
idx <- which(abs(M) > 0.6 & upper.tri(M), arr.ind = TRUE)
if (nrow(idx) > 0) {
  for (i in seq_len(nrow(idx))) {
    cat(sprintf("  %s ~ %s : r = %+.3f\n",
                rownames(M)[idx[i,1]], colnames(M)[idx[i,2]], M[idx[i,1], idx[i,2]]))
  }
} else {
  cat("  No se detectaron correlaciones fuertes (|r| > 0.6).\n")
}




# ============================================================
# 13. RELACIÓN DEXTRANA vs RESTO DE VARIABLES POR TRATAMIENTO
#     (representación clara de la tendencia)
# ============================================================

# Variables respuesta (todas menos Dextrana)
vars_resp <- setdiff(vars, "Dextrana.PPM")
var_x     <- "Dextrana.PPM"

# ------------------------------------------------------------
# 13.1. TABLA DE CORRELACIONES: Dextrana vs cada variable, por tratamiento
# ------------------------------------------------------------
cor_dextrana <- map_dfr(levels(datos_sub$Tratamiento), function(t) {
  sub_t <- subset(datos_sub, Tratamiento == t)
  map_dfr(vars_resp, function(v) {
    ok <- complete.cases(sub_t[[var_x]], sub_t[[v]])
    if (sum(ok) < 3) return(tibble())
    ct <- cor.test(sub_t[[var_x]][ok], sub_t[[v]][ok], method = "pearson")
    tibble(
      Tratamiento = t,
      Variable    = v,
      r           = unname(ct$estimate),
      p_value     = ct$p.value,
      n           = sum(ok)
    )
  })
}) %>%
  mutate(
    Signif = ifelse(p_value < 0.15, "*", "ns"),
    Direccion = case_when(
      r >  0.3 ~ "Positiva",
      r < -0.3 ~ "Negativa",
      TRUE     ~ "Débil"
    )
  )

cat("\n==================================================\n")
cat("CORRELACIÓN DEXTRANA vs RESTO DE VARIABLES POR TRATAMIENTO\n")
cat("==================================================\n")
print(as.data.frame(cor_dextrana %>%
                      select(Tratamiento, Variable, r, p_value, Signif) %>%
                      mutate(r = round(r, 3), p_value = signif(p_value, 3))),
      row.names = FALSE)

# ------------------------------------------------------------
# 13.2. GRÁFICO DE BARRAS HORIZONTALES: r(Dextrana, Variable)
#       facetado por tratamiento -> se ve el patrón de un vistazo
# ------------------------------------------------------------
p_barras <- ggplot(cor_dextrana,
                   aes(x = r, y = fct_reorder(Variable, r),
                       fill = Direccion)) +
  geom_col(color = "grey20", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_text(aes(label = paste0(round(r, 2), Signif)),
            hjust = ifelse(cor_dextrana$r >= 0, -0.15, 1.15),
            size = 3.2, color = "black") +
  scale_fill_manual(values = c("Positiva" = "#2166AC",
                               "Negativa" = "#B2182B",
                               "Débil"    = "grey75")) +
  facet_wrap(~ Tratamiento, ncol = 2) +
  coord_cartesian(xlim = c(-1.15, 1.15)) +
  labs(title = "Relación de Dextrana con cada variable, por tratamiento",
       subtitle = "Barras = coeficiente de correlación de Pearson (r). * = p<0.15",
       x = "r (Dextrana vs Variable)", y = "", fill = "Dirección") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(p_barras)


# ------------------------------------------------------------
# 13.3. SCATTERPLOTS: Dextrana (X) vs cada variable (Y)
#       facetado por variable y coloreado por tratamiento
#       -> muestra la tendencia real, no solo r
# ------------------------------------------------------------
datos_long_dex <- datos_sub %>%
  pivot_longer(all_of(vars_resp),
               names_to = "Variable", values_to = "Valor") %>%
  filter(!is.na(Valor), !is.na(.data[[var_x]]))

p_scatter <- ggplot(datos_long_dex,
                    aes(x = .data[[var_x]], y = Valor,
                        color = Tratamiento, fill = Tratamiento)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 0.9) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  labs(title = "Tendencia de cada variable frente a Dextrana por tratamiento",
       subtitle = "Líneas = regresión lineal simple con banda de confianza 95%",
       x = "Dextrana (PPM)", y = "Valor", color = "Tratamiento") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(p_scatter)


# ------------------------------------------------------------
# 13.4. SLOPE CHART: cambio de r(Dextrana, Variable) entre tratamientos
#       -> muestra la "caída" o "subida" de la relación
# ------------------------------------------------------------
p_slope <- ggplot(cor_dextrana,
                  aes(x = Tratamiento, y = r,
                      group = Variable, color = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_text(data = cor_dextrana %>% filter(Tratamiento == levels(datos_sub$Tratamiento)[1]),
            aes(label = Variable), hjust = 1.1, size = 3, show.legend = FALSE) +
  geom_text(data = cor_dextrana %>% filter(Tratamiento == levels(datos_sub$Tratamiento)[nlevels(datos_sub$Tratamiento)]),
            aes(label = Variable), hjust = -0.1, size = 3, show.legend = FALSE) +
  coord_cartesian(ylim = c(-1, 1), clip = "off") +
  labs(title = "Cambio de la relación Dextrana–Variable según tratamiento",
       subtitle = "r de Pearson. Valores positivos: suben juntas; negativos: una sube y la otra baja",
       x = "", y = "r (Dextrana vs Variable)", color = "Variable") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.margin = margin(5.5, 90, 5.5, 90))

print(p_slope)


# ------------------------------------------------------------
# 13.5. HEATMAP SOLO DE DEXTRANA vs RESTO, POR TRATAMIENTO
#       -> más limpio que el heatmap completo
# ------------------------------------------------------------
p_heat_dex <- ggplot(cor_dextrana,
                     aes(x = Tratamiento, y = Variable, fill = r)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f%s", r, Signif)),
            size = 3.4, color = "black") +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  labs(title = "Correlación Dextrana vs cada variable, por tratamiento",
       x = "", y = "") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

print(p_heat_dex)


# ------------------------------------------------------------
# 13.6. INTERPRETACIÓN DESCRIPTIVA AUTOMÁTICA
# ------------------------------------------------------------
cat("\n==================================================\n")
cat("INTERPRETACIÓN DESCRIPTIVA: DEXTRANA vs RESTO\n")
cat("==================================================\n")

for (t in levels(datos_sub$Tratamiento)) {
  cat("\n----", t, "----\n")
  sub <- cor_dextrana %>% filter(Tratamiento == t) %>% arrange(desc(abs(r)))
  for (i in seq_len(nrow(sub))) {
    cat(sprintf("  Dextrana ~ %-14s r = %+.2f  (%s, %s)\n",
                sub$Variable[i], sub$r[i], sub$Direccion[i], sub$Signif[i]))
  }
  neg <- sum(sub$Direccion == "Negativa")
  pos <- sum(sub$Direccion == "Positiva")
  cat(sprintf("  >> %d relación(es) negativa(s), %d positiva(s), %d débil(es)\n",
              neg, pos, nrow(sub) - neg - pos))
}







# ============================================================
# ANOVA + FISHER LSD (DMS) — igual que Infostat
# ============================================================

library(dplyr); library(tidyr); library(gt); library(car)
if (!require("agricolae", quietly = TRUE)) install.packages("agricolae")
library(agricolae)

# ---- Datos (elige uno) ----
# Opción A: TODOS los datos (como Infostat)
datos_analisis <- datos
# Opción B: sin Tiempo=4
# datos_analisis <- datos %>% filter(Tiempo.Muestreo.Patio != "4") %>% droplevels()

datos_analisis$Tratamiento <- factor(datos_analisis$Tratamiento)

# ---- Contrastes sum-to-zero ----
old <- options(contrasts = c("contr.sum", "contr.poly"))
on.exit(options(old), add = TRUE)

alpha     <- 0.15
vars_resp <- c("Brix","Pol","POL...JUGO","Pza",
               "X..Az.Reductor","Dextrana.PPM")
niveles_trat <- levels(datos_analisis$Tratamiento)

# ============================================================
# BUCLE POR VARIABLE
# ============================================================
resumen_anova <- list()
resumen_tabla <- list()

for (v in vars_resp) {
  
  df <- datos_analisis %>%
    select(y = all_of(v), Tratamiento) %>%
    filter(!is.na(y))
  
  m  <- aov(y ~ Tratamiento, data = df)
  an <- car::Anova(m, type = "III")
  p_trat <- an["Tratamiento", "Pr(>F)"]
  
  cat("\n\n=========================================================\n")
  cat("VARIABLE:", v, "  (p =", sprintf("%.4f", p_trat), ")\n")
  cat("=========================================================\n")
  print(an)
  
  # ---- Fisher LSD (DMS) con agricolae ----
  lsd <- tryCatch(
    agricolae::LSD.test(m, "Tratamiento",
                        alpha = alpha,
                        console = TRUE,
                        group = TRUE),
    error = function(e) NULL
  )
  
  if (!is.null(lsd)) {
    grupos <- lsd$groups
    grupos$Tratamiento <- rownames(grupos)
    
    medias <- df %>%
      group_by(Tratamiento) %>%
      summarise(Media = mean(y, na.rm = TRUE), .groups = "drop") %>%
      mutate(Tratamiento = as.character(Tratamiento))
    
    tab_v <- medias %>%
      left_join(grupos %>% select(Tratamiento, Letras = groups),
                by = "Tratamiento") %>%
      mutate(Celda = sprintf("%.2f %s", round(Media, 2), Letras)) %>%
      select(Tratamiento, Celda) %>%
      rename(!!v := Celda)
  } else {
    tab_v <- tibble(Tratamiento = niveles_trat,
                    !!v := NA_character_)
  }
  
  resumen_anova[[v]] <- tibble(
    Variable      = v,
    p_Tratamiento = p_trat,
    Signif        = dplyr::case_when(
      p_trat < 0.01 ~ "***",
      p_trat < 0.05 ~ "**",
      p_trat < 0.15 ~ "*",
      TRUE          ~ "ns"
    )
  )
  resumen_tabla[[v]] <- tab_v
}

# ============================================================
# TABLA FINAL
# ============================================================
tabla_medias <- purrr::reduce(resumen_tabla, full_join, by = "Tratamiento") %>%
  mutate(Tratamiento = factor(Tratamiento, levels = niveles_trat)) %>%
  arrange(Tratamiento) %>%
  mutate(Tratamiento = as.character(Tratamiento))

tabla_pvalores <- bind_rows(resumen_anova) %>%
  mutate(p_Tratamiento = sprintf("%.4f", p_Tratamiento)) %>%
  select(Variable, p_Tratamiento, Signif)

cat("\n\n=========================================================\n")
cat("TABLA: MEDIA Y GRUPOS FISHER LSD (α = 0.15)\n")
cat("=========================================================\n")
print(as.data.frame(tabla_medias), row.names = FALSE)
print(as.data.frame(tabla_pvalores), row.names = FALSE)

tabla_medias %>%
  gt() %>%
  tab_header(
    title    = "ANOVA simple — efecto de Tratamiento",
    subtitle = "Media real y grupos Fisher LSD / DMS (α = 0.15) — estilo Infostat"
  ) %>%
  cols_label(Tratamiento = "Tratamiento") %>%
  print()





# ============================================================
# PARTE 2. INTERACCIÓN Tratamiento × Tiempo.Muestreo.Patio
# - Excluye tiempos con < 2 tratamientos (evita aliasing)
# - Fisher LSD dentro de cada tiempo
# - Dextrana.PPM: la MEJOR es la de MENOR media (letra "a")
# - Resto: la MEJOR es la de MAYOR media (letra "a")
# ============================================================

# ---- Variables con sentido inverso (menor = mejor) ----
vars_menor_mejor <- c("Dextrana.PPM")

# ---- Datos para la interacción ----
conteo_tt <- table(datos_analisis$Tratamiento,
                   datos_analisis$Tiempo.Muestreo.Patio)
tiempos_validos <- colnames(conteo_tt)[colSums(conteo_tt > 0) >= 2]

datos_int <- datos_analisis %>%
  filter(as.character(Tiempo.Muestreo.Patio) %in% tiempos_validos) %>%
  droplevels()

datos_int$Tratamiento           <- factor(datos_int$Tratamiento)
datos_int$Tiempo.Muestreo.Patio <- factor(datos_int$Tiempo.Muestreo.Patio)

niveles_tiem_int <- levels(datos_int$Tiempo.Muestreo.Patio)
niveles_trat_int <- levels(datos_int$Tratamiento)

# ============================================================
# Helper: Fisher LSD con dirección configurable
# invertir = FALSE → mayor media = "a" (default)
# invertir = TRUE  → menor media = "a"
# ============================================================
letras_fisher <- function(sub, alpha = 0.15, invertir = FALSE) {
  
  sub <- sub %>% filter(!is.na(y))
  trat_presentes <- unique(as.character(sub$Tratamiento))
  
  if (length(trat_presentes) < 2) {
    return(tibble(Tratamiento = trat_presentes, Letras = "a"))
  }
  
  # --- Paso 1: obtener matriz de no-diferencia (p > alpha → no difieren) ---
  no_dif <- NULL
  
  # Intento 1: agricolae
  res_ag <- tryCatch({
    m_t <- aov(y ~ Tratamiento, data = sub)
    lsd <- agricolae::LSD.test(m_t, "Tratamiento", alpha = alpha,
                               console = FALSE, group = TRUE)
    g <- lsd$groups
    col_let <- intersect(c("groups","Letters","letras"), names(g))[1]
    if (is.na(col_let)) stop("no letters")
    # Convertir letras a matriz de no-diferencia: misma letra = no difieren
    trats <- as.character(rownames(g))
    letras_raw <- as.character(g[[col_let]])
    nd <- matrix(FALSE, length(trats), length(trats),
                 dimnames = list(trats, trats))
    diag(nd) <- TRUE
    for (i in seq_along(trats)) for (j in seq_along(trats)) {
      if (i != j) {
        li <- strsplit(letras_raw[i], "")[[1]]
        lj <- strsplit(letras_raw[j], "")[[1]]
        if (length(intersect(li, lj)) > 0) nd[i, j] <- TRUE
      }
    }
    list(trats = trats, nd = nd)
  }, error = function(e) NULL)
  
  # Intento 2: emmeans
  if (is.null(res_ag)) {
    res_ag <- tryCatch({
      m_t <- aov(y ~ Tratamiento, data = sub)
      emm <- emmeans::emmeans(m_t, ~ Tratamiento)
      pw  <- as.data.frame(pairs(emm, adjust = "none"))
      trats <- levels(droplevels(sub$Tratamiento))
      nd <- matrix(FALSE, length(trats), length(trats),
                   dimnames = list(trats, trats))
      diag(nd) <- TRUE
      for (i in seq_len(nrow(pw))) {
        partes <- strsplit(gsub(" ", "", pw$contrast[i]), "-")[[1]]
        a <- partes[1]; b <- partes[2]
        if (a %in% trats && b %in% trats && pw$p.value[i] > alpha) {
          nd[a, b] <- TRUE; nd[b, a] <- TRUE
        }
      }
      list(trats = trats, nd = nd)
    }, error = function(e) NULL)
  }
  
  # Fallback: todo "a"
  if (is.null(res_ag)) {
    return(tibble(Tratamiento = trat_presentes, Letras = "a"))
  }
  
  # --- Paso 2: reasignar letras según la dirección deseada ---
  meds <- sub %>%
    group_by(Tratamiento) %>%
    summarise(Media = mean(y, na.rm = TRUE), .groups = "drop") %>%
    mutate(Tratamiento = as.character(Tratamiento))
  
  # Ordenar según dirección: si invertir → ascendente; si no → descendente
  if (invertir) {
    meds <- meds %>% arrange(Media)         # menor primero
  } else {
    meds <- meds %>% arrange(desc(Media))   # mayor primero
  }
  trats_orden <- meds$Tratamiento
  
  nd <- res_ag$nd
  # reordenar matriz
  trats_ok <- intersect(trats_orden, rownames(nd))
  nd <- nd[trats_ok, trats_ok, drop = FALSE]
  
  # Algoritmo codicioso de asignación de letras
  letras <- setNames(rep("", length(trats_ok)), trats_ok)
  alf    <- letters
  i <- 1
  while (any(letras == "")) {
    pend <- names(letras)[letras == ""]
    base <- pend[1]
    miem <- setdiff(pend[nd[base, pend]], names(letras)[letras != ""])
    letras[miem] <- paste0(letras[miem], alf[i]); i <- i + 1
  }
  
  tibble(Tratamiento = trats_ok, Letras = unname(letras[trats_ok]))
}

# ============================================================
# BUCLE POR VARIABLE
# ============================================================
resumen_inter_medias <- list()
resumen_inter_pval   <- list()

for (v in vars_resp) {
  
  invertir <- v %in% vars_menor_mejor
  
  df <- datos_int %>%
    select(y = all_of(v), Tratamiento, Tiempo.Muestreo.Patio) %>%
    filter(!is.na(y)) %>%
    droplevels()
  
  m2  <- aov(y ~ Tratamiento * Tiempo.Muestreo.Patio, data = df)
  an2 <- car::Anova(m2, type = "III")
  p_inter <- an2["Tratamiento:Tiempo.Muestreo.Patio", "Pr(>F)"]
  
  cat("\n\n=========================================================\n")
  cat("INTERACCIÓN —", v,
      if (invertir) "  [MENOR = MEJOR, 'a' a la media más baja]" else "",
      "  (p =", sprintf("%.4f", p_inter), ")\n")
  cat("=========================================================\n")
  print(an2)
  
  # ---- Medias reales ----
  medias_ct <- df %>%
    group_by(Tratamiento, Tiempo.Muestreo.Patio) %>%
    summarise(Media = mean(y, na.rm = TRUE), .groups = "drop") %>%
    mutate(Tratamiento = as.character(Tratamiento),
           Tiempo.Muestreo.Patio = as.character(Tiempo.Muestreo.Patio))
  
  # ---- Letras Fisher LSD dentro de cada tiempo ----
  letras_lista <- list()
  for (tt in niveles_tiem_int) {
    sub <- df %>%
      filter(as.character(Tiempo.Muestreo.Patio) == tt) %>%
      droplevels()
    if (nrow(sub) == 0) next
    
    letras_tt <- tryCatch(
      letras_fisher(sub, alpha = alpha, invertir = invertir),
      error = function(e) NULL)
    if (is.null(letras_tt) || nrow(letras_tt) == 0) next
    
    letras_lista[[tt]] <- letras_tt %>%
      mutate(Tiempo.Muestreo.Patio = tt)
  }
  letras_all <- if (length(letras_lista) > 0) bind_rows(letras_lista) else
    tibble(Tratamiento = character(), Tiempo.Muestreo.Patio = character(),
           Letras = character())
  
  # ---- Tabla ancha ----
  tab_v <- medias_ct %>%
    left_join(letras_all,
              by = c("Tratamiento","Tiempo.Muestreo.Patio")) %>%
    mutate(
      Media_txt = sprintf("%.2f", round(Media, 2)),
      Letras    = ifelse(is.na(Letras), "", Letras),
      Celda     = trimws(paste(Media_txt, Letras))
    ) %>%
    select(Tratamiento, Tiempo.Muestreo.Patio, Celda) %>%
    pivot_wider(names_from = Tiempo.Muestreo.Patio, values_from = Celda)
  
  for (tt in niveles_tiem_int)
    if (!tt %in% names(tab_v)) tab_v[[tt]] <- NA_character_
  
  tab_v <- tab_v %>%
    select(Tratamiento, all_of(niveles_tiem_int)) %>%
    mutate(Tratamiento = factor(Tratamiento, levels = niveles_trat_int)) %>%
    arrange(Tratamiento) %>%
    mutate(Tratamiento = as.character(Tratamiento))
  
  resumen_inter_medias[[v]] <- tab_v
  resumen_inter_pval[[v]] <- tibble(
    Variable      = v,
    p_Interaccion = p_inter,
    Signif        = dplyr::case_when(
      p_inter < 0.01 ~ "***",
      p_inter < 0.05 ~ "**",
      p_inter < 0.15 ~ "*",
      TRUE           ~ "ns"))
}

# ============================================================
# IMPRIMIR UNA TABLA POR VARIABLE
# ============================================================
for (v in vars_resp) {
  
  tab_v <- resumen_inter_medias[[v]]
  p_row <- resumen_inter_pval[[v]]
  invertir <- v %in% vars_menor_mejor
  
  cat("\n\n=========================================================\n")
  cat("INTERACCIÓN Tratamiento × Tiempo —", v, "\n")
  cat("=========================================================\n")
  print(as.data.frame(tab_v), row.names = FALSE)
  cat(sprintf("p(Interacción) = %.4f %s\n",
              p_row$p_Interaccion, p_row$Signif))
  
  subtitulo <- if (invertir) {
    "Cada celda: media real + letra Fisher LSD (α = 0.15). En esta variable MENOR = MEJOR ('a' = mejor)."
  } else {
    "Cada celda: media real + letra Fisher LSD (α = 0.15). Letras iguales dentro de una columna = no difieren."
  }
  
  tab_v %>%
    gt() %>%
    tab_header(
      title    = md(paste0("**", v, "** — Interacción Tratamiento × Tiempo",
                           if (invertir) " (menor = mejor)" else "")),
      subtitle = subtitulo
    ) %>%
    cols_label(Tratamiento = "Tratamiento") %>%
    tab_footnote(footnote = sprintf(
      "p(Interacción) = %.4f %s   (α = 0.15; * p<0.15, ** p<0.05, *** p<0.01)",
      p_row$p_Interaccion, p_row$Signif)) %>%
    tab_options(table.font.size = 12) %>%
    print()
}

# ============================================================
# TABLA RESUMEN: p-valor de interacción por variable
# ============================================================
tabla_p_inter <- bind_rows(resumen_inter_pval) %>%
  mutate(p_Interaccion = sprintf("%.4f", p_Interaccion))

cat("\n\n=== p-valores de la interacción Tratamiento × Tiempo ===\n")
print(as.data.frame(tabla_p_inter), row.names = FALSE)





# ============================================================
# GRÁFICO DE LÍNEAS CON LETRAS FISHER LSD Y P-VALOR DE INTERACCIÓN
# - Un panel por variable
# - Letras Fisher LSD sobre cada punto
# - p(Interacción) en el subtítulo de cada panel
# - Dextrana.PPM: se indica "menor = mejor"
# ============================================================

library(ggplot2); library(dplyr); library(tidyr)

# ---- Reconstruir dataset largo desde los objetos ya generados ----
# (usa resumen_inter_medias, resumen_inter_pval, datos_int, vars_resp, alpha)

graficos_lista <- list()

for (v in vars_resp) {
  
  invertir <- v %in% vars_menor_mejor
  
  # Medias reales por Tratamiento × Tiempo
  df <- datos_int %>%
    select(y = all_of(v), Tratamiento, Tiempo.Muestreo.Patio) %>%
    filter(!is.na(y))
  
  medias_ct <- df %>%
    group_by(Tratamiento, Tiempo.Muestreo.Patio) %>%
    summarise(Media = mean(y, na.rm = TRUE), .groups = "drop")
  
  # Recuperar letras desde la tabla ya calculada en la parte 2
  tab_v <- resumen_inter_medias[[v]]  # Tratamiento | t1 | t2 | ...
  letras_long <- tab_v %>%
    pivot_longer(-Tratamiento,
                 names_to  = "Tiempo.Muestreo.Patio",
                 values_to = "Celda") %>%
    mutate(
      Letras = sub("^[0-9\\.]+\\s*", "", Celda),  # quitar la media
      Letras = ifelse(Letras == "", NA, Letras)
    )
  
  p_inter <- resumen_inter_pval[[v]]$p_Interaccion
  signif  <- resumen_inter_pval[[v]]$Signif
  
  df_plot <- medias_ct %>%
    mutate(Tiempo.Muestreo.Patio = as.character(Tiempo.Muestreo.Patio)) %>%
    left_join(letras_long,
              by = c("Tratamiento","Tiempo.Muestreo.Patio"))
  
  # Orden del eje X según el orden natural de los tiempos
  df_plot$Tiempo.Muestreo.Patio <- factor(df_plot$Tiempo.Muestreo.Patio,
                                          levels = niveles_tiem_int)
  
  # ---- Subtítulo con p-valor ----
  subt <- sprintf("p(Interacción) = %.4f %s", p_inter, signif)
  if (invertir) subt <- paste(subt, "— menor = mejor (a = mejor)")
  
  # ---- Gráfico ----
  p <- ggplot(df_plot, aes(x = Tiempo.Muestreo.Patio, y = Media,
                           color = Tratamiento, group = Tratamiento)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_text(aes(label = Letras),
              vjust = -1.3, size = 3.6, fontface = "bold",
              show.legend = FALSE) +
    labs(title    = v,
         subtitle = subt,
         x = "Tiempo de patio",
         y = v,
         color = "Tratamiento") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          plot.title    = element_text(face = "bold"),
          plot.subtitle = element_text(size = 10))
  
  graficos_lista[[v]] <- p
}

# ============================================================
# 1. UN GRÁFICO POR VARIABLE (se guardan como PNG)
# ============================================================
for (v in vars_resp) {
  print(graficos_lista[[v]])
  ggsave(paste0("interaccion_", gsub("[^A-Za-z]", "_", v), ".png"),
         graficos_lista[[v]],
         width = 8, height = 5, dpi = 150)
}

# ============================================================
# 2. PANEL FACETADO CON TODAS LAS VARIABLES
# ============================================================
library(patchwork)
p_all <- wrap_plots(graficos_lista, ncol = 3) +
  plot_annotation(
    title = "Interacción Tratamiento × Tiempo de patio por variable",
    subtitle = "Letras Fisher LSD (α = 0.15) sobre cada punto. Letras iguales dentro de un tiempo = no difieren."
  )
print(p_all)



library(ggplot2); library(dplyr)

for (v in vars_resp) {
  invertir <- v %in% vars_menor_mejor
  p_inter  <- resumen_inter_pval[[v]]$p_Interaccion
  signif   <- resumen_inter_pval[[v]]$Signif
  
  df_v <- letras_long_all %>% filter(Variable == v)
  df_v$Tiempo.Muestreo.Patio <- factor(df_v$Tiempo.Muestreo.Patio,
                                       levels = niveles_tiem_int)
  df_v$Tratamiento <- factor(df_v$Tratamiento, levels = niveles_trat_int)
  
  subt <- sprintf("p(Interacción) = %.4f %s", p_inter, signif)
  if (invertir) subt <- paste(subt, "— menor = mejor")
  
  p <- ggplot(df_v, aes(x = Tiempo.Muestreo.Patio, y = Media,
                        fill = Tratamiento)) +
    geom_col(position = position_dodge(width = 0.8),
             width = 0.75, color = "grey30") +
    geom_text(aes(label = Letras),
              position = position_dodge(width = 0.8),
              vjust = -0.4, size = 3.2, fontface = "bold") +
    scale_fill_brewer(palette = "Set2") +
    labs(title = v, subtitle = subt,
         x = "Tiempo de patio", y = v, fill = "Tratamiento") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"))
  
  print(p)
  ggsave(paste0("barras_", gsub("[^A-Za-z]", "_", v), ".png"),
         p, width = 8, height = 5, dpi = 150)
}



# ============================================================
# GRÁFICO FACETADO — Efecto del Tratamiento por variable
# - En variables ns (p ≥ 0.15): TODOS los tratamientos llevan "a"
# - Solo se genera el panel facetado (sin gráficos individuales)
# ============================================================

library(dplyr); library(tidyr); library(ggplot2)
library(patchwork); library(car); library(agricolae)

colores <- c(
  "Bioxida 0.50%" = "#E41A1C",
  "Bioxida 1.00%" = "#4DAF4A",
  "Digra LM-25M"  = "#377EB8",
  "Testigo"       = "#984EA3"
)

etiquetas_cortas <- c(
  "Bioxida 0.50%" = "Biox\n0.50%",
  "Bioxida 1.00%" = "Biox\n1.00%",
  "Digra LM-25M"  = "Digra\nLM-25M",
  "Testigo"       = "Testigo"
)

datos_graf <- datos_analisis
datos_graf$Tratamiento <- factor(datos_graf$Tratamiento)
niveles_trat <- levels(datos_graf$Tratamiento)

# ============================================================
# 1. Medias + letras Fisher LSD + p-valor por variable
# ============================================================
resumen <- list()

for (v in vars_resp) {
  
  df <- datos_graf %>%
    select(y = all_of(v), Tratamiento) %>%
    filter(!is.na(y))
  
  m  <- aov(y ~ Tratamiento, data = df)
  an <- car::Anova(m, type = "III")
  p_trat <- an["Tratamiento", "Pr(>F)"]
  
  # --- Si NO es significativo (p >= 0.15), todos van con "a" ---
  if (p_trat >= alpha) {
    letras <- tibble(Tratamiento = niveles_trat, Letras = "a")
  } else {
    lsd <- tryCatch(
      agricolae::LSD.test(m, "Tratamiento", alpha = alpha,
                          console = FALSE, group = TRUE),
      error = function(e) NULL)
    if (!is.null(lsd)) {
      g <- lsd$groups
      g$Tratamiento <- rownames(g)
      letras <- tibble(Tratamiento = as.character(g$Tratamiento),
                       Letras      = as.character(g$groups))
    } else {
      letras <- tibble(Tratamiento = niveles_trat, Letras = "a")
    }
  }
  
  medias <- df %>%
    group_by(Tratamiento) %>%
    summarise(Media = mean(y, na.rm = TRUE), .groups = "drop") %>%
    mutate(Tratamiento = as.character(Tratamiento)) %>%
    left_join(letras, by = "Tratamiento")
  
  resumen[[v]] <- list(
    p      = p_trat,
    signif = dplyr::case_when(
      p_trat < 0.01 ~ "***",
      p_trat < 0.05 ~ "**",
      p_trat < 0.15 ~ "*",
      TRUE          ~ "ns"
    ),
    medias = medias
  )
}

# ============================================================
# 2. Función de un gráfico por variable
# ============================================================
grafico_var <- function(v) {
  
  info     <- resumen[[v]]
  invertir <- v %in% vars_menor_mejor
  ns       <- info$p >= alpha
  
  medias <- info$medias
  medias$Tratamiento <- factor(medias$Tratamiento, levels = niveles_trat)
  
  # Orden por media
  if (invertir) medias <- medias %>% arrange(desc(Media))
  else          medias <- medias %>% arrange(Media)
  medias$Tratamiento <- factor(medias$Tratamiento, levels = medias$Tratamiento)
  medias$x <- seq_len(nrow(medias))
  
  # Marcar mejor solo si es significativo
  mejor_idx <- if (invertir) which.min(medias$Media) else which.max(medias$Media)
  medias$Mejor <- FALSE
  if (!ns) medias$Mejor[mejor_idx] <- TRUE
  
  subt <- sprintf("p(Tratamiento) = %.4f %s", info$p, info$signif)
  if (invertir) subt <- paste(subt, "— menor = mejor")
  if (ns)       subt <- paste(subt, "— todos iguales (a)")
  
  # Rango Y amplio
  rango <- range(medias$Media, na.rm = TRUE)
  span  <- diff(rango)
  y_min <- rango[1] - 0.45 * span
  y_max <- rango[2] + 0.35 * span
  
  ggplot(medias, aes(x = x, y = Media,
                     color = Tratamiento, group = 1)) +
    geom_line(linewidth = 1.1, color = "grey40", alpha = 0.7) +
    # Halo solo si hay un mejor
    geom_point(data = medias %>% filter(Mejor),
               size = 9, shape = 21, fill = NA, stroke = 2, alpha = 0.35) +
    geom_point(size = 4.2, shape = 21, fill = "white", stroke = 1.4) +
    geom_point(data = medias %>% filter(Mejor),
               size = 4.6, shape = 21, fill = "black", stroke = 1.4) +
    geom_text(aes(label = Letras),
              vjust = -1.6, size = 5.5, fontface = "bold",
              color = "black", show.legend = FALSE) +
    geom_text(aes(label = sprintf("%.2f", Media)),
              vjust = 3.4, size = 4, color = "grey25",
              show.legend = FALSE) +
    scale_color_manual(values = colores) +
    scale_x_continuous(
      breaks = medias$x,
      labels = etiquetas_cortas[as.character(medias$Tratamiento)],
      expand = expansion(add = 0.6)
    ) +
    coord_cartesian(ylim = c(y_min, y_max)) +
    labs(title    = v,
         subtitle = subt,
         x = NULL, y = v, color = NULL) +
    theme_bw(base_size = 15) +
    theme(
      legend.position  = "none",
      plot.title       = element_text(face = "bold", size = 17),
      plot.subtitle    = element_text(size = 12, color = "grey25"),
      axis.title       = element_text(face = "bold", size = 13),
      axis.text.x      = element_text(size = 10, face = "bold",
                                      lineheight = 0.9),
      axis.text.y      = element_text(size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey92"),
      plot.margin      = margin(10, 15, 10, 10)
    )
}

# ============================================================
# 3. SOLO PANEL FACETADO
# ============================================================
graficos_tend <- lapply(vars_resp, grafico_var)
names(graficos_tend) <- vars_resp

p_all <- wrap_plots(graficos_tend, ncol = 3) +
  plot_annotation(
    title    = "Efecto del Tratamiento por variable",
    subtitle = paste0("Letras Fisher LSD (α = 0.15). ",
                      "Punto relleno = mejor tratamiento ",
                      "(para Dextrana.PPM: menor = mejor). ",
                      "Variables ns: todos los tratamientos comparten 'a'."),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12, color = "grey25")
    )
  )

print(p_all)



# ============================================================
# CURVA DEL DIFERENCIAL RESPECTO AL TESTIGO
# - Solo Tratamiento (promedio sobre tiempos de patio)
# - Eje X: variables (agrupadas por tipo)
# - Eje Y: diferencial relativo (%) vs Testigo
# - Una curva por tratamiento
# - Con etiquetas de valores
# ============================================================

library(dplyr); library(tidyr); library(ggplot2); library(ggrepel)

# ---- Configuración ----
vars_analisis <- c("Brix","Pol","POL...JUGO","Pza",
                   "X..Az.Reductor","Dextrana.PPM")
trat_control  <- "Testigo"

colores_trat <- c(
  "Bioxida 0.50%" = "#E41A1C",
  "Bioxida 1.00%" = "#4DAF4A",
  "Digra LM-25M"  = "#377EB8"
)

formas_trat <- c(
  "Bioxida 0.50%" = 16,
  "Bioxida 1.00%" = 17,
  "Digra LM-25M"  = 15
)

# ---- Datos ----
datos_dif <- datos_analisis
datos_dif$Tratamiento <- factor(datos_dif$Tratamiento)
niveles_trat <- setdiff(levels(datos_dif$Tratamiento), trat_control)

# ============================================================
# 1. Medias por Variable × Tratamiento (promediando el tiempo)
# ============================================================
medias_long <- datos_dif %>%
  pivot_longer(all_of(vars_analisis),
               names_to = "Variable", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  group_by(Variable, Tratamiento) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    SE    = sd(Valor, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

medias_testigo <- medias_long %>%
  filter(Tratamiento == trat_control) %>%
  select(Variable, Media_ref = Media, SE_ref = SE)

# ============================================================
# 2. Diferencial relativo (%) vs Testigo
# ============================================================
diferenciales <- medias_long %>%
  filter(Tratamiento != trat_control) %>%
  left_join(medias_testigo, by = "Variable") %>%
  mutate(
    Dif_pct = 100 * (Media - Media_ref) / Media_ref,
    SE_dif  = sqrt(SE^2 + SE_ref^2),
    # SE del diferencial % (propagación aproximada)
    SE_pct  = abs(Dif_pct) * sqrt((SE_dif / (Media - Media_ref))^2),
    Etiqueta = sprintf("%+.1f%%", Dif_pct)
  )

# Orden de las variables en el eje X
diferenciales$Variable <- factor(diferenciales$Variable,
                                 levels = vars_analisis)
diferenciales$Tratamiento <- factor(diferenciales$Tratamiento,
                                    levels = niveles_trat)

# ============================================================
# 3. CURVA DEL DIFERENCIAL — un panel, 3 curvas
# ============================================================
p_curva <- ggplot(diferenciales,
                  aes(x = Variable, y = Dif_pct,
                      color = Tratamiento,
                      group = Tratamiento,
                      shape = Tratamiento)) +
  # Línea de referencia Testigo = 0
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey20", linewidth = 0.8) +
  # Banda de error (± SE del diferencial %)
  geom_ribbon(aes(ymin = Dif_pct - SE_pct,
                  ymax = Dif_pct + SE_pct,
                  fill = Tratamiento),
              alpha = 0.13, color = NA) +
  # Curva
  geom_line(linewidth = 1.4) +
  # Puntos
  geom_point(size = 4.5, fill = "white", stroke = 1.6) +
  # Etiquetas con valores
  geom_text_repel(
    aes(label = Etiqueta),
    size = 4, fontface = "bold", color = "black",
    direction   = "both",
    box.padding = 0.9,
    point.padding = 0.5,
    segment.size  = 0.35,
    segment.color = "grey55",
    min.segment.length = 0,
    max.overlaps = Inf,
    force = 6,
    seed = 42,
    show.legend = FALSE
  ) +
  scale_color_manual(values = colores_trat) +
  scale_fill_manual(values  = colores_trat) +
  scale_shape_manual(values = formas_trat) +
  labs(
    title    = "Curva del diferencial relativo (%) respecto al Testigo",
    subtitle = paste0("Promedio sobre todos los tiempos de patio. ",
                      "Línea 0 = Testigo (arriba = mayor, abajo = menor). ",
                      "Bandas = ± SE."),
    x        = NULL,
    y        = "Diferencial relativo vs Testigo (%)",
    color    = NULL, shape = NULL, fill = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 13),
    legend.key.width = unit(1.4, "cm"),
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 11, color = "grey25"),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(face = "bold", size = 12),
    axis.text.y      = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey93"),
    plot.margin      = margin(15, 20, 15, 15)
  )

print(p_curva)
ggsave("curva_diferencial_tratamiento.png", p_curva,
       width = 12, height = 7, dpi = 300, bg = "white")

# ============================================================
# 4. VERSIÓN ALTERNA — eje X = tratamientos, una curva por variable
#    (útil si quieres ver el perfil de cada variable entre tratamientos)
# ============================================================
p_curva_alt <- ggplot(diferenciales,
                      aes(x = Tratamiento, y = Dif_pct,
                          color = Variable, group = Variable)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey20", linewidth = 0.8) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3.6) +
  geom_text_repel(
    aes(label = sprintf("%+.1f%%", Dif_pct)),
    size = 3.4, fontface = "bold",
    box.padding = 0.7, max.overlaps = Inf,
    seed = 42, show.legend = FALSE
  ) +
  labs(
    title    = "Perfil del diferencial por tratamiento",
    subtitle = "Cada línea = una variable. Línea 0 = Testigo.",
    x        = NULL,
    y        = "Diferencial relativo vs Testigo (%)",
    color    = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 11, color = "grey25"),
    axis.text.x      = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

print(p_curva_alt)
ggsave("curva_diferencial_perfil.png", p_curva_alt,
       width = 12, height = 7, dpi = 300, bg = "white")

# ============================================================
# 5. Tabla del diferencial
# ============================================================
tabla_dif <- diferenciales %>%
  select(Variable, Tratamiento, Media, Media_ref, Dif_pct) %>%
  mutate(
    Media     = round(Media, 2),
    Media_ref = round(Media_ref, 2),
    Dif_pct   = round(Dif_pct, 1)
  ) %>%
  arrange(Variable, Tratamiento)


# ============================================================
# DIFERENCIAL vs TESTIGO — Barras + Mapa de calor
# - Gráfico de barras del diferencial (%) por variable y tratamiento
# - Mapa de calor 1: diferencial % vs Testigo (color = magnitud)
# - Mapa de calor 2: medias crudas + letras Fisher LSD (α = 0.15)
# - En cada celda: valor crudo y % relativo + letra
# ============================================================

library(dplyr); library(tidyr); library(ggplot2)
library(ggrepel); library(car); library(agricolae)

# ---- Configuración ----
vars_analisis <- c("Brix","Pol","POL...JUGO","Pza",
                   "X..Az.Reductor","Dextrana.PPM")
trat_control  <- "Testigo"
alpha         <- 0.15

colores_trat <- c(
  "Bioxida 0.50%" = "#E41A1C",
  "Bioxida 1.00%" = "#4DAF4A",
  "Digra LM-25M"  = "#377EB8",
  "Testigo"       = "#984EA3"
)

# ---- Datos ----
datos_dif <- datos_analisis
datos_dif$Tratamiento <- factor(datos_dif$Tratamiento)
niveles_trat_full <- levels(datos_dif$Tratamiento)
niveles_trat      <- setdiff(niveles_trat_full, trat_control)

# ============================================================
# 1. Medias por Variable × Tratamiento + Fisher LSD
# ============================================================
medias_long <- datos_dif %>%
  pivot_longer(all_of(vars_analisis),
               names_to = "Variable", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  group_by(Variable, Tratamiento) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    SD    = sd(Valor,   na.rm = TRUE),
    SE    = SD / sqrt(n()),
    n     = n(),
    .groups = "drop"
  )

# Letras Fisher LSD por variable
letras_lsd <- purrr::map_dfr(vars_analisis, function(v) {
  df_v <- datos_dif %>%
    select(y = all_of(v), Tratamiento) %>%
    filter(!is.na(y))
  m  <- aov(y ~ Tratamiento, data = df_v)
  p  <- car::Anova(m, type = "III")["Tratamiento", "Pr(>F)"]
  if (p >= alpha) {
    return(tibble(Variable = v,
                  Tratamiento = niveles_trat_full,
                  Letra = "a"))
  }
  lsd <- tryCatch(
    agricolae::LSD.test(m, "Tratamiento", alpha = alpha,
                        console = FALSE, group = TRUE),
    error = function(e) NULL)
  if (is.null(lsd)) return(tibble())
  g <- lsd$groups
  g$Tratamiento <- rownames(g)
  tibble(Variable = v,
         Tratamiento = as.character(g$Tratamiento),
         Letra = as.character(g$groups))
})

medias_long <- medias_long %>%
  left_join(letras_lsd, by = c("Variable","Tratamiento"))

# ============================================================
# 2. Diferencial relativo (%) vs Testigo
# ============================================================
medias_testigo <- medias_long %>%
  filter(Tratamiento == trat_control) %>%
  select(Variable, Media_ref = Media, SE_ref = SE)

diferenciales <- medias_long %>%
  filter(Tratamiento != trat_control) %>%
  left_join(medias_testigo, by = "Variable") %>%
  mutate(
    Dif_pct = 100 * (Media - Media_ref) / Media_ref,
    SE_dif  = sqrt(SE^2 + SE_ref^2)
  )

# ============================================================
# 3. GRÁFICO DE BARRAS — Diferencial relativo (%) vs Testigo
# ============================================================
df_plot <- diferenciales
df_plot$Tratamiento <- factor(df_plot$Tratamiento, levels = niveles_trat)
df_plot$Variable    <- factor(df_plot$Variable,    levels = vars_analisis)

# Etiquetas con nombres partidos para evitar traslape
etiquetas_x <- c(
  "Brix"          = "Brix",
  "Pol"           = "Pol",
  "POL...JUGO"    = "POL...\nJUGO",
  "Pza"           = "Pza",
  "X..Az.Reductor"= "X..Az.\nReductor",
  "Dextrana.PPM"  = "Dextrana\nPPM"
)

p_barras <- ggplot(df_plot,
                   aes(x = Variable, y = Dif_pct, fill = Tratamiento)) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "grey20", linewidth = 0.8) +
  geom_col(position = position_dodge(width = 0.8),
           width = 0.72, color = "grey25", linewidth = 0.4) +
  geom_errorbar(aes(ymin = Dif_pct - SE_dif,
                    ymax = Dif_pct + SE_dif),
                position = position_dodge(width = 0.8),
                width = 0.2, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%+.1f%%", Dif_pct)),
            position = position_dodge(width = 0.8),
            vjust = ifelse(df_plot$Dif_pct >= 0, -0.6, 1.2),
            size = 3.8, fontface = "bold") +
  scale_fill_manual(values = colores_trat[niveles_trat]) +
  scale_x_discrete(labels = etiquetas_x) +
  labs(
    title    = "Diferencial relativo (%) de cada tratamiento respecto al Testigo",
    subtitle = "Promedio sobre todos los tiempos de patio. Línea 0 = Testigo.",
    x        = NULL,
    y        = "Diferencia relativa vs Testigo (%)",
    fill     = NULL
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 12),
    legend.key.width = unit(1.4, "cm"),
    plot.title       = element_text(face = "bold", size = 17),
    plot.subtitle    = element_text(size = 11, color = "grey25"),
    axis.title       = element_text(face = "bold", size = 12),
    axis.text.x      = element_text(face = "bold", size = 11, lineheight = 0.9),
    panel.grid.minor = element_blank()
  )

print(p_barras)
ggsave("diferencial_tratamiento_pct.png", p_barras,
       width = 13, height = 7, dpi = 300, bg = "white")

# ============================================================
# 4. MAPA DE CALOR — Diferencial % vs Testigo
#    Fill = diferencia relativa (%)
#    Texto = valor crudo + % + letra Fisher LSD
# ============================================================
df_heat <- diferenciales
df_heat$Tratamiento <- factor(df_heat$Tratamiento, levels = niveles_trat)
df_heat$Variable    <- factor(df_heat$Variable,    levels = rev(vars_analisis))

# Etiqueta combinada: "media cruda\n(+X.X%) letra"
df_heat <- df_heat %>%
  mutate(
    Etiqueta = sprintf("%.2f\n(%+.1f%%) %s",
                       Media, Dif_pct, Letra)
  )

# Paleta divergente simétrica respecto a 0
lim <- max(abs(df_heat$Dif_pct), na.rm = TRUE)

p_heat_pct <- ggplot(df_heat,
                     aes(x = Tratamiento, y = Variable, fill = Dif_pct)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Etiqueta),
            size = 3.4, fontface = "bold",
            color = "black", lineheight = 0.95) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0,
                       limits = c(-lim, lim),
                       name = "Dif % vs Testigo") +
  labs(
    title    = "Mapa de calor — Diferencial respecto al Testigo",
    subtitle = paste0("Color = % de diferencia vs Testigo. ",
                      "Texto = media cruda + (% dif) + letra Fisher LSD (α = 0.15).\n",
                      "Azul = por debajo del Testigo | Rojo = por encima del Testigo."),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    axis.text.x   = element_text(face = "bold", size = 12, angle = 20, hjust = 1),
    axis.text.y   = element_text(face = "bold", size = 11),
    panel.grid    = element_blank(),
    legend.position = "right"
  )

print(p_heat_pct)
ggsave("heatmap_diferencial_pct.png", p_heat_pct,
       width = 11, height = 7, dpi = 300, bg = "white")

# ============================================================
# 5. MAPA DE CALOR — Media cruda + letra Fisher LSD
#    (versión paralela para ver los valores absolutos)
# ============================================================
df_heat_medias <- medias_long %>%
  mutate(Tratamiento = factor(Tratamiento, levels = niveles_trat_full),
         Variable    = factor(Variable,    levels = rev(vars_analisis)),
         Etiqueta    = sprintf("%.2f\n%s", Media, Letra))

lim_m <- range(df_heat_medias$Media, na.rm = TRUE)

p_heat_medias <- ggplot(df_heat_medias,
                        aes(x = Tratamiento, y = Variable, fill = Media)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Etiqueta),
            size = 3.6, fontface = "bold",
            color = "black", lineheight = 0.95) +
  scale_fill_gradient(low = "#f7f7f7", high = "#4575b4",
                      name = "Media cruda") +
  labs(
    title    = "Mapa de calor — Medias crudas por tratamiento y variable",
    subtitle = "Texto = media real + letra Fisher LSD (α = 0.15). Letras iguales = no difieren.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    axis.text.x   = element_text(face = "bold", size = 12, angle = 20, hjust = 1),
    axis.text.y   = element_text(face = "bold", size = 11),
    panel.grid    = element_blank(),
    legend.position = "right"
  )

print(p_heat_medias)
ggsave("heatmap_medias_crudas.png", p_heat_medias,
       width = 11, height = 7, dpi = 300, bg = "white")

# ============================================================
# 6. MAPA DE CALOR COMBINADO (opcional) — dos en uno
# ============================================================
df_heat_combo <- df_heat %>%
  mutate(
    Etiqueta2 = sprintf("%.2f\n(%+.1f%%) %s",
                        Media, Dif_pct, Letra)
  )

p_heat_combo <- ggplot(df_heat_combo,
                       aes(x = Tratamiento, y = Variable, fill = Dif_pct)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = Etiqueta2),
            size = 3.6, fontface = "bold",
            color = "black", lineheight = 0.9) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0,
                       limits = c(-lim, lim),
                       name = "Dif % vs Testigo") +
  labs(
    title    = "Diferencial (%) y valores crudos respecto al Testigo",
    subtitle = paste0("Color = % de diferencia. Texto = media cruda, (% dif) y letra Fisher LSD (α = 0.15)."),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    axis.text.x   = element_text(face = "bold", size = 12, angle = 20, hjust = 1),
    axis.text.y   = element_text(face = "bold", size = 11),
    panel.grid    = element_blank()
  )

print(p_heat_combo)
ggsave("heatmap_diferencial_combo.png", p_heat_combo,
       width = 11, height = 7, dpi = 300, bg = "white")

# ============================================================
# 7. TABLA DETALLADA
# ============================================================
tabla_dif <- diferenciales %>%
  select(Variable, Tratamiento, Media, SD, Letra, Media_ref, Dif_pct) %>%
  mutate(
    Media     = round(Media, 2),
    SD        = round(SD, 2),
    Media_ref = round(Media_ref, 2),
    Dif_pct   = round(Dif_pct, 1)
  ) %>%
  arrange(Variable, Tratamiento)

write.csv(tabla_dif, "tabla_diferencial_testigo.csv", row.names = FALSE)

cat("\n=========================================================\n")
cat("TABLA: DIFERENCIAL vs TESTIGO\n")
cat("=========================================================\n")
print(as.data.frame(tabla_dif), row.names = FALSE)





# ============================================================
# MAPA DE CALOR — Diferencial respecto al Testigo
# - Dextrana.PPM: MENOR = MEJOR (se invierte para colorear)
# - Se marca con recuadro negro la mejor celda de cada variable
# - Texto: media cruda + (% dif real) + letra Fisher LSD
# ============================================================

library(dplyr); library(tidyr); library(ggplot2)
library(car); library(agricolae)

# ---- Configuración ----
vars_analisis    <- c("Brix","Pol","POL...JUGO","Pza",
                      "X..Az.Reductor","Dextrana.PPM")
vars_menor_mejor <- c("Dextrana.PPM")   # variables donde MENOR = MEJOR
trat_control     <- "Testigo"
alpha            <- 0.15

colores_trat <- c(
  "Bioxida 0.50%" = "#E41A1C",
  "Bioxida 1.00%" = "#4DAF4A",
  "Digra LM-25M"  = "#377EB8",
  "Testigo"       = "#984EA3"
)

# ---- Datos ----
datos_dif <- datos_analisis
datos_dif$Tratamiento <- factor(datos_dif$Tratamiento)
niveles_trat_full <- levels(datos_dif$Tratamiento)
niveles_trat      <- setdiff(niveles_trat_full, trat_control)

# ============================================================
# 1. Medias por Variable × Tratamiento + Fisher LSD
# ============================================================
medias_long <- datos_dif %>%
  pivot_longer(all_of(vars_analisis),
               names_to = "Variable", values_to = "Valor") %>%
  filter(!is.na(Valor)) %>%
  group_by(Variable, Tratamiento) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    SD    = sd(Valor,   na.rm = TRUE),
    SE    = SD / sqrt(n()),
    n     = n(),
    .groups = "drop"
  )

letras_lsd <- purrr::map_dfr(vars_analisis, function(v) {
  df_v <- datos_dif %>%
    select(y = all_of(v), Tratamiento) %>%
    filter(!is.na(y))
  m <- aov(y ~ Tratamiento, data = df_v)
  p <- car::Anova(m, type = "III")["Tratamiento", "Pr(>F)"]
  if (p >= alpha) {
    return(tibble(Variable = v,
                  Tratamiento = niveles_trat_full,
                  Letra = "a"))
  }
  lsd <- tryCatch(
    agricolae::LSD.test(m, "Tratamiento", alpha = alpha,
                        console = FALSE, group = TRUE),
    error = function(e) NULL)
  if (is.null(lsd)) return(tibble())
  g <- lsd$groups
  g$Tratamiento <- rownames(g)
  tibble(Variable = v,
         Tratamiento = as.character(g$Tratamiento),
         Letra = as.character(g$groups))
})

medias_long <- medias_long %>%
  left_join(letras_lsd, by = c("Variable","Tratamiento"))

# ============================================================
# 2. Diferencial (%) vs Testigo
# ============================================================
medias_testigo <- medias_long %>%
  filter(Tratamiento == trat_control) %>%
  select(Variable, Media_ref = Media)

diferenciales <- medias_long %>%
  filter(Tratamiento != trat_control) %>%
  left_join(medias_testigo, by = "Variable") %>%
  mutate(
    Dif_pct = 100 * (Media - Media_ref) / Media_ref,
    # ----- BENEFICIO: positivo = MEJOR que el Testigo -----
    Beneficio = ifelse(Variable %in% vars_menor_mejor,
                       -Dif_pct,   # menor = mejor → se invierte
                       Dif_pct)    # mayor = mejor → se mantiene
  )

# Mejor tratamiento por variable (mayor beneficio)
diferenciales <- diferenciales %>%
  group_by(Variable) %>%
  mutate(EsMejor = Beneficio == max(Beneficio, na.rm = TRUE)) %>%
  ungroup()

# ============================================================
# 3. Mapa de calor
# ============================================================
df_heat <- diferenciales
df_heat$Tratamiento <- factor(df_heat$Tratamiento, levels = niveles_trat)
df_heat$Variable    <- factor(df_heat$Variable,    levels = rev(vars_analisis))

# Etiqueta combinada
df_heat <- df_heat %>%
  mutate(
    Etiqueta = sprintf("%.2f\n(%+.1f%%) %s",
                       Media, Dif_pct, Letra)
  )

# Límites simétricos
lim <- max(abs(df_heat$Beneficio), na.rm = TRUE)

# Celdas del mejor (para el recuadro)
mejores <- df_heat %>% filter(EsMejor)

p_heat <- ggplot(df_heat,
                 aes(x = Tratamiento, y = Variable)) +
  # Fondo coloreado por BENEFICIO (positivo = mejor = verde)
  geom_tile(aes(fill = Beneficio), color = "white", linewidth = 1) +
  # Recuadro negro grueso alrededor del mejor tratamiento por variable
  geom_tile(data = mejores,
            aes(x = Tratamiento, y = Variable),
            fill = NA, color = "black", linewidth = 1.8) +
  # Texto: media cruda + (% dif real) + letra
  geom_text(aes(label = Etiqueta),
            size = 3.4, fontface = "bold",
            color = "black", lineheight = 0.95) +
  scale_fill_gradient2(
    low      = "#B2182B",   # peor que el Testigo
    mid      = "white",
    high     = "#1B7837",   # mejor que el Testigo
    midpoint = 0,
    limits   = c(-lim, lim),
    name     = "Beneficio vs Testigo\n(verde = mejor)"
  ) +
  labs(
    title    = "Diferencial (%) y valores crudos respecto al Testigo",
    subtitle = paste0(
      "Color = beneficio vs Testigo (verde = mejor, rojo = peor). ",
      "Texto = media cruda, (% dif real) y letra Fisher LSD (α = 0.15).\n",
      "Recuadro negro = mejor tratamiento de cada variable. ",
      "Dextrana.PPM: menor valor = mejor."
    ),
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 17),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    axis.text.x   = element_text(face = "bold", size = 12, angle = 20, hjust = 1),
    axis.text.y   = element_text(face = "bold", size = 11),
    panel.grid    = element_blank(),
    legend.position = "right"
  )

print(p_heat)
ggsave("heatmap_diferencial_mejor.png", p_heat,
       width = 12, height = 7.5, dpi = 300, bg = "white")

# ============================================================
# 4. Tabla con el mejor tratamiento marcado
# ============================================================
tabla_final <- diferenciales %>%
  select(Variable, Tratamiento, Media, Letra,
         Media_ref, Dif_pct, Beneficio, EsMejor) %>%
  mutate(
    Media     = round(Media, 2),
    Media_ref = round(Media_ref, 2),
    Dif_pct   = round(Dif_pct, 1),
    Beneficio = round(Beneficio, 1),
    EsMejor   = ifelse(EsMejor, "★ MEJOR", "")
  ) %>%
  arrange(Variable, Tratamiento)

write.csv(tabla_final, "tabla_diferencial_mejor.csv", row.names = FALSE)

cat("\n=========================================================\n")
cat("DIFERENCIAL vs TESTIGO (con mejor tratamiento marcado)\n")
cat("=========================================================\n")
print(as.data.frame(tabla_final), row.names = FALSE)



# ============================================================
# 14. ANÁLISIS DE CORRELACIÓN AMPLIADO
#     Incluye Tiempo.Muestreo.Patio y temperatura
# ============================================================

library(dplyr); library(tidyr); library(ggplot2); library(corrplot)

# ---- 14.0. Variables a correlacionar ----
# Se añaden Tiempo de patio (numérico) y temperatura a las 6 variables originales
datos_cor <- datos %>%
  mutate(
    Tiempo.num   = suppressWarnings(as.numeric(as.character(Tiempo.Muestreo.Patio))),
    Temperatura  = suppressWarnings(as.numeric(as.character(temperatura)))
  ) %>%
  select(Tratamiento,
         Brix, Pol, POL...JUGO, Pza, X..Az.Reductor, Dextrana.PPM,
         Tiempo.num, Temperatura)

vars_cor <- c("Brix","Pol","POL...JUGO","Pza","X..Az.Reductor",
              "Dextrana.PPM","Tiempo.num","Temperatura")

# ============================================================
# 14.1. MATRIZ DE CORRELACIÓN GLOBAL (Pearson + p-valores)
# ============================================================
cat("\n==================================================\n")
cat("MATRIZ DE CORRELACIÓN GLOBAL (con Tiempo y Temperatura)\n")
cat("==================================================\n")

M_glob  <- cor(datos_cor[, vars_cor], use = "pairwise.complete.obs")
cat("\n-- Coeficientes de Pearson --\n")
print(round(M_glob, 3))

# p-valores pareados
p_glob <- matrix(NA, length(vars_cor), length(vars_cor),
                 dimnames = list(vars_cor, vars_cor))
n_glob <- matrix(NA, length(vars_cor), length(vars_cor),
                 dimnames = list(vars_cor, vars_cor))

for (i in seq_along(vars_cor)) {
  for (j in seq_along(vars_cor)) {
    if (i == j) next
    ok <- complete.cases(datos_cor[[vars_cor[i]]], datos_cor[[vars_cor[j]]])
    if (sum(ok) >= 3) {
      ct <- suppressWarnings(
        cor.test(datos_cor[[vars_cor[i]]][ok],
                 datos_cor[[vars_cor[j]]][ok],
                 method = "pearson"))
      p_glob[i, j] <- ct$p.value
      n_glob[i, j] <- sum(ok)
    }
  }
}

cat("\n-- p-valores --\n")
print(round(p_glob, 4))

cat("\n-- Matriz de significancia (α = 0.15) --\n")
sig_glob <- ifelse(is.na(p_glob), "",
                   ifelse(p_glob < 0.01, "***",
                          ifelse(p_glob < 0.05, "**",
                                 ifelse(p_glob < 0.15, "*", "ns"))))
diag(sig_glob) <- ""
print(sig_glob)

# ============================================================
# 14.2. HEATMAP GLOBAL con p-valores anotados
# ============================================================
df_glob <- as.data.frame(as.table(M_glob))
df_glob$p    <- as.vector(p_glob)
df_glob$sig  <- as.vector(sig_glob)
df_glob$lab  <- sprintf("%.2f\n%s", df_glob$Freq, df_glob$sig)

# Reordenar para que Tiempo y Temperatura queden al final
niveles_ord <- c("Brix","Pol","POL...JUGO","Pza","X..Az.Reductor",
                 "Dextrana.PPM","Tiempo.num","Temperatura")
df_glob$Var1 <- factor(df_glob$Var1, levels = niveles_ord)
df_glob$Var2 <- factor(df_glob$Var2, levels = rev(niveles_ord))

p_heat_glob <- ggplot(df_glob, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = lab), size = 3.2,
            fontface = "bold", color = "black", lineheight = 0.9) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1),
                       name = "r de Pearson") +
  labs(title    = "Matriz de correlación global — incluye Tiempo de patio y Temperatura",
       subtitle = "Valores = r (Pearson). Significancia: * p<0.15, ** p<0.05, *** p<0.01",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    axis.text.x   = element_text(angle = 30, hjust = 1,
                                 face = "bold", size = 11),
    axis.text.y   = element_text(face = "bold", size = 11),
    panel.grid    = element_blank()
  )

print(p_heat_glob)
ggsave("cor_global_tiempo_temp.png", p_heat_glob,
       width = 11, height = 9, dpi = 300, bg = "white")

# ============================================================
# 14.3. CORRELACIÓN POR TRATAMIENTO (con Tiempo y Temperatura)
# ============================================================
cat("\n==================================================\n")
cat("CORRELACIÓN POR TRATAMIENTO (con Tiempo y Temperatura)\n")
cat("==================================================\n")

lista_cor_trat <- list()
for (t in levels(datos_cor$Tratamiento)) {
  sub_t <- subset(datos_cor, Tratamiento == t)[, vars_cor]
  M_t   <- cor(sub_t, use = "pairwise.complete.obs")
  cat("\n---- Tratamiento:", t, "----\n")
  print(round(M_t, 3))
  lista_cor_trat[[t]] <- M_t
}

# ============================================================
# 14.4. HEATMAP POR TRATAMIENTO (todos en un solo panel)
# ============================================================
df_trat <- purrr::imap_dfr(lista_cor_trat, function(M_t, t) {
  d <- as.data.frame(as.table(M_t))
  d$Tratamiento <- t
  d
})

df_trat$Var1 <- factor(df_trat$Var1, levels = niveles_ord)
df_trat$Var2 <- factor(df_trat$Var2, levels = rev(niveles_ord))

p_heat_trat <- ggplot(df_trat, aes(Var1, Var2, fill = Freq)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Freq)),
            size = 2.6, color = "black") +
  facet_wrap(~ Tratamiento, ncol = 2) +
  scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC",
                       midpoint = 0, limits = c(-1, 1),
                       name = "r de Pearson") +
  labs(title    = "Correlaciones por tratamiento — incluye Tiempo y Temperatura",
       subtitle = "Cada panel = un tratamiento. Se añaden Tiempo de patio y Temperatura.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    axis.text.x   = element_text(angle = 30, hjust = 1, size = 9),
    axis.text.y   = element_text(size = 9),
    panel.grid    = element_blank()
  )

print(p_heat_trat)
ggsave("cor_por_tratamiento_tiempo_temp.png", p_heat_trat,
       width = 14, height = 10, dpi = 300, bg = "white")

# ============================================================
# 14.5. CORRELACIÓN ESPECÍFICA: Tiempo y Temperatura con cada variable
#       (tabla ordenada por |r|)
# ============================================================
focus <- purrr::map_dfr(c("Tiempo.num","Temperatura"), function(vx) {
  purrr::map_dfr(setdiff(vars_cor, c("Tiempo.num","Temperatura")), function(vy) {
    # Global
    ok <- complete.cases(datos_cor[[vx]], datos_cor[[vy]])
    ct <- suppressWarnings(cor.test(datos_cor[[vx]][ok],
                                    datos_cor[[vy]][ok]))
    glob <- tibble(
      Variable_x  = vx,
      Variable_y  = vy,
      Tratamiento = "Global",
      r           = unname(ct$estimate),
      p_value     = ct$p.value
    )
    # Por tratamiento
    por_trat <- purrr::map_dfr(levels(datos_cor$Tratamiento), function(t) {
      sub <- subset(datos_cor, Tratamiento == t)
      ok2 <- complete.cases(sub[[vx]], sub[[vy]])
      if (sum(ok2) < 3) return(tibble())
      ct2 <- suppressWarnings(cor.test(sub[[vx]][ok2], sub[[vy]][ok2]))
      tibble(
        Variable_x  = vx,
        Variable_y  = vy,
        Tratamiento = t,
        r           = unname(ct2$estimate),
        p_value     = ct2$p.value
      )
    })
    bind_rows(glob, por_trat)
  })
}) %>%
  mutate(
    Signif = dplyr::case_when(
      p_value < 0.01 ~ "***",
      p_value < 0.05 ~ "**",
      p_value < 0.15 ~ "*",
      TRUE           ~ "ns"
    ),
    Direccion = dplyr::case_when(
      r >  0.3 ~ "Positiva",
      r < -0.3 ~ "Negativa",
      TRUE     ~ "Débil"
    )
  )

cat("\n==================================================\n")
cat("CORRELACIÓN: Tiempo de patio y Temperatura vs cada variable\n")
cat("==================================================\n")
print(as.data.frame(focus %>%
                      select(Variable_x, Variable_y, Tratamiento, r, p_value, Signif) %>%
                      mutate(r = round(r, 3), p_value = signif(p_value, 3)) %>%
                      arrange(Variable_x, Variable_y, Tratamiento)),
      row.names = FALSE)

write.csv(focus, "cor_tiempo_temperatura.csv", row.names = FALSE)

# ============================================================
# 14.6. GRÁFICO DE BARRAS: r(Tiempo, Variable) y r(Temperatura, Variable)
#       por tratamiento
# ============================================================
focus_plot <- focus %>%
  filter(Tratamiento != "Global") %>%
  mutate(Variable_y = factor(Variable_y,
                             levels = c("Brix","Pol","POL...JUGO","Pza",
                                        "X..Az.Reductor","Dextrana.PPM")))

p_barras_focus <- ggplot(focus_plot,
                         aes(x = r, y = Variable_y,
                             fill = Direccion)) +
  geom_col(color = "grey20", linewidth = 0.3) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f%s", r, Signif)),
            hjust = ifelse(focus_plot$r >= 0, -0.15, 1.15),
            size = 2.9, color = "black") +
  scale_fill_manual(values = c("Positiva" = "#2166AC",
                               "Negativa" = "#B2182B",
                               "Débil"    = "grey75")) +
  facet_grid(Variable_x ~ Tratamiento, scales = "free_x") +
  coord_cartesian(xlim = c(-1.2, 1.2)) +
  labs(title    = "Relación del Tiempo de patio y la Temperatura con cada variable",
       subtitle = "r de Pearson. * p<0.15, ** p<0.05, *** p<0.01",
       x = "r (coeficiente de correlación)", y = "",
       fill = "Dirección") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 10, color = "grey25"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )

print(p_barras_focus)
ggsave("cor_tiempo_temp_barras.png", p_barras_focus,
       width = 14, height = 7, dpi = 300, bg = "white")

# ============================================================
# 14.7. INTERPRETACIÓN AUTOMÁTICA
# ============================================================
cat("\n==================================================\n")
cat("INTERPRETACIÓN — Tiempo y Temperatura\n")
cat("==================================================\n")

for (vx in c("Tiempo.num","Temperatura")) {
  cat("\n###", vx, "###\n")
  
  # Global
  g <- focus %>% filter(Variable_x == vx, Tratamiento == "Global") %>%
    arrange(desc(abs(r)))
  cat("  [Global]\n")
  for (i in seq_len(nrow(g))) {
    cat(sprintf("    %-14s r = %+.2f  (%s, %s)\n",
                g$Variable_y[i], g$r[i], g$Direccion[i], g$Signif[i]))
  }
  
  # Por tratamiento — solo las correlaciones significativas
  cat("  [Por tratamiento — solo significativas (p < 0.15)]\n")
  pt <- focus %>%
    filter(Variable_x == vx, Tratamiento != "Global", Signif != "ns") %>%
    arrange(Tratamiento, desc(abs(r)))
  if (nrow(pt) == 0) {
    cat("    Ninguna correlación significativa.\n")
  } else {
    for (i in seq_len(nrow(pt))) {
      cat(sprintf("    %-15s %-14s r = %+.2f  %s\n",
                  pt$Tratamiento[i], pt$Variable_y[i],
                  pt$r[i], pt$Signif[i]))
    }
  }
}





# ============================================================
# ICG POR TRATAMIENTO — Curva suavizada con punto de corte
# (versión completa y corregida)
# ============================================================

pkgs <- c("dplyr","tidyr","ggplot2","patchwork","segmented")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p)
library(ggplot2); library(tidyr); library(patchwork); library(segmented)
library(dplyr)

# ---- Configuración ----
vars_analisis    <- c("Brix","Pol","POL...JUGO"
                      )
vars_menor_mejor <- c()

# ============================================================
# 1. Datos y construcción del ICG
# ============================================================
datos_q <- datos_analisis %>%
  dplyr::select(Tratamiento, Tiempo.Muestreo.Patio,
                dplyr::all_of(vars_analisis)) %>%
  dplyr::mutate(Tiempo_num = as.numeric(as.character(Tiempo.Muestreo.Patio))) %>%
  dplyr::filter(!is.na(Tiempo_num)) %>%
  tidyr::drop_na(dplyr::all_of(vars_analisis))
datos_q$Tratamiento <- factor(datos_q$Tratamiento)

# Normalizar cada variable a [0,1] con el rango global
datos_norm <- datos_q %>%
  dplyr::mutate(dplyr::across(dplyr::all_of(vars_analisis), function(x) {
    rng <- range(x, na.rm = TRUE)
    (x - rng[1]) / (rng[2] - rng[1])
  }))

# Invertir las de "menor = mejor"
for (v in vars_menor_mejor) datos_norm[[v]] <- 1 - datos_norm[[v]]

# ICG = promedio × 100 → escala 0-100
datos_norm <- datos_norm %>%
  dplyr::mutate(ICG = 100 * rowMeans(dplyr::select(., dplyr::all_of(vars_analisis)),
                                     na.rm = TRUE))

# ============================================================
# 2. Δ ICG vs línea base (t = 0)
# ============================================================
baseline <- datos_norm %>%
  dplyr::group_by(Tratamiento) %>%
  dplyr::filter(Tiempo_num == min(Tiempo_num)) %>%
  dplyr::summarise(ICG_ref = mean(ICG, na.rm = TRUE), .groups = "drop")

datos_norm <- datos_norm %>%
  dplyr::left_join(baseline, by = "Tratamiento") %>%
  dplyr::mutate(Delta_ICG = ICG - ICG_ref)

# ============================================================
# 3. Medias por Tratamiento × Tiempo
# ============================================================
medias <- datos_norm %>%
  dplyr::group_by(Tratamiento, Tiempo_num) %>%
  dplyr::summarise(
    Delta_media = mean(Delta_ICG, na.rm = TRUE),
    SE          = sd(Delta_ICG, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups     = "drop"
  )

# ============================================================
# 4. Detección del punto de corte (segmented → loess fallback)
# ============================================================
detectar_corte <- function(sub) {
  
  sub <- sub %>% dplyr::arrange(Tiempo_num)
  if (nrow(sub) < 4) return(list(t_corte = NA, y_corte = NA, metodo = "sin datos"))
  
  # --- Intento 1: segmented ---
  lm_fit <- lm(Delta_media ~ Tiempo_num, data = sub)
  seg <- tryCatch({
    segmented(lm_fit, seg.Z = ~Tiempo_num,
              psi = list(Tiempo_num = median(sub$Tiempo_num)),
              control = seg.control(n.boot = 0, it.max = 100))
  }, error = function(e) NULL)
  
  if (!is.null(seg)) {
    grid <- seq(min(sub$Tiempo_num), max(sub$Tiempo_num), length.out = 200)
    pred <- as.numeric(predict(seg, newdata = data.frame(Tiempo_num = grid)))
    idx  <- which.max(pred)
    return(list(t_corte = grid[idx], y_corte = pred[idx], metodo = "segmented"))
  }
  
  # --- Fallback: loess ---
  lo <- tryCatch(
    loess(Delta_media ~ Tiempo_num, data = sub, span = 1.2),
    error = function(e) NULL)
  if (is.null(lo)) return(list(t_corte = NA, y_corte = NA, metodo = "sin modelo"))
  
  grid <- seq(min(sub$Tiempo_num), max(sub$Tiempo_num), length.out = 200)
  pred <- predict(lo, newdata = data.frame(Tiempo_num = grid))
  idx  <- which.max(pred)
  list(t_corte = grid[idx], y_corte = pred[idx], metodo = "loess")
}

cortes <- medias %>%
  dplyr::group_by(Tratamiento) %>%
  dplyr::group_modify(~ {
    r <- detectar_corte(.x)
    tibble::tibble(t_corte = r$t_corte, y_corte = r$y_corte, metodo = r$metodo)
  }) %>%
  dplyr::ungroup()

cat("\n=========================================================\n")
cat("PUNTO DE CORTE POR TRATAMIENTO (ICG)\n")
cat("=========================================================\n")
print(as.data.frame(cortes), row.names = FALSE)
write.csv(cortes, "tabla_icg_cortes.csv", row.names = FALSE)

# ============================================================
# 5. Función de gráfico por tratamiento (corregida)
# ============================================================
grafico_trat <- function(trat) {
  
  sub <- datos_norm %>% dplyr::filter(Tratamiento == trat)
  med <- medias     %>% dplyr::filter(Tratamiento == trat)
  cor <- cortes     %>% dplyr::filter(Tratamiento == trat)
  ref <- baseline$ICG_ref[baseline$Tratamiento == trat]
  
  subt <- sprintf("ICG(t = 0) = %.1f   |   corte = %.0f h  (%s)",
                  ref,
                  ifelse(is.na(cor$t_corte), NA, cor$t_corte),
                  cor$metodo)
  
  ggplot(sub, aes(x = Tiempo_num, y = Delta_ICG)) +
    
    # Línea base y = 0
    geom_hline(yintercept = 0, linetype = "dashed",
               color = "grey45", linewidth = 0.6) +
    
    # Banda de confianza del suavizado (loess)
    geom_smooth(aes(group = 1),
                method = "loess", span = 1.2, se = TRUE,
                color = "#1F4E79", fill = "#4A90E2",
                alpha = 0.22, linewidth = 1.2) +
    
    # Puntos individuales
    geom_jitter(width = 0.4, height = 0, size = 1.6,
                color = "#1F4E79", alpha = 0.35) +
    
    # Medias observadas ± SE (con inherit.aes = FALSE)
    geom_errorbar(data = med,
                  aes(x = Tiempo_num,
                      ymin = Delta_media - SE,
                      ymax = Delta_media + SE),
                  inherit.aes = FALSE,
                  width = 0.8, color = "#0B2545", linewidth = 0.5) +
    geom_point(data = med,
               aes(x = Tiempo_num, y = Delta_media),
               inherit.aes = FALSE,
               size = 3.6, color = "#0B2545") +
    
    # Línea vertical del corte
    { if (!is.na(cor$t_corte))
      geom_vline(xintercept = cor$t_corte,
                 linetype = "dashed",
                 color = "#C0392B", linewidth = 1) } +
    
    # Punto del corte
    { if (!is.na(cor$t_corte))
      annotate("point",
               x = cor$t_corte, y = cor$y_corte,
               size = 5, shape = 21,
               fill = "white", color = "#C0392B", stroke = 1.6) } +
    
    # Etiqueta del corte
    { if (!is.na(cor$t_corte))
      annotate("label",
               x = cor$t_corte, y = cor$y_corte,
               label = sprintf("Corte = %.0f h", cor$t_corte),
               vjust = -1.5, hjust = 0.5,
               size = 3.6, fontface = "bold",
               color = "#C0392B", fill = "white", label.size = 0.3) } +
    
    scale_x_continuous(breaks = sort(unique(sub$Tiempo_num))) +
    labs(title    = paste("Tratamiento:", trat),
         subtitle = subt,
         x        = "Tiempo de patio (h)",
         y        = "Δ ICG respecto a t = 0") +
    theme_bw(base_size = 13) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(color = "grey94", linewidth = 0.3),
      plot.title        = element_text(face = "bold", size = 15),
      plot.subtitle     = element_text(size = 10, color = "grey45"),
      axis.title        = element_text(face = "bold", size = 11, color = "grey25"),
      axis.text         = element_text(size = 10.5, color = "grey25"),
      plot.margin       = margin(12, 16, 8, 8)
    )
}

# ---- Generar y guardar un gráfico por tratamiento ----
graficos <- lapply(unique(datos_norm$Tratamiento), grafico_trat)
names(graficos) <- unique(datos_norm$Tratamiento)

for (trat in names(graficos)) {
  print(graficos[[trat]])
  ggsave(paste0("ICG_curva_", gsub("[^A-Za-z0-9]", "_", trat), ".png"),
         graficos[[trat]],
         width = 9, height = 6, dpi = 300, bg = "white")
}

# ============================================================
# 6. Panel facetado
# ============================================================
p_all <- wrap_plots(graficos, ncol = 2) +
  plot_annotation(
    title    = "Índice de Calidad Global (ICG) por tratamiento",
    subtitle = paste0("Δ ICG respecto a t = 0. Curva = loess con IC 95%. ",
                      "Línea roja discontinua = punto de corte (máximo de la curva ajustada)."),
    caption  = "ICG = promedio × 100 de las 6 variables normalizadas (0-100). Dextrana invertida.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 18, hjust = 0),
      plot.subtitle = element_text(size = 11, color = "grey30", lineheight = 1.2),
      plot.caption  = element_text(size = 9, color = "grey55", hjust = 0)
    )
  )

print(p_all)
ggsave("ICG_facetado_tratamiento.png", p_all,
       width = 15, height = 10, dpi = 300, bg = "white")

# ============================================================
# 7. Interpretación automática
# ============================================================
cat("\n=========================================================\n")
cat("INTERPRETACIÓN — CORTE DEL ICG\n")
cat("=========================================================\n")
for (i in seq_len(nrow(cortes))) {
  cat(sprintf("  %-15s corte = %.0f h   Δ ICG en corte = %+.2f   método = %s\n",
              cortes$Tratamiento[i],
              cortes$t_corte[i],
              cortes$y_corte[i],
              cortes$metodo[i]))
}
cat("\n>> Un corte más tardío indica mayor tiempo de protección del biocida.\n")
cat(">> Un Δ ICG positivo y grande indica que el ICG llegó a subir por encima del valor inicial.\n")




# ============================================================
# REGRESIONES LINEALES: Tiempo de patio vs cada variable
# ------------------------------------------------------------
# - Modelo por Variable × Tratamiento: y ~ Tiempo.Muestreo.Patio
# - Extrae pendiente, error estándar, p-valor, R²
# - Dirección (sube/baja) y significancia
# - Gráficos facetados por tratamiento
# ============================================================

# ---- Paquetes ----
pkgs <- c("dplyr","tidyr","ggplot2","patchwork","broom")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p)
library(ggplot2); library(tidyr); library(patchwork); library(broom)
library(dplyr)

# ---- Configuración ----
vars_analisis    <- c("Brix","Pol","POL...JUGO","Pza",
                      "X..Az.Reductor","Dextrana.PPM")
vars_menor_mejor <- c("Dextrana.PPM")   # deterioro = pendiente POSITIVA
alpha            <- 0.15                # umbral de significancia

colores_var <- c(
  "Brix"           = "#C0392B",
  "Pol"            = "#1F4E79",
  "POL...JUGO"     = "#1E8449",
  "Pza"            = "#7B241C",
  "X..Az.Reductor" = "#5B2C6F",
  "Dextrana.PPM"   = "#0E6251"
)

# ============================================================
# 1. Datos
# ============================================================
datos_q <- datos_analisis %>%
  dplyr::select(Tratamiento, Tiempo.Muestreo.Patio,
                dplyr::all_of(vars_analisis)) %>%
  dplyr::mutate(Tiempo_num = as.numeric(as.character(Tiempo.Muestreo.Patio))) %>%
  dplyr::filter(!is.na(Tiempo_num))
datos_q$Tratamiento <- factor(datos_q$Tratamiento)

# Formato largo
datos_long <- datos_q %>%
  tidyr::pivot_longer(dplyr::all_of(vars_analisis),
                      names_to = "Variable", values_to = "Valor") %>%
  dplyr::filter(!is.na(Valor))

# ============================================================
# 2. Ajuste de regresión lineal por Variable × Tratamiento
# ============================================================
tabla_reg <- datos_long %>%
  dplyr::group_by(Tratamiento, Variable) %>%
  dplyr::group_modify(~ {
    fit <- lm(Valor ~ Tiempo_num, data = .x)
    s   <- summary(fit)
    tibble::tibble(
      n          = nrow(.x),
      intercepto = coef(fit)[1],
      pendiente  = coef(fit)[2],
      SE_pend    = s$coefficients[2, "Std. Error"],
      t_valor    = s$coefficients[2, "t value"],
      p_valor    = s$coefficients[2, "Pr(>|t|)"],
      R2         = s$r.squared,
      R2_ajust   = s$adj.r.squared
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Direccion = dplyr::case_when(
      pendiente >  0 & Variable %in% vars_menor_mejor ~ "Aumenta (deterioro)",
      pendiente <  0 & Variable %in% vars_menor_mejor ~ "Disminuye (mejora)",
      pendiente >  0                                   ~ "Aumenta",
      pendiente <  0                                   ~ "Disminuye",
      TRUE                                             ~ "Estable"
    ),
    Signif = dplyr::case_when(
      is.na(p_valor)   ~ "",
      p_valor < 0.01   ~ "***",
      p_valor < 0.05   ~ "**",
      p_valor < alpha  ~ "*",
      TRUE             ~ "ns"
    ),
    Interpretacion = dplyr::case_when(
      Variable %in% vars_menor_mejor & pendiente > 0 & p_valor < alpha ~
        "Deterioro significativo",
      Variable %in% vars_menor_mejor & pendiente < 0 & p_valor < alpha ~
        "Mejora significativa",
      !Variable %in% vars_menor_mejor & pendiente < 0 & p_valor < alpha ~
        "Deterioro significativo",
      !Variable %in% vars_menor_mejor & pendiente > 0 & p_valor < alpha ~
        "Mejora significativa",
      p_valor < alpha ~ "Cambio significativo",
      TRUE            ~ "Sin cambio significativo"
    )
  )

# ---- Tabla ordenada para visualizar ----
tabla_reg_ord <- tabla_reg %>%
  dplyr::select(Tratamiento, Variable, pendiente, SE_pend,
                p_valor, R2, Signif, Direccion, Interpretacion) %>%
  dplyr::arrange(Tratamiento, Variable) %>%
  dplyr::mutate(
    pendiente = round(pendiente, 4),
    SE_pend   = round(SE_pend, 4),
    p_valor   = signif(p_valor, 4),
    R2        = round(R2, 4)
  )

cat("\n=========================================================\n")
cat("REGRESIÓN LINEAL: Valor ~ Tiempo de patio\n")
cat("(por Variable × Tratamiento)\n")
cat("=========================================================\n")
print(as.data.frame(tabla_reg_ord), row.names = FALSE)

write.csv(tabla_reg_ord, "tabla_regresiones_tiempo.csv", row.names = FALSE)

# ============================================================
# 3. Tabla resumen ancha: pendiente por tratamiento × variable
# ============================================================
tabla_pendiente_ancha <- tabla_reg %>%
  dplyr::mutate(Celda = sprintf("%+.4f %s", pendiente, Signif)) %>%
  dplyr::select(Variable, Tratamiento, Celda) %>%
  tidyr::pivot_wider(names_from = Tratamiento, values_from = Celda) %>%
  dplyr::arrange(Variable)

cat("\n=========================================================\n")
cat("TABLA ANCHA: PENDIENTES POR TRATAMIENTO Y VARIABLE\n")
cat("(signo = dirección del cambio; * p<0.15, ** p<0.05, *** p<0.01)\n")
cat("=========================================================\n")
print(as.data.frame(tabla_pendiente_ancha), row.names = FALSE)

write.csv(tabla_pendiente_ancha, "tabla_pendientes_ancha.csv", row.names = FALSE)

# ============================================================
# 4. Gráfico: pendientes por variable, facetado por tratamiento
# ============================================================
grafico_pendientes <- function(trat) {
  sub <- tabla_reg %>% dplyr::filter(Tratamiento == trat)
  
  ggplot(sub, aes(x = pendiente,
                  y = reorder(Variable, pendiente),
                  fill = Signif != "ns")) +
    geom_col(color = "grey25", linewidth = 0.4, width = 0.68) +
    geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
    # Etiqueta con valor + significancia
    geom_text(aes(label = sprintf("%+.4f %s", pendiente, Signif),
                  hjust = ifelse(pendiente >= 0, -0.10, 1.10)),
              size = 3.3, fontface = "bold", color = "black") +
    scale_fill_manual(values = c(`TRUE` = "#2C7FB8", `FALSE` = "grey75"),
                      labels = c("No significativa", "Significativa")) +
    scale_x_continuous(
      expand = expansion(mult = c(0.25, 0.30))
    ) +
    labs(title    = paste("Tratamiento:", trat),
         subtitle = "Pendiente de la regresión lineal (Valor ~ Tiempo de patio)",
         x        = "Pendiente por hora (unidades/h)",
         y        = NULL,
         fill     = "Tendencia") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "grey93", linewidth = 0.3),
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 9.5, color = "grey45"),
      legend.position  = "bottom",
      legend.key.width = unit(0.9, "cm"),
      plot.margin      = margin(10, 15, 8, 8)
    )
}

graficos_pend <- lapply(unique(tabla_reg$Tratamiento), grafico_pendientes)
names(graficos_pend) <- unique(tabla_reg$Tratamiento)

for (trat in names(graficos_pend)) {
  print(graficos_pend[[trat]])
  ggsave(paste0("pendientes_", gsub("[^A-Za-z0-9]", "_", trat), ".png"),
         graficos_pend[[trat]],
         width = 8.5, height = 5.5, dpi = 300, bg = "white")
}

# ---- Panel facetado ----
p_all_pend <- wrap_plots(graficos_pend, ncol = 2, guides = "collect") +
  plot_annotation(
    title    = "Pendientes de regresión: Tiempo de patio vs variables",
    subtitle = paste0("Pendiente > 0 = la variable aumenta con el tiempo. ",
                      "Pendiente < 0 = disminuye. Significancia: * p<0.15, ** p<0.05, *** p<0.01."),
    caption  = "Modelo: Valor ~ Tiempo_num ajustado por Variable × Tratamiento.",
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 17, hjust = 0),
      plot.subtitle = element_text(size = 10.5, color = "grey30", lineheight = 1.2),
      plot.caption  = element_text(size = 9, color = "grey55", hjust = 0),
      legend.position = "bottom"
    )
  ) & theme(legend.position = "bottom")

print(p_all_pend)
ggsave("pendientes_facetado.png", p_all_pend,
       width = 15, height = 10, dpi = 300, bg = "white")

# ============================================================
# 5. Heatmap de pendientes (color = magnitud, signo = dirección)
# ============================================================
lim <- max(abs(tabla_reg$pendiente), na.rm = TRUE)

# Escalar todas las pendientes al rango [-1, 1] para que variables con
# distintas unidades sean comparables en el heatmap
tabla_reg_norm <- tabla_reg %>%
  dplyr::group_by(Variable) %>%
  dplyr::mutate(
    pendiente_norm = pendiente / max(abs(pendiente), na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

p_heat_pend <- ggplot(tabla_reg_norm,
                      aes(x = Tratamiento, y = Variable,
                          fill = pendiente_norm)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%+.3f\n%s",
                                pendiente, Signif)),
            size = 3.1, fontface = "bold",
            color = "black", lineheight = 0.9) +
  scale_fill_gradient2(
    low      = "#B2182B",
    mid      = "white",
    high     = "#2166AC",
    midpoint = 0, limits = c(-1, 1),
    name     = "Pendiente\nnormalizada"
  ) +
  labs(title    = "Mapa de calor de pendientes por tratamiento",
       subtitle = paste0("Color = pendiente normalizada por variable (azul = aumenta, rojo = disminuye).\n",
                         "Texto = pendiente real + significancia (* p<0.15, ** p<0.05, *** p<0.01)."),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title    = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 10, color = "grey30", lineheight = 1.2),
    axis.text.x   = element_text(face = "bold", size = 11, angle = 20, hjust = 1),
    axis.text.y   = element_text(face = "bold", size = 11),
    panel.grid    = element_blank(),
    legend.position = "right"
  )

print(p_heat_pend)
ggsave("pendientes_heatmap.png", p_heat_pend,
       width = 11, height = 6.5, dpi = 300, bg = "white")

# ============================================================
# 6. Gráfico con rectas ajustadas por variable y tratamiento
# ============================================================
grafico_rectas <- function(v) {
  sub <- datos_long %>% dplyr::filter(Variable == v)
  
  tabla_v <- tabla_reg %>% dplyr::filter(Variable == v)
  
  ggplot(sub, aes(x = Tiempo_num, y = Valor,
                  color = Tratamiento, group = Tratamiento)) +
    geom_point(alpha = 0.45, size = 1.6) +
    geom_smooth(method = "lm", se = TRUE, alpha = 0.15, linewidth = 1) +
    facet_wrap(~ Tratamiento, scales = "free_y", ncol = 4) +
    scale_color_manual(values = c(
      "Bioxida 0.50%" = "#E41A1C",
      "Bioxida 1.00%" = "#4DAF4A",
      "Digra LM-25M"  = "#377EB8",
      "Testigo"       = "#984EA3"
    )) +
    labs(title    = v,
         subtitle = "Recta ajustada con IC 95% (modelo lineal)",
         x        = "Tiempo de patio (h)",
         y        = v,
         color    = NULL) +
    theme_bw(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 9.5, color = "grey45"),
      strip.text    = element_text(face = "bold", size = 10),
      strip.background = element_rect(fill = "grey96", color = NA),
      legend.position  = "none"
    )
}

for (v in vars_analisis) {
  p <- grafico_rectas(v)
  print(p)
  ggsave(paste0("rectas_", gsub("[^A-Za-z]", "_", v), ".png"),
         p, width = 13, height = 3.5, dpi = 300, bg = "white")
}




# ============================================================
# HEATMAP DE INTERACCIÓN Tratamiento × Tiempo
# - Dextrana y Az.Reductor: MENOR = MEJOR (verde)
# - Resto: MAYOR = MEJOR (verde)
# - Mejor celda marcada con asterisco (sin marcos dorados)
# - Solo se muestran celdas con dato
# ============================================================

pkgs <- c("dplyr","tidyr","ggplot2","patchwork","stringr")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p)
library(ggplot2); library(tidyr); library(patchwork); library(stringr)
library(dplyr)

# ============================================================
# 1. Datos
# ============================================================
datos <- tibble::tribble(
  ~No,   ~Tratamiento,          ~Brix,      ~Pol,       ~POL...JUGO, ~Pza,       ~X..Az.Reductor, ~Dextrana.PPM,
  "T1",  "Bioxida 0.50%:0",     "16.20 a",  "55.46 a",  "13.56 a",   "83.68 ab", "0.77 cde",      "535.00 fg",
  "T2",  "Bioxida 0.50%:24",    "15.66 cde","52.91 bcd","12.96 bcd", "82.80 abc","0.92 abcd",     "501.50 g",
  "T3",  "Bioxida 0.50%:36",    "15.03 g",  "49.04 gh", "12.05 hi",  "80.19 de", "0.70 e",        "656.50 bcd",
  "T4",  "Bioxida 0.50%:48",    "15.64 cdef","52.13 cdef","12.78 cdefg","81.69 bcde","0.83 abcde","669.00 bcd",
  "T5",  "Bioxida 0.50%:9",     "15.94 abc","54.22 ab", "13.27 ab",  "83.28 ab", "0.78 bcde",     NA,
  "T6",  "Bioxida 1.00%:0",     "15.33 fg", "50.70 efg","12.44 fgh", "81.17 cde","0.94 abc",      "575.00 def",
  "T7",  "Bioxida 1.00%:24",    "15.45 ef", "51.70 def","12.68 defg","82.04 abcd","0.91 abcd",    "590.00 cdef",
  "T8",  "Bioxida 1.00%:36",    "15.22 fg", "50.76 efg","12.46 efgh","82.02 abcd","0.90 abcd",    "417.50 h",
  "T9",  "Bioxida 1.00%:48",    "15.56 def","51.63 defg","12.66 defgh","81.35 bcde","1.06 ab",     "679.00 bc",
  "T10", "Bioxida 1.00%:9",     "15.65 cde","53.70 abc","13.16 abc", "84.06 a",  "0.81 bcde",     NA,
  "T11", "Digra LM-25M:0",      "14.67 h",  "47.49 h",  "11.68 i",   "79.66 e",  "0.97 ab",       "613.50 bcde",
  "T12", "Digra LM-25M:24",     "15.68 cde","52.59 bcd","12.88 bcde","82.15 abcd","0.80 bcde",    "783.00 a",
  "T13", "Digra LM-25M:36",     "15.58 def","52.15 cde","12.79 cdef","82.06 abcd","0.75 de",      "678.50 bc",
  "T14", "Digra LM-25M:48",     "15.40 efg","50.29 efgh","12.34 fghi","80.11 de","1.11 a",        "694.00 ab",
  "T15", "Digra LM-25M:9",      "15.81 bcd","53.22 bcd","13.04 bcd", "82.42 abc","0.75 de",       NA,
  "T16", "Testigo:-4",          "15.52 def","52.76 bcd","12.94 bcd", "83.39 ab", "0.95 abc",      "559.00 efg",
  "T17", "Testigo:0",           "16.12 ab", "55.26 a",  "13.52 a",   "83.86 a",  "0.99 ab",       "567.00 def",
  "T18", "Testigo:24",          "15.76 cde","52.64 bcd","12.89 bcd", "81.80 bcde","0.71 e",       "529.00 fg",
  "T19", "Testigo:36",          "15.22 fg", "50.08 fgh","12.29 ghi", "80.76 cde","0.74 de",       "536.00 fg",
  "T20", "Testigo:48",          "15.23 fg", "50.76 defg","12.46 defgh","81.82 abcde","0.86 abcde", "703.00 ab",
  "T21", "Testigo:9",           NA, NA, NA, NA, NA, NA
)

# ============================================================
# 2. Parseo y limpieza
# ============================================================
vars_num <- c("Brix","Pol","POL...JUGO","Pza","X..Az.Reductor","Dextrana.PPM")

letras_long <- datos %>%
  tidyr::separate(Tratamiento, into = c("Biocida","Tiempo"),
                  sep = ":", remove = FALSE, fill = "right") %>%
  dplyr::mutate(Biocida = stringr::str_trim(Biocida),
                Tiempo  = as.numeric(Tiempo)) %>%
  dplyr::filter(!is.na(Tiempo)) %>%
  tidyr::pivot_longer(dplyr::all_of(vars_num),
                      names_to = "Variable_orig", values_to = "Celda") %>%
  dplyr::mutate(
    Valor = as.numeric(stringr::str_extract(Celda, "^[0-9.]+")),
    Letra = stringr::str_extract(Celda, "[a-z]+$"),
    Variable = dplyr::recode(Variable_orig,
                             "POL...JUGO"     = "Pol jugo",
                             "X..Az.Reductor" = "% Az. Reductor",
                             "Dextrana.PPM"   = "Dextrana (ppm)")
  ) %>%
  dplyr::filter(!is.na(Valor), !is.na(Letra))

# Quitar tiempos con menos de 3 tratamientos
letras_long <- letras_long %>%
  dplyr::group_by(Variable, Tiempo) %>%
  dplyr::filter(dplyr::n() >= 3) %>%
  dplyr::ungroup()

orden_biocida <- c("Testigo","Bioxida 0.50%","Bioxida 1.00%","Digra LM-25M")
orden_var     <- c("Brix","Pol","Pol jugo","Pza","% Az. Reductor","Dextrana (ppm)")

letras_long$Biocida  <- factor(letras_long$Biocida,  levels = orden_biocida)
letras_long$Variable <- factor(letras_long$Variable, levels = orden_var)

# ============================================================
# 3. ★ Invertir la escala para "menor = mejor"
#    Marcamos el "mejor" por Variable × Tiempo y ponemos un *
# ============================================================
vars_menor_mejor <- c("Dextrana (ppm)", "% Az. Reductor")

# Marcar el mejor por Variable × Tiempo
letras_long <- letras_long %>%
  dplyr::group_by(Variable, Tiempo) %>%
  dplyr::mutate(
    # mejor = mayor media, salvo en las variables invertidas
    Mejor = ifelse(Variable %in% vars_menor_mejor,
                   Valor == min(Valor, na.rm = TRUE),
                   Valor == max(Valor, na.rm = TRUE)),
    Mejor = ifelse(is.na(Mejor), FALSE, Mejor),
    # ★ Añadir asterisco al mejor
    Celda_txt = sprintf("%.2f\n%s%s", Valor, Letra,
                        ifelse(Mejor, " *", ""))
  ) %>%
  dplyr::ungroup()

# ★ Variable de color "invertida" (verde = mejor)
# Para las variables invertidas usamos -Valor para que el gradiente
# funcione como si fuera mayor = mejor
letras_long <- letras_long %>%
  dplyr::mutate(
    Valor_para_color = ifelse(Variable %in% vars_menor_mejor,
                              -Valor, Valor)
  )

# ============================================================
# 4. Heatmap por variable
# ============================================================
heat_plot <- function(v) {
  
  sub <- letras_long %>% dplyr::filter(Variable == v)
  med <- median(sub$Valor_para_color, na.rm = TRUE)
  
  ggplot(sub, aes(x = factor(Tiempo, levels = sort(unique(Tiempo))),
                  y = Biocida)) +
    geom_tile(aes(fill = Valor_para_color),
              color = "white", linewidth = 0.5) +
    geom_text(aes(label = Celda_txt),
              size = 3.1, fontface = "bold",
              lineheight = 0.9, color = "black") +
    scale_fill_gradient2(
      low      = "#B2182B",
      mid      = "#FFFFBF",
      high     = "#1B7837",
      midpoint = med,
      name     = NULL,
      guide    = "none"
    ) +
    labs(title    = v,
         subtitle = if (v %in% vars_menor_mejor)
           "Menor = mejor (verde). * marca el mejor en cada tiempo"
         else
           "Mayor = mejor (verde). * marca el mejor en cada tiempo",
         x = "Tiempo de patio (h)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid        = element_blank(),
      panel.border      = element_blank(),
      axis.ticks        = element_blank(),
      strip.background  = element_blank(),
      plot.title        = element_text(face = "bold", size = 13),
      plot.subtitle     = element_text(size = 8.5, color = "grey45",
                                       margin = margin(b = 6)),
      axis.title.x      = element_text(face = "bold", size = 10),
      axis.text.x       = element_text(face = "bold", size = 10),
      axis.text.y       = element_text(face = "bold", size = 10),
      plot.margin       = margin(8, 12, 8, 8)
    )
}

graficos_heat <- lapply(orden_var, heat_plot)

# ============================================================
# 5. Panel facetado
# ============================================================
p_heat <- patchwork::wrap_plots(graficos_heat, ncol = 2) +
  patchwork::plot_annotation(
    title    = "Interacción Tratamiento × Tiempo — Mapa de calor",
    subtitle = paste0("Cada celda: media + letra Fisher LSD (α = 0.15). ",
                      "Verde = mejor. * = mejor tratamiento en cada tiempo. ",
                      "Para % Az. Reductor y Dextrana (ppm): MENOR es mejor."),
    theme    = theme(
      plot.title    = element_text(face = "bold", size = 17),
      plot.subtitle = element_text(size = 10.5, color = "grey30", lineheight = 1.2)
    )
  )

print(p_heat)
ggsave("interaccion_heatmap.png", p_heat,
       width = 12, height = 11, dpi = 300, bg = "white")

# ============================================================
# 6. Tabla ancha
# ============================================================
tabla_ancha <- letras_long %>%
  dplyr::mutate(Celda = sprintf("%.2f %s%s", Valor, Letra,
                                ifelse(Mejor, " *", ""))) %>%
  dplyr::select(Variable, Biocida, Tiempo, Celda) %>%
  tidyr::pivot_wider(names_from = Tiempo, values_from = Celda,
                     names_prefix = "t=") %>%
  dplyr::arrange(Variable, Biocida)

cat("\n=========================================================\n")
cat("TABLA ANCHA: media + letra + (* mejor) por variable y biocida\n")
cat("Para % Az. Reductor y Dextrana (ppm): menor = mejor\n")
cat("=========================================================\n")
print(as.data.frame(tabla_ancha), row.names = FALSE)
write.csv(tabla_ancha, "tabla_heatmap_interaccion.csv", row.names = FALSE)

# ============================================================
# 7. Tabla del mejor por Variable × Tiempo
# ============================================================
tabla_mejores <- letras_long %>%
  dplyr::filter(Mejor) %>%
  dplyr::select(Variable, Tiempo, Biocida, Valor, Letra) %>%
  dplyr::arrange(Variable, Tiempo)

cat("\n=========================================================\n")
cat("MEJOR TRATAMIENTO POR VARIABLE Y TIEMPO\n")
cat("=========================================================\n")
print(as.data.frame(tabla_mejores), row.names = FALSE)
write.csv(tabla_mejores, "mejores_por_tiempo.csv", row.names = FALSE)



# ============================================================
# DIFERENCIAL RELATIVO (%) — TESTIGO -4h COMO REFERENCIA
# - El tiempo -4 h APARECE en el eje X
# - El Testigo -4h se muestra como punto de referencia (0 %)
# - Los biocidas se grafican desde 0 h
# - LEYENDA clara al pie con todos los elementos
# ============================================================

pkgs <- c("dplyr","tidyr","plotly","htmlwidgets")
for (p in pkgs) if (!require(p, character.only = TRUE)) install.packages(p)
library(plotly); library(tidyr); library(dplyr)

# ---- Configuración ----
vars_analisis <- c("Brix","Pol","POL...JUGO","Pza",
                   "X..Az.Reductor","Dextrana.PPM")
trat_control  <- "Testigo"
tiempo_base   <- -4      # referencia absoluta

colores_trat <- c(
  "Bioxida 0.50%" = "#E41A1C",   # ROJO
  "Bioxida 1.00%" = "#4DAF4A",   # VERDE
  "Digra LM-25M"  = "#377EB8",   # AZUL
  "Referencia"    = "#1B1B1B"    # NEGRO (punto -4 h)
)

simbolos_trat <- c(
  "Bioxida 0.50%" = "circle",
  "Bioxida 1.00%" = "triangle-up",
  "Digra LM-25M"  = "square",
  "Referencia"    = "diamond"
)

# ============================================================
# 1. Datos
# ============================================================
datos_long <- datos_analisis %>%
  dplyr::select(Tratamiento, Tiempo.Muestreo.Patio,
                dplyr::all_of(vars_analisis)) %>%
  dplyr::mutate(Tiempo_num = as.numeric(as.character(Tiempo.Muestreo.Patio))) %>%
  dplyr::filter(!is.na(Tiempo_num)) %>%
  tidyr::pivot_longer(dplyr::all_of(vars_analisis),
                      names_to = "Variable", values_to = "Valor") %>%
  dplyr::filter(!is.na(Valor))

medias <- datos_long %>%
  dplyr::group_by(Variable, Tratamiento, Tiempo_num) %>%
  dplyr::summarise(
    Media = mean(Valor, na.rm = TRUE),
    SE    = sd(Valor, na.rm = TRUE) / sqrt(dplyr::n()),
    .groups = "drop"
  )

# ============================================================
# 2. Referencia: Testigo a -4 h
# ============================================================
ref_base <- medias %>%
  dplyr::filter(Tratamiento == trat_control,
                Tiempo_num == tiempo_base) %>%
  dplyr::select(Variable, Media_ref = Media)

cat("\n=========================================================\n")
cat("REFERENCIA (Testigo a", tiempo_base, "h):\n")
cat("=========================================================\n")
print(as.data.frame(ref_base), row.names = FALSE)

# ============================================================
# 3. Δ% de cada biocida respecto a -4 h
# ============================================================
dif <- medias %>%
  dplyr::filter(Tratamiento != trat_control) %>%
  dplyr::left_join(ref_base, by = "Variable") %>%
  dplyr::mutate(
    Dif_pct = 100 * (Media - Media_ref) / Media_ref
  ) %>%
  dplyr::filter(!is.na(Dif_pct))

orden_trat <- c("Bioxida 0.50%","Bioxida 1.00%","Digra LM-25M")
dif$Tratamiento <- factor(dif$Tratamiento, levels = orden_trat)

# ============================================================
# 4. Subplots
# ============================================================
plots <- list()

for (i in seq_along(vars_analisis)) {
  
  v   <- vars_analisis[i]
  sub <- dif %>% dplyr::filter(Variable == v) %>% dplyr::arrange(Tiempo_num)
  
  # ★ Eje X incluye -4 h explícitamente
  tiempos_eje <- sort(unique(c(tiempo_base, sub$Tiempo_num)))
  
  p <- plot_ly()
  
  # ---- ★ Punto de referencia Testigo -4 h (0 %) ----
  p <- p %>%
    add_trace(
      x           = tiempo_base,
      y           = 0,
      type        = "scatter",
      mode        = "markers",
      name        = paste0("Testigo (", tiempo_base, " h) - Referencia"),
      legendgroup = "Referencia",
      showlegend  = (i == 1),           # mostrar solo en el primer panel
      marker      = list(
        color  = colores_trat["Referencia"],
        size   = 15,
        symbol = "diamond",
        line   = list(color = "white", width = 2)
      ),
      hovertemplate = paste0(
        "<b>Testigo ", tiempo_base, " h</b><br>",
        "Variable: ", v, "<br>",
        "Tiempo: <b>", tiempo_base, " h</b><br>",
        "Δ% = 0.00% (referencia)",
        "<extra></extra>"
      )
    )
  
  # ---- ★ Línea de los biocidas ----
  for (tr in orden_trat) {
    st <- sub %>% dplyr::filter(as.character(Tratamiento) == tr)
    if (nrow(st) == 0) next
    
    mostrar_leyenda <- (i == 1)
    
    p <- p %>%
      add_trace(
        data        = st,
        x           = ~Tiempo_num,
        y           = ~Dif_pct,
        type        = "scatter",
        mode        = "lines+markers",
        name        = tr,
        legendgroup = tr,
        showlegend  = mostrar_leyenda,
        line        = list(color = colores_trat[tr], width = 3),
        marker      = list(
          color  = colores_trat[tr],
          size   = 12,
          symbol = simbolos_trat[tr],
          line   = list(color = "white", width = 1.8)
        ),
        hovertemplate = paste0(
          "<b style='color:", colores_trat[tr], "'>", tr, "</b><br>",
          "Variable: ", v, "<br>",
          "Tiempo: <b>%{x:.0f} h</b><br>",
          "Δ% vs Testigo ", tiempo_base, " h: <b>%{y:+.2f}%</b>",
          "<extra></extra>"
        )
      )
  }
  
  # ---- Línea horizontal 0 % ----
  p <- p %>%
    add_trace(
      x    = c(min(tiempos_eje), max(tiempos_eje)),
      y    = c(0, 0),
      type = "scatter",
      mode = "lines",
      line = list(color = "grey30", width = 1.2, dash = "dot"),
      hoverinfo = "skip",
      showlegend = FALSE
    )
  
  p <- p %>%
    layout(
      xaxis = list(
        title     = if (i >= 4) "<b>Tiempo de patio (h)</b>" else "",
        tickmode  = "array",
        tickvals  = tiempos_eje,
        ticktext  = as.character(tiempos_eje),
        showgrid  = TRUE,
        gridcolor = "rgba(200,200,200,0.35)",
        zeroline  = FALSE,
        tickfont  = list(size = 10),
        showline  = TRUE,
        linecolor = "grey60"
      ),
      yaxis = list(
        title     = if (i %% 3 == 1)
          paste0("<b>Δ% vs Testigo ", tiempo_base, " h</b>")
        else "",
        showgrid  = TRUE,
        gridcolor = "rgba(200,200,200,0.35)",
        zeroline  = FALSE,
        tickfont  = list(size = 10),
        showline  = TRUE,
        linecolor = "grey60"
      ),
      showlegend = FALSE,
      margin = list(l = 60, r = 30, t = 55, b = 55),
      plot_bgcolor  = "white",
      paper_bgcolor = "white",
      annotations = list(
        list(
          text      = paste0("<b>", v, "</b>"),
          x         = 0, y = 1.12,
          xref      = "paper", yref = "paper",
          xanchor   = "left", showarrow = FALSE,
          font      = list(size = 14, color = "grey15")
        )
      )
    )
  
  plots[[i]] <- p
}

# ============================================================
# 5. Combinar con UNA SOLA LEYENDA
# ============================================================
fig <- subplot(
  plots,
  nrows    = 2,
  shareX   = FALSE,
  shareY   = FALSE,
  titleX   = TRUE,
  titleY   = TRUE,
  margin   = 0.07
) %>%
  layout(
    title = list(
      text = paste0(
        "<b>Diferencial relativo (%) respecto al Testigo a ", tiempo_base, " h</b>",
        "<br><sup>Cada panel = una variable · Referencia = Testigo a ", tiempo_base,
        " h (0 %) · Pasa el cursor sobre los puntos para ver los valores</sup>"
      ),
      x = 0.02, xanchor = "left", font = list(size = 16)
    ),
    # ★ LEYENDA COMPLETA AL PIE
    legend = list(
      orientation = "h",
      xanchor     = "center",
      x           = 0.5,
      y           = -0.10,
      font        = list(size = 14, family = "Arial Black"),
      bgcolor     = "rgba(255,255,255,0.98)",
      bordercolor = "rgba(60,60,60,0.8)",
      borderwidth = 2,
      itemsizing  = "constant",
      itemwidth   = 70,
      traceorder  = "normal",
      title       = list(
        text = "<b>Tratamientos:</b>",
        font = list(size = 14, color = "black")
      )
    ),
    margin = list(l = 70, r = 30, t = 100, b = 140),
    hovermode = "closest",
    plot_bgcolor  = "white",
    paper_bgcolor = "white"
  )

fig

# Guardar
htmlwidgets::saveWidget(fig,
                        file = "diferencial_testigo_-4h.html",
                        selfcontained = TRUE)

cat("\n>> Generado: diferencial_testigo_-4h.html\n")

# ============================================================
# 6. Leyenda explícita en consola
# ============================================================
cat("\n=========================================================\n")
cat("LEYENDA DE TRATAMIENTOS\n")
cat("=========================================================\n")
cat("  [◆] ROMBO     NEGRO   (#1B1B1B)  →  Testigo ", tiempo_base, " h (Referencia = 0%)\n")
cat("  [●] CÍRCULO   ROJO    (#E41A1C)  →  Bioxida 0.50%\n")
cat("  [▲] TRIÁNGULO VERDE   (#4DAF4A)  →  Bioxida 1.00%\n")
cat("  [■] CUADRADO  AZUL    (#377EB8)  →  Digra LM-25M\n")
cat("  [- - -] GRIS PUNTEADO            →  Línea de referencia (0 %)\n")
cat("=========================================================\n")

# ============================================================
# 7. Tabla numérica
# ============================================================
tabla_dif <- dif %>%
  dplyr::mutate(
    Dif_txt = sprintf("%+.1f%%", Dif_pct),
    Tiempo  = sprintf("%.0f h", Tiempo_num)
  ) %>%
  dplyr::select(Variable, Tiempo, Tratamiento, Dif_txt) %>%
  tidyr::pivot_wider(names_from = Tratamiento, values_from = Dif_txt) %>%
  dplyr::arrange(Variable, Tiempo)

cat("\n=========================================================\n")
cat("TABLA: Δ% vs TESTIGO A", tiempo_base, "h\n")
cat("=========================================================\n")
print(as.data.frame(tabla_dif), row.names = FALSE)
write.csv(tabla_dif, "tabla_diferencial_testigo_-4h.csv", row.names = FALSE)






