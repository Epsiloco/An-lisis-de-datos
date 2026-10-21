#################### Análisis de correlación (calidad de jugi vs nivel de daño) ####################

datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))
View(datos)


#################### Análisis de correlación + clúster + correspondencia ####################

# ==============================================================================
# 0. PARÁMETROS GLOBALES
# ==============================================================================
alpha <- 0.15    # Nivel de significancia global

# ==============================================================================
# 1. DATOS
# ==============================================================================
stopifnot(exists("datos"))
cat("\n--- Variables detectadas ---\n"); print(names(datos))
cat("Filas en 'datos':", nrow(datos), "\n")

# ==============================================================================
# 2. LIBRERÍAS
# ==============================================================================
paquetes <- c("corrplot", "Hmisc", "FactoMineR", "ggplot2", "tidyr", "reshape2")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(corrplot); library(Hmisc); library(FactoMineR)
library(ggplot2); library(tidyr); library(reshape2)

# ==============================================================================
# 3. VARIABLES
# ==============================================================================
columna_dano <- "ND"

variables_calidad <- c("Altura", "Brix", "Pureza",
                       "Reductor", "Fibra", "Pol_caña", "Peso_tallo", "Dextrana", "Humedad")

variables_modelo <- c("Altura", "Brix", "Pureza",
                      "Reductor", "Fibra", "Pol_caña", "Peso_tallo", "Dextrana", "Humedad")

columnas_analisis <- c(columna_dano, variables_calidad)

faltantes <- setdiff(columnas_analisis, names(datos))
if (length(faltantes) > 0)
  stop("Estas columnas no existen en 'datos': ",
       paste(faltantes, collapse = ", "))

# ==============================================================================
# 4. SUBCONJUNTO Y PREPARACIÓN
# ==============================================================================
df <- datos[, columnas_analisis, drop = FALSE]
df[] <- lapply(df, function(x) suppressWarnings(as.numeric(as.character(x))))

cat("\n--- NAs por columna ---\n"); print(colSums(is.na(df)))
cat("Filas en df:", nrow(df), "\n")

df$ND_factor <- factor(df[[columna_dano]],
                       levels = sort(unique(df[[columna_dano]])))
cat("\nDistribución de ND:\n"); print(table(df$ND_factor))

# ==============================================================================
# 5. CORRELACIÓN Y CORRELOGRAMA (α = 15 %)
# ==============================================================================
matriz_cor <- rcorr(as.matrix(df[, columnas_analisis]), type = "spearman")
matriz_cor$P[is.na(matriz_cor$P)] <- 0

cat("\n--- MATRIZ DE P-VALORES ---\n"); print(round(matriz_cor$P, 4))

corrplot(matriz_cor$r, p.mat = matriz_cor$P, type = "lower", diag = FALSE,
         sig.level = alpha, insig = "pch", pch.col = "red", pch.cex = 1.5,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         col = colorRampPalette(c("#BB4444","#EE9988","#FFFFFF",
                                  "#77AADD","#4477AA"))(200),
         title = sprintf("Correlograma: Calidad vs Nivel de Daño (α = %.0f%%)",
                         alpha * 100),
         mar = c(0, 0, 2, 0))

# ==============================================================================
# 5b. CORRELACIÓN ESPECÍFICA: Jugo vs ND
# ==============================================================================
cat("\n========================================\n")
cat("Correlación específica: Jugo vs ND\n")
cat("========================================\n")

ct_pearson  <- cor.test(df$Jugo, df$ND, method = "pearson")
ct_spearman <- cor.test(df$Jugo, df$ND, method = "spearman")

cat(sprintf("\nPearson :  r = %.3f   p = %.4f   n = %d\n",
            ct_pearson$estimate,  ct_pearson$p.value,  nrow(df)))
cat(sprintf("Spearman:  rho = %.3f p = %.4f   n = %d\n",
            ct_spearman$estimate, ct_spearman$p.value, nrow(df)))

cat("\nResumen de Jugo por nivel de daño:\n")
print(aggregate(Jugo ~ ND, data = df,
                FUN = function(x) c(n = length(x),
                                    media = mean(x, na.rm = TRUE),
                                    sd    = sd(x,   na.rm = TRUE))))

cat("\nCorrelación de Jugo con cada parámetro de calidad:\n")
vars_otros <- setdiff(variables_calidad, "Jugo")
tabla_jugo <- t(sapply(vars_otros, function(v) {
  ct <- suppressWarnings(cor.test(df$Jugo, df[[v]], method = "spearman"))
  c(rho = unname(ct$estimate), p_valor = ct$p.value)
}))
tabla_jugo <- as.data.frame(round(tabla_jugo, 4))
tabla_jugo$signif <- ifelse(tabla_jugo$p_valor < alpha, "*", "")
print(tabla_jugo)

cor_jugo <- rcorr(as.matrix(df[, variables_calidad]), type = "spearman")
cor_jugo$P[is.na(cor_jugo$P)] <- 0

corrplot(cor_jugo$r, p.mat = cor_jugo$P, type = "full",
         sig.level = alpha, insig = "pch", pch.col = "red", pch.cex = 1.2,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         col = colorRampPalette(c("#BB4444","#EE9988","#FFFFFF",
                                  "#77AADD","#4477AA"))(200),
         title = "Correlaciones de Jugo con el resto",
         mar = c(0, 0, 2, 0))

p_jugo <- ggplot(df, aes(x = ND_factor, y = Jugo, fill = ND_factor)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(width = 0.15, size = 1.8, alpha = 0.7, na.rm = TRUE) +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  labs(title = "Jugo vs Nivel de Daño (ND)",
       subtitle = sprintf("Spearman rho = %.3f  (p = %.4f)",
                          ct_spearman$estimate, ct_spearman$p.value),
       x = "Nivel de Daño", y = "Jugo", fill = "ND")

print(p_jugo)

# ==============================================================================
# 6. MODELO DE REGRESIÓN MÚLTIPLE
# ==============================================================================
formula_lm <- as.formula(paste(columna_dano, "~",
                               paste(variables_modelo, collapse = " + ")))
modelo_lm  <- lm(formula_lm, data = df)

cat("\n--- RESUMEN DEL MODELO MULTIVARIADO ---\n"); print(summary(modelo_lm))
cat("\nFilas usadas por lm:", nobs(modelo_lm), "de", nrow(df), "totales\n")

cat("\n--- ALIAS ---\n"); print(alias(modelo_lm))
if (requireNamespace("car", quietly = TRUE)) {
  cat("\n--- VIF ---\n"); print(car::vif(modelo_lm))
}

# ==============================================================================
# 7. BOXPLOTS
# ==============================================================================
df_long <- pivot_longer(df, cols = all_of(variables_calidad),
                        names_to = "Parametro", values_to = "Valor")

grafico_cajas <- ggplot(df_long,
                        aes(x = ND_factor, y = Valor, fill = ND_factor)) +
  geom_boxplot(alpha = 0.8, outlier.color = "red", outlier.shape = 1,
               na.rm = TRUE) +
  facet_wrap(~ Parametro, scales = "free_y") +
  scale_fill_brewer(palette = "YlOrRd") +
  theme_minimal() +
  labs(title = "Distribución de Parámetros de Calidad por Nivel de Daño",
       subtitle = "Escala: 1 (Normal) al 4 (Daño Severo)",
       x = "Nivel de Daño", y = "Valor del Parámetro", fill = "Nivel de Daño")

print(grafico_cajas)

# ==============================================================================
# 8. PCA
# ==============================================================================
X <- df[, variables_modelo]

na_antes <- colSums(is.na(X))
for (j in seq_along(X)) {
  med <- median(X[[j]], na.rm = TRUE)
  X[[j]][is.na(X[[j]])] <- med
}
cat("\n--- Imputación por mediana (solo para el PCA) ---\n")
cat("NAs antes  :", paste(names(na_antes), na_antes, sep = "=", collapse = ", "), "\n")
cat("NAs después:", paste(colSums(is.na(X)), collapse = ", "), "\n")

res.pca <- PCA(X, scale.unit = TRUE, graph = FALSE)

# ==============================================================================
# 8b. BIPLOT PCA en ggplot2
# ==============================================================================
ind <- as.data.frame(res.pca$ind$coord[, 1:2])
names(ind) <- c("Dim1", "Dim2"); rownames(ind) <- NULL
stopifnot(nrow(ind) == nrow(df))
ind$Grupo <- df$ND_factor

var <- as.data.frame(res.pca$var$coord[, 1:2])
names(var) <- c("Dim1", "Dim2"); var$Variable <- rownames(var)

eig <- res.pca$eig[1:2, 1]
var$Dim1 <- var$Dim1 * sqrt(eig[1]) * 0.75
var$Dim2 <- var$Dim2 * sqrt(eig[2]) * 0.75
pct <- round(res.pca$eig[1:2, 2], 1)

paleta_nd <- c("#00AFBB", "#E7B800", "#FC4E07", "#8B0000", "#808080")
paleta_nd <- paleta_nd[seq_len(nlevels(ind$Grupo))]

biplot_pca <- ggplot() +
  geom_point(data = ind, aes(Dim1, Dim2, color = Grupo),
             size = 2.5, alpha = 0.85) +
  stat_ellipse(data = ind, aes(Dim1, Dim2, color = Grupo),
               level = 0.95, type = "norm", linetype = 2, linewidth = 0.4) +
  geom_segment(data = var,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.18, "cm")),
               color = "black", linewidth = 0.4) +
  geom_text(data = var,
            aes(Dim1 * 1.08, Dim2 * 1.08, label = Variable),
            color = "black", size = 3.5) +
  scale_color_manual(values = paleta_nd) +
  labs(x = paste0("Dim1 (", pct[1], "%)"),
       y = paste0("Dim2 (", pct[2], "%)"),
       title = "PCA: Agrupación Multivariada por Nivel de Daño",
       color = "Nivel de Daño (ND)") +
  theme_minimal()

print(biplot_pca)

# ==============================================================================
# 9. ANÁLISIS DE CLÚSTER
# ==============================================================================
cat("\n========================================\n")
cat("9a. Clúster de variables de calidad\n")
cat("========================================\n")

cor_vars  <- cor(df[, variables_calidad], use = "pairwise.complete.obs")
dist_vars <- as.dist(1 - abs(cor_vars))
hc_vars   <- hclust(dist_vars, method = "ward.D2")

plot(hc_vars,
     main = "Clúster de variables de calidad (1 - |cor|)",
     xlab = "", sub = "", ylab = "Distancia (1 - |cor|)")
rect.hclust(hc_vars, k = 3, border = 2:4)

grupos_vars <- cutree(hc_vars, k = 3)
cat("\nAsignación de variables a 3 grupos:\n"); print(grupos_vars)

cat("\n========================================\n")
cat("9b. Clúster de muestras (HCPC sobre PCA)\n")
cat("========================================\n")

res.hcpc <- HCPC(res.pca, nb.clust = -1, consol = TRUE, graph = FALSE)

cat("\n--- Variables que caracterizan cada clúster (α = ",
    alpha * 100, "%) ---\n", sep = "")
caract_list <- res.hcpc$desc.var$quanti
tabla_caract <- do.call(rbind, lapply(names(caract_list), function(cl) {
  d <- caract_list[[cl]]
  d <- d[d$p.value < alpha, , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  data.frame(Cluster = cl,
             Variable = rownames(d),
             Media_cluster = round(d[["Mean in category"]], 2),
             Media_global  = round(d[["Overall mean"]], 2),
             p_valor = round(d$p.value, 4))
}))
if (!is.null(tabla_caract) && nrow(tabla_caract) > 0) {
  tabla_caract$Tendencia <- ifelse(tabla_caract$Media_cluster > tabla_caract$Media_global,
                                   "Alta", "Baja")
  print(tabla_caract[order(tabla_caract$Cluster, tabla_caract$p_valor), ])
} else {
  cat("Ningún clúster tiene variables caracterizantes al ", alpha*100, "%.\n", sep = "")
}

cat("\n--- Cruce clúster x nivel de daño ---\n")
tabla_hcpc <- table(Cluster = res.hcpc$data.clust$clust,
                    ND      = df$ND_factor)
print(tabla_hcpc)

cat("\n--- Chi-cuadrado clúster x ND ---\n")
ct_hcpc <- suppressWarnings(chisq.test(tabla_hcpc))
print(ct_hcpc)
cat(sprintf("Significativo al %.0f%%: %s\n", alpha * 100,
            ifelse(ct_hcpc$p.value < alpha, "Sí", "No")))

ind_hcpc <- data.frame(
  Dim1    = res.pca$ind$coord[, 1],
  Dim2    = res.pca$ind$coord[, 2],
  Cluster = factor(res.hcpc$data.clust$clust),
  ND      = df$ND_factor
)

p_clust <- ggplot(ind_hcpc,
                  aes(Dim1, Dim2, color = Cluster, shape = ND)) +
  geom_point(size = 3, alpha = 0.85) +
  stat_ellipse(aes(group = Cluster), type = "norm",
               linetype = 2, linewidth = 0.4) +
  labs(title = "Clúster de muestras (HCPC) sobre el plano PCA",
       subtitle = "Color = clúster; forma = nivel de daño",
       x = paste0("Dim1 (", pct[1], "%)"),
       y = paste0("Dim2 (", pct[2], "%)")) +
  theme_minimal()

print(p_clust)

# ==============================================================================
# 10. ANÁLISIS DE CORRESPONDENCIA (MCA)
# ==============================================================================
cat("\n========================================\n")
cat("10. Análisis de correspondencia (MCA)\n")
cat("========================================\n")

df_cat <- data.frame(ND = factor(df$ND))
for (v in variables_calidad) {
  qs <- unique(quantile(df[[v]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
  if (length(qs) < 3) next
  etiquetas <- c("Bajo", "Medio", "Alto")[seq_len(length(qs) - 1)]
  df_cat[[v]] <- cut(df[[v]], breaks = qs,
                     labels = etiquetas, include.lowest = TRUE)
}

cat("\n--- Chi-cuadrado ND × variable categorizada (α = ",
    alpha * 100, "%) ---\n", sep = "")
chi_res <- data.frame()
for (v in setdiff(names(df_cat), "ND")) {
  tt <- table(df_cat$ND, df_cat[[v]])
  ct <- suppressWarnings(chisq.test(tt))
  chi_res <- rbind(chi_res, data.frame(
    Variable = v,
    chi2     = round(unname(ct$statistic), 2),
    gl       = unname(ct$parameter),
    p_valor  = round(ct$p.value, 4),
    Sig      = ifelse(ct$p.value < alpha, "Sí", "No")
  ))
}
chi_res <- chi_res[order(chi_res$p_valor), ]
print(chi_res)

res.mca <- MCA(df_cat, graph = FALSE)
pct_mca <- round(res.mca$eig[1:2, 2], 1)

ind_mca <- as.data.frame(res.mca$ind$coord[, 1:2])
names(ind_mca) <- c("Dim1", "Dim2"); ind_mca$ND <- df_cat$ND

var_mca <- as.data.frame(res.mca$var$coord[, 1:2])
names(var_mca) <- c("Dim1", "Dim2"); var_mca$Categoria <- rownames(var_mca)

paleta_mca <- c("#00AFBB", "#E7B800", "#FC4E07", "#8B0000", "#808080")
paleta_mca <- paleta_mca[seq_len(nlevels(ind_mca$ND))]

p_mca <- ggplot() +
  geom_point(data = ind_mca, aes(Dim1, Dim2, color = ND),
             size = 2, alpha = 0.55) +
  stat_ellipse(data = ind_mca, aes(Dim1, Dim2, color = ND),
               level = 0.95, type = "norm", linetype = 2, linewidth = 0.4) +
  geom_segment(data = var_mca,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.15, "cm")),
               color = "darkred", linewidth = 0.3) +
  geom_text(data = var_mca,
            aes(Dim1 * 1.06, Dim2 * 1.06, label = Categoria),
            color = "darkred", size = 2.8, fontface = "bold") +
  scale_color_manual(values = paleta_mca) +
  labs(title = "MCA: Asociación ND ↔ categorías de calidad",
       subtitle = "Puntos = muestras por ND; flechas rojas = categorías Bajo/Medio/Alto",
       x = paste0("Dim1 (", pct_mca[1], "%)"),
       y = paste0("Dim2 (", pct_mca[2], "%)"),
       color = "Nivel de Daño") +
  theme_minimal()

print(p_mca)

cat("\n--- Contribuciones de las categorías a Dim1 y Dim2 (top 15) ---\n")
contrib <- as.data.frame(round(res.mca$var$contrib[, 1:2], 2))
contrib <- contrib[order(-contrib[["Dim 1"]]), ]
print(head(contrib, 15))

# ==============================================================================
# 11. TABLA MAESTRA: VARIABLE ↔ ND (α = 15 %)
# ==============================================================================
cat("\n========================================\n")
cat("11. TABLA MAESTRA: variable ↔ ND (α = ",
    alpha * 100, "%) \n", sep = "")
cat("========================================\n")

tabla_maestra <- data.frame()
for (v in variables_calidad) {
  f <- as.formula(paste(v, "~ ND_factor"))
  kt <- kruskal.test(f, data = df)
  medias <- tapply(df[[v]], df$ND_factor, mean, na.rm = TRUE)
  medias <- medias[!is.na(medias)]
  if (length(medias) == 0) next
  nd_max <- names(medias)[which.max(medias)]
  nd_min <- names(medias)[which.min(medias)]
  
  tabla_maestra <- rbind(tabla_maestra, data.frame(
    Variable        = v,
    p_KW            = round(kt$p.value, 4),
    Sig_15          = ifelse(kt$p.value < alpha, "Sí", "No"),
    ND_mas_alto     = nd_max,
    Media_mas_alta  = round(medias[nd_max], 2),
    ND_mas_bajo     = nd_min,
    Media_mas_baja  = round(medias[nd_min], 2),
    Asociada_a_ND   = ifelse(kt$p.value < alpha, nd_max, "-"),
    Tendencia       = ifelse(kt$p.value < alpha,
                             paste0("Mayor valor en ND = ", nd_max),
                             "Sin patrón claro")
  ))
}
tabla_maestra <- tabla_maestra[order(tabla_maestra$p_KW), ]

cat("\n--- Tabla maestra (ordenada por p-valor) ---\n")
print(tabla_maestra, row.names = FALSE)

cat("\n--- Comparaciones por pares (Wilcoxon, α = ",
    alpha * 100, "%) ---\n", sep = "")
pares <- combn(levels(df$ND_factor), 2)
pair_res <- data.frame()
for (v in variables_calidad) {
  for (i in seq_len(ncol(pares))) {
    g1 <- pares[1, i]; g2 <- pares[2, i]
    x1 <- df[[v]][df$ND_factor == g1]
    x2 <- df[[v]][df$ND_factor == g2]
    if (sum(!is.na(x1)) < 2 || sum(!is.na(x2)) < 2) next
    wt <- suppressWarnings(wilcox.test(x1, x2))
    pair_res <- rbind(pair_res, data.frame(
      Variable = v, Grupo1 = g1, Grupo2 = g2,
      p_valor = round(wt$p.value, 4),
      Sig_15 = ifelse(wt$p.value < alpha, "Sí", "No")
    ))
  }
}
matriz_pares <- reshape(pair_res[, c("Variable","Grupo1","Grupo2","Sig_15")],
                        idvar = "Variable",
                        timevar = c("Grupo1","Grupo2"),
                        direction = "wide")
cat("\n--- Matriz de comparaciones por pares (Sí = difieren al ",
    alpha * 100, "%) ---\n", sep = "")
print(matriz_pares, row.names = FALSE)

cat("\n--- Vista rápida: variables asociadas a cada nivel de daño ---\n")
vista <- data.frame(ND = levels(df$ND_factor), Variables = NA_character_)
for (i in seq_len(nrow(vista))) {
  nd <- vista$ND[i]
  vars_nd <- tabla_maestra$Variable[tabla_maestra$Asociada_a_ND == nd]
  vista$Variables[i] <- if (length(vars_nd) == 0) "-" else
    paste(vars_nd, collapse = ", ")
}
print(vista, row.names = FALSE)

# ==============================================================================
# 11d. HEATMAP z-score (relativo, para comparar patrones entre variables)
# ==============================================================================
df_medias <- aggregate(df[, variables_calidad],
                       by = list(ND = df$ND_factor),
                       FUN = function(x) mean(x, na.rm = TRUE))

mat_medias <- as.matrix(df_medias[, -1])
mat_z <- scale(mat_medias)

df_hm <- melt(mat_z, varnames = c("ND", "Variable"), value.name = "z")
df_hm$ND <- factor(df_hm$ND, levels = levels(df$ND_factor))

p_hm_z <- ggplot(df_hm, aes(x = ND, y = Variable, fill = z)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = round(z, 2)), size = 3.2, color = "black") +
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA",
                       midpoint = 0, name = "z-score") +
  labs(title = "Heatmap z-score: patrón relativo de cada variable por ND",
       subtitle = sprintf("Rojo = por debajo del promedio global; azul = por encima. α = %.0f%%",
                          alpha * 100),
       x = "Nivel de Daño", y = "Variable") +
  theme_minimal()
print(p_hm_z)

# ==============================================================================
# 11e. MAPA DE CALOR CON VALORES PROMEDIO CRUDOS (facetado, escala libre)
# ==============================================================================
cat("\n========================================\n")
cat("11e. Mapa de calor: promedio crudo por ND y variable\n")
cat("========================================\n")

# Ordenar variables por p-valor de Kruskal-Wallis (más asociadas primero)
orden_vars <- tabla_maestra$Variable[order(tabla_maestra$p_KW)]

df_medias_long <- melt(df_medias, id.vars = "ND",
                       variable.name = "Variable", value.name = "Media")
df_medias_long$ND <- factor(df_medias_long$ND, levels = levels(df$ND_factor))
df_medias_long$Variable <- factor(df_medias_long$Variable, levels = orden_vars)

# Agregar significancia y una etiqueta de tendencia
df_medias_long$Sig     <- tabla_maestra$Sig_15[match(df_medias_long$Variable,
                                                     tabla_maestra$Variable)]
df_medias_long$ND_max  <- tabla_maestra$ND_mas_alto[match(df_medias_long$Variable,
                                                          tabla_maestra$Variable)]
df_medias_long$Es_max  <- ifelse(as.character(df_medias_long$ND) ==
                                   df_medias_long$ND_max, "★", "")

p_hm_crudo <- ggplot(df_medias_long,
                     aes(x = ND, y = 1, fill = Media)) +
  geom_tile(color = "white", linewidth = 1.2) +
  geom_text(aes(label = sprintf("%.2f%s", Media, Es_max)),
            color = "black", size = 4.2, fontface = "bold") +
  facet_wrap(~ Variable, ncol = 4, scales = "free") +
  scale_fill_gradient(low = "#FFF4D2", high = "#8B0000",
                      name = "Promedio") +
  labs(title = "Promedio crudo de cada variable por nivel de daño",
       subtitle = sprintf("★ = nivel de daño con el promedio más alto de esa variable. Escala de color libre por variable. α = %.0f%%",
                          alpha * 100),
       x = "Nivel de Daño (ND)", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid  = element_blank(),
        strip.text  = element_text(face = "bold", size = 10),
        strip.background = element_rect(fill = "grey92", color = NA))

print(p_hm_crudo)

# ==============================================================================
# 11f. MAPA DE CALOR ÚNICO CON PROMEDIOS CRUDOS COMO TEXTO
#      y color = patrón dentro de cada variable (para ver tendencias juntas)
# ==============================================================================
# Para que las variables con distintas unidades sean comparables en color,
# normalizamos por fila (cada variable a su escala), pero MOSTRAMOS el valor crudo.
df_medias_z <- df_medias
df_medias_z[, -1] <- scale(as.matrix(df_medias[, -1]))

df_hm_dual <- melt(df_medias_z, id.vars = "ND",
                   variable.name = "Variable", value.name = "z")
df_hm_dual$Media <- melt(df_medias, id.vars = "ND",
                         variable.name = "Variable", value.name = "Media")$Media
df_hm_dual$ND <- factor(df_hm_dual$ND, levels = levels(df$ND_factor))
df_hm_dual$Variable <- factor(df_hm_dual$Variable, levels = rev(orden_vars))

p_hm_dual <- ggplot(df_hm_dual, aes(x = ND, y = Variable, fill = z)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f", Media)),
            size = 3.6, color = "black", fontface = "bold") +
  scale_fill_gradient2(low = "#BB4444", mid = "white", high = "#4477AA",
                       midpoint = 0, name = "Patrón\n(z-score)") +
  labs(title = "Promedio de cada variable por nivel de daño",
       subtitle = paste0("Texto = promedio crudo. Color = patrón relativo ",
                         "dentro de la misma variable (rojo = bajo, azul = alto).\n",
                         "Filas ordenadas de arriba a abajo por asociación con ND ",
                         "(Kruskal-Wallis)."),
       x = "Nivel de Daño (ND)", y = "Variable") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold"))

print(p_hm_dual)

# ==============================================================================
# 11g. TENDENCIAS: media ± error estándar por ND (facetado)
# ==============================================================================
df_sum_long <- pivot_longer(df, cols = all_of(variables_calidad),
                            names_to = "Variable", values_to = "Valor")

df_summary <- aggregate(Valor ~ Variable + ND_factor,
                        data = df_sum_long,
                        FUN = function(x) {
                          c(media = mean(x, na.rm = TRUE),
                            se    = sd(x, na.rm = TRUE) /
                              sqrt(sum(!is.na(x))))
                        })
df_summary <- do.call(data.frame, df_summary)
names(df_summary) <- c("Variable", "ND_factor", "media", "se")
df_summary$Variable <- factor(df_summary$Variable, levels = orden_vars)

p_tend <- ggplot(df_summary,
                 aes(x = ND_factor, y = media, group = 1)) +
  geom_line(color = "#4477AA", linewidth = 0.7) +
  geom_point(size = 2.5, color = "#4477AA") +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                width = 0.15, color = "#4477AA", linewidth = 0.5) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
  labs(title = "Tendencia de cada variable a lo largo de los niveles de daño",
       subtitle = "Punto = media; barras = ± 1 error estándar. Filas ordenadas por asociación con ND.",
       x = "Nivel de Daño (ND)", y = "Media ± SE") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

print(p_tend)

# ==============================================================================
# 11h. TABLA COMPACTA DE PROMEDIOS (± DE) POR VARIABLE Y ND
# ==============================================================================
cat("\n--- Tabla: promedio ± DE por variable y nivel de daño ---\n")
tabla_prom <- data.frame(Variable = variables_calidad)
for (nd in levels(df$ND_factor)) {
  col <- sapply(variables_calidad, function(v) {
    x <- df[[v]][df$ND_factor == nd]
    if (all(is.na(x))) return("NA")
    sprintf("%.2f ± %.2f", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  })
  tabla_prom[[paste0("ND=", nd)]] <- col
}
# Añadir p-valor KW al final
tabla_prom$`p_KW` <- round(tabla_maestra$p_KW[
  match(tabla_prom$Variable, tabla_maestra$Variable)], 4)
tabla_prom$`Sig_15` <- ifelse(tabla_prom$`p_KW` < alpha, "*", "")
tabla_prom <- tabla_prom[order(tabla_prom$`p_KW`), ]
print(tabla_prom, row.names = FALSE)


# ==============================================================================
# 12. EXPORTAR TODOS LOS RESULTADOS A UN PDF
# ==============================================================================

# ------------------------------------------------------------------------------
# 12.0 VERIFICACIONES PREVIAS
# ------------------------------------------------------------------------------
objetos_requeridos <- c(
  "df", "alpha", "variables_calidad", "variables_modelo",
  "matriz_cor", "cor_jugo", "p_jugo", "grafico_cajas",
  "biplot_pca", "hc_vars", "grupos_vars", "p_clust", "p_mca",
  "p_hm_z", "p_hm_crudo", "p_hm_dual", "p_tend",
  "tabla_maestra", "tabla_prom", "matriz_pares", "vista",
  "tabla_jugo", "modelo_lm", "tabla_hcpc", "ct_hcpc",
  "chi_res", "contrib", "res.pca", "res.mca"
)
faltan_obj <- objetos_requeridos[!sapply(objetos_requeridos, exists)]
if (length(faltan_obj) > 0) {
  stop("Faltan objetos en el entorno: ", paste(faltan_obj, collapse = ", "),
       "\nCorre primero las secciones 0 a 11 en la MISMA sesión de R.")
}

# ------------------------------------------------------------------------------
# 12.1 LIBRERÍAS
# ------------------------------------------------------------------------------
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")
if (!requireNamespace("grid",      quietly = TRUE)) install.packages("grid")
library(gridExtra)
library(grid)

# ------------------------------------------------------------------------------
# 12.2 HELPERS (definidos aquí, no en otra parte)
# ------------------------------------------------------------------------------
pagina_tabla <- function(titulo, tabla, subtitulo = NULL) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(20, 1)))
  grid.text(titulo, vp = viewport(layout.pos.row = 1),
            gp = gpar(fontsize = 14, fontface = "bold"))
  if (!is.null(subtitulo)) {
    grid.text(subtitulo, vp = viewport(layout.pos.row = 2),
              gp = gpar(fontsize = 10, col = "gray40"))
    fila_tabla <- 3:20
  } else {
    fila_tabla <- 2:20
  }
  tg <- tableGrob(tabla, rows = NULL,
                  theme = ttheme_minimal(base_size = 9))
  pushViewport(viewport(layout.pos.row = fila_tabla))
  grid.draw(tg)
  popViewport(2)
}

pagina_plot <- function(titulo, p) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(20, 1)))
  grid.text(titulo, vp = viewport(layout.pos.row = 1),
            gp = gpar(fontsize = 13, fontface = "bold"))
  print(p, vp = viewport(layout.pos.row = 2:20))
  popViewport()
}

# ------------------------------------------------------------------------------
# 12.3 ABRIR EL DISPOSITIVO PDF
# ------------------------------------------------------------------------------
ruta_pdf <- "Analisis_calidad_vs_ND.pdf"

# Cerrar cualquier device abierto previamente
while (dev.cur() > 1) dev.off()

if (capabilities("cairo")) {
  cairo_pdf(ruta_pdf, width = 11, height = 8.5, onefile = TRUE)
  cat("[PDF] Usando cairo_pdf (soporta ñ y tildes)\n")
} else {
  pdf(ruta_pdf, width = 11, height = 8.5, onefile = TRUE)
  cat("[PDF] Usando pdf() estándar\n")
}

# ------------------------------------------------------------------------------
# 12.4 PORTADA
# ------------------------------------------------------------------------------
grid.newpage()
grid.text("Análisis de correlación: Calidad de jugo vs Nivel de Daño",
          y = 0.78, gp = gpar(fontsize = 19, fontface = "bold"))
grid.text(sprintf("n = %d observaciones   |   α = %.0f%%",
                  nrow(df), alpha * 100),
          y = 0.68, gp = gpar(fontsize = 13))
grid.text(paste("Generado:", format(Sys.Date(), "%d/%m/%Y")),
          y = 0.63, gp = gpar(fontsize = 11, col = "gray30"))
grid.text("Variables de calidad analizadas:",
          y = 0.52, gp = gpar(fontsize = 11, fontface = "bold"))
grid.text(paste(variables_calidad, collapse = " · "),
          y = 0.47, gp = gpar(fontsize = 10))
grid.text(sprintf("Modelo: ND ~ %s",
                  paste(variables_modelo, collapse = " + ")),
          y = 0.36, gp = gpar(fontsize = 9, col = "gray30"))

# ------------------------------------------------------------------------------
# 12.5 GRÁFICOS
# ------------------------------------------------------------------------------
# Correlograma principal
corrplot(matriz_cor$r, p.mat = matriz_cor$P, type = "lower", diag = FALSE,
         sig.level = alpha, insig = "pch", pch.col = "red", pch.cex = 1.5,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         col = colorRampPalette(c("#BB4444","#EE9988","#FFFFFF",
                                  "#77AADD","#4477AA"))(200),
         title = sprintf("Correlograma: Calidad vs ND (α = %.0f%%)",
                         alpha * 100),
         mar = c(0, 0, 2, 0))

# Correlograma de Jugo
corrplot(cor_jugo$r, p.mat = cor_jugo$P, type = "full",
         sig.level = alpha, insig = "pch", pch.col = "red", pch.cex = 1.2,
         tl.col = "black", tl.srt = 45, tl.cex = 0.8,
         col = colorRampPalette(c("#BB4444","#EE9988","#FFFFFF",
                                  "#77AADD","#4477AA"))(200),
         title = "Correlaciones de Jugo con el resto",
         mar = c(0, 0, 2, 0))

# ggplot pages
pagina_plot("Jugo vs Nivel de Daño", p_jugo)
pagina_plot("Distribución de parámetros de calidad por ND", grafico_cajas)
pagina_plot("Biplot PCA por nivel de daño", biplot_pca)

# Dendrograma
plot(hc_vars,
     main = "Clúster de variables de calidad (1 - |cor|)",
     xlab = "", sub = "", ylab = "Distancia (1 - |cor|)")
rect.hclust(hc_vars, k = 3, border = 2:4)

pagina_plot("Clúster de muestras (HCPC) sobre el plano PCA", p_clust)
pagina_plot("MCA: Asociación ND ↔ categorías de calidad", p_mca)
pagina_plot("Heatmap z-score por ND", p_hm_z)
pagina_plot("Mapa de calor: promedio crudo por ND (facetado)", p_hm_crudo)
pagina_plot("Promedio crudo con patrón de color", p_hm_dual)
pagina_plot("Tendencias: media ± error estándar por ND", p_tend)

# ------------------------------------------------------------------------------
# 12.6 TABLAS
# ------------------------------------------------------------------------------
pagina_tabla("TABLA MAESTRA: Asociación variable ↔ nivel de daño",
             tabla_maestra,
             sprintf("Kruskal-Wallis, α = %.0f%%", alpha * 100))

pagina_tabla("Promedios ± DE por variable y nivel de daño",
             tabla_prom,
             sprintf("* = significativo al α = %.0f%%  (ordenado por p-valor)",
                     alpha * 100))

pagina_tabla("Comparaciones por pares (Wilcoxon)",
             matriz_pares,
             sprintf("Sí = difieren al α = %.0f%%", alpha * 100))

pagina_tabla("Vista rápida: variables asociadas a cada nivel de daño", vista)

tabla_jugo_print <- cbind(Variable = rownames(tabla_jugo), tabla_jugo)
rownames(tabla_jugo_print) <- NULL
pagina_tabla("Correlaciones de Jugo con otras variables",
             tabla_jugo_print,
             sprintf("Spearman, α = %.0f%%", alpha * 100))

coef_lm <- as.data.frame(round(summary(modelo_lm)$coefficients, 4))
coef_lm <- cbind(Termino = rownames(coef_lm), coef_lm)
rownames(coef_lm) <- NULL
pagina_tabla("Modelo de regresión múltiple: coeficientes",
             coef_lm,
             sprintf("R² ajustado = %.4f | RSE = %.4f | gl = %d",
                     summary(modelo_lm)$adj.r.squared,
                     summary(modelo_lm)$sigma,
                     summary(modelo_lm)$df[2]))

if (requireNamespace("car", quietly = TRUE)) {
  vif_vals <- tryCatch(car::vif(modelo_lm), error = function(e) NULL)
  if (!is.null(vif_vals)) {
    vif_tab <- data.frame(Variable = names(vif_vals),
                          VIF = round(as.numeric(vif_vals), 2))
    pagina_tabla("VIF (factores de inflación de varianza)", vif_tab,
                 "VIF > 10 indica colinealidad problemática")
  }
}

grupos_vars_df <- data.frame(Variable = names(grupos_vars),
                             Grupo = as.integer(grupos_vars))
pagina_tabla("Clúster de variables de calidad (3 grupos)",
             grupos_vars_df,
             "Asignación por hclust sobre 1 - |correlación|")

if (exists("tabla_caract") && !is.null(tabla_caract) &&
    nrow(tabla_caract) > 0) {
  pagina_tabla("Variables que caracterizan cada clúster (HCPC)",
               tabla_caract,
               sprintf("Solo variables con p < %.2f", alpha))
}

tabla_hcpc_df <- as.data.frame.matrix(tabla_hcpc)
tabla_hcpc_df <- cbind(Cluster = rownames(tabla_hcpc_df), tabla_hcpc_df)
rownames(tabla_hcpc_df) <- NULL
pagina_tabla("Cruce clúster × nivel de daño", tabla_hcpc_df,
             sprintf("Chi² = %.2f | gl = %d | p = %.4f",
                     ct_hcpc$statistic, ct_hcpc$parameter, ct_hcpc$p.value))

pagina_tabla("Chi-cuadrado: ND × variable categorizada (terciles)",
             chi_res,
             sprintf("α = %.0f%%", alpha * 100))

contrib_top <- head(contrib, 15)
contrib_top <- cbind(Categoria = rownames(contrib_top), contrib_top)
rownames(contrib_top) <- NULL
pagina_tabla("Contribuciones MCA (top 15 a Dim1 y Dim2)",
             contrib_top)

pca_eig_df <- data.frame(
  Dimension   = paste0("Dim", seq_len(min(5, nrow(res.pca$eig)))),
  Autovalor   = round(res.pca$eig[1:min(5, nrow(res.pca$eig)), 1], 3),
  `% var`     = round(res.pca$eig[1:min(5, nrow(res.pca$eig)), 2], 2),
  `% acum`    = round(res.pca$eig[1:min(5, nrow(res.pca$eig)), 3], 2),
  check.names = FALSE
)
pagina_tabla("PCA: varianza explicada por dimensión", pca_eig_df)

mca_eig_df <- data.frame(
  Dimension = paste0("Dim", seq_len(min(5, nrow(res.mca$eig)))),
  `% var`   = round(res.mca$eig[1:min(5, nrow(res.mca$eig)), 2], 2),
  check.names = FALSE
)
pagina_tabla("MCA: varianza explicada por dimensión", mca_eig_df)

# ------------------------------------------------------------------------------
# 12.7 CERRAR EL PDF
# ------------------------------------------------------------------------------
dev.off()

cat("\n========================================\n")
cat("[PDF generado] ", ruta_pdf, "\n", sep = "")
cat("Ubicación: ", normalizePath(ruta_pdf), "\n", sep = "")
cat("========================================\n")









##########################################

#################### Análisis: pérdida de calidad por nivel de daño ####################

alpha <- 0.15

# ==============================================================================
# 1. DATOS
# ==============================================================================
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
stopifnot(exists("datos"))
names(print(datos))

# ==============================================================================
# 2. LIBRERÍAS
# ==============================================================================
paquetes <- c("Hmisc","FactoMineR","ggplot2","tidyr","reshape2",
              "gridExtra","grid","ggrepel","corrplot")
faltan <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltan) > 0) install.packages(faltan)

library(Hmisc); library(FactoMineR); library(ggplot2)
library(tidyr); library(reshape2); library(gridExtra)
library(grid); library(ggrepel); library(corrplot)

# ==============================================================================
# 3. VARIABLES
# ==============================================================================
columna_dano <- "ND"
variables_calidad <- c("Altura", "Brix", "Pureza",
                       "Reductor", "Fibra", "Pol_caña", "Peso_tallo", "Dextrana", "Humedad")

variables_modelo <- c("Altura", "Brix", "Pureza",
                      "Reductor", "Fibra", "Pol_caña", "Peso_tallo", "Dextrana", "Humedad")

df <- datos[, c(columna_dano, variables_calidad), drop = FALSE]
df[] <- lapply(df, function(x) suppressWarnings(as.numeric(as.character(x))))
df$ND_factor <- factor(df[[columna_dano]],
                       levels = sort(unique(df[[columna_dano]])))

cat("Distribución de ND:\n"); print(table(df$ND_factor))

# ==============================================================================
# 4b. GRUPOS DE VARIABLES — versión con variables estandarizadas
# ==============================================================================
# Estandarizar cada variable antes de promediar (cada variable pesa igual)
df_z <- df[, variables_calidad]
df_z[] <- lapply(df_z, function(x) {
  s <- sd(x, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(x)))
  (x - mean(x, na.rm = TRUE)) / s
})

X_grupos <- data.frame(ND = df$ND_factor)
for (g in names(lista_grupos)) {
  X_grupos[[g]] <- rowMeans(df_z[, lista_grupos[[g]], drop = FALSE],
                            na.rm = TRUE)
}

# ==============================================================================
# 5. PROMEDIOS POR ND (variables y grupos)
# ==============================================================================
medias_var <- aggregate(df[, variables_calidad],
                        by = list(ND = df$ND_factor), FUN = mean, na.rm = TRUE)
medias_grp <- aggregate(X_grupos[, -1, drop = FALSE],
                        by = list(ND = X_grupos$ND), FUN = mean, na.rm = TRUE)

cat("\n--- Promedios de variables por ND ---\n"); print(medias_var)
cat("\n--- Promedios de grupos por ND ---\n");    print(medias_grp)

# ==============================================================================
# 6. CÁLCULO CLAVE: PÉRDIDA POR ND RESPECTO A ND = 1
# ==============================================================================
# % de pérdida = (valor_ND − valor_ND1) / valor_ND1 × 100
calcular_perdida <- function(medias_df, cols) {
  ref <- as.numeric(medias_df[medias_df$ND == levels(df$ND_factor)[1], cols])
  out <- medias_df[, c("ND", cols)]
  for (j in seq_along(cols)) {
    out[[cols[j]]] <- round(
      100 * (medias_df[[cols[j]]] - ref[j]) / ref[j], 1)
  }
  out
}

# --- Pérdida por variable (esto ya estaba bien) ---
perdida_var <- calcular_perdida(medias_var, variables_calidad)

# --- CORREGIDO: pérdida por grupo = promedio de las pérdidas de sus variables ---
# Cada variable pesa igual, sin importar su unidad
perdida_grp <- data.frame(ND = perdida_var$ND)
for (g in names(lista_grupos)) {
  vars_g <- intersect(lista_grupos[[g]], names(perdida_var))
  if (length(vars_g) == 0) next
  perdida_grp[[g]] <- round(
    rowMeans(perdida_var[, vars_g, drop = FALSE], na.rm = TRUE), 1)
}

# --- Además: medias crudas por grupo SOLO para referencia (no para %) ---
# Se calculan con X_grupos pero ya no se usan para el heatmap de pérdida
medias_grp_crudas <- aggregate(X_grupos[, -1, drop = FALSE],
                               by = list(ND = X_grupos$ND),
                               FUN = mean, na.rm = TRUE)

cat("\n--- Pérdida % de cada variable por ND (ref = ND 1) ---\n")
print(perdida_var, row.names = FALSE)
cat("\n--- Pérdida % de cada grupo por ND (ref = ND 1) ---\n")
cat("    (promedio de los % de pérdida de las variables del grupo)\n")
print(perdida_grp, row.names = FALSE)

# ==============================================================================
# 7. TABLA CLAVE: ND CON MAYOR Y MENOR PÉRDIDA POR VARIABLE Y POR GRUPO
# ==============================================================================
resumen_pérdida <- function(df_med, df_per, cols) {
  out <- data.frame()
  for (v in cols) {
    valores_perdida <- df_per[[v]]
    # Solo consideramos ND != 1 para hablar de "pérdida"
    idx <- which(df_per$ND != df_per$ND[1])
    if (length(idx) == 0) next
    i_max <- idx[which.min(valores_perdida[idx])]   # más negativo = mayor pérdida
    i_min <- idx[which.max(valores_perdida[idx])]   # menos negativo = menor pérdida
    
    out <- rbind(out, data.frame(
      Variable           = v,
      Grupo              = ifelse(v %in% names(var_a_grupo),
                                  var_a_grupo[v], v),
      # Mayor pérdida
      ND_mayor_perdida   = as.character(df_per$ND[i_max]),
      Perdida_max_pct    = valores_perdida[i_max],
      Valor_en_ND_max    = round(df_med[[v]][i_max], 2),
      # Menor pérdida
      ND_menor_perdida   = as.character(df_per$ND[i_min]),
      Perdida_min_pct    = valores_perdida[i_min],
      Valor_en_ND_min    = round(df_med[[v]][i_min], 2),
      stringsAsFactors = FALSE
    ))
  }
  out
}

tabla_perdida_var <- resumen_pérdida(medias_var, perdida_var, variables_calidad)
tabla_perdida_grp <- resumen_pérdida(medias_grp, perdida_grp, names(lista_grupos))

tabla_perdida <- rbind(
  cbind(Nivel = "Variable", tabla_perdida_var, stringsAsFactors = FALSE),
  cbind(Nivel = "Grupo",    tabla_perdida_grp, stringsAsFactors = FALSE)
)

# Ordenar por la magnitud de la mayor pérdida (más negativa primero)
tabla_perdida <- tabla_perdida[order(tabla_perdida$Perdida_max_pct), ]

cat("\n========================================\n")
cat("TABLA CLAVE: dónde cae más y dónde cae menos\n")
cat("========================================\n")
print(tabla_perdida, row.names = FALSE)

# ==============================================================================
# 8. TABLA COMPLEMENTARIA: MATRIZ DE PÉRDIDA % POR ND
# ==============================================================================
# Formato largo para la presentación
perdida_long <- melt(perdida_var, id.vars = "ND",
                     variable.name = "Variable", value.name = "Perdida_pct")
perdida_long$Grupo <- var_a_grupo[as.character(perdida_long$Variable)]

cat("\n--- Matriz pérdida % por ND (para heatmap) ---\n")
print(perdida_long, row.names = FALSE)


# ==============================================================================
# 8b. RED DE CORRELACIÓN
# ==============================================================================
# Requiere: igraph, ggraph, tidygraph
paquetes_red <- c("igraph", "ggraph", "tidygraph")
faltan_red <- paquetes_red[!paquetes_red %in% rownames(installed.packages())]
if (length(faltan_red) > 0) install.packages(faltan_red)

library(igraph); library(ggraph); library(tidygraph)

# --- 1. Matriz de correlación Spearman que YA calculaste ---
# matriz_cor$r  contiene los coeficientes
# matriz_cor$P  contiene los p-valores (con la diagonal ya rellenada con 0)

# --- 2. Umbral de visualización ---
# Solo se muestran conexiones con |r| >= r_min Y p < alpha
r_min <- 0.10   # ajusta: 0.20 más denso, 0.40 más limpio

nodos <- colnames(matriz_cor$r)   # ND + variables de calidad

# --- 3. Construir tabla de aristas ---
edges <- data.frame()
for (i in 1:(length(nodos) - 1)) {
  for (j in (i + 1):length(nodos)) {
    r <- matriz_cor$r[i, j]
    p <- matriz_cor$P[i, j]
    if (is.na(r) || is.na(p)) next
    if (abs(r) >= r_min && p < alpha) {
      edges <- rbind(edges, data.frame(
        from     = nodos[i],
        to       = nodos[j],
        r        = round(r, 3),
        p        = round(p, 4),
        signo    = ifelse(r > 0, "Positiva", "Negativa"),
        abs_r    = abs(r),
        stringsAsFactors = FALSE
      ))
    }
  }
}

cat("\n--- Aristas significativas en la red (|r| ≥ ", r_min,
    " y p < ", alpha, ") ---\n", sep = "")
print(edges[order(-edges$abs_r), ], row.names = FALSE)

# --- 4. Nodos con atributos ---
nodos_df <- data.frame(name = nodos, stringsAsFactors = FALSE)
nodos_df$tipo <- ifelse(nodos_df$name == columna_dano, "ND",
                        ifelse(nodos_df$name %in% names(var_a_grupo),
                               var_a_grupo[nodos_df$name], "Otra"))

# Fuerza = suma de |r| de las aristas que tocan al nodo
fuerza <- sapply(nodos_df$name, function(n) {
  sum(edges$abs_r[edges$from == n | edges$to == n])
})
nodos_df$fuerza <- round(fuerza, 2)

cat("\n--- Fuerza de cada nodo (suma de |r| de sus conexiones) ---\n")
print(nodos_df[order(-nodos_df$fuerza), ], row.names = FALSE)

# --- 5. Construir grafo ---
g <- tbl_graph(nodes = nodos_df, edges = edges, directed = FALSE)

# --- 6. Layout: Fruchterman-Reingold (fuerza dirigida) ---
set.seed(123)
layout_manual <- create_layout(g, layout = "fr")

# Fijar ND en el centro (opcional, queda más claro)
# Para eso, primero localizamos su posición tras el layout y ajustamos
# O simplemente dejamos que el algoritmo lo acomode y ya sale central porque
# tiene más conexiones.

# --- 7. Paleta por grupo/tipo de nodo ---
todos_los_tipos <- unique(nodos_df$tipo)
paleta_nodos <- c(
  "ND"       = "#000000",
  "Grupo 1"  = "#1F78B4",
  "Grupo 2"  = "#33A02C",
  "Grupo 3"  = "#E31A1C",
  "Grupo 4"  = "#FF7F00",
  "Grupo 5"  = "#6A3D9A",
  "Otra"     = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% todos_los_tipos]

# --- 8. Gráfico de red ---
set.seed(123)
p_red <- ggraph(g, layout = "fr") +
  # Aristas: color por signo, grosor por |r|
  geom_edge_link(aes(color = signo, edge_width = abs_r),
                 alpha = 0.7, show.legend = TRUE) +
  scale_edge_color_manual(values = c("Positiva" = "#4477AA",
                                     "Negativa" = "#BB4444"),
                          name = "Tipo de\ncorrelación") +
  scale_edge_width_continuous(range = c(0.4, 2.5), guide = "none") +
  # Nodos
  geom_node_point(aes(color = tipo, size = fuerza)) +
  # Etiquetas de nodo, repelidas
  geom_node_text(aes(label = name), repel = TRUE,
                 size = 4, fontface = "bold",
                 bg.color = "white", bg.r = 0.15) +
  scale_color_manual(values = paleta_nodos, name = "Nodo") +
  scale_size_continuous(range = c(4, 12),
                        name = "Fuerza\n(Σ |r|)") +
  labs(title = "Red de correlación: variables de calidad y ND",
       subtitle = paste0("Solo se muestran correlaciones con |r| ≥ ",
                         r_min, " y p < ", alpha, "\n",
                         "Azul = correlación positiva · Rojo = negativa · ",
                         "Tamaño del nodo = fuerza de conexión")) +
  theme_void(base_size = 12) +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        plot.margin   = margin(15, 15, 15, 15),
        legend.position = "right")

print(p_red)

# --- 9. Tabla resumen de la red ---
# Grado de cada nodo (número de conexiones)
grados <- data.frame(
  Nodo     = nodos_df$name,
  Tipo     = nodos_df$tipo,
  Grado    = as.numeric(degree(g)),
  Fuerza   = nodos_df$fuerza
)
grados <- grados[order(-grados$Fuerza), ]
cat("\n--- Tabla: nodos ordenados por fuerza de conexión ---\n")
print(grados, row.names = FALSE)

# --- 10. ¿Qué se conecta directamente con ND? ---
vecinos_nd <- edges[edges$from == columna_dano | edges$to == columna_dano, ]
if (nrow(vecinos_nd) > 0) {
  vecinos_nd$Variable <- ifelse(vecinos_nd$from == columna_dano,
                                vecinos_nd$to, vecinos_nd$from)
  vecinos_nd <- vecinos_nd[, c("Variable", "r", "p", "signo")]
  vecinos_nd <- vecinos_nd[order(-abs(vecinos_nd$r)), ]
  cat("\n--- Variables directamente conectadas con ND (|r| ≥ ",
      r_min, " y p < ", alpha, ") ---\n", sep = "")
  print(vecinos_nd, row.names = FALSE)
} else {
  cat("\nNinguna variable supera el umbral |r| ≥ ", r_min,
      " directamente con ND.\n", sep = "")
  cat("Baja el umbral (r_min) o revisa los p-valores.\n")
}


# ==============================================================================
# 8b. RED DE CORRELACIÓN — VERSIÓN LIMPIA
# ==============================================================================
paquetes_red <- c("igraph", "ggraph", "tidygraph", "graphlayouts")
faltan_red <- paquetes_red[!paquetes_red %in% rownames(installed.packages())]
if (length(faltan_red) > 0) install.packages(faltan_red)

library(igraph); library(ggraph); library(tidygraph); library(graphlayouts)

# --- 1. Parámetros de limpieza ---
r_min      <- 0.01   # umbral mínimo de |r|
top_n      <- 20     # máximo de aristas a mostrar (las más fuertes)
curvatura  <- 0.20   # curvatura de las aristas

nodos <- colnames(matriz_cor$r)

# --- 2. Construir tabla de aristas ---
edges <- data.frame()
for (i in 1:(length(nodos) - 1)) {
  for (j in (i + 1):length(nodos)) {
    r <- matriz_cor$r[i, j]
    p <- matriz_cor$P[i, j]
    if (is.na(r) || is.na(p)) next
    if (abs(r) >= r_min && p < alpha) {
      edges <- rbind(edges, data.frame(
        from  = nodos[i],
        to    = nodos[j],
        r     = round(r, 3),
        p     = round(p, 4),
        signo = ifelse(r > 0, "Positiva", "Negativa"),
        abs_r = abs(r),
        stringsAsFactors = FALSE
      ))
    }
  }
}

# Ordenar por |r| y quedarnos con las top_n
edges <- edges[order(-edges$abs_r), ]
if (nrow(edges) > top_n) edges <- edges[1:top_n, ]

cat("\n--- Top", nrow(edges), "aristas mostradas (de",
    sum(abs(matriz_cor$r[upper.tri(matriz_cor$r)]) >= r_min &
          matriz_cor$P[upper.tri(matriz_cor$P)] < alpha, na.rm = TRUE),
    "que pasan el umbral) ---\n")
print(edges, row.names = FALSE)

# --- 3. Nodos con atributos ---
nodos_df <- data.frame(name = nodos, stringsAsFactors = FALSE)
nodos_df$tipo <- ifelse(nodos_df$name == columna_dano, "ND",
                        ifelse(nodos_df$name %in% names(var_a_grupo),
                               var_a_grupo[nodos_df$name], "Otra"))

fuerza <- sapply(nodos_df$name, function(n) {
  sum(edges$abs_r[edges$from == n | edges$to == n])
})
nodos_df$fuerza <- round(fuerza, 2)

# --- 4. Construir grafo ---
g <- tbl_graph(nodes = nodos_df, edges = edges, directed = FALSE)

# --- 5. Layout stress (mejor separación visual) ---
set.seed(42)
layout_manual <- create_layout(g, layout = "stress")

# Fijar ND en el origen para que quede como referencia central
idx_nd <- which(layout_manual$name == columna_dano)
if (length(idx_nd) == 1) {
  layout_manual$x <- layout_manual$x - layout_manual$x[idx_nd]
  layout_manual$y <- layout_manual$y - layout_manual$y[idx_nd]
  # Reescalar para que ND no domine la escala
  escala <- max(abs(c(layout_manual$x, layout_manual$y)), na.rm = TRUE)
  layout_manual$x <- layout_manual$x / escala
  layout_manual$y <- layout_manual$y / escala
}

# --- 6. Paleta ---
paleta_nodos <- c(
  "ND"       = "#000000",
  "Grupo 1"  = "#1F78B4",
  "Grupo 2"  = "#33A02C",
  "Grupo 3"  = "#E31A1C",
  "Grupo 4"  = "#FF7F00",
  "Grupo 5"  = "#6A3D9A",
  "Otra"     = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% unique(nodos_df$tipo)]

# --- 7. Gráfico ---
set.seed(42)
p_red <- ggraph(layout_manual) +
  # Aristas curvadas: evita que se encimen al cruzar
  geom_edge_arc(aes(color = signo, edge_width = abs_r),
                strength = curvatura,
                alpha = 0.75,
                show.legend = TRUE,
                end_cap = circle(6, "mm"),
                start_cap = circle(6, "mm")) +
  scale_edge_color_manual(
    values = c("Positiva" = "#3B7DD8",
               "Negativa" = "#D1495B"),
    name   = "Tipo de\ncorrelación"
  ) +
  scale_edge_width_continuous(
    range = c(0.6, 3.5),
    breaks = c(0.3, 0.5, 0.7, 0.9),
    name   = "|r|"
  ) +
  # Nodos: círculo blanco grueso detrás, color del grupo delante
  geom_node_point(aes(color = tipo, size = fuerza),
                  stroke = 1.2, shape = 21, fill = NA) +
  geom_node_point(aes(color = tipo, size = fuerza),
                  fill = "white", shape = 21, stroke = 0) +
  # Etiquetas repelidas, siempre visibles
  geom_node_text(aes(label = name),
                 repel         = TRUE,
                 size          = 4.2,
                 fontface      = "bold",
                 color         = "grey15",
                 bg.color      = "white",
                 bg.r          = 0.18,
                 point.padding = 0.3,
                 box.padding   = 0.6,
                 max.overlaps  = Inf,
                 seed          = 42) +
  scale_color_manual(values = paleta_nodos, name = "Nodo") +
  scale_size_continuous(
    range  = c(5, 14),
    breaks = round(quantile(nodos_df$fuerza,
                            probs = c(0.25, 0.5, 0.75)), 2),
    name   = "Fuerza\n(Σ |r|)"
  ) +
  labs(
    title    = "Red de correlación entre variables de calidad y ND",
    subtitle = paste0(
      "Top ", nrow(edges), " correlaciones más fuertes  ·  ",
      "|r| ≥ ", r_min, "  ·  p < ", alpha,
      "\nAzul = positiva  ·  Rojo = negativa  ·  ",
      "Tamaño del nodo = fuerza de conexión"
    ),
    caption = "Layout stress con ND fijado en el centro"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   margin = margin(b = 4)),
    plot.subtitle   = element_text(hjust = 0.5, size = 10,
                                   color = "grey30",
                                   margin = margin(b = 12)),
    plot.caption    = element_text(hjust = 0.5, size = 8,
                                   color = "grey50",
                                   margin = margin(t = 12)),
    plot.margin     = margin(20, 30, 20, 30),
    legend.position = "right",
    legend.title    = element_text(size = 10, face = "bold"),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.6, "cm")
  )

print(p_red)



# ==============================================================================
# 8b. RED DE CORRELACIÓN: variables con ND y entre sí
# ==============================================================================
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggplot2); library(ggrepel)

# ------------------------------------------------------------------------------
# 1. UMBRALES
# ------------------------------------------------------------------------------
r_min_nd  <- 0.25
r_min_var <- 0.45

nodos <- colnames(matriz_cor$r)

# ------------------------------------------------------------------------------
# 2. ARISTAS
# ------------------------------------------------------------------------------
edges <- data.frame()
for (i in 1:(length(nodos) - 1)) {
  for (j in (i + 1):length(nodos)) {
    r <- matriz_cor$r[i, j]
    p <- matriz_cor$P[i, j]
    if (is.na(r) || is.na(p)) next
    
    es_nd  <- (nodos[i] == columna_dano || nodos[j] == columna_dano)
    umbral <- if (es_nd) r_min_nd else r_min_var
    
    if (abs(r) >= umbral && p < alpha) {
      edges <- rbind(edges, data.frame(
        from        = nodos[i],
        to          = nodos[j],
        r           = round(r, 3),
        p           = round(p, 4),
        signo       = ifelse(r > 0, "Positiva", "Negativa"),
        abs_r       = abs(r),
        tipo_arista = ifelse(es_nd, "ND-variable", "Variable-variable"),
        stringsAsFactors = FALSE
      ))
    }
  }
}

cat("\n--- Aristas ND ↔ variable ---\n")
print(edges[edges$tipo_arista == "ND-variable",
            c("from","to","r","p","signo")], row.names = FALSE)

cat("\n--- Aristas variable ↔ variable ---\n")
print(edges[edges$tipo_arista == "Variable-variable",
            c("from","to","r","p","signo")], row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. NODOS
# ------------------------------------------------------------------------------
nodos_df <- data.frame(name = nodos, stringsAsFactors = FALSE)
nodos_df$tipo <- ifelse(nodos_df$name == columna_dano, "ND",
                        ifelse(nodos_df$name %in% names(var_a_grupo),
                               var_a_grupo[nodos_df$name], "Otra"))

fuerza <- sapply(nodos_df$name, function(n) {
  sum(edges$abs_r[edges$from == n | edges$to == n])
})
nodos_df$fuerza <- round(fuerza, 2)

# ------------------------------------------------------------------------------
# 4. POSICIONES: ND al centro, variables en círculo
# ------------------------------------------------------------------------------
vars    <- nodos_df$name[nodos_df$name != columna_dano]
n_vars  <- length(vars)
angulos <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars] + pi / 2
radio   <- 1

pos <- data.frame(
  name   = c(columna_dano, vars),
  x      = c(0, radio * cos(angulos)),
  y      = c(0, radio * sin(angulos)),
  tipo   = nodos_df$tipo[match(c(columna_dano, vars), nodos_df$name)],
  fuerza = nodos_df$fuerza[match(c(columna_dano, vars), nodos_df$name)],
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 5. COORDENADAS DE LAS ARISTAS
# ------------------------------------------------------------------------------
edges$x_start <- pos$x[match(edges$from, pos$name)]
edges$y_start <- pos$y[match(edges$from, pos$name)]
edges$x_end   <- pos$x[match(edges$to,   pos$name)]
edges$y_end   <- pos$y[match(edges$to,   pos$name)]

edges_nd <- edges[edges$tipo_arista == "ND-variable", ]
edges_vv <- edges[edges$tipo_arista == "Variable-variable", ]

# ------------------------------------------------------------------------------
# 6. PALETA DE NODOS
# ------------------------------------------------------------------------------
paleta_nodos <- c(
  "ND"      = "#111111",
  "Grupo 1" = "#1F78B4",
  "Grupo 2" = "#33A02C",
  "Grupo 3" = "#E31A1C",
  "Grupo 4" = "#FF7F00",
  "Grupo 5" = "#6A3D9A",
  "Otra"    = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% unique(pos$tipo)]

# ------------------------------------------------------------------------------
# 7. GRÁFICO con ggplot2 puro (sin ggraph → sin prefijos)
# ------------------------------------------------------------------------------
p_red_full <- ggplot() +
  # --- Aristas ND ↔ variable: líneas rectas desde el centro ---
  geom_segment(
    data = edges_nd,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    alpha = 0.50
  ) +
  # --- Aristas variable ↔ variable: curvas por fuera ---
  geom_curve(
    data = edges_vv,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    curvature = 0.35,
    alpha     = 0.85
  ) +
  # --- Nodos: círculos con fill por grupo, borde blanco ---
  geom_point(
    data  = pos,
    aes(x = x, y = y, fill = tipo, size = fuerza),
    shape = 21, color = "white", stroke = 1.8
  ) +
  # --- Etiquetas repelidas, con fondo blanco para legibilidad ---
  geom_text_repel(
    data = pos,
    aes(x = x, y = y, label = name),
    size          = 3.8,
    fontface      = "bold",
    color         = "grey15",
    bg.color      = "white",
    bg.r          = 0.15,
    box.padding   = 0.5,
    point.padding = 0.3,
    max.overlaps  = Inf,
    seed          = 42
  ) +
  # --- Escalas ---
  scale_color_manual(
    values = c("Positiva" = "#3B7DD8", "Negativa" = "#D1495B"),
    name   = "Correlación"
  ) +
  scale_fill_manual(
    values = paleta_nodos,
    name   = "Nodo"
  ) +
  scale_linewidth_continuous(
    range  = c(0.5, 3),
    breaks = c(0.3, 0.6, 0.9),
    name   = "|r|"
  ) +
  scale_size_continuous(
    range  = c(4, 12),
    breaks = round(quantile(pos$fuerza,
                            probs = c(0.25, 0.5, 0.75)), 1),
    name   = "Fuerza\n(Σ |r|)"
  ) +
  coord_fixed(clip = "off") +
  labs(
    title    = "Red de correlación: todas las variables con ND y entre sí",
    subtitle = paste0(
      "Centro = ND  ·  Círculo = variables agrupadas por color\n",
      "ND↔var: |r| ≥ ", r_min_nd, "  ·  ",
      "var↔var: |r| ≥ ", r_min_var, "  ·  p < ", alpha,
      "\nAzul = positiva  ·  Rojo = negativa"
    ),
    caption = "Grosor de arista proporcional a |r|"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   size = 14, margin = margin(b = 4)),
    plot.subtitle   = element_text(hjust = 0.5, size = 9.5,
                                   color = "grey30",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(hjust = 0.5, size = 8,
                                   color = "grey50",
                                   margin = margin(t = 10)),
    plot.margin     = margin(20, 30, 20, 30),
    legend.position = "right",
    legend.title    = element_text(size = 10, face = "bold"),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.55, "cm")
  )

print(p_red_full)

# ------------------------------------------------------------------------------
# 8. TABLAS DE RESPALDO
# ------------------------------------------------------------------------------
resumen_red <- data.frame(
  Variable        = nodos_df$name,
  Grupo           = nodos_df$tipo,
  Fuerza_total    = nodos_df$fuerza,
  Conex_ND        = sapply(nodos_df$name, function(n)
    sum(edges$tipo_arista == "ND-variable" &
          (edges$from == n | edges$to == n))),
  Conex_variables = sapply(nodos_df$name, function(n)
    sum(edges$tipo_arista == "Variable-variable" &
          (edges$from == n | edges$to == n)))
)
resumen_red <- resumen_red[order(-resumen_red$Fuerza_total), ]
cat("\n--- Resumen de la red por variable ---\n")
print(resumen_red, row.names = FALSE)

vecinos_nd <- edges[edges$tipo_arista == "ND-variable",
                    c("from","to","r","p","signo")]
if (nrow(vecinos_nd) > 0) {
  vecinos_nd$Variable <- ifelse(vecinos_nd$from == columna_dano,
                                vecinos_nd$to, vecinos_nd$from)
  vecinos_nd <- vecinos_nd[, c("Variable","r","p","signo")]
  vecinos_nd <- vecinos_nd[order(-abs(vecinos_nd$r)), ]
  cat("\n--- Conexiones directas con ND ---\n")
  print(vecinos_nd, row.names = FALSE)
} else {
  cat("\nNinguna variable pasa el umbral con ND.\n")
}

# ------------------------------------------------------------------------------
# 9. TABLAS ASOCIADAS
# ------------------------------------------------------------------------------
cat("\n--- Resumen de la red por variable ---\n")
resumen_red <- data.frame(
  Variable        = nodos_df$name,
  Grupo           = nodos_df$tipo,
  Fuerza_total    = nodos_df$fuerza,
  Conex_ND        = sapply(nodos_df$name, function(n)
    sum(edges$tipo_arista == "ND-variable" &
          (edges$from == n | edges$to == n))),
  Conex_variables = sapply(nodos_df$name, function(n)
    sum(edges$tipo_arista == "Variable-variable" &
          (edges$from == n | edges$to == n)))
)
resumen_red <- resumen_red[order(-resumen_red$Fuerza_total), ]
print(resumen_red, row.names = FALSE)

cat("\n--- Conexiones directas con ND ---\n")

vecinos_nd <- edges[edges$tipo_arista == "ND-variable",
                    c("from","to","r","p","signo")]
if (nrow(vecinos_nd) > 0) {
  vecinos_nd$Variable <- ifelse(vecinos_nd$from == columna_dano,
                                vecinos_nd$to, vecinos_nd$from)
  vecinos_nd <- vecinos_nd[, c("Variable","r","p","signo")]
  vecinos_nd <- vecinos_nd[order(-abs(vecinos_nd$r)), ]
  print(vecinos_nd, row.names = FALSE)
} else {
  cat("Ninguna variable pasa el umbral con ND (|r| ≥ ",
      r_min_nd, " y p < ", alpha, ").\n", sep = "")
}



# ==============================================================================
# 8b. RED DE CORRELACIÓN: solo ND ↔ variables
# ==============================================================================
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggplot2); library(ggrepel)

# ------------------------------------------------------------------------------
# 1. UMBRAL
# ------------------------------------------------------------------------------
r_min <- 0.10   # solo se muestran correlaciones con ND con |r| >= r_min

nodos <- colnames(matriz_cor$r)

# ------------------------------------------------------------------------------
# 2. ARISTAS: solo ND ↔ cada variable
# ------------------------------------------------------------------------------
edges <- data.frame()
for (v in setdiff(nodos, columna_dano)) {
  r <- matriz_cor$r[columna_dano, v]
  p <- matriz_cor$P[columna_dano, v]
  if (is.na(r) || is.na(p)) next
  if (abs(r) >= r_min && p < alpha) {
    edges <- rbind(edges, data.frame(
      Variable = v,
      r        = round(r, 3),
      p        = round(p, 4),
      signo    = ifelse(r > 0, "Positiva", "Negativa"),
      abs_r    = abs(r),
      stringsAsFactors = FALSE
    ))
  }
}

# Orden descendente por |r|: primero las más fuertes
edges <- edges[order(-edges$abs_r), ]

cat("\n--- Variables correlacionadas con ND (|r| ≥ ", r_min,
    ", p < ", alpha, ") ---\n", sep = "")
print(edges, row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. NODOS: ND al centro, variables en círculo
#    Solo se muestran las variables que aparecen en 'edges'
# ------------------------------------------------------------------------------
vars <- edges$Variable
n_vars <- length(vars)

# Ángulos: empezando arriba y girando en sentido horario
angulos <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars] + pi / 2
radio   <- 1

pos <- data.frame(
  name = c(columna_dano, vars),
  x    = c(0, radio * cos(angulos)),
  y    = c(0, radio * sin(angulos)),
  tipo = c("ND", var_a_grupo[vars]),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 4. COORDENADAS DE LAS ARISTAS
# ------------------------------------------------------------------------------
edges$x_start <- 0
edges$y_start <- 0
edges$x_end   <- pos$x[match(edges$Variable, pos$name)]
edges$y_end   <- pos$y[match(edges$Variable, pos$name)]

# ------------------------------------------------------------------------------
# 5. PALETA
# ------------------------------------------------------------------------------
paleta_nodos <- c(
  "ND"      = "#111111",
  "Grupo 1" = "#1F78B4",
  "Grupo 2" = "#33A02C",
  "Grupo 3" = "#E31A1C",
  "Grupo 4" = "#FF7F00",
  "Grupo 5" = "#6A3D9A",
  "Otra"    = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% unique(pos$tipo)]

# ------------------------------------------------------------------------------
# 6. GRÁFICO
# ------------------------------------------------------------------------------
p_red_nd <- ggplot() +
  # Aristas ND-variable
  geom_segment(
    data = edges,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    alpha = 0.75
  ) +
  # Nodos
  geom_point(
    data = pos,
    aes(x = x, y = y, fill = tipo, size = name),
    shape = 21, color = "white", stroke = 1.8
  ) +
  # Etiquetas con valor de |r| debajo del nombre
  geom_text_repel(
    data = pos,
    aes(x = x, y = y,
        label = ifelse(name == columna_dano,
                       name,
                       paste0(name, "\n|r| = ",
                              sprintf("%.2f",
                                      edges$abs_r[match(name, edges$Variable)])))),
    size          = 3.8,
    fontface      = "bold",
    color         = "grey15",
    bg.color      = "white",
    bg.r          = 0.15,
    box.padding   = 0.6,
    point.padding = 0.4,
    max.overlaps  = Inf,
    seed          = 42
  ) +
  scale_color_manual(
    values = c("Positiva" = "#3B7DD8", "Negativa" = "#D1495B"),
    name   = "Correlación con ND"
  ) +
  scale_fill_manual(values = paleta_nodos, name = "Grupo") +
  scale_linewidth_continuous(
    range  = c(0.6, 4),
    breaks = c(0.3, 0.5, 0.7),
    name   = "|r|"
  ) +
  scale_size_manual(
    values = c("ND" = 16,
               setNames(rep(10, length(vars)), vars)),
    guide  = "none"
  ) +
  coord_fixed(clip = "off", xlim = c(-1.35, 1.35), ylim = c(-1.35, 1.35)) +
  labs(
    title    = "Correlación de cada variable con el ND",
    subtitle = paste0(
      "Centro = ND  ·  Solo se muestran correlaciones con |r| ≥ ",
      r_min, " y p < ", alpha, "\n",
      "Azul = correlación positiva  ·  Rojo = negativa  ·  ",
      "Grosor proporcional a |r|"
    ),
    caption = "Etiqueta = variable y |r| con el ND"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   size = 14, margin = margin(b = 4)),
    plot.subtitle   = element_text(hjust = 0.5, size = 9.5,
                                   color = "grey30",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(hjust = 0.5, size = 8,
                                   color = "grey50",
                                   margin = margin(t = 10)),
    plot.margin     = margin(20, 30, 20, 30),
    legend.position = "right",
    legend.title    = element_text(size = 10, face = "bold"),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.55, "cm")
  )

print(p_red_nd)

# ------------------------------------------------------------------------------
# 7. TABLA DE RESPALDO
# ------------------------------------------------------------------------------
cat("\n--- Resumen: variables ordenadas por |r| con ND ---\n")
tabla_red_nd <- edges[, c("Variable", "r", "p", "signo")]
tabla_red_nd$Grupo <- var_a_grupo[tabla_red_nd$Variable]
tabla_red_nd <- tabla_red_nd[, c("Variable", "Grupo", "r", "p", "signo")]
print(tabla_red_nd, row.names = FALSE)




# ==============================================================================
# 8b. RED DE CORRELACIÓN: ND ↔ TODAS las variables
# ==============================================================================
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(ggplot2); library(ggrepel)

# ------------------------------------------------------------------------------
# 1. UMBRAL
# ------------------------------------------------------------------------------
r_min <- 0.25

nodos <- colnames(matriz_cor$r)

# ------------------------------------------------------------------------------
# 2. ARISTAS: TODAS las variables, marcando cuáles son significativas
# ------------------------------------------------------------------------------
edges <- data.frame()
for (v in setdiff(nodos, columna_dano)) {
  r <- matriz_cor$r[columna_dano, v]
  p <- matriz_cor$P[columna_dano, v]
  if (is.na(r) || is.na(p)) next
  
  es_sig <- (abs(r) >= r_min && p < alpha)
  
  edges <- rbind(edges, data.frame(
    Variable  = v,
    r         = round(r, 3),
    p         = round(p, 4),
    abs_r     = abs(r),
    signo     = ifelse(r > 0, "Positiva", "Negativa"),
    estado    = ifelse(es_sig, "Significativa", "No significativa"),
    stringsAsFactors = FALSE
  ))
}

# Ordenar: primero las significativas, ordenadas por |r| desc
edges$estado <- factor(edges$estado,
                       levels = c("Significativa", "No significativa"))
edges <- edges[order(edges$estado, -edges$abs_r), ]

cat("\n--- Correlación de cada variable con ND ---\n")
print(edges[, c("Variable","r","p","signo","estado")], row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. NODOS: ND al centro, TODAS las variables en círculo
# ------------------------------------------------------------------------------
vars   <- edges$Variable
n_vars <- length(vars)

# Ángulos empezando arriba, distribuidos uniformemente
angulos <- seq(0, 2 * pi, length.out = n_vars + 1)[1:n_vars] + pi / 2
radio   <- 1

pos <- data.frame(
  name  = c(columna_dano, vars),
  x     = c(0, radio * cos(angulos)),
  y     = c(0, radio * sin(angulos)),
  tipo  = c("ND", var_a_grupo[vars]),
  estado = c("ND", as.character(edges$estado[match(vars, edges$Variable)])),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 4. COORDENADAS DE LAS ARISTAS
# ------------------------------------------------------------------------------
edges$x_start <- 0
edges$y_start <- 0
edges$x_end   <- pos$x[match(edges$Variable, pos$name)]
edges$y_end   <- pos$y[match(edges$Variable, pos$name)]

# Separar para dibujarlas con dos estilos distintos
edges_sig <- edges[edges$estado == "Significativa", ]
edges_no  <- edges[edges$estado == "No significativa", ]

# ------------------------------------------------------------------------------
# 5. PALETA DE NODOS
# ------------------------------------------------------------------------------
paleta_nodos <- c(
  "ND"      = "#111111",
  "Grupo 1" = "#1F78B4",
  "Grupo 2" = "#33A02C",
  "Grupo 3" = "#E31A1C",
  "Grupo 4" = "#FF7F00",
  "Grupo 5" = "#6A3D9A",
  "Otra"    = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% unique(pos$tipo)]

# ------------------------------------------------------------------------------
# 6. GRÁFICO
# ------------------------------------------------------------------------------
p_red_nd <- ggplot() +
  # --- Aristas NO significativas: gris punteado, finas ---
  geom_segment(
    data = edges_no,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
    color     = "grey70",
    linetype  = "dashed",
    linewidth = 0.4,
    alpha     = 0.7
  ) +
  # --- Aristas significativas: color por signo, grosor por |r| ---
  geom_segment(
    data = edges_sig,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    alpha = 0.85
  ) +
  # --- Nodos: ND negro grande, variables por grupo ---
  geom_point(
    data = pos,
    aes(x = x, y = y, fill = tipo),
    shape = 21, color = "white", stroke = 1.8,
    size = ifelse(pos$name == columna_dano, 14, 9)
  ) +
  # --- Etiquetas ---
  geom_text_repel(
    data = pos,
    aes(x = x, y = y,
        label = ifelse(name == columna_dano,
                       name,
                       paste0(name, "\n|r| = ",
                              sprintf("%.2f",
                                      edges$abs_r[match(name, edges$Variable)]),
                              ifelse(edges$estado[match(name, edges$Variable)] ==
                                       "No significativa",
                                     " (n.s.)", "")))),
    size          = 3.6,
    fontface      = "bold",
    color         = "grey15",
    bg.color      = "white",
    bg.r          = 0.15,
    box.padding   = 0.6,
    point.padding = 0.4,
    max.overlaps  = Inf,
    seed          = 42
  ) +
  scale_color_manual(
    values = c("Positiva" = "#3B7DD8", "Negativa" = "#D1495B"),
    name   = "Correlación con ND"
  ) +
  scale_fill_manual(values = paleta_nodos, name = "Grupo") +
  scale_linewidth_continuous(
    range  = c(0.6, 4),
    breaks = c(0.3, 0.5, 0.7),
    name   = "|r|"
  ) +
  coord_fixed(clip = "off", xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4)) +
  labs(
    title    = "Correlación de cada variable con el ND",
    subtitle = paste0(
      "Centro = ND  ·  Flechas de color = correlación significativa ",
      "(|r| ≥ ", r_min, " y p < ", alpha, ")\n",
      "Flechas grises punteadas = sin correlación significativa\n",
      "Azul = positiva  ·  Rojo = negativa  ·  ",
      "Grosor proporcional a |r|"
    ),
    caption = "Etiqueta = variable, |r| con ND y (n.s.) si no es significativa"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   size = 14, margin = margin(b = 4)),
    plot.subtitle   = element_text(hjust = 0.5, size = 9.5,
                                   color = "grey30",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(hjust = 0.5, size = 8,
                                   color = "grey50",
                                   margin = margin(t = 10)),
    plot.margin     = margin(20, 30, 20, 30),
    legend.position = "right",
    legend.title    = element_text(size = 10, face = "bold"),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.55, "cm")
  )

print(p_red_nd)

# ------------------------------------------------------------------------------
# 7. TABLA DE RESPALDO
# ------------------------------------------------------------------------------
tabla_red_nd <- edges[, c("Variable", "r", "p", "signo", "estado")]
tabla_red_nd$Grupo <- var_a_grupo[tabla_red_nd$Variable]
tabla_red_nd <- tabla_red_nd[, c("Variable", "Grupo", "r", "p", "signo", "estado")]
cat("\n--- Tabla: correlación de cada variable con ND ---\n")
print(tabla_red_nd, row.names = FALSE)




# ==============================================================================
# 8c. RED DE CORRELACIÓN ENTRE VARIABLES (sin ND)
# ==============================================================================
if (!requireNamespace("igraph",  quietly = TRUE)) install.packages("igraph")
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(igraph); library(ggplot2); library(ggrepel)

# ------------------------------------------------------------------------------
# 1. UMBRAL
# ------------------------------------------------------------------------------
r_min <- 0.45   # sube a 0.55 si sale muy densa, baja a 0.35 si sale muy vacía

# Variables de calidad (sin ND)
vars_red <- variables_calidad

# ------------------------------------------------------------------------------
# 2. ARISTAS entre variables (todas las combinaciones)
# ------------------------------------------------------------------------------
edges <- data.frame()
for (i in 1:(length(vars_red) - 1)) {
  for (j in (i + 1):length(vars_red)) {
    v1 <- vars_red[i]; v2 <- vars_red[j]
    r  <- matriz_cor$r[v1, v2]
    p  <- matriz_cor$P[v1, v2]
    if (is.na(r) || is.na(p)) next
    if (abs(r) >= r_min && p < alpha) {
      edges <- rbind(edges, data.frame(
        from  = v1,
        to    = v2,
        r     = round(r, 3),
        p     = round(p, 4),
        abs_r = abs(r),
        signo = ifelse(r > 0, "Positiva", "Negativa"),
        stringsAsFactors = FALSE
      ))
    }
  }
}
edges <- edges[order(-edges$abs_r), ]

cat("\n--- Correlaciones entre variables (|r| ≥ ", r_min,
    ", p < ", alpha, ") ---\n", sep = "")
print(edges, row.names = FALSE)

# ------------------------------------------------------------------------------
# 3. NODOS con atributos
# ------------------------------------------------------------------------------
nodos_df <- data.frame(
  name  = vars_red,
  tipo  = var_a_grupo[vars_red],
  stringsAsFactors = FALSE
)

# Fuerza = suma de |r| de sus conexiones (para tamaño del nodo)
nodos_df$fuerza <- sapply(nodos_df$name, function(n) {
  sum(edges$abs_r[edges$from == n | edges$to == n])
})
nodos_df$fuerza <- round(nodos_df$fuerza, 2)

# ------------------------------------------------------------------------------
# 4. LAYOUT con igraph (Fruchterman-Reingold)
# ------------------------------------------------------------------------------
g_ig <- igraph::graph_from_data_frame(
  d        = edges[, c("from", "to")],
  vertices = data.frame(name = nodos_df$name),
  directed = FALSE
)

set.seed(42)
layout_ig <- igraph::layout_with_fr(g_ig, niter = 2000)

pos <- data.frame(
  name = igraph::V(g_ig)$name,
  x    = layout_ig[, 1],
  y    = layout_ig[, 2],
  stringsAsFactors = FALSE
)
pos$tipo   <- nodos_df$tipo[match(pos$name, nodos_df$name)]
pos$fuerza <- nodos_df$fuerza[match(pos$name, nodos_df$name)]

# ------------------------------------------------------------------------------
# 5. COORDENADAS DE LAS ARISTAS
# ------------------------------------------------------------------------------
edges$x_start <- pos$x[match(edges$from, pos$name)]
edges$y_start <- pos$y[match(edges$from, pos$name)]
edges$x_end   <- pos$x[match(edges$to,   pos$name)]
edges$y_end   <- pos$y[match(edges$to,   pos$name)]

# ------------------------------------------------------------------------------
# 6. PALETA DE NODOS (una por grupo)
# ------------------------------------------------------------------------------
paleta_nodos <- c(
  "Grupo 1" = "#1F78B4",
  "Grupo 2" = "#33A02C",
  "Grupo 3" = "#E31A1C",
  "Grupo 4" = "#FF7F00",
  "Grupo 5" = "#6A3D9A",
  "Otra"    = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% unique(pos$tipo)]

# ------------------------------------------------------------------------------
# 7. GRÁFICO
# ------------------------------------------------------------------------------
p_red_vars <- ggplot() +
  # Aristas curvadas: mejor separación visual
  geom_curve(
    data = edges,
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    curvature = 0.20,
    alpha     = 0.75
  ) +
  # Nodos: círculos con borde blanco, tamaño por fuerza
  geom_point(
    data  = pos,
    aes(x = x, y = y, fill = tipo, size = fuerza),
    shape = 21, color = "white", stroke = 1.8
  ) +
  # Etiquetas con fondo blanco
  geom_text_repel(
    data = pos,
    aes(x = x, y = y, label = name),
    size          = 4,
    fontface      = "bold",
    color         = "grey15",
    bg.color      = "white",
    bg.r          = 0.15,
    box.padding   = 0.6,
    point.padding = 0.4,
    max.overlaps  = Inf,
    seed          = 42
  ) +
  scale_color_manual(
    values = c("Positiva" = "#3B7DD8", "Negativa" = "#D1495B"),
    name   = "Correlación"
  ) +
  scale_fill_manual(values = paleta_nodos, name = "Grupo") +
  scale_linewidth_continuous(
    range  = c(0.6, 3.5),
    breaks = c(0.5, 0.7, 0.9),
    name   = "|r|"
  ) +
  scale_size_continuous(
    range  = c(5, 14),
    breaks = round(quantile(pos$fuerza,
                            probs = c(0.25, 0.5, 0.75)), 1),
    name   = "Fuerza\n(Σ |r|)"
  ) +
  coord_fixed(clip = "off") +
  labs(
    title    = "Red de correlación entre variables de calidad",
    subtitle = paste0(
      "Solo se muestran correlaciones con |r| ≥ ", r_min,
      " y p < ", alpha, "\n",
      "Azul = positiva  ·  Rojo = negativa  ·  ",
      "Grosor proporcional a |r|  ·  ",
      "Tamaño del nodo = fuerza (Σ |r|)"
    ),
    caption = "Color del nodo = grupo funcional (clúster Ward)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   size = 14, margin = margin(b = 4)),
    plot.subtitle   = element_text(hjust = 0.5, size = 9.5,
                                   color = "grey30",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(hjust = 0.5, size = 8,
                                   color = "grey50",
                                   margin = margin(t = 10)),
    plot.margin     = margin(20, 30, 20, 30),
    legend.position = "right",
    legend.title    = element_text(size = 10, face = "bold"),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.55, "cm")
  )

print(p_red_vars)

# ------------------------------------------------------------------------------
# 8. TABLAS DE RESPALDO
# ------------------------------------------------------------------------------
cat("\n--- Nodos ordenados por fuerza (Σ |r|) ---\n")
tabla_nodos_red <- nodos_df[order(-nodos_df$fuerza), ]
print(tabla_nodos_red, row.names = FALSE)

cat("\n--- Aristas ordenadas por |r| ---\n")
print(edges[, c("from","to","r","p","signo")], row.names = FALSE)




# ==============================================================================
# 8c. RED DE CORRELACIÓN ENTRE VARIABLES (más conectada)
# ==============================================================================
if (!requireNamespace("igraph",  quietly = TRUE)) install.packages("igraph")
if (!requireNamespace("ggrepel", quietly = TRUE)) install.packages("ggrepel")
library(igraph); library(ggplot2); library(ggrepel)

# ------------------------------------------------------------------------------
# 1. PARÁMETROS
# ------------------------------------------------------------------------------
r_min    <- 0.1   # umbral global relajado
top_k    <- 3      # cada variable conserva al menos sus top_k correlaciones

vars_red <- variables_calidad

# ------------------------------------------------------------------------------
# 2. TODAS las correlaciones por pares
# ------------------------------------------------------------------------------
todas <- data.frame()
for (i in 1:(length(vars_red) - 1)) {
  for (j in (i + 1):length(vars_red)) {
    v1 <- vars_red[i]; v2 <- vars_red[j]
    r  <- matriz_cor$r[v1, v2]
    p  <- matriz_cor$P[v1, v2]
    if (is.na(r) || is.na(p)) next
    todas <- rbind(todas, data.frame(
      from  = v1, to = v2,
      r     = round(r, 3),
      p     = round(p, 4),
      abs_r = abs(r),
      signo = ifelse(r > 0, "Positiva", "Negativa"),
      stringsAsFactors = FALSE
    ))
  }
}

# ------------------------------------------------------------------------------
# 3. SELECCIÓN: umbral global O top_k por variable
# ------------------------------------------------------------------------------
# A) Las que pasan el umbral con p < alpha
sel_umbral <- todas[todas$abs_r >= r_min & todas$p < alpha, ]

# B) Las top_k más fuertes de cada variable (aunque no pasen el umbral)
top_vars <- do.call(rbind, lapply(vars_red, function(v) {
  sub <- todas[todas$from == v | todas$to == v, ]
  head(sub[order(-sub$abs_r), ], top_k)
}))

# Unir y quitar duplicados
edges <- unique(rbind(sel_umbral, top_vars))
edges <- edges[order(-edges$abs_r), ]
rownames(edges) <- NULL

cat("\n--- Aristas seleccionadas (umbral o top_k) ---\n")
print(edges[, c("from","to","r","p","signo")], row.names = FALSE)

# ------------------------------------------------------------------------------
# 4. NODOS
# ------------------------------------------------------------------------------
nodos_df <- data.frame(
  name = vars_red,
  tipo = var_a_grupo[vars_red],
  stringsAsFactors = FALSE
)
nodos_df$fuerza <- sapply(nodos_df$name, function(n) {
  sum(edges$abs_r[edges$from == n | edges$to == n])
})
nodos_df$fuerza <- round(nodos_df$fuerza, 2)

# ------------------------------------------------------------------------------
# 5. LAYOUT con igraph
# ------------------------------------------------------------------------------
g_ig <- igraph::graph_from_data_frame(
  d        = edges[, c("from","to")],
  vertices = data.frame(name = nodos_df$name),
  directed = FALSE
)

set.seed(42)
layout_ig <- igraph::layout_with_fr(g_ig, niter = 3000)

pos <- data.frame(
  name   = igraph::V(g_ig)$name,
  x      = layout_ig[, 1],
  y      = layout_ig[, 2],
  tipo   = nodos_df$tipo[match(igraph::V(g_ig)$name, nodos_df$name)],
  fuerza = nodos_df$fuerza[match(igraph::V(g_ig)$name, nodos_df$name)],
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 6. COORDENADAS DE ARISTAS
# ------------------------------------------------------------------------------
edges$x_start <- pos$x[match(edges$from, pos$name)]
edges$y_start <- pos$y[match(edges$from, pos$name)]
edges$x_end   <- pos$x[match(edges$to,   pos$name)]
edges$y_end   <- pos$y[match(edges$to,   pos$name)]

# Marcar cuáles son significativas (para estilo distinto)
edges$estado <- ifelse(edges$abs_r >= r_min & edges$p < alpha,
                       "Significativa", "Secundaria")

# ------------------------------------------------------------------------------
# 7. PALETA
# ------------------------------------------------------------------------------
paleta_nodos <- c(
  "Grupo 1" = "#1F78B4",
  "Grupo 2" = "#33A02C",
  "Grupo 3" = "#E31A1C",
  "Grupo 4" = "#FF7F00",
  "Grupo 5" = "#6A3D9A",
  "Otra"    = "#999999"
)
paleta_nodos <- paleta_nodos[names(paleta_nodos) %in% unique(pos$tipo)]

# ------------------------------------------------------------------------------
# 8. GRÁFICO
# ------------------------------------------------------------------------------
p_red_vars <- ggplot() +
  # Aristas secundarias: tenues
  geom_curve(
    data = edges[edges$estado == "Secundaria", ],
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    curvature = 0.20,
    alpha     = 0.30,
    linetype  = "dashed"
  ) +
  # Aristas significativas: plenas
  geom_curve(
    data = edges[edges$estado == "Significativa", ],
    aes(x = x_start, y = y_start, xend = x_end, yend = y_end,
        color = signo, linewidth = abs_r),
    curvature = 0.20,
    alpha     = 0.85
  ) +
  # Nodos
  geom_point(
    data  = pos,
    aes(x = x, y = y, fill = tipo, size = fuerza),
    shape = 21, color = "white", stroke = 1.8
  ) +
  # Etiquetas con |r| acumulado
  geom_text_repel(
    data = pos,
    aes(x = x, y = y, label = name),
    size          = 4,
    fontface      = "bold",
    color         = "grey15",
    bg.color      = "white",
    bg.r          = 0.15,
    box.padding   = 0.6,
    point.padding = 0.4,
    max.overlaps  = Inf,
    seed          = 42
  ) +
  scale_color_manual(
    values = c("Positiva" = "#3B7DD8", "Negativa" = "#D1495B"),
    name   = "Correlación"
  ) +
  scale_fill_manual(values = paleta_nodos, name = "Grupo") +
  scale_linewidth_continuous(
    range  = c(0.5, 3.5),
    breaks = c(0.3, 0.5, 0.7, 0.9),
    name   = "|r|"
  ) +
  scale_size_continuous(
    range  = c(5, 14),
    breaks = round(quantile(pos$fuerza,
                            probs = c(0.25, 0.5, 0.75)), 1),
    name   = "Fuerza\n(Σ |r|)"
  ) +
  coord_fixed(clip = "off") +
  labs(
    title    = "Red de correlación entre variables de calidad",
    subtitle = paste0(
      "Aristas sólidas = |r| ≥ ", r_min, " y p < ", alpha, "  ·  ",
      "Aristas punteadas = top ", top_k, " de cada variable\n",
      "Azul = positiva  ·  Rojo = negativa  ·  ",
      "Grosor proporcional a |r|  ·  ",
      "Tamaño del nodo = fuerza (Σ |r|)"
    ),
    caption = "Color del nodo = grupo funcional (clúster Ward)"
  ) +
  theme_void(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5,
                                   size = 14, margin = margin(b = 4)),
    plot.subtitle   = element_text(hjust = 0.5, size = 9.5,
                                   color = "grey30",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(hjust = 0.5, size = 8,
                                   color = "grey50",
                                   margin = margin(t = 10)),
    plot.margin     = margin(20, 30, 20, 30),
    legend.position = "right",
    legend.title    = element_text(size = 10, face = "bold"),
    legend.text     = element_text(size = 9),
    legend.key.size = unit(0.55, "cm")
  )

print(p_red_vars)

# ------------------------------------------------------------------------------
# 9. TABLAS
# ------------------------------------------------------------------------------
cat("\n--- Fuerza de cada variable en la red ---\n")
tabla_nodos_red <- nodos_df[order(-nodos_df$fuerza), ]
print(tabla_nodos_red, row.names = FALSE)

cat("\n--- Aristas de la red ---\n")
print(edges[, c("from","to","r","p","signo","estado")], row.names = FALSE)





# ==============================================================================
# 9. HEATMAP DE PÉRDIDA % — la imagen más clara de tu pregunta
# ==============================================================================
# Ordenar variables: primero las que más caen
orden <- tabla_perdida_var$Variable[order(tabla_perdida_var$Perdida_max_pct)]
perdida_long$Variable <- factor(perdida_long$Variable, levels = rev(orden))
perdida_long$Grupo <- factor(perdida_long$Grupo)

p_perdida_hm <- ggplot(perdida_long,
                       aes(x = ND, y = Variable, fill = Perdida_pct)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = sprintf("%+.1f%%", Perdida_pct)),
            color = "black", size = 3.8, fontface = "bold") +
  scale_fill_gradient2(low = "#8B0000", mid = "white", high = "#4477AA",
                       midpoint = 0, name = "% pérdida\nvs ND=1") +
  labs(title = "Pérdida % de cada variable respecto al nivel sin daño (ND = 1)",
       subtitle = paste0("Rojo = caída (pérdida)  ·  Azul = aumento  ·  ",
                         "Blanco = sin cambio\n",
                         "Filas ordenadas por magnitud de la mayor pérdida"),
       x = "Nivel de Daño", y = "Variable") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold"))

print(p_perdida_hm)

# ==============================================================================
# 10. PÉRDIDA POR GRUPO
# ==============================================================================
perdida_grp_long <- melt(perdida_grp, id.vars = "ND",
                         variable.name = "Grupo", value.name = "Perdida_pct")

p_perdida_grp <- ggplot(perdida_grp_long,
                        aes(x = ND, y = Grupo, fill = Perdida_pct)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%+.1f%%", Perdida_pct)),
            color = "black", size = 4.5, fontface = "bold") +
  scale_fill_gradient2(low = "#8B0000", mid = "white", high = "#4477AA",
                       midpoint = 0, name = "% pérdida\nvs ND=1") +
  labs(title = "Pérdida % de cada grupo respecto a ND = 1",
       subtitle = "Rojo = pérdida · Azul = aumento",
       x = "Nivel de Daño", y = "Grupo") +
  theme_minimal(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(face = "bold"))

print(p_perdida_grp)

# ==============================================================================
# 11. GRÁFICO DE TENDENCIA CLARO: dónde cae cada variable
# ==============================================================================
perdida_plot_long <- melt(perdida_var, id.vars = "ND",
                          variable.name = "Variable",
                          value.name = "Perdida")
perdida_plot_long$Variable <- factor(perdida_plot_long$Variable,
                                     levels = orden)

p_tend_perdida <- ggplot(perdida_plot_long,
                         aes(x = ND, y = Perdida, group = 1)) +
  geom_hline(yintercept = 0, linetype = 3, color = "grey50") +
  geom_line(color = "#4477AA", linewidth = 0.8) +
  geom_point(color = "#4477AA", size = 2.5) +
  geom_text(aes(label = sprintf("%+.1f%%", Perdida)),
            vjust = -1.2, size = 3, color = "black") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 4) +
  labs(title = "Pérdida % a lo largo de los niveles de daño",
       subtitle = "Cada línea parte del mismo ND = 1 (referencia = 0%)",
       x = "Nivel de Daño", y = "Pérdida % respecto a ND=1") +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold"))

print(p_tend_perdida)

# ==============================================================================
# 12. ACP POR GRUPOS — resaltando quién cae más
# ==============================================================================
# ACP con promedios de grupo
res.pca <- PCA(X_grupos[, -1, drop = FALSE], scale.unit = TRUE, graph = FALSE)
centro <- as.data.frame(res.pca$ind$coord[, 1:2])
names(centro) <- c("Dim1", "Dim2"); centro$ND <- X_grupos$ND
centro_nd <- aggregate(cbind(Dim1, Dim2) ~ ND, data = centro, FUN = mean)

# Vectores de grupos
vec <- as.data.frame(res.pca$var$coord[, 1:2])
names(vec) <- c("Dim1", "Dim2"); vec$Grupo <- rownames(vec)
pct <- round(res.pca$eig[1:2, 2], 1)
esc <- 0.8 * max(abs(as.matrix(centro[, 1:2]))) / max(abs(as.matrix(vec[, 1:2])))
vec$Dim1 <- vec$Dim1 * esc; vec$Dim2 <- vec$Dim2 * esc

# Tamaño del punto ND proporcional a la pérdida media del grupo con mayor caída
perdida_media_nd <- apply(perdida_grp[, -1, drop = FALSE], 1,
                          function(x) min(x))  # la mayor caída en ese ND
centro_nd$perdida <- perdida_media_nd[match(centro_nd$ND, perdida_grp$ND)]

paleta_nd <- c("#00AFBB","#E7B800","#FC4E07","#8B0000","#808080")
paleta_nd <- paleta_nd[seq_len(nlevels(centro$ND))]

p_acp_perdida <- ggplot() +
  geom_point(data = centro, aes(Dim1, Dim2), color = "grey85", size = 1.6) +
  geom_point(data = centro_nd,
             aes(Dim1, Dim2, color = ND, size = -perdida)) +
  geom_label_repel(data = centro_nd,
                   aes(Dim1, Dim2, label = paste0("ND ", ND,
                                                  "\n", sprintf("%.0f%%", perdida))),
                   size = 4, fontface = "bold",
                   box.padding = 0.6, fill = "white") +
  geom_segment(data = vec,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.22, "cm")),
               color = "darkgreen", linewidth = 0.7) +
  geom_label_repel(data = vec,
                   aes(Dim1 * 1.12, Dim2 * 1.12, label = Grupo),
                   color = "darkgreen", size = 4, fontface = "bold",
                   fill = "white") +
  scale_color_manual(values = paleta_nd, name = "ND") +
  scale_size_continuous(range = c(4, 10), guide = "none") +
  labs(x = paste0("Dim1 (", pct[1], "%)"),
       y = paste0("Dim2 (", pct[2], "%)"),
       title = "ACP de grupos: el ND que más pierde se aleja del centro",
       subtitle = "Tamaño del rombo = magnitud de la mayor pérdida de ese ND · Flechas verdes = grupos") +
  theme_minimal(base_size = 12)

print(p_acp_perdida)

# ==============================================================================
# 13. MCA — categorías Bajo/Medio/Alto y qué ND se asocia a "Bajo"
# ==============================================================================
df_cat <- data.frame(ND = df$ND_factor)
for (g in names(lista_grupos)) {
  qs <- unique(quantile(X_grupos[[g]], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE))
  if (length(qs) < 3) next
  df_cat[[g]] <- cut(X_grupos[[g]], breaks = qs,
                     labels = c("Bajo","Medio","Alto")[seq_len(length(qs) - 1)],
                     include.lowest = TRUE)
}

res.mca <- MCA(df_cat, graph = FALSE)

var_mca <- as.data.frame(res.mca$var$coord[, 1:2])
names(var_mca) <- c("Dim1", "Dim2")
var_mca$Categoria <- rownames(var_mca)
var_mca$Grupo <- sub("_(Bajo|Medio|Alto)$", "", var_mca$Categoria)
var_mca$Nivel <- sub("^.*_(Bajo|Medio|Alto)$", "\\1", var_mca$Categoria)

ind_mca <- as.data.frame(res.mca$ind$coord[, 1:2])
names(ind_mca) <- c("Dim1", "Dim2"); ind_mca$ND <- df_cat$ND
centro_mca <- aggregate(cbind(Dim1, Dim2) ~ ND, data = ind_mca, FUN = mean)

pct_mca <- round(res.mca$eig[1:2, 2], 1)

# Colorear solo "Bajo" (el nivel asociado a pérdida)
var_mca$Destacar <- ifelse(var_mca$Nivel == "Bajo", "Bajo (pérdida)", "Otros")

p_mca_perdida <- ggplot() +
  geom_point(data = var_mca,
             aes(Dim1, Dim2, shape = Nivel, color = Destacar),
             size = 3.5) +
  geom_label_repel(data = var_mca,
                   aes(Dim1, Dim2, label = Categoria),
                   size = 3.4, box.padding = 0.4,
                   fill = "white", segment.color = "grey50") +
  geom_point(data = centro_mca, aes(Dim1, Dim2),
             color = "black", size = 5, shape = 18) +
  geom_label_repel(data = centro_mca,
                   aes(Dim1, Dim2, label = paste0("ND ", ND)),
                   color = "black", size = 4.5, fontface = "bold",
                   box.padding = 0.6, fill = "lightyellow") +
  scale_color_manual(values = c("Bajo (pérdida)" = "#8B0000",
                                "Otros" = "grey60"),
                     name = "") +
  scale_shape_manual(values = c("Bajo" = 15, "Medio" = 16, "Alto" = 17),
                     name = "Categoría") +
  labs(x = paste0("Dim1 (", pct_mca[1], "%)"),
       y = paste0("Dim2 (", pct_mca[2], "%)"),
       title = "MCA: categorías Bajas de cada grupo cerca de qué ND",
       subtitle = "Cuadros rojos = categoría 'Bajo' · Rombos negros = centroide ND") +
  theme_minimal(base_size = 12)

print(p_mca_perdida)

# ==============================================================================
# 14. EXPORTAR A PDF
# ==============================================================================
ruta_pdf <- "Analisis_perdida_por_ND.pdf"
while (dev.cur() > 1) dev.off()
if (capabilities("cairo")) {
  cairo_pdf(ruta_pdf, width = 11, height = 8.5, onefile = TRUE)
} else {
  pdf(ruta_pdf, width = 11, height = 8.5, onefile = TRUE)
}

pagina_tabla <- function(titulo, tabla, subtitulo = NULL) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(20, 1)))
  grid.text(titulo, vp = viewport(layout.pos.row = 1),
            gp = gpar(fontsize = 13, fontface = "bold"))
  fila <- 2:20
  if (!is.null(subtitulo)) {
    grid.text(subtitulo, vp = viewport(layout.pos.row = 2),
              gp = gpar(fontsize = 10, col = "gray40"))
    fila <- 3:20
  }
  tg <- tableGrob(tabla, rows = NULL,
                  theme = ttheme_minimal(base_size = 9))
  pushViewport(viewport(layout.pos.row = fila)); grid.draw(tg); popViewport(2)
}
pagina_plot <- function(titulo, p) {
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(20, 1)))
  grid.text(titulo, vp = viewport(layout.pos.row = 1),
            gp = gpar(fontsize = 13, fontface = "bold"))
  print(p, vp = viewport(layout.pos.row = 2:20))
  popViewport()
}

# Portada
grid.newpage()
grid.text("¿Dónde se pierde más calidad por el daño?",
          y = 0.78, gp = gpar(fontsize = 20, fontface = "bold"))
grid.text(sprintf("n = %d · referencia ND = 1 (sin daño)", nrow(df)),
          y = 0.68, gp = gpar(fontsize = 13))

# Tabla clave
pagina_tabla("TABLA CLAVE: ND con mayor y menor pérdida por variable",
             tabla_perdida,
             "Pérdida % respecto al promedio en ND = 1")

# Gráficos de pérdida
pagina_plot("Pérdida % por variable y ND", p_perdida_hm)
pagina_plot("Pérdida % por grupo y ND", p_perdida_grp)
pagina_plot("Tendencia de pérdida por variable", p_tend_perdida)

# Gráficos multivariados
pagina_plot("ACP de grupos con tamaño = magnitud de pérdida", p_acp_perdida)
pagina_plot("MCA: categoría 'Bajo' de cada grupo", p_mca_perdida)

# Tablas de respaldo
pagina_tabla("Promedio de cada variable por ND", medias_var)
pagina_tabla("Promedio de cada grupo por ND",   medias_grp)
pagina_tabla("Matriz pérdida % de variables",   perdida_var)
pagina_tabla("Matriz pérdida % de grupos",      perdida_grp)

dev.off()
cat("\n[PDF generado]", normalizePath(ruta_pdf), "\n")









# ==============================================================================
# ACP DE GRUPOS — ¿qué ND reduce más las variables?
# ==============================================================================
library(FactoMineR); library(ggplot2); library(ggrepel); library(reshape2)

# --- 1. Promedios de cada grupo por muestra ---
X_grupos <- data.frame(ND = df$ND_factor)
for (g in names(lista_grupos)) {
  X_grupos[[g]] <- rowMeans(df[, lista_grupos[[g]], drop = FALSE],
                            na.rm = TRUE)
}

# --- 2. ACP sobre los promedios de grupo ---
res.pca <- PCA(X_grupos[, -1, drop = FALSE],
               scale.unit = TRUE, graph = FALSE)

# --- 3. Coordenadas de las muestras (puntos tenues de fondo) ---
ind <- as.data.frame(res.pca$ind$coord[, 1:2])
names(ind) <- c("Dim1", "Dim2")
ind$ND <- X_grupos$ND

# --- 4. Centroide de cada ND ---
centro <- aggregate(cbind(Dim1, Dim2) ~ ND, data = ind, FUN = mean)

# --- 5. Pérdida % promedio de cada ND (respecto a ND = 1) ---
# Se calcula sobre las medias de grupo por ND
medias_grp <- aggregate(X_grupos[, -1, drop = FALSE],
                        by = list(ND = X_grupos$ND),
                        FUN = mean, na.rm = TRUE)

ref <- as.numeric(medias_grp[medias_grp$ND == levels(df$ND_factor)[1],
                             names(lista_grupos)])
perdida_mat <- sapply(names(lista_grupos), function(g) {
  100 * (medias_grp[[g]] - ref[which(names(lista_grupos) == g)]) /
    ref[which(names(lista_grupos) == g)]
})
perdida_media_nd <- apply(perdida_mat, 1, mean)   # promedio de los grupos
centro$perdida <- perdida_media_nd[match(centro$ND, medias_grp$ND)]

cat("\n--- Pérdida media por ND (promedio de los 3 grupos) ---\n")
print(data.frame(ND = centro$ND,
                 Perdida_media_pct = round(centro$perdida, 1)))

# --- 6. Vectores de grupos ---
vec <- as.data.frame(res.pca$var$coord[, 1:2])
names(vec) <- c("Dim1", "Dim2")
vec$Grupo <- rownames(vec)

# Reescalar vectores al rango de los centroides
esc <- 0.75 * max(abs(as.matrix(ind[, 1:2]))) / max(abs(as.matrix(vec[, 1:2])))
vec$Dim1 <- vec$Dim1 * esc
vec$Dim2 <- vec$Dim2 * esc
pct <- round(res.pca$eig[1:2, 2], 1)

# --- 7. Paleta ---
paleta_nd <- c("#00AFBB", "#E7B800", "#FC4E07", "#8B0000", "#808080")
paleta_nd <- paleta_nd[seq_len(nlevels(centro$ND))]

# --- 8. Gráfico ---
p_acp <- ggplot() +
  # Muestras tenues (contexto)
  geom_point(data = ind, aes(Dim1, Dim2),
             color = "grey85", size = 1.4, alpha = 0.7) +
  # Rombo por ND, tamaño proporcional a |pérdida|
  geom_point(data = centro,
             aes(Dim1, Dim2, color = ND, size = abs(perdida))) +
  # Etiqueta con ND + % de pérdida
  geom_label_repel(data = centro,
                   aes(Dim1, Dim2,
                       label = paste0("ND ", ND, "\n",
                                      sprintf("%+.1f%%", perdida)),
                       color = ND),
                   size = 4.2, fontface = "bold",
                   box.padding = 0.8, point.padding = 0.5,
                   segment.color = "grey40", show.legend = FALSE) +
  # Vectores de grupo
  geom_segment(data = vec,
               aes(x = 0, y = 0, xend = Dim1, yend = Dim2),
               arrow = arrow(length = unit(0.22, "cm")),
               color = "darkgreen", linewidth = 0.8) +
  geom_label_repel(data = vec,
                   aes(Dim1 * 1.12, Dim2 * 1.12, label = Grupo),
                   color = "darkgreen", size = 4.2, fontface = "bold",
                   fill = "white", box.padding = 0.4) +
  scale_color_manual(values = paleta_nd, name = "Nivel de Daño") +
  scale_size_continuous(range = c(4, 12), guide = "none") +
  labs(x = paste0("Dim1 (", pct[1], "%)"),
       y = paste0("Dim2 (", pct[2], "%)"),
       title = "ACP de grupos: ¿qué ND reduce más las variables?",
       subtitle = paste0("Tamaño del rombo = magnitud de la pérdida media ",
                         "respecto a ND = 1\n",
                         "Flechas verdes = grupos funcionales · ",
                         "Etiqueta = ND y % de pérdida")) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

print(p_acp)








# ==============================================================================
# DIFERENCIAS ENTRE ND PARA LAS VARIABLES DE INTERÉS
# ==============================================================================
vars_interes <- c("Altura", "Brix", "Pureza", "Pol_caña", "Jugo")

# --- 1.1 Kruskal-Wallis global + tamaño del efecto (epsilon^2) ---
kw_tabla <- data.frame()
for (v in vars_interes) {
  kt <- kruskal.test(df[[v]] ~ df$ND_factor)
  n  <- sum(!is.na(df[[v]]))
  k  <- nlevels(df$ND_factor)
  # epsilon^2 = (H - k + 1) / (n - k)
  eps2 <- (unname(kt$statistic) - k + 1) / (n - k)
  kw_tabla <- rbind(kw_tabla, data.frame(
    Variable = v,
    H        = round(unname(kt$statistic), 2),
    gl       = unname(kt$parameter),
    p_valor  = round(kt$p.value, 4),
    epsilon2 = round(eps2, 3),
    Efecto   = ifelse(eps2 < 0.01, "Muy bajo",
                      ifelse(eps2 < 0.06, "Bajo",
                             ifelse(eps2 < 0.14, "Moderado", "Alto"))),
    Signif_15 = ifelse(kt$p.value < alpha, "Sí", "No")
  ))
}
cat("\n=== Kruskal-Wallis: diferencias globales entre ND ===\n")
print(kw_tabla, row.names = FALSE)

# --- 1.2 Wilcoxon por pares con corrección de Holm ---
pares <- combn(levels(df$ND_factor), 2)
pares_tabla <- data.frame()
for (v in vars_interes) {
  p_vals <- sapply(seq_len(ncol(pares)), function(i) {
    g1 <- pares[1, i]; g2 <- pares[2, i]
    x1 <- df[[v]][df$ND_factor == g1]
    x2 <- df[[v]][df$ND_factor == g2]
    if (sum(!is.na(x1)) < 2 || sum(!is.na(x2)) < 2) return(NA_real_)
    suppressWarnings(wilcox.test(x1, x2)$p.value)
  })
  p_adj <- p.adjust(p_vals, method = "holm")
  for (i in seq_len(ncol(pares))) {
    pares_tabla <- rbind(pares_tabla, data.frame(
      Variable = v,
      Par      = paste0(pares[1, i], " vs ", pares[2, i]),
      p_bruto  = round(p_vals[i], 4),
      p_Holm   = round(p_adj[i], 4),
      Signif   = ifelse(!is.na(p_adj[i]) && p_adj[i] < alpha, "*", "")
    ))
  }
}
cat("\n=== Wilcoxon por pares (p-valores corregidos con Holm) ===\n")
print(pares_tabla, row.names = FALSE)

# --- 1.3 Medias por ND con letras de significancia (compact letter display) ---
# Asignamos letras: ND con la misma letra NO difieren significativamente
# (basado en los pares Wilcoxon-Holm al 15%)
medias_nd <- aggregate(df[, vars_interes],
                       by = list(ND = df$ND_factor),
                       FUN = function(x) mean(x, na.rm = TRUE))

# Construir letras por variable
letras_por_var <- lapply(vars_interes, function(v) {
  pt <- pares_tabla[pares_tabla$Variable == v, ]
  nds <- levels(df$ND_factor)
  # Matriz de diferencias significativas
  sig_mat <- matrix(FALSE, length(nds), length(nds),
                    dimnames = list(nds, nds))
  for (i in seq_len(nrow(pt))) {
    a <- strsplit(pt$Par[i], " vs ")[[1]][1]
    b <- strsplit(pt$Par[i], " vs ")[[1]][2]
    if (pt$p_Holm[i] < alpha) {
      sig_mat[a, b] <- TRUE; sig_mat[b, a] <- TRUE
    }
  }
  # Asignar letras: se agrupan ND que NO difieren entre sí
  letras <- character(length(nds))
  siguiente <- 1
  for (i in seq_along(nds)) {
    if (letras[i] != "") next
    grupo <- nds[i]
    for (j in seq_along(nds)) {
      if (letras[j] != "") next
      # ¿nds[j] NO difiere con todos los del grupo?
      compatible <- all(!sig_mat[grupo, nds[j]])
      if (compatible) {
        letras[j] <- LETTERS[siguiente]
        grupo <- c(grupo, nds[j])
      }
    }
    siguiente <- siguiente + 1
  }
  data.frame(Variable = v, ND = nds, letra = letras)
})
letras_df <- do.call(rbind, letras_por_var)

# Pegar letras a las medias
medias_nd_long <- reshape2::melt(medias_nd, id.vars = "ND",
                                 variable.name = "Variable",
                                 value.name = "Media")
medias_nd_long <- merge(medias_nd_long, letras_df,
                        by = c("ND", "Variable"), all.x = TRUE)
medias_nd_long$Media <- round(medias_nd_long$Media, 2)

cat("\n=== Medias por ND con letras de significancia ===\n")
print(medias_nd_long[order(medias_nd_long$Variable, medias_nd_long$ND), ],
      row.names = FALSE)




names(datos)

# ==============================================================================
# DIFERENCIAS ENTRE ND CON LETRAS DE SIGNIFICANCIA — α = 15 %
# ==============================================================================
if (!requireNamespace("multcompView", quietly = TRUE))
  install.packages("multcompView")
if (!requireNamespace("flextable", quietly = TRUE))
  install.packages("flextable")
library(multcompView); library(flextable); library(officer); library(ggplot2)

# ------------------------------------------------------------------------------
# 0. NIVEL DE SIGNIFICANCIA
# ------------------------------------------------------------------------------
alpha <- 0.15        # 15 %
cat(sprintf("\n=== Análisis con α = %.0f%% ===\n", alpha * 100))

# ------------------------------------------------------------------------------
# 1. Variables a analizar
# ------------------------------------------------------------------------------
vars_letras <- c("Altura", "Brix", "Reductor",
                 "Fibra", "Peso_tallo", "Pureza")

df$ND_factor <- factor(df$ND, levels = sort(unique(df$ND)))

# ------------------------------------------------------------------------------
# 2. Cálculo: medias, KW global y letras (Wilcoxon + Holm, α = 15%)
# ------------------------------------------------------------------------------
letras_tabla <- data.frame()

for (v in vars_letras) {
  medias <- tapply(df[[v]], df$ND_factor, mean, na.rm = TRUE)
  
  kw <- kruskal.test(df[[v]] ~ df$ND_factor)
  
  pares  <- combn(levels(df$ND_factor), 2)
  p_vals <- sapply(seq_len(ncol(pares)), function(i) {
    x1 <- df[[v]][df$ND_factor == pares[1, i]]
    x2 <- df[[v]][df$ND_factor == pares[2, i]]
    if (sum(!is.na(x1)) < 2 || sum(!is.na(x2)) < 2) return(1)
    suppressWarnings(wilcox.test(x1, x2)$p.value)
  })
  p_adj <- p.adjust(p_vals, method = "holm")
  names(p_adj) <- apply(pares, 2, function(x) paste(x[1], x[2], sep = "-"))
  
  # IMPORTANTE: threshold = alpha = 0.15
  letras <- multcompLetters(p_adj, threshold = alpha)$Letters
  
  letras_tabla <- rbind(letras_tabla, data.frame(
    ND       = names(medias),
    Variable = v,
    Media    = round(as.numeric(medias), 2),
    Letra    = as.character(letras[names(medias)]),
    p_valor  = round(kw$p.value, 4),
    Signif   = ifelse(kw$p.value < alpha, "Sí", "No"),
    stringsAsFactors = FALSE
  ))
}

letras_tabla$Variable <- factor(letras_tabla$Variable, levels = vars_letras)
letras_tabla <- letras_tabla[order(letras_tabla$Variable,
                                   as.integer(letras_tabla$ND)), ]
letras_tabla$Variable <- as.character(letras_tabla$Variable)
rownames(letras_tabla) <- NULL

# ------------------------------------------------------------------------------
# 3. Tabla de consola (p-valor solo en la primera fila de cada variable)
# ------------------------------------------------------------------------------
tabla_print <- letras_tabla
tabla_print$p_valor_chr <- sprintf("%.4f %s",
                                   tabla_print$p_valor,
                                   ifelse(tabla_print$Signif == "Sí", "*", ""))
for (v in unique(tabla_print$Variable)) {
  idx <- which(tabla_print$Variable == v)
  if (length(idx) > 1) tabla_print$p_valor_chr[idx[-1]] <- ""
}
cat("\n--- Diferencias entre ND con letras de significancia (α = ",
    alpha * 100, "%) ---\n", sep = "")
print(tabla_print[, c("ND","Variable","Media","Letra","p_valor_chr")],
      row.names = FALSE)
cat("Nota: * = significativo al 15 %\n")

# ------------------------------------------------------------------------------
# 4. flextable bonita
# ------------------------------------------------------------------------------
ft_letras <- flextable(letras_tabla[, c("ND","Variable","Media","Letra",
                                        "p_valor","Signif")])
ft_letras <- theme_box(ft_letras)
ft_letras <- bg(ft_letras, bg = "#DCE6F1", part = "header")
ft_letras <- bold(ft_letras, part = "header")
ft_letras <- align(ft_letras, align = "center", part = "all")
ft_letras <- fontsize(ft_letras, size = 10, part = "all")
ft_letras <- merge_v(ft_letras, j = c("Variable","p_valor","Signif"))
ft_letras <- color(ft_letras,
                   i = which(letras_tabla$Letra != "A"),
                   j = "Letra", color = "#C00000")
ft_letras <- bold(ft_letras, j = "Letra", part = "body")
ft_letras

# ------------------------------------------------------------------------------
# 5. Versión ggplot de la tabla
# ------------------------------------------------------------------------------
letras_tabla$color_letra <- ifelse(letras_tabla$Letra == "A",
                                   "grey15", "#C00000")
letras_tabla$p_mostrar <- ifelse(
  duplicated(letras_tabla$Variable), "",
  sprintf("p = %.4f %s", letras_tabla$p_valor,
          ifelse(letras_tabla$Signif == "Sí", "*", "")))

p_tabla_letras <- ggplot(letras_tabla, aes(x = ND, y = Variable)) +
  geom_tile(fill = "white", color = "grey80", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Media)),
            vjust = -0.35, size = 3.6, color = "grey25") +
  geom_text(aes(label = Letra, color = color_letra),
            vjust = 0.95, size = 5.5, fontface = "bold") +
  geom_text(aes(x = length(levels(df$ND_factor)) + 0.75,
                label = p_mostrar),
            hjust = 0, size = 3.4, color = "grey30") +
  scale_color_identity() +
  scale_x_discrete(expand = expansion(add = c(0.5, 2))) +
  labs(
    title    = "Diferencias de medias entre ND con letras de significancia",
    subtitle = paste0(
      "Misma letra = sin diferencia significativa  ·  ",
      "Letras distintas = difieren (Wilcoxon-Holm)\n",
      "Nivel de significancia α = ", alpha * 100, "%  ·  ",
      "* = KW global significativo  ·  ",
      "Número = media de la variable en ese ND"
    ),
    x = "Nivel de Daño (ND)", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid    = element_blank(),
    axis.text.y   = element_text(face = "bold", size = 11),
    plot.title    = element_text(face = "bold"),
    plot.subtitle = element_text(size = 9.5, color = "grey30")
  )

print(p_tabla_letras)

# ------------------------------------------------------------------------------
# 6. Boxplots con letras
# ------------------------------------------------------------------------------
df_long <- do.call(rbind, lapply(vars_letras, function(v) {
  data.frame(Variable = v, ND = df$ND_factor, Valor = df[[v]],
             stringsAsFactors = FALSE)
}))
df_long$Variable <- factor(df_long$Variable, levels = vars_letras)

pos_letras <- letras_tabla
pos_letras$y_pos <- mapply(function(v, nd) {
  x <- df[[v]][df$ND_factor == nd]
  max(x, na.rm = TRUE)
}, pos_letras$Variable, pos_letras$ND)
pos_letras <- do.call(rbind, lapply(split(pos_letras, pos_letras$Variable),
                                    function(d) {
                                      ymin <- min(df_long$Valor[df_long$Variable == d$Variable[1]], na.rm = TRUE)
                                      ymax <- max(df_long$Valor[df_long$Variable == d$Variable[1]], na.rm = TRUE)
                                      d$y_pos <- ymax + 0.05 * (ymax - ymin)
                                      d
                                    }))

p_letras_box <- ggplot(df_long, aes(x = ND, y = Valor, fill = ND)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, na.rm = TRUE) +
  geom_jitter(width = 0.15, size = 1.4, alpha = 0.55, na.rm = TRUE) +
  geom_text(data = pos_letras,
            aes(x = ND, y = y_pos, label = Letra),
            inherit.aes = FALSE,
            size = 5, fontface = "bold", color = "grey15") +
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title    = "Distribución por ND con letras de significancia",
    subtitle = paste0("Letras distintas = diferencias significativas ",
                      "(Wilcoxon-Holm, α = ", alpha * 100, "%)"),
    x = "Nivel de Daño (ND)", y = "Valor", fill = "ND"
  ) +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

print(p_letras_box)
