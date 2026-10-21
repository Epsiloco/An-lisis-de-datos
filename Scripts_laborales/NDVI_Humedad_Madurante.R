# ============================================================
# 0. CARGA DE DATOS
# ============================================================
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(datos))

# ============================================================
# LLAVE MAESTRA
# ============================================================
VAR_NDVI  <- "NDVI_._Ini"      # o "NDVI_._Fin"
VAR_HIDRO <- "Hidro_100_Ini"   # o "Hidro_100_Fin"
K_OPT     <- 2                 # 2, 3, 4, 5... el que necesites

stopifnot(VAR_NDVI  %in% names(datos))
stopifnot(VAR_HIDRO %in% names(datos))
cat("\n→ Usando NDVI :", VAR_NDVI, "\n")
cat("→ Usando NDWI :", VAR_HIDRO, "\n")
cat("→ Grupos (k)  :", K_OPT, "\n\n")

# ============================================================
# 1. PAQUETES
# ============================================================
paquetes <- c("tidyverse", "factoextra", "cluster", "car",
              "ggplot2", "dunn.test", "rstatix", "ggpubr",
              "dendextend", "agricolae", "knitr")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
set.seed(123)

# ============================================================
# 2. PREPARACIÓN
# ============================================================
df_cluster <- datos %>%
  select(ID_Ensayo,
         NDVI = all_of(VAR_NDVI),
         NDWI = all_of(VAR_HIDRO)) %>%
  drop_na()

df_cluster$NDVI_sc <- scale(df_cluster$NDVI)

cat("Observaciones usadas:", nrow(df_cluster), "\n")

# ============================================================
# 3. NÚMERO ÓPTIMO DE CLÚSTERES
# ============================================================
fviz_nbclust(df_cluster["NDVI_sc"], kmeans, method = "wss") +
  labs(title = "Método del Codo (WSS)")

fviz_nbclust(df_cluster["NDVI_sc"], kmeans, method = "silhouette") +
  labs(title = "Método de la Silueta")

# ============================================================
# 4. K-MEANS
# ============================================================
k_opt <- K_OPT

km <- kmeans(df_cluster$NDVI_sc, centers = k_opt, nstart = 25)
df_cluster$Cluster_NDVI <- factor(km$cluster)

centroides <- df_cluster %>%
  group_by(Cluster_NDVI) %>%
  summarise(n        = n(),
            NDVI_med = mean(NDVI, na.rm = TRUE),
            NDWI_med = mean(NDWI, na.rm = TRUE),
            .groups  = "drop")
print(centroides)

# ============================================================
# 5. BOXPLOTS EXPLORATORIOS
# ============================================================
ggplot(df_cluster, aes(x = Cluster_NDVI, y = NDVI, fill = Cluster_NDVI)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.15, alpha = 0.5) +
  theme_minimal() +
  labs(title = paste("Distribución de", VAR_NDVI, "por Clúster"),
       x = "Clúster", y = VAR_NDVI)

ggplot(df_cluster, aes(x = Cluster_NDVI, y = NDWI, fill = Cluster_NDVI)) +
  geom_boxplot(alpha = 0.7) + geom_jitter(width = 0.15, alpha = 0.5) +
  theme_minimal() +
  labs(title = paste("Distribución de", VAR_HIDRO, "por Clúster"),
       x = "Clúster", y = VAR_HIDRO)

# ============================================================
# 6. SUPUESTOS
# ============================================================
shapiro_res <- df_cluster %>%
  group_by(Cluster_NDVI) %>%
  shapiro_test(NDWI)
print(shapiro_res)

levene_res <- leveneTest(NDWI ~ Cluster_NDVI, data = df_cluster)
print(levene_res)

# ============================================================
# 7. PRUEBAS GLOBALES
# ============================================================
anova_res <- aov(NDWI ~ Cluster_NDVI, data = df_cluster)
summary(anova_res)

kruskal_res <- kruskal.test(NDWI ~ Cluster_NDVI, data = df_cluster)
print(kruskal_res)

# ============================================================
# 8. POST-HOC
# ============================================================
tukey_res <- TukeyHSD(anova_res)
print(tukey_res)

dunn_tidy <- df_cluster %>%
  dunn_test(NDWI ~ Cluster_NDVI, p.adjust.method = "bonferroni")
print(dunn_tidy)

# ============================================================
# 9. BOXPLOT CON SIGNIFICANCIA (comparaciones automáticas)
# ============================================================
comparaciones <- combn(levels(df_cluster$Cluster_NDVI), 2, simplify = FALSE)

ggboxplot(df_cluster, x = "Cluster_NDVI", y = "NDWI",
          color = "Cluster_NDVI", add = "jitter", palette = "jco") +
  stat_compare_means(method = "kruskal.test",
                     label.y = max(df_cluster$NDWI) * 1.10) +
  stat_compare_means(comparisons = comparaciones,
                     method = "wilcox.test", label = "p.signif") +
  labs(title = paste("Comparación de", VAR_HIDRO, "entre clústeres"),
       x = "Clúster", y = VAR_HIDRO) +
  theme_minimal()

# ============================================================
# 10. CLÚSTER JERÁRQUICO
# ============================================================
dist_mat <- dist(df_cluster$NDVI_sc, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")
df_cluster$Grupo_HC <- factor(cutree(hc, k = k_opt))

table(KMeans = df_cluster$Cluster_NDVI, Jerarquico = df_cluster$Grupo_HC)

# ============================================================
# 11. PALETA EXPANDIBLE (soporta hasta 8 grupos)
# ============================================================
paleta_base <- c("#D7301F", "#2166AC", "#4DAF4A", "#F4A582",
                 "#762A83", "#1B7837", "#E08214", "#35978F")

# Si k_opt <= 8 usa la paleta curada; si no, genera colores automáticamente
if (k_opt <= length(paleta_base)) {
  colores_grupo <- paleta_base[1:k_opt]
} else {
  colores_grupo <- hcl.colors(k_opt, palette = "Dark 3")
}
names(colores_grupo) <- as.character(seq_len(k_opt))

cat("\nColores asignados:\n"); print(colores_grupo)

# ============================================================
# 12. DENDROGRAMA + RESUMEN (layout robusto, cualquier k)
# ============================================================
while (!is.null(dev.list())) dev.off()
par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE, family = "")

resumen_grupos <- df_cluster %>%
  group_by(Grupo_HC) %>%
  summarise(
    NDVI_min   = min(NDVI, na.rm = TRUE),
    NDVI_max   = max(NDVI, na.rm = TRUE),
    NDWI_media = mean(NDWI, na.rm = TRUE),
    NDWI_min   = min(NDWI, na.rm = TRUE),
    NDWI_max   = max(NDWI, na.rm = TRUE),
    n          = n(),
    .groups    = "drop"
  )

dend <- as.dendrogram(hc) %>%
  set("labels", rep("", length(labels(hc)))) %>%
  set("branches_lwd", 1) %>%
  color_branches(k = k_opt, col = colores_grupo)

layout(matrix(c(1, 2), nrow = 1), widths = c(3.5, 1.4))

# --- PANEL 1: DENDROGRAMA ---
par(mar = c(2, 3, 6, 1), xpd = NA, bg = "white", family = "serif")

plot(dend, main = "", ylab = "Altura", leaflab = "none",
     cex.lab = 0.95, font.lab = 3, col.lab = "gray30")

mtext("Dendrograma", side = 3, line = 4.5, adj = 0.5,
      cex = 1.6, font = 2, family = "serif", col = "gray15")

mtext(paste0("Agrupamiento según ", VAR_NDVI,
             " · Ward.D2 · Distancia euclidiana"),
      side = 3, line = 2.8, adj = 0.5,
      cex = 0.85, font = 3, family = "serif", col = "gray40")

mtext(strrep("─", 55), side = 3, line = 1.9, adj = 0.5,
      cex = 0.6, col = "gray75")

rect.dendrogram(dend, k = k_opt, border = colores_grupo,
                lty = 2, lwd = 1.7, xpd = NA)

# --- PANEL 2: RESUMEN POR GRUPO ---
par(mar = c(2, 0.5, 6, 2), xpd = NA, bg = "white", family = "serif")
plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))

text(0.5, 0.96, "Resumen por grupo", font = 2, cex = 1.15, col = "gray15")
segments(0.02, 0.92, 0.98, 0.92, col = "gray75", lwd = 1)

# --- ✅ FIX: usa case_when en vez de if/else ---
n_g <- nrow(resumen_grupos)

cex_g <- dplyr::case_when(
  n_g <= 2 ~ 0.85,
  n_g == 3 ~ 0.75,
  n_g == 4 ~ 0.70,
  TRUE     ~ 0.60
)
cex_d <- dplyr::case_when(
  n_g <= 2 ~ 0.72,
  n_g == 3 ~ 0.62,
  n_g == 4 ~ 0.58,
  TRUE     ~ 0.50
)

slots   <- seq(0.86, 0.04, length.out = n_g + 1)
centros <- (slots[-1] + slots[-(n_g + 1)]) / 2

for (i in seq_len(n_g)) {
  g  <- as.character(resumen_grupos$Grupo_HC[i])
  yc <- centros[i]
  
  points(0.07, yc + 0.025, pch = 22, cex = 2.4,
         bg = colores_grupo[g], col = NA)
  
  text(0.18, yc + 0.05,
       labels = sprintf("Grupo %s  (n = %d)", g, resumen_grupos$n[i]),
       adj = 0, cex = cex_g, font = 2, col = "gray15")
  
  text(0.18, yc - 0.035,
       labels = sprintf("NDVI: %.3f – %.3f\nNDWI medio: %.2f\nNDWI rango: %.2f – %.2f",
                        resumen_grupos$NDVI_min[i], resumen_grupos$NDVI_max[i],
                        resumen_grupos$NDWI_media[i],
                        resumen_grupos$NDWI_min[i], resumen_grupos$NDWI_max[i]),
       adj = 0, cex = cex_d, col = "gray30")
}

layout(1)
par(mar = c(5, 4, 4, 2) + 0.1, xpd = FALSE, family = "")

# ============================================================
# 13. TABLA RESUMEN FINAL
# ============================================================
resumen <- df_cluster %>%
  group_by(Cluster_NDVI) %>%
  summarise(
    n = n(),
    NDVI_min    = min(NDVI, na.rm = TRUE),
    NDVI_max    = max(NDVI, na.rm = TRUE),
    NDVI_rango  = NDVI_max - NDVI_min,
    NDVI_media  = mean(NDVI, na.rm = TRUE),
    NDVI_sd     = sd(NDVI,   na.rm = TRUE),
    NDWI_min    = min(NDWI, na.rm = TRUE),
    NDWI_max    = max(NDWI, na.rm = TRUE),
    NDWI_rango  = NDWI_max - NDWI_min,
    Media       = mean(NDWI, na.rm = TRUE),
    SD          = sd(NDWI,   na.rm = TRUE),
    EE          = SD / sqrt(n),
    Q1          = quantile(NDWI, 0.25, na.rm = TRUE),
    Mediana     = median(NDWI,      na.rm = TRUE),
    Q3          = quantile(NDWI, 0.75, na.rm = TRUE),
    IC95_inf    = Media - qt(0.975, n - 1) * EE,
    IC95_sup    = Media + qt(0.975, n - 1) * EE,
    .groups     = "drop"
  )

modelo_aov <- aov(NDWI ~ Cluster_NDVI, data = df_cluster)
p_anova    <- summary(modelo_aov)[[1]][["Pr(>F)"]][1]

if (p_anova < 0.15) {
  tukey_letras <- HSD.test(modelo_aov, trt = "Cluster_NDVI",
                           alpha = 0.15, group = TRUE, console = FALSE)
  letras_df <- data.frame(
    Cluster_NDVI = rownames(tukey_letras$groups),
    Letras       = tukey_letras$groups$groups
  )
  resumen <- merge(resumen, letras_df, by = "Cluster_NDVI", all.x = TRUE)
} else {
  resumen$Letras <- "-"
}

p_kruskal <- kruskal.test(NDWI ~ Cluster_NDVI, data = df_cluster)$p.value
resumen$p_ANOVA   <- round(p_anova,   4)
resumen$p_Kruskal <- round(p_kruskal, 4)

resumen <- resumen %>%
  arrange(desc(Media)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

cat("\n=========================================================\n")
cat("  TABLA RESUMEN - ", VAR_NDVI, " y ", VAR_HIDRO, " por clúster\n", sep = "")
cat("=========================================================\n")
print(as.data.frame(resumen))

kable(resumen %>%
        select(Cluster_NDVI, n,
               NDVI_min, NDVI_max, NDVI_rango, NDVI_media, NDVI_sd,
               NDWI_min, NDWI_max, NDWI_rango,
               Media, SD, EE, IC95_inf, IC95_sup,
               Letras, p_ANOVA, p_Kruskal),
      caption = paste0("Resumen de ", VAR_NDVI, " y ", VAR_HIDRO,
                       " por clúster · Tukey α = 0.15"),
      digits  = 3,
      col.names = c("Clúster", "n",
                    "NDVI mín", "NDVI máx", "NDVI rango", "NDVI media", "NDVI SD",
                    "NDWI mín", "NDWI máx", "NDWI rango",
                    "NDWI media", "NDWI SD", "NDWI EE",
                    "IC95 inf", "IC95 sup",
                    "Letras", "p-ANOVA", "p-Kruskal"))

write.csv(resumen, "resumen_clusters.csv", row.names = FALSE)
cat("\nTabla guardada en: resumen_clusters.csv\n")




# ============================================================
# 14. MÓDULO DE CORRELACIÓN (correlograma + p-valores)
# ============================================================
if (!require(corrplot))   install.packages("corrplot")
if (!require(Hmisc))      install.packages("Hmisc")
if (!require(ggcorrplot)) install.packages("ggcorrplot")
library(corrplot)
library(Hmisc)
library(ggcorrplot)
library(dplyr)

# --- 14.1 Variables a correlacionar ------------------------
# Por defecto: NDVI y NDWI (de la llave maestra).
# Puedes agregar más variables numéricas del dataset original, p.ej.:
#   vars_cor <- c("NDVI", "NDWI", "TCH", "Rendimiento", "Pureza")
vars_cor <- c("NDVI", "NDWI")

# Si quieres añadir automáticamente variables numéricas adicionales:
# vars_cor <- c("NDVI", "NDWI", "TCH", "Rendimiento", "TAH", "Pureza")

df_cor <- df_cluster %>%
  select(all_of(vars_cor)) %>%
  drop_na()

cat("\n--- Variables incluidas en la correlación ---\n")
print(names(df_cor))
cat("Observaciones:", nrow(df_cor), "\n")

# --- 14.2 Matriz de correlación + p-valores (Pearson) ------
mat_cor <- rcorr(as.matrix(df_cor), type = "pearson")

R <- mat_cor$r        # coeficientes de correlación
P <- mat_cor$P        # p-valores
n_obs <- mat_cor$n    # número de observaciones por par

cat("\n===== Matriz de correlaciones (Pearson) =====\n")
print(round(R, 3))

cat("\n===== Matriz de p-valores =====\n")
print(round(P, 4))

# --- 14.3 Correlograma con corrplot ------------------------
# Opción A — corrplot() directo (evita el conflicto de "col")
corrplot(R,
         method       = "color",            # círculos de color
         type         = "upper",            # solo mitad superior
         addCoef.col  = "black",            # coeficientes numéricos
         number.cex   = 0.9,
         tl.col       = "black",
         tl.srt       = 45,
         tl.cex       = 0.9,
         col          = colorRampPalette(c("#B2182B", "white", "#2166AC"))(200),
         title        = paste0("Correlograma · Pearson (n = ", nrow(df_cor), ")"),
         mar          = c(0, 0, 2, 0))

# Opción B — corrplot.mixed() sin el argumento "col" problemático
corrplot.mixed(R,
               lower       = "number",
               upper       = "circle",
               tl.col      = "black",
               tl.srt      = 45,
               number.cex  = 0.9,
               tl.cex      = 0.9,
               col         = colorRampPalette(c("#B26E6E", "white", "#2166AC"))(200),
               mar         = c(0, 0, 2, 0))
# --- 14.4 Correlograma ggplot (alternativa más limpia) -----
ggcorrplot(R,
           method       = "circle",
           type         = "upper",
           lab          = TRUE,
           lab_size     = 4,
           colors       = c("#B2182B", "white", "#2166AC"),
           outline.color = "gray70",
           title        = paste0("Correlograma · Pearson (n = ", nrow(df_cor), ")"),
           ggtheme      = theme_minimal(base_size = 12)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --- 14.5 Correlograma con p-valores superpuestos (estrellas) ---
ggcorrplot(R,
           method        = "square",
           type          = "full",
           lab           = TRUE,
           lab_size      = 4,
           p.mat         = P,
           sig.level     = 0.15,          # mismo α que usamos en Tukey
           insig         = "blank",       # oculta no significativas
           colors        = c("#B2182B", "white", "#2166AC"),
           outline.color = "gray70",
           title         = paste0("Correlograma con significancia (α = 0.15, n = ", nrow(df_cor), ")"),
           ggtheme      = theme_minimal(base_size = 12)) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --- 14.6 Tabla tidy de correlaciones y p-valores ----------
# Convierte la matriz triangular superior a formato largo (tidy)
tabla_cor <- as.data.frame(as.table(R)) %>%
  rename(Var1 = Var2, Var2 = Var3, r = Freq)   # cuidado con nombres

# Alternativa robusta (más legible):
combinaciones <- combn(colnames(R), 2, simplify = FALSE)

tabla_cor <- do.call(rbind, lapply(combinaciones, function(par) {
  v1 <- par[1]; v2 <- par[2]
  data.frame(
    Variable_1 = v1,
    Variable_2 = v2,
    r          = round(R[v1, v2], 3),
    p_valor    = round(P[v1, v2], 4),
    n          = n_obs[v1, v2],
    Signif     = dplyr::case_when(
      P[v1, v2] < 0.01  ~ "**",
      P[v1, v2] < 0.05  ~ "*",
      P[v1, v2] < 0.15  ~ ".",
      TRUE              ~ "ns"
    )
  )
}))

cat("\n===== Tabla de correlaciones (formato largo) =====\n")
print(tabla_cor)

# --- 14.7 Tabla formateada con knitr -----------------------
kable(tabla_cor,
      caption = "Coeficientes de correlación de Pearson con p-valores",
      col.names = c("Variable 1", "Variable 2", "r", "p-valor", "n", "Signif."),
      digits = 3)

# --- 14.8 Guardar la tabla en CSV --------------------------
write.csv(tabla_cor, "tabla_correlaciones.csv", row.names = FALSE)
cat("\nTabla guardada en: tabla_correlaciones.csv\n")
