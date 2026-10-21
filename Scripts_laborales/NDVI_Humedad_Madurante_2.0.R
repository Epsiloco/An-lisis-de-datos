# ============================================================
# ANÁLISIS DE CLÚSTER k = 2  (VERSIÓN CORREGIDA v3 - sin .data[[ ]])
# ============================================================

paquetes <- c("dplyr", "tidyr", "ggplot2", "dendextend",
              "car", "rstatix", "ggpubr", "agricolae", "knitr")
for (p in paquetes) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}
set.seed(123)

# --- 1. Variables ---
VARS_CLUSTER <- c("NDVI_+_Ini", "Hidro_100_Ini", "TAH_PRZ_Rel")
K_OPT        <- 2

falta <- setdiff(VARS_CLUSTER, names(datos))
if (length(falta) > 0) stop(paste("Faltan:", paste(falta, collapse = ", ")))

# --- 2. Renombrar a nombres cortos y literales ---
df <- datos %>%
  dplyr::select(NDVI    = "NDVI_+_Ini",
                Hidro   = "Hidro_100_Ini",
                TAH     = "TAH_PRZ_Rel") %>%
  tidyr::drop_na()

cat("\nFilas usadas:", nrow(df), "\n")

# Escalar
mat_scaled <- scale(as.data.frame(df))
colnames(mat_scaled) <- c("NDVI", "Hidro", "TAH")

# ============================================================
# 3. DENDROGRAMA
# ============================================================
dist_mat <- dist(mat_scaled, method = "euclidean")
hc       <- hclust(dist_mat, method = "ward.D2")

df$Grupo <- factor(cutree(hc, k = K_OPT))

# Reordenar: G1 = NDVI bajo, G2 = NDVI alto
orden <- df %>%
  dplyr::group_by(Grupo) %>%
  dplyr::summarise(ndvi_medio = mean(NDVI), .groups = "drop") %>%
  dplyr::arrange(ndvi_medio) %>%
  dplyr::mutate(Nuevo = dplyr::row_number())

df <- df %>%
  dplyr::left_join(orden %>% dplyr::select(Grupo, Nuevo), by = "Grupo") %>%
  dplyr::mutate(Grupo = factor(Nuevo)) %>%
  dplyr::select(-Nuevo)

# --- Dendrograma ---
colores <- c("#D7301F", "#2166AC")

dend <- as.dendrogram(hc)
dend <- dendextend::set(dend, "labels", rep("", length(labels(hc))))
dend <- dendextend::set(dend, "branches_lwd", 1)
dend <- dendextend::color_branches(dend, k = K_OPT, col = colores)

par(mar = c(3, 4, 5, 2))
plot(dend, main = "Dendrograma (Ward.D2)",
     ylab = "Distancia", leaflab = "none", cex.main = 1.4)
dendextend::rect.dendrogram(dend, k = K_OPT, border = colores,
                            lty = 2, lwd = 2, xpd = NA)
mtext("Variables: NDVI_Ini · Hidro_100_Ini · TAH_PRZ_Rel",
      side = 3, line = 0.5, adj = 0.5, cex = 0.85, font = 3, col = "gray40")

# ============================================================
# 4. TABLA DE RANGOS
# ============================================================
tabla <- df %>%
  dplyr::group_by(Grupo) %>%
  dplyr::summarise(
    n           = dplyr::n(),
    NDVI_min    = round(min(NDVI), 3),
    NDVI_max    = round(max(NDVI), 3),
    NDVI_media  = round(mean(NDVI), 3),
    Hidro_min   = round(min(Hidro), 1),
    Hidro_max   = round(max(Hidro), 1),
    Hidro_media = round(mean(Hidro), 2),
    TAH_min     = round(min(TAH), 3),
    TAH_max     = round(max(TAH), 3),
    TAH_media   = round(mean(TAH), 3),
    .groups = "drop"
  )

cat("\n=========================================================\n")
cat("  RANGOS Y RESUMEN POR GRUPO\n")
cat("=========================================================\n")
print(as.data.frame(tabla), row.names = FALSE)

# ============================================================
# 5. VALIDACIÓN ESTADÍSTICA (Hidro ~ Grupo)
# ============================================================
cat("\n=========================================================\n")
cat("  VALIDACIÓN: Hidro_100_Ini ~ Grupo\n")
cat("=========================================================\n")

cat("\n--- Shapiro-Wilk por grupo ---\n")
print(df %>% dplyr::group_by(Grupo) %>% rstatix::shapiro_test(Hidro))

cat("\n--- Levene ---\n")
print(car::leveneTest(Hidro ~ Grupo, data = df))

anova_res <- aov(Hidro ~ Grupo, data = df)
cat("\n--- ANOVA ---\n"); print(summary(anova_res))

cat("\n--- Kruskal-Wallis ---\n")
print(kruskal.test(Hidro ~ Grupo, data = df))

cat("\n--- Tukey HSD (α = 0.15) ---\n")
tk <- agricolae::HSD.test(anova_res, trt = "Grupo",
                          alpha = 0.15, group = TRUE, console = FALSE)
print(tk$groups)

letras_df <- data.frame(Grupo = rownames(tk$groups),
                        Letras = tk$groups$groups)
tabla <- tabla %>% dplyr::left_join(letras_df, by = "Grupo")

cat("\n=========================================================\n")
cat("  TABLA FINAL (con letras de Tukey)\n")
cat("=========================================================\n")
print(as.data.frame(tabla), row.names = FALSE)

print(knitr::kable(tabla,
                   caption = paste0("Resumen por grupo (k = ", K_OPT,
                                    ") · Tukey α = 0.15 sobre Hidro_100_Ini"),
                   digits = 3))

# ============================================================
# 6. BOXPLOT con significancia
# ============================================================
p <- ggpubr::ggboxplot(df, x = "Grupo", y = "Hidro",
                       color = "Grupo", add = "jitter",
                       palette = c("#D7301F", "#2166AC")) +
  ggpubr::stat_compare_means(method = "kruskal.test",
                             label.y = max(df$Hidro) * 1.05) +
  ggpubr::stat_compare_means(comparisons = list(c("1", "2")),
                             method = "wilcox.test", label = "p.signif") +
  labs(title = "Hidro_100_Ini por Grupo",
       subtitle = "Grupos formados con: NDVI_Ini + Hidro_100_Ini + TAH_PRZ_Rel",
       x = "Grupo", y = "Hidro_100_Ini") +
  theme_minimal()
print(p)

# ============================================================
# 7. GUARDAR
# ============================================================
write.csv(tabla, "tabla_clusters_k2.csv", row.names = FALSE)
cat("\nGuardado: tabla_clusters_k2.csv\n")