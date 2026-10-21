datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

names(datos)
View(datos)

# ==============================================================================
# ANÁLISIS MULTIAMBIENTAL (TxE): ESTABILIDAD DE PRODUCTOS / MADURANTES
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(nlme)
library(emmeans)
library(multcomp)
library(multcompView)
library(factoextra)
library(cluster)

# 1. CONFIGURACIÓN INICIAL
# ------------------------------------------------------------------------------
# Define tu columna de tratamientos principal. 
# Asegúrate de que esta columna contenga SOLO los tratamientos a evaluar, 
# separados de cualquier otra clasificación secundaria.
columna_tratamiento <- "Tratamiento" 

# Variables de respuesta agronómica e industrial
variables_resp <- c("TCH", "Rendimiento", "TAH_PRZ", "Pureza")

# Convertir factores clave
datos$Finca <- as.factor(datos$Finca)
datos$Replica <- as.factor(datos$Replica)

# Forzar el orden alfabético estricto de los tratamientos para tablas y gráficos
datos[[columna_tratamiento]] <- as.factor(datos[[columna_tratamiento]])
datos[[columna_tratamiento]] <- factor(datos[[columna_tratamiento]], 
                                       levels = sort(levels(datos[[columna_tratamiento]])))

# Crear ID único para réplicas por finca (Previene el error "Singularity in backsolve")
datos$ID_Bloque <- interaction(datos$Finca, datos$Replica, drop = TRUE)

# 2. MODELOS LINEALES MIXTOS (LMM) Y COMPARACIÓN DE MEDIAS
# ------------------------------------------------------------------------------
resultados_medias <- list()

for (var in variables_resp) {
  cat("\n========================================================\n")
  cat("Analizando eficacia sobre:", var, "\n")
  
  df_var <- datos %>% 
    filter(!is.na(.data[[var]])) %>%
    droplevels()
  
  # Fórmula: Evalúa el efecto del Madurante, la Finca y cómo interactúan
  formula_fija <- as.formula(paste(var, "~", columna_tratamiento, "* Finca"))
  
  tryCatch({
    modelo <- lme(fixed = formula_fija, 
                  random = ~ 1 | ID_Bloque, 
                  data = df_var,
                  na.action = na.omit)
    
    print(anova(modelo))
    
    # EMMeans respetará el orden alfabético preestablecido
    medias <- emmeans(modelo, specs = as.formula(paste("~", columna_tratamiento, "| Finca")))
    letras <- cld(medias, alpha = 0.05, Letters = letters, adjust = "tukey")
    letras$.group <- trimws(letras$.group)
    
    resultados_medias[[var]] <- letras
    
  }, error = function(e) {
    cat("  [!] No se pudo modelar", var, "debido a un desbalance severo en los datos.\n")
  })
}

# 3. JOIN PLOT (BIPLOT TxE) - ESTABILIDAD DEL PRODUCTO
# ------------------------------------------------------------------------------
# Evaluaremos la estabilidad de los madurantes usando TAH como ejemplo clave.
# Un madurante cerca del centro del gráfico es "estable" (actúa igual en todos lados).
# Un madurante lejos del centro y alineado con una finca específica tiene una "interacción positiva" allí.

matriz_txe <- datos %>%
  group_by(!!sym(columna_tratamiento), Finca) %>%
  summarise(Media_TAH = mean(TAH, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Finca, values_from = Media_TAH) %>%
  column_to_rownames(var = columna_tratamiento)

pca_txe <- prcomp(matriz_txe, scale. = TRUE)

# El gráfico se abrirá directamente en el panel estándar de 'Plots'
p_biplot <- fviz_pca_biplot(pca_txe, 
                            repel = TRUE,
                            col.var = "#D95F02", # Fincas (Ambientes)
                            col.ind = "#1B9E77", # Madurantes (Tratamientos)
                            title = "Estabilidad de Madurantes a través de Fincas (TxE) - TAH",
                            xlab = paste0("CP1 (", round(summary(pca_txe)$importance[2,1]*100, 1), "%)"),
                            ylab = paste0("CP2 (", round(summary(pca_txe)$importance[2,2]*100, 1), "%)")) +
  theme_minimal() +
  labs(subtitle = "Vectores: Ambientes de prueba | Puntos: Tratamientos evaluados")

print(p_biplot)

# 4. ANÁLISIS DE GRUPOS - PERFILAMIENTO DE PRODUCTOS
# ------------------------------------------------------------------------------
# Agrupa los madurantes basándose en su comportamiento global considerando TODAS 
# las variables (TCH, TAH, Pureza, etc.) simultáneamente.

matriz_multitrait <- datos %>%
  group_by(!!sym(columna_tratamiento)) %>%
  summarise(across(all_of(variables_resp), \(x) mean(x, na.rm = TRUE))) %>%
  column_to_rownames(var = columna_tratamiento)

matriz_escalada <- scale(matriz_multitrait)
distancias <- dist(matriz_escalada, method = "euclidean")
cluster_hc <- hclust(distancias, method = "ward.D2")

p_dendro <- fviz_dend(cluster_hc, 
                      k = 3, # Ajusta el número de grupos (ej. Alto, Medio, Bajo impacto)
                      cex = 0.9, 
                      k_colors = "jco",
                      color_labels_by_k = TRUE, 
                      rect = TRUE,
                      main = "Clúster de Similitud entre Madurantes",
                      ylab = "Distancia Euclidiana")

print(p_dendro)