# ==============================================================================
# SÚPER SCRIPT MAESTRO: SUPERFICIE DE RESPUESTA, VALIDACIÓN Y ZONAS OPERATIVAS
# ==============================================================================
# Limpiar entorno de trabajo y gráficos previos
rm(list = ls())
graphics.off()

# Cargar todas las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)
library(ggrepel)
library(metR)
library(writexl)
library(lme4)
library(lmerTest) 
library(emmeans)
library(multcomp)
if (!requireNamespace("sjPlot", quietly = TRUE)) install.packages("sjPlot")
library(sjPlot)

# ==============================================================================
# 1. CONFIGURACIÓN GLOBAL (Zona de Mando y Reglas de Negocio)
# ==============================================================================
cat("\n====================================================================\n")
cat("1. LEYENDO CONFIGURACIÓN Y REGLAS DE NEGOCIO\n")
cat("====================================================================\n")

names(datos)
View(datos)

# --- A. Variables del Modelo ---
variables_y <- c("TAH_Rel")   # Cambia a "TAH_Rel" o "TCH_Rel"
var_x1      <- "CCTon"      # Eje X (Dosis)
var_x2      <- "Hidro_100"  # Eje Y (Humedad)

# --- B. Control de Extensión del Gráfico (Ejes XY Forzados) ---
forzar_grid_manual <- FALSE  # TRUE = Usa los límites de abajo | FALSE = Auto-escala con la data real
grid_min_humedad   <- 60
grid_max_humedad   <- 90
grid_min_dosis     <- 0
grid_max_dosis     <- 10

# --- C. Puntos Fijos para Análisis de Cortes (Líneas Discontinuas) ---
dosis_objetivo_fijas   <- c(3, 4, 5, 6, 7, 8) 
humedad_objetivo_fijas <- c(65, 70, 75, 80, 85, 90)

# --- D. Parámetros de Sensibilidad y Resolución ---
paso_isolineas       <- 0.05  # Reduce a 0.01 para TCH_Rel; usa 0.05 para TAH_Rel
k_grupos_estadistica <- 4     # Cuadrantes para la validación de Modelo Mixto

paso_pixel_dosis   <- 1     # Incremento de Dosis en la Matriz Excel
paso_pixel_humedad <- 1     # Incremento de Humedad en la Matriz Excel

# SENSIBILIDAD DEL MODELO PARA RANGOS ÓPTIMOS: 
tolerancia_centro  <- 0.01  # Caída máxima permitida para la Dosis "Ideal"
tolerancia_orillas <- 0.02  # Caída máxima permitida para los "Límites"
round_output       <- 2     # Usa 2 para TAH. Usa 3 o 4 para ver micro-variaciones en TCH

# --- E. CONFIGURACIÓN DE RANGOS DE HUMEDAD PARA LAS TABLAS ---
usar_rangos_manuales <- FALSE 

# Si TRUE: Cortará exactamente en estos valores (Ej. para 60-64, 66-72, 74-82, 84-90)
limites_operativos_manuales   <- c(-Inf, 65, 73, 83, Inf)
etiquetas_operativas_manuales <- c("Zona 1", "Zona 2", "Zona 3", "Zona 4")

# Si FALSE: Calculará las zonas matemáticamente con K-Means
k_zonas_mosaico <- 4 

# --- F. OPCIONES DE VISUALIZACIÓN ---
mostrar_dosis_en_grafico_zonas <- T # TRUE = Muestra "APLICAR X cc/ton" | FALSE = Solo muestra el nombre de la Zona

# ==============================================================================
# 2. CARGA DE DATOS Y AUTO-ADAPTACIÓN DINÁMICA
# ==============================================================================
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
datos_limpios <- datos %>% tidyr::drop_na(all_of(c(variables_y, var_x1, var_x2)))

if (forzar_grid_manual) {
  min_x1 <- grid_min_dosis; max_x1 <- grid_max_dosis
  min_x2 <- grid_min_humedad; max_x2 <- grid_max_humedad
  cat("=> Escala del gráfico FORZADA manualmente por el usuario.\n")
} else {
  min_x1 <- floor(min(datos_limpios[[var_x1]], na.rm = TRUE))
  max_x1 <- ceiling(max(datos_limpios[[var_x1]], na.rm = TRUE))
  min_x2 <- floor(min(datos_limpios[[var_x2]], na.rm = TRUE))
  max_x2 <- ceiling(max(datos_limpios[[var_x2]], na.rm = TRUE))
  cat("=> Escala del gráfico AUTO-ADAPTADA a los datos reales.\n")
}

cat(sprintf("   Eje X (%s): %d a %d\n", var_x1, min_x1, max_x1))
cat(sprintf("   Eje Y (%s): %d a %d\n", var_x2, min_x2, max_x2))

paleta_agronomica <- c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850")

# ==============================================================================
# 3. ANÁLISIS DE SUPERFICIE BÁSICA Y ÓPTIMO GLOBAL
# ==============================================================================
cat("\n====================================================================\n")
cat("2. ANÁLISIS DE SUPERFICIE BÁSICA Y CURVAS DE PERFIL\n")
cat("====================================================================\n")

var_y_main <- variables_y[1]
formula_rsm <- as.formula(paste(var_y_main, "~", var_x1, "*", var_x2, "+ I(", var_x1, "^2) + I(", var_x2, "^2)"))
modelo_main <- lm(formula_rsm, data = datos_limpios)

grid_continuo <- expand.grid(X1 = seq(min_x1, max_x1, length.out = 200), X2 = seq(min_x2, max_x2, length.out = 200))
names(grid_continuo) <- c(var_x1, var_x2)
grid_continuo$Prediccion_Z <- predict(modelo_main, newdata = grid_continuo)
optimo_global <- grid_continuo[which.max(grid_continuo$Prediccion_Z), ]

# GRÁFICO 1: Superficie Básica
grafico_basico <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_gradientn(colors = paleta_agronomica, name = paste("Max", var_y_main)) +
  geom_point(data = optimo_global, aes_string(x = var_x1, y = var_x2), color = "black", fill = "white", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  geom_text(data = optimo_global, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0("Óptimo:\\n(", round(%s, 2), ", ", round(%s, 2), ")")', var_x1, var_x2)), vjust = -1.2, fontface = "bold", inherit.aes = FALSE) +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2), expand = FALSE) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(title = paste("1. Superficie de Respuesta para", var_y_main), subtitle = paste("Interacción:", var_x1, "x", var_x2), x = var_x1, y = var_x2) + theme_minimal()
print(grafico_basico)

# GRÁFICO 2: Curvas de Perfil Escaladas por Factor (Gráfico de Perturbación)
# 1. Perfil para var_x1 (manteniendo var_x2 en el óptimo)
df_x1 <- data.frame(Valor_Real = seq(min_x1, max_x1, length.out = 200), X2 = optimo_global[[var_x2]])
names(df_x1) <- c(var_x1, var_x2)
df_x1$Valor_Real <- df_x1[[var_x1]]
pred_x1 <- predict(modelo_main, newdata = df_x1)
df_x1$Y_Escalado <- (pred_x1 - min(pred_x1)) / (max(pred_x1) - min(pred_x1))
df_x1$X_Escalado <- (df_x1[[var_x1]] - min_x1) / (max_x1 - min_x1)
df_x1$Factor <- var_x1

# 2. Perfil para var_x2 (manteniendo var_x1 en el óptimo)
df_x2 <- data.frame(X1 = optimo_global[[var_x1]], Valor_Real = seq(min_x2, max_x2, length.out = 200))
names(df_x2) <- c(var_x1, var_x2)
df_x2$Valor_Real <- df_x2[[var_x2]]
pred_x2 <- predict(modelo_main, newdata = df_x2)
df_x2$Y_Escalado <- (pred_x2 - min(pred_x2)) / (max(pred_x2) - min(pred_x2))
df_x2$X_Escalado <- (df_x2[[var_x2]] - min_x2) / (max_x2 - min_x2)
df_x2$Factor <- var_x2

# Integrar ambos factores
df_perfil_factores <- rbind(
  df_x1[, c("Factor", "Valor_Real", "X_Escalado", "Y_Escalado")],
  df_x2[, c("Factor", "Valor_Real", "X_Escalado", "Y_Escalado")]
)

picos_factores <- df_perfil_factores %>% dplyr::group_by(Factor) %>% dplyr::slice_max(Y_Escalado, n = 1)

grafico_curvas <- ggplot(df_perfil_factores, aes(x = X_Escalado, y = Y_Escalado, color = Factor)) +
  geom_line(linewidth = 1.5) +
  geom_point(data = picos_factores, aes(x = X_Escalado, y = Y_Escalado, color = Factor), size = 4, shape = 8, stroke = 2) +
  geom_text_repel(data = picos_factores, aes(x = X_Escalado, y = Y_Escalado, label = sprintf("%s = %.2f", Factor, Valor_Real)), vjust = -1.5, size = 4, fontface = "bold", show.legend = FALSE, direction = "y") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "2. Perfil de Equilibrio: Sensibilidad por Factor", 
       subtitle = "Evaluación de cada factor mientras el otro se mantiene en su óptimo (Gráfico de Perturbación)", 
       x = "Avance en el Rango del Factor (% del Mínimo al Máximo)", 
       y = "Proporción del Rendimiento (0 a 1)", 
       color = "Factor Evaluado") + 
  theme_minimal() + 
  theme(legend.position = "bottom")
print(grafico_curvas)

# ==============================================================================
# 4. GRÁFICOS CONDICIONADOS Y MAESTRO INTEGRADO 
# ==============================================================================
cat("\n====================================================================\n")
cat("3. ANÁLISIS DE CORTES CONDICIONADOS Y GRÁFICO MAESTRO\n")
cat("====================================================================\n")

res_dosis <- data.frame(); seq_h <- seq(min_x2, max_x2, length.out = 500)
for (d in dosis_objetivo_fijas) {
  if(d >= min_x1 && d <= max_x1) { g_tmp <- data.frame(X1 = d, X2 = seq_h); names(g_tmp) <- c(var_x1, var_x2); g_tmp$Prediccion_Z <- predict(modelo_main, newdata = g_tmp); g_tmp$Etiqueta <- paste("Dosis", d); res_dosis <- bind_rows(res_dosis, g_tmp[which.max(g_tmp$Prediccion_Z), ]) }
}

res_humedad <- data.frame(); seq_d <- seq(min_x1, max_x1, length.out = 500)
for (h in humedad_objetivo_fijas) {
  if(h >= min_x2 && h <= max_x2) { g_tmp <- data.frame(X1 = seq_d, X2 = h); names(g_tmp) <- c(var_x1, var_x2); g_tmp$Prediccion_Z <- predict(modelo_main, newdata = g_tmp); g_tmp$Etiqueta <- paste("Humedad", h); res_humedad <- bind_rows(res_humedad, g_tmp[which.max(g_tmp$Prediccion_Z), ]) }
}

# GRÁFICO 3: Óptimos por Dosis Fija
grafico_dosis_fija <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) + geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_vline(xintercept = res_dosis[[var_x1]], linetype = "dashed", color = "black", alpha = 0.4) +
  geom_point(data = res_dosis, aes_string(x = var_x1, y = var_x2), fill = "black", color = "white", shape = 24, size = 4, inherit.aes = FALSE) +
  geom_point(data = optimo_global, aes_string(x = var_x1, y = var_x2), fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  geom_label_repel(data = res_dosis, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0(Etiqueta, "\\nH: ", round(%s, 1), "\\nValor: ", round(Prediccion_Z, %s))', var_x2, round_output)), box.padding = 0.5, nudge_y = -3, direction = "y", size = 3, fontface = "bold", fill = alpha("white", 0.8), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2)) +
  scale_x_continuous(breaks = dosis_objetivo_fijas) + scale_y_continuous(breaks = seq(0, 200, by = 10), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "3. Superficie de Respuesta: Óptimos Condicionados por Dosis", subtitle = "Comparativa de Óptimo Global (círculo) vs. Dosis Fijas (triángulos)", x = var_x1, y = var_x2) + theme_minimal()
print(grafico_dosis_fija)

# GRÁFICO 4: Óptimos por Humedad Fija
grafico_humedad_fija <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) + geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_hline(yintercept = res_humedad[[var_x2]], linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(data = res_humedad, aes_string(x = var_x1, y = var_x2), fill = "black", color = "white", shape = 22, size = 4, inherit.aes = FALSE) +
  geom_point(data = optimo_global, aes_string(x = var_x1, y = var_x2), fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  geom_label_repel(data = res_humedad, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0(Etiqueta, "\\nDosis Ópt: ", round(%s, 2), "\\nValor: ", round(Prediccion_Z, %s))', var_x1, round_output)), box.padding = 0.8, nudge_x = 0.5, direction = "x", size = 3.5, fontface = "bold", fill = alpha("white", 0.8), inherit.aes = FALSE) +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2)) +
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  labs(title = "4. Optimización de Dosis Condicionada por Humedad", subtitle = "Dosis ideal para alcanzar el máximo en humedades fijas", x = var_x1, y = var_x2) + theme_minimal()
print(grafico_humedad_fija)

# GRÁFICO 5: Maestro Integrado
grafico_maestro <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) + geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_vline(xintercept = res_dosis[[var_x1]], linetype = "dotted", color = "black", alpha = 0.4) +
  geom_hline(yintercept = res_humedad[[var_x2]], linetype = "dotted", color = "black", alpha = 0.4) +
  geom_point(data = res_dosis, aes_string(x = var_x1, y = var_x2), fill = "black", color = "white", shape = 24, size = 4, inherit.aes = FALSE) +
  geom_point(data = res_humedad, aes_string(x = var_x1, y = var_x2), fill = "black", color = "white", shape = 22, size = 4, inherit.aes = FALSE) +
  geom_point(data = optimo_global, aes_string(x = var_x1, y = var_x2), fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  geom_label_repel(data = res_dosis, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0("D: ", %s, "\\nH: ", round(%s, 1), "\\nVal: ", round(Prediccion_Z, %s))', var_x1, var_x2, round_output)), nudge_y = 2, direction = "y", size = 2.8, fontface = "bold", color = "#003366", inherit.aes = FALSE) +
  geom_label_repel(data = res_humedad, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0("H: ", %s, "\\nD: ", round(%s, 2), "\\nVal: ", round(Prediccion_Z, %s))', var_x2, var_x1, round_output)), nudge_x = 1, direction = "x", size = 2.8, fontface = "bold", color = "#800000", inherit.aes = FALSE) +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2)) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "5. Gráfico Maestro: Matriz de Decisiones Agronómicas", subtitle = "Óptimos por Dosis (Triángulos), por Humedad (Cuadrados) y Global (Círculo)", x = var_x1, y = var_x2) + theme_minimal()
print(grafico_maestro)


# ==============================================================================
# 5. CREACIÓN ANTICIPADA DEL GRID DE PÍXELES PARA DEFINIR LÍMITES
# ==============================================================================
cat("\n====================================================================\n")
cat("4. PROCESAMIENTO DE MAPA PIXELADO (EXTRACCIÓN DE PICOS ABSOLUTOS)\n")
cat("====================================================================\n")

grid_pixeles <- expand.grid(X1 = seq(min_x1, max_x1, by = paso_pixel_dosis), X2 = seq(min_x2, max_x2, by = paso_pixel_humedad))
names(grid_pixeles) <- c(var_x1, var_x2)
grid_pixeles$TAH_Esperado <- predict(modelo_main, newdata = grid_pixeles)

# --- MÉTODO ORIGINAL: EXTRACCIÓN DEL "CAMINO ÓPTIMO" DESDE LOS PÍXELES ---
optimos_pixeles <- grid_pixeles %>%
  dplyr::group_by(.data[[var_x2]]) %>%
  dplyr::filter(TAH_Esperado == max(TAH_Esperado)) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup()

# Configuración de los Límites y Zonas para el resto de gráficos y tablas
if (usar_rangos_manuales) {
  optimos_pixeles <- optimos_pixeles %>%
    dplyr::mutate(Zona = cut(.data[[var_x2]], breaks = limites_operativos_manuales, labels = etiquetas_operativas_manuales, include.lowest = TRUE))
  
  cortes_finales <- limites_operativos_manuales
  nombres_base   <- etiquetas_operativas_manuales
  
} else {
  # --- K-MEANS ORIGINAL SOBRE EL EJE DE HUMEDAD DE LOS PÍXELES ÓPTIMOS ---
  set.seed(42)
  km_bandas <- kmeans(optimos_pixeles[[var_x2]], centers = k_zonas_mosaico)
  optimos_pixeles$Grupo_KMeans <- km_bandas$cluster
  
  orden_clusters <- optimos_pixeles %>%
    dplyr::group_by(Grupo_KMeans) %>%
    dplyr::summarise(Centro_H = mean(.data[[var_x2]])) %>%
    dplyr::arrange(Centro_H) %>%
    dplyr::mutate(Nuevo_Grupo = 1:k_zonas_mosaico)
  
  optimos_pixeles <- optimos_pixeles %>%
    dplyr::left_join(orden_clusters, by = "Grupo_KMeans") %>%
    dplyr::mutate(Zona = paste("Zona", Nuevo_Grupo))
  
  # Generar límites matemáticos para los gráficos basados en este K-Means
  resumen_zonas <- optimos_pixeles %>% dplyr::group_by(Zona) %>% dplyr::summarise(Max_H = max(.data[[var_x2]]), Min_H = min(.data[[var_x2]]))
  cortes_finales <- c(-Inf)
  for(i in 1:(k_zonas_mosaico-1)) cortes_finales <- c(cortes_finales, mean(c(resumen_zonas$Max_H[i], resumen_zonas$Min_H[i+1])))
  cortes_finales <- c(cortes_finales, Inf)
  
  # CREAR ETIQUETAS DINÁMICAS QUE INCLUYAN LOS PORCENTAJES DE HUMEDAD
  nombres_base <- c(paste("Zona 1: <", round(cortes_finales[2], 1), "%"))
  if(k_zonas_mosaico > 2) {
    for(i in 2:(k_zonas_mosaico-1)) nombres_base <- c(nombres_base, paste("Zona", i, ":", round(cortes_finales[i], 1), "a", round(cortes_finales[i+1], 1), "%"))
  }
  nombres_base <- c(nombres_base, paste("Zona", k_zonas_mosaico, ": >=", round(cortes_finales[k_zonas_mosaico], 1), "%"))
}

# ==============================================================================
# 6. SECTORES Y VECTORES DE INFLUENCIA 
# ==============================================================================
cat("\n====================================================================\n")
cat("5. ZONAS OPERATIVAS Y VECTORES DE INFLUENCIA\n")
cat("====================================================================\n")

# Calcular dosis recomendada por zona para las etiquetas del gráfico
k_zonas_actual <- length(nombres_base)
etiquetas_con_dosis <- character(k_zonas_actual)
dosis_media_zona <- numeric(k_zonas_actual)

for(i in 1:k_zonas_actual) {
  subset_opt <- optimos_pixeles %>% dplyr::filter(Zona == paste("Zona", i)) # Usar la Zona limpia para filtrar
  if(nrow(subset_opt) > 0) {
    d_media <- mean(subset_opt[[var_x1]])
    if(mostrar_dosis_en_grafico_zonas) {
      # CONCATENAR EL NOMBRE BASE (CON PORCENTAJES) Y LA DOSIS APLICADA
      etiquetas_con_dosis[i] <- paste0(nombres_base[i], " • APLICAR ", round(d_media, 1), " cc/ton")
    } else {
      etiquetas_con_dosis[i] <- nombres_base[i]
    }
    dosis_media_zona[i] <- d_media
  } else {
    etiquetas_con_dosis[i] <- paste0(nombres_base[i], " • (Sin datos)")
    dosis_media_zona[i] <- NA
  }
}

# Coordenadas Y para centrar los textos
limites_recortados <- cortes_finales
limites_recortados[1] <- min_x2
limites_recortados[length(limites_recortados)] <- max_x2
pos_y_labels <- limites_recortados[-length(limites_recortados)] + diff(limites_recortados) / 2

paleta_etiquetas <- c("#990000", "#004d00", "#003366", "#800080", "#E56B24", "#000000")
etiq_grafico_zonas <- data.frame(
  X = min_x1 + (max_x1 - min_x1) * 0.15, Y = pos_y_labels, Label = etiquetas_con_dosis, Color = paleta_etiquetas[1:k_zonas_actual]
)

cortes_interiores <- cortes_finales[!is.infinite(cortes_finales)]

# GRÁFICO 6: Sectores Principales
g_sectores <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_contour(color = "black", alpha = 0.5, bins = 20, linewidth = 0.3) +
  geom_text_contour(stroke = 0.15, size = 3, skip = 1, check_overlap = TRUE) +
  geom_point(data = datos_limpios, aes_string(x = var_x1, y = var_x2), color = "black", alpha = 0.2, size = 1.5, inherit.aes = FALSE) +
  geom_hline(yintercept = cortes_interiores, linetype = "dashed", color = paleta_etiquetas[1:length(cortes_interiores)], linewidth = 0.8) +
  geom_text(data = etiq_grafico_zonas, aes(x = X, y = Y, label = Label), color = etiq_grafico_zonas$Color, fontface = "bold", size = 4, hjust = 0, inherit.aes = FALSE) +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2), expand = FALSE) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(title = "6. Mapa de Zonas Operativas de Negocio (Auto-Detectadas)", subtitle = "Cortes de humedad y áreas de decisión", x = var_x1, y = var_x2) + theme_minimal()
print(g_sectores)

# GRÁFICO 7: Vectores de Influencia
df_flechas <- data.frame(X = na.omit(dosis_media_zona), Y_inicio = min_x2, Y_fin = max_x2)
g_vectores <- g_sectores +
  geom_segment(data = df_flechas, aes(x = X, xend = X, y = Y_inicio, yend = Y_fin), arrow = arrow(length = unit(0.4, "cm"), type = "closed"), color = "#E56B24", linewidth = 1.2, alpha = 0.9, inherit.aes = FALSE) +
  labs(title = "7. Sectores Operativos y Vectores de Influencia (Dirección de Aplicación)")
print(g_vectores)

# ==============================================================================
# 7. MATRIZ BIVARIADA K-MEANS Y VALIDACIÓN LMM
# ==============================================================================
cat("\n====================================================================\n")
cat("6. MATRIZ K-MEANS BIVARIADO Y VALIDACIÓN (LMM)\n")
cat("====================================================================\n")

calc_kmeans_cuts_biv <- function(variable, k) {
  km <- kmeans(na.omit(variable), centers = k)
  res <- data.frame(v = na.omit(variable), c = km$cluster) %>% dplyr::group_by(c) %>% dplyr::summarise(min=min(v), max=max(v), cen=mean(v)) %>% dplyr::arrange(cen)
  cuts <- c()
  for(i in 1:(k-1)) cuts <- c(cuts, mean(c(res$max[i], res$min[i+1])))
  return(cuts)
}

set.seed(123)
cortes_k_h <- calc_kmeans_cuts_biv(datos_limpios[[var_x2]], k_grupos_estadistica)
cortes_k_d <- calc_kmeans_cuts_biv(datos_limpios[[var_x1]], k_grupos_estadistica)

min_iso <- floor(min(grid_continuo$Prediccion_Z) / paso_isolineas) * paso_isolineas
max_iso <- ceiling(max(grid_continuo$Prediccion_Z) / paso_isolineas) * paso_isolineas
isolineas_fijas <- seq(min_iso, max_iso, by = paso_isolineas)

# GRÁFICO 8: Matriz de Cuadrantes 
grafico_cuadrantes <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_contour(color = "black", alpha = 0.5, breaks = isolineas_fijas, linewidth = 0.4) +
  geom_text_contour(breaks = isolineas_fijas, aes(label = sprintf(paste0("%.", round_output, "f"), after_stat(level))), stroke = 0.15, size = 3.5, fontface = "bold") +
  geom_point(data = datos_limpios, aes_string(x = var_x1, y = var_x2), color = "black", alpha = 0.3, size = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = cortes_k_h, linetype = "twodash", color = "#cc0000", linewidth = 1) +
  geom_vline(xintercept = cortes_k_d, linetype = "twodash", color = "#003366", linewidth = 1) +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2), expand = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title = paste("8. Matriz Operativa de Cuadrantes (K-Means Bivariado k =", k_grupos_estadistica, ")"), x = var_x1, y = var_x2) + theme_minimal()
print(grafico_cuadrantes)

# Validación Mixta LME4
lims_h <- c(-Inf, cortes_k_h, Inf); etiq_h_letras <- paste0("H", 1:(length(cortes_k_h) + 1)) 
lims_d <- c(-Inf, cortes_k_d, Inf); etiq_d_letras <- paste0("D", 1:(length(cortes_k_d) + 1)) 

datos_anova <- datos_limpios %>%
  dplyr::mutate(
    Grp_H = cut(.data[[var_x2]], breaks = lims_h, labels = etiq_h_letras, include.lowest = TRUE),
    Grp_D = cut(.data[[var_x1]], breaks = lims_d, labels = etiq_d_letras, include.lowest = TRUE),
    Cuadrante = as.factor(paste(Grp_H, Grp_D, sep = "_")),
    Humedad_Bloque = as.factor(round(.data[[var_x2]], 0)) 
  ) %>% dplyr::group_by(Cuadrante) %>% dplyr::filter(n() >= 3) %>% dplyr::ungroup()

modelo_mixto <- lmer(as.formula(paste(var_y_main, "~ Cuadrante + (1 | Humedad_Bloque)")), data = datos_anova)
medias_mixtas <- emmeans::emmeans(modelo_mixto, specs = ~ Cuadrante)
letras_cld <- multcomp::cld(medias_mixtas, alpha = 0.15, Letters = letters, reversed = TRUE, adjust = "tukey") %>%
  as.data.frame() %>% dplyr::mutate(Letra = trimws(.group)) %>% dplyr::select(Cuadrante, Letra)

# GRÁFICO 9: Superficie Validada
centros_quad <- datos_anova %>% dplyr::group_by(Cuadrante) %>% dplyr::summarise(CX = mean(.data[[var_x1]]), CY = mean(.data[[var_x2]])) %>% dplyr::left_join(letras_cld, by="Cuadrante")
g_sup_letras <- grafico_cuadrantes + 
  geom_label(data = centros_quad, aes(x = CX, y = CY, label = paste0(Cuadrante, "\n[", Letra, "]")), fill = "white", color = "black", fontface = "bold", alpha = 0.8, inherit.aes = FALSE) +
  labs(title = "9. Superficie Validada (Letras de Significancia Mixta)")
print(g_sup_letras)

# GRÁFICO 10: Fusión de Zonas
grid_fusion <- grid_continuo %>%
  dplyr::mutate(Grp_H = cut(.data[[var_x2]], breaks = lims_h, labels = etiq_h_letras, include.lowest = TRUE), Grp_D = cut(.data[[var_x1]], breaks = lims_d, labels = etiq_d_letras, include.lowest = TRUE), Cuadrante = as.factor(paste(Grp_H, Grp_D, sep = "_"))) %>%
  dplyr::left_join(letras_cld, by = "Cuadrante") %>% tidyr::replace_na(list(Letra = "Sin Datos"))

g_fusion <- ggplot(grid_fusion, aes_string(x = var_x1, y = var_x2)) +
  geom_raster(aes(fill = Letra), interpolate = FALSE, alpha = 0.75) +
  geom_contour(aes(z = Prediccion_Z), color = "black", alpha = 0.3, linewidth = 0.3) +
  geom_point(data = datos_limpios, aes_string(x = var_x1, y = var_x2), color = "black", alpha = 0.4, size = 1.5, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "Set1") + 
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2), expand = FALSE) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(title = "10. Macro-Zonas por Similitud Estadística (LMM)", x = var_x1, y = var_x2) + theme_minimal()
print(g_fusion)

# ==============================================================================
# 8. MATRIZ PIXELADA Y TABLAS FINALES DE DECISIÓN
# ==============================================================================
cat("\n====================================================================\n")
cat("7. CREANDO MATRIZ DE PÍXELES Y TABLAS DE EXPORTACIÓN\n")
cat("====================================================================\n")

# GRÁFICO 11: Mosaico Pixelado
g_pixeles <- ggplot(grid_pixeles, aes_string(x = var_x1, y = var_x2, fill = "TAH_Esperado")) +
  geom_tile(color = "white", linewidth = 0.5) + 
  scale_fill_gradientn(colors = paleta_agronomica, name = "Retorno") +
  geom_text(aes(label = sprintf(paste0("%.", round_output, "f"), TAH_Esperado)), size = 2.5, color = "black", fontface = "bold") +
  coord_cartesian(xlim = c(min_x1, max_x1), ylim = c(min_x2, max_x2), expand = FALSE) +
  scale_x_continuous(breaks = unique(grid_pixeles[[var_x1]]), expand = c(0, 0)) + scale_y_continuous(breaks = unique(grid_pixeles[[var_x2]]), expand = c(0, 0)) +
  labs(title = "11. Mosaico Pixelado Operativo (Consulta Rápida)", subtitle = "Cada celda representa una intersección exacta", x = var_x1, y = var_x2) + theme_minimal() + theme(panel.grid = element_blank())
print(g_pixeles)

# --- TABLA A: RESUMEN EXACTO (CLON DEL SCRIPT ORIGINAL) ---
tabla_simple <- optimos_pixeles %>%
  dplyr::mutate(Zona_Formato = cut(.data[[var_x2]], breaks = cortes_finales, labels = nombres_base, include.lowest = TRUE)) %>%
  dplyr::group_by(Zona_Formato) %>%
  dplyr::summarise(
    Humedad_Min = min(.data[[var_x2]]), Humedad_Max = max(.data[[var_x2]]),
    Dosis_Min = min(.data[[var_x1]]), Dosis_Max = max(.data[[var_x1]]),
    TAH_Min = min(TAH_Esperado), TAH_Max = max(TAH_Esperado), .groups = "drop"
  ) %>%
  dplyr::mutate(
    Rango_Humedad_Detectada = paste(Humedad_Min, "a", Humedad_Max, "%"),
    Dosis_Requerida_cc_ton = paste(Dosis_Min, "a", Dosis_Max),
    Retorno_TAH_Esperado = paste(sprintf(paste0("%.", round_output, "f"), TAH_Min), "a", sprintf(paste0("%.", round_output, "f"), TAH_Max))
  )

tabla_simple_final <- as.data.frame(tabla_simple)[, c("Zona_Formato", "Rango_Humedad_Detectada", "Dosis_Requerida_cc_ton", "Retorno_TAH_Esperado")] %>% dplyr::rename(Zona = Zona_Formato)

cat("\n--- TABLA SOLICITADA: RESUMEN EXACTO DE PICOS ABSOLUTOS ---\n")
print(tabla_simple_final, row.names = FALSE)
print(sjPlot::tab_df(tabla_simple_final))

# --- TABLA B: CENTRO IDEAL VS ORILLAS (PRECISIÓN) ---
grid_clasificado <- grid_pixeles %>%
  dplyr::mutate(Zona_Operativa = cut(.data[[var_x2]], breaks = cortes_finales, labels = nombres_base, include.lowest = TRUE)) %>%
  dplyr::group_by(Zona_Operativa) %>%
  dplyr::mutate(Max_TAH_Zona = max(TAH_Esperado), Es_Centro = TAH_Esperado >= (Max_TAH_Zona - tolerancia_centro), Es_Orilla = TAH_Esperado >= (Max_TAH_Zona - tolerancia_orillas)) %>%
  dplyr::summarise(
    Rango_Humedad = paste(min(.data[[var_x2]]), "a", max(.data[[var_x2]]), "%"),
    D_Centro = paste(min(.data[[var_x1]][Es_Centro]), "a", max(.data[[var_x1]][Es_Centro])),
    Val_Centro = paste(sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado[Es_Centro])), "a", sprintf(paste0("%.", round_output, "f"), max(Max_TAH_Zona))),
    D_Orillas = paste(min(.data[[var_x1]][Es_Orilla]), "a", max(.data[[var_x1]][Es_Orilla])),
    Val_Orillas = paste(sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado[Es_Orilla])), "a", sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado[Es_Centro]) - 10^(-round_output))), .groups = "drop"
  ) %>%
  dplyr::mutate(Recomendacion_CENTRO = D_Centro, Limites_ORILLAS = D_Orillas)

tabla_precision_final <- as.data.frame(grid_clasificado)[, c("Zona_Operativa", "Rango_Humedad", "Recomendacion_CENTRO", "Val_Centro", "Limites_ORILLAS", "Val_Orillas")]

# ==============================================================================
# 9. EXPORTACIÓN MASIVA A EXCEL
# ==============================================================================
matriz_excel_visual <- grid_pixeles %>% dplyr::mutate(TAH_Esperado = round(TAH_Esperado, round_output)) %>%
  tidyr::pivot_wider(names_from = all_of(var_x1), values_from = TAH_Esperado, names_prefix = "D_") %>% dplyr::arrange(desc(.data[[var_x2]]))

matriz_teorica <- grid_fusion %>% dplyr::group_by(Grp_H, Grp_D) %>%
  dplyr::summarise(Val = round(mean(Prediccion_Z), round_output), .groups="drop") %>%
  tidyr::pivot_wider(names_from = Grp_D, values_from = Val, names_prefix = "D_") %>% dplyr::arrange(Grp_H)

tiempo_actual <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_final <- paste0("Res_H100", tiempo_actual, ".xlsx")

writexl::write_xlsx(
  list(
    "1_Resumen_Simple_Optimos" = tabla_simple_final,
    "2_Matriz_Visual_Pixelada" = matriz_excel_visual,
    "3_Rangos_Centro_y_Orillas" = tabla_precision_final,
    "4_Validacion_Estadistica_LMM" = letras_cld,
    "5_Matriz_KMeans_Teorica" = matriz_teorica
  ), path = archivo_final
)
cat(paste("\n¡Ejecución Perfecta! Las tablas y gráficos solicitados se exportaron a:", archivo_final, "\n"))