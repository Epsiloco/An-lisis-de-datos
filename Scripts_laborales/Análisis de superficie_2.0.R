# ==============================================================================
# SÚPER SCRIPT MAESTRO: AUTO-DETECCIÓN DE ZONAS, VALIDACIÓN Y MOSAICO
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
# 1. CONFIGURACIÓN GLOBAL (Zona de Mando)
# ==============================================================================
cat("\n====================================================================\n")
cat("1. LEYENDO CONFIGURACIÓN (MODO: AUTO-DETECCIÓN ESTADÍSTICA)\n")
cat("====================================================================\n")

# --- A. Variables del Modelo ---
variables_y <- c("TAH_Rel")   # Cambia a "TCH_Rel" o "TAH_Rel" según necesites
var_x1      <- "CCTon"      # Eje X (Dosis)
var_x2      <- "Hidro_100"  # Eje Y (Humedad)

# --- B. Puntos Fijos para Análisis de Cortes (Gráficos Condicionados) ---
dosis_objetivo_fijas   <- c(3, 4, 5, 6, 7, 8) 
humedad_objetivo_fijas <- c(65, 70, 75, 80, 85, 90)

# --- C. Parámetros de Resolución y Clústeres ---
paso_isolineas       <- 0.05  # Cambia a 0.01 para TCH_Rel para ver más curvas
k_grupos_estadistica <- 4     # Cuadrantes para la matriz y validación LMM

# --- D. Resolución Operativa (Mosaico Pixelado) ---
paso_pixel_dosis   <- 1     # Incremento de Dosis en la matriz final
paso_pixel_humedad <- 2     # Incremento de Humedad en la matriz final

# SENSIBILIDAD DEL MODELO PARA CAPTURAR RANGOS DE DOSIS: 
tolerancia_centro  <- 0.01  # Caída máxima de TAH para atrapar toda la dosis "Ideal"
tolerancia_orillas <- 0.03  # Caída máxima de TAH para delimitar los "Límites"
round_output       <- 2     # Usa 2 para TAH. Sube a 3 o 4 para TCH

# --- E. CONFIGURACIÓN DE RANGOS DE HUMEDAD (MANUAL VS AUTOMÁTICO) ---
# TRUE  = Usa los límites manuales del Ingenio que definas abajo.
# FALSE = Usa K-Means para descubrir los rangos matemáticos automáticamente.
usar_rangos_manuales <- F 

# Si 'usar_rangos_manuales' es TRUE, el código cortará la caña en estos rangos.
limites_operativos_manuales   <- c(-Inf, 65, 73, 83, Inf)
etiquetas_operativas_manuales <- c("Zona 1", "Zona 2", "Zona 3", "Zona 4")

# Si 'usar_rangos_manuales' es FALSE, creará esta cantidad de Zonas automáticas:
k_zonas_mosaico <- 4 

# ==============================================================================
# 2. CARGA DE DATOS Y MOTOR DE AUTO-ADAPTACIÓN
# ==============================================================================
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
datos_limpios <- datos %>% tidyr::drop_na(all_of(c(variables_y, var_x1, var_x2)))

# DETECCIÓN AUTOMÁTICA DE LA ESCALA DEL GRÁFICO
min_x1 <- floor(min(datos_limpios[[var_x1]], na.rm = TRUE))
max_x1 <- ceiling(max(datos_limpios[[var_x1]], na.rm = TRUE))
min_x2 <- floor(min(datos_limpios[[var_x2]], na.rm = TRUE))
max_x2 <- ceiling(max(datos_limpios[[var_x2]], na.rm = TRUE))

cat(sprintf("=> Escala dinámica ajustada para %s: %d a %d\n", var_x1, min_x1, max_x1))
cat(sprintf("=> Escala dinámica ajustada para %s: %d a %d\n", var_x2, min_x2, max_x2))

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
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) + geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_gradientn(colors = paleta_agronomica, name = paste("Max", var_y_main)) +
  geom_point(data = optimo_global, aes_string(x = var_x1, y = var_x2), color = "black", fill = "white", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  geom_text(data = optimo_global, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0("Óptimo:\\n(", round(%s, 2), ", ", round(%s, 2), ")")', var_x1, var_x2)), vjust = -1.2, fontface = "bold", inherit.aes = FALSE) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(title = paste("1. Superficie de Respuesta para", var_y_main), subtitle = paste("Interacción:", var_x1, "x", var_x2), x = var_x1, y = var_x2) + theme_minimal()
print(grafico_basico)

# GRÁFICO 2: Curvas de Perfil Escaladas
df_perfil <- data.frame(X1 = seq(min_x1, max_x1, length.out = 200), X2 = optimo_global[[var_x2]])
names(df_perfil) <- c(var_x1, var_x2)
pred_perfil <- predict(modelo_main, newdata = df_perfil)
df_perfil$Escalado <- (pred_perfil - min(pred_perfil)) / (max(pred_perfil) - min(pred_perfil))
pico_escalado <- df_perfil[which.max(df_perfil$Escalado), ]

grafico_curvas <- ggplot(df_perfil, aes_string(x = var_x1, y = "Escalado")) +
  geom_line(linewidth = 1.5, color = "#2b83ba") + geom_vline(xintercept = pico_escalado[[var_x1]], linetype = "dashed", linewidth = 0.8) +
  geom_point(data = pico_escalado, aes_string(x = var_x1, y = "Escalado"), size = 4, shape = 8, stroke = 2) +
  geom_text(data = pico_escalado, aes_string(x = var_x1, y = "Escalado", label = sprintf("round(%s, 2)", var_x1)), vjust = -1.5, size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "2. Perfil de Equilibrio: Curvas Escaladas", subtitle = sprintf("Humedad fijada en %.2f", optimo_global[[var_x2]]), x = var_x1, y = "Proporción del Rendimiento (0 a 1)") + theme_minimal()
print(grafico_curvas)

# ==============================================================================
# 4. GRÁFICOS CONDICIONADOS Y MAESTRO INTEGRADO
# ==============================================================================
cat("\n====================================================================\n")
cat("3. ANÁLISIS DE CORTES CONDICIONADOS Y GRÁFICO MAESTRO\n")
cat("====================================================================\n")

res_dosis <- data.frame(); seq_h <- seq(min_x2, max_x2, length.out = 500)
for (d in dosis_objetivo_fijas) {
  if(d >= min_x1 && d <= max_x1) { g_tmp <- data.frame(X1 = d, X2 = seq_h); names(g_tmp) <- c(var_x1, var_x2); g_tmp$Pred <- predict(modelo_main, newdata = g_tmp); g_tmp$Etiqueta <- paste("Dosis", d); res_dosis <- bind_rows(res_dosis, g_tmp[which.max(g_tmp$Pred), ]) }
}

res_humedad <- data.frame(); seq_d <- seq(min_x1, max_x1, length.out = 500)
for (h in humedad_objetivo_fijas) {
  if(h >= min_x2 && h <= max_x2) { g_tmp <- data.frame(X1 = seq_d, X2 = h); names(g_tmp) <- c(var_x1, var_x2); g_tmp$Pred <- predict(modelo_main, newdata = g_tmp); g_tmp$Etiqueta <- paste("Humedad", h); res_humedad <- bind_rows(res_humedad, g_tmp[which.max(g_tmp$Pred), ]) }
}

# GRÁFICO 3: Maestro Integrado
grafico_maestro <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) + geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_vline(xintercept = res_dosis[[var_x1]], linetype = "dotted", color = "black", alpha = 0.4) + geom_hline(yintercept = res_humedad[[var_x2]], linetype = "dotted", color = "black", alpha = 0.4) +
  geom_point(data = res_dosis, aes_string(x = var_x1, y = var_x2), fill = "black", color = "white", shape = 24, size = 4, inherit.aes = FALSE) +
  geom_point(data = res_humedad, aes_string(x = var_x1, y = var_x2), fill = "black", color = "white", shape = 22, size = 4, inherit.aes = FALSE) +
  geom_point(data = optimo_global, aes_string(x = var_x1, y = var_x2), fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  geom_label_repel(data = res_dosis, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0("D: ", %s, "\\nH: ", round(%s, 1), "\\nVal: ", round(Pred, %s))', var_x1, var_x2, round_output)), nudge_y = 2, direction = "y", size = 2.8, fontface = "bold", color = "#003366", inherit.aes = FALSE) +
  geom_label_repel(data = res_humedad, aes_string(x = var_x1, y = var_x2, label = sprintf('paste0("H: ", %s, "\\nD: ", round(%s, 2), "\\nVal: ", round(Pred, %s))', var_x2, var_x1, round_output)), nudge_x = 1, direction = "x", size = 2.8, fontface = "bold", color = "#800000", inherit.aes = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) + scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  labs(title = "3. Gráfico Maestro: Matriz de Decisiones Agronómicas", subtitle = "Óptimos por Dosis (Triángulos), por Humedad (Cuadrados) y Global (Círculo)", x = var_x1, y = var_x2) + theme_minimal()
print(grafico_maestro)

# ==============================================================================
# 5. DEFINICIÓN DE ZONAS OPERATIVAS Y DOSIS SUGERIDA (GRÁFICOS 4 Y 5)
# ==============================================================================
cat("\n====================================================================\n")
cat("4. ZONAS OPERATIVAS Y VECTORES DE INFLUENCIA\n")
cat("====================================================================\n")

# Extraer la "cresta" óptima de la superficie continua
optimos_superficie <- grid_continuo %>%
  dplyr::group_by(.data[[var_x2]]) %>%
  dplyr::filter(Prediccion_Z == max(Prediccion_Z)) %>%
  dplyr::slice(1) %>% dplyr::ungroup()

# Definir los límites y nombres de zona según configuración (Manual vs Auto)
if (usar_rangos_manuales) {
  limites_finales <- limites_operativos_manuales
  nombres_base    <- etiquetas_operativas_manuales
  titulo_grafico  <- "4. Mapa de Zonas Operativas de Negocio (Límites Manuales)"
} else {
  set.seed(42)
  km_bandas <- kmeans(optimos_superficie[[var_x2]], centers = k_zonas_mosaico)
  ctmp <- data.frame(H = optimos_superficie[[var_x2]], C = km_bandas$cluster) %>% dplyr::group_by(C) %>% dplyr::summarise(Max_H = max(H), Min_H = min(H)) %>% dplyr::arrange(Max_H)
  limites_finales <- c(-Inf)
  for(i in 1:(k_zonas_mosaico-1)) limites_finales <- c(limites_finales, mean(c(ctmp$Max_H[i], ctmp$Min_H[i+1])))
  limites_finales <- c(limites_finales, Inf)
  
  nombres_base <- c(paste("Zona 1: <", round(limites_finales[2], 1), "%"))
  for (i in 2:(k_zonas_mosaico-1)) nombres_base <- c(nombres_base, paste("Zona", i, ":", round(limites_finales[i], 1), "a", round(limites_finales[i+1], 1), "%"))
  nombres_base <- c(nombres_base, paste("Zona", k_zonas_mosaico, ": >=", round(limites_finales[k_zonas_mosaico], 1), "%"))
  titulo_grafico  <- "4. Mapa de Zonas Operativas de Negocio (Auto-Detectadas)"
}

# Calcular Dosis Recomendada por Zona y Preparar Etiquetas del Gráfico
k_zonas_actual <- length(nombres_base)
etiquetas_con_dosis <- character(k_zonas_actual)
dosis_media_zona <- numeric(k_zonas_actual)

for(i in 1:k_zonas_actual) {
  subset_opt <- optimos_superficie %>% dplyr::filter(.data[[var_x2]] > limites_finales[i] & .data[[var_x2]] <= limites_finales[i+1])
  if(nrow(subset_opt) > 0) {
    d_media <- mean(subset_opt[[var_x1]])
    etiquetas_con_dosis[i] <- paste0(nombres_base[i], " • APLICAR ", round(d_media, 1), " cc/ton")
    dosis_media_zona[i] <- d_media
  } else {
    etiquetas_con_dosis[i] <- paste0(nombres_base[i], " • (Sin datos)")
    dosis_media_zona[i] <- NA
  }
}

# Preparar las coordenadas "Y" dinámicas para centrar los textos
limites_recortados <- limites_finales
limites_recortados[1] <- min_x2
limites_recortados[length(limites_recortados)] <- max_x2
pos_y_labels <- limites_recortados[-length(limites_recortados)] + diff(limites_recortados) / 2

paleta_etiquetas <- c("#990000", "#004d00", "#003366", "#800080", "#E56B24", "#000000")
etiq_grafico_zonas <- data.frame(
  X = min_x1 + (max_x1 - min_x1) * 0.15, 
  Y = pos_y_labels, 
  Label = etiquetas_con_dosis, 
  Color = paleta_etiquetas[1:k_zonas_actual]
)

cortes_interiores <- limites_finales[!is.infinite(limites_finales)]

# GRÁFICO 4: Sectores Principales con Dosis
g_sectores <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_contour(color = "black", alpha = 0.5, bins = 20, linewidth = 0.3) +
  geom_text_contour(stroke = 0.15, size = 3, skip = 1, check_overlap = TRUE) +
  geom_point(data = datos_limpios, aes_string(x = var_x1, y = var_x2), color = "black", alpha = 0.2, size = 1.5, inherit.aes = FALSE) +
  geom_hline(yintercept = cortes_interiores, linetype = "dashed", color = paleta_etiquetas[1:length(cortes_interiores)], linewidth = 0.8) +
  geom_text(data = etiq_grafico_zonas, aes(x = X, y = Y, label = Label), color = etiq_grafico_zonas$Color, fontface = "bold", size = 4, hjust = 0, inherit.aes = FALSE) +
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(title = titulo_grafico, subtitle = "Cortes de humedad con la Dosis Óptima recomendada para la franja", x = var_x1, y = var_x2) + theme_minimal()
print(g_sectores)

# GRÁFICO 5: Vectores de Influencia
df_flechas <- data.frame(X = na.omit(dosis_media_zona), Y_inicio = min_x2, Y_fin = max_x2)
g_vectores <- g_sectores +
  geom_segment(data = df_flechas, aes(x = X, xend = X, y = Y_inicio, yend = Y_fin), arrow = arrow(length = unit(0.4, "cm"), type = "closed"), color = "#E56B24", linewidth = 1.2, alpha = 0.9, inherit.aes = FALSE) +
  labs(title = "5. Sectores Operativos y Vectores de Influencia (Dirección de Aplicación)")
print(g_vectores)

# ==============================================================================
# 6. MATRIZ BIVARIADA K-MEANS Y VALIDACIÓN LMM
# ==============================================================================
cat("\n====================================================================\n")
cat("5. MATRIZ K-MEANS BIVARIADO Y VALIDACIÓN (LMM)\n")
cat("====================================================================\n")

calc_kmeans_cuts_bivariado <- function(variable, k) {
  km <- kmeans(na.omit(variable), centers = k)
  res <- data.frame(v = na.omit(variable), c = km$cluster) %>% dplyr::group_by(c) %>% dplyr::summarise(min=min(v), max=max(v), cen=mean(v)) %>% dplyr::arrange(cen)
  cuts <- c()
  for(i in 1:(k-1)) cuts <- c(cuts, mean(c(res$max[i], res$min[i+1])))
  return(cuts)
}

set.seed(123)
cortes_k_h <- calc_kmeans_cuts_bivariado(datos_limpios[[var_x2]], k_grupos_estadistica)
cortes_k_d <- calc_kmeans_cuts_bivariado(datos_limpios[[var_x1]], k_grupos_estadistica)

min_iso <- floor(min(grid_continuo$Prediccion_Z) / paso_isolineas) * paso_isolineas
max_iso <- ceiling(max(grid_continuo$Prediccion_Z) / paso_isolineas) * paso_isolineas
isolineas_fijas <- seq(min_iso, max_iso, by = paso_isolineas)

# GRÁFICO 6: Matriz de Cuadrantes 
grafico_cuadrantes <- ggplot(grid_continuo, aes_string(x = var_x1, y = var_x2, z = "Prediccion_Z")) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(colors = paleta_agronomica, name = var_y_main) +
  geom_contour(color = "black", alpha = 0.5, breaks = isolineas_fijas, linewidth = 0.4) +
  geom_text_contour(breaks = isolineas_fijas, aes(label = sprintf(paste0("%.", round_output, "f"), after_stat(level))), stroke = 0.15, size = 3.5, fontface = "bold") +
  geom_point(data = datos_limpios, aes_string(x = var_x1, y = var_x2), color = "black", alpha = 0.3, size = 1, inherit.aes = FALSE) +
  geom_hline(yintercept = cortes_k_h, linetype = "twodash", color = "#cc0000", linewidth = 1) +
  geom_vline(xintercept = cortes_k_d, linetype = "twodash", color = "#003366", linewidth = 1) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0)) +
  labs(title = paste("6. Matriz Operativa de Cuadrantes (K-Means Bivariado k =", k_grupos_estadistica, ")"), x = var_x1, y = var_x2) + theme_minimal()
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
letras_cld <- multcomp::cld(medias_mixtas, Letters = letters, reversed = TRUE, adjust = "tukey") %>%
  as.data.frame() %>% dplyr::mutate(Letra = trimws(.group)) %>% dplyr::select(Cuadrante, Media_Estimada = emmean, Letra)

# GRÁFICO 7: Superficie Validada (Letras)
centros_quad <- datos_anova %>% dplyr::group_by(Cuadrante) %>% dplyr::summarise(CX = mean(.data[[var_x1]]), CY = mean(.data[[var_x2]])) %>% dplyr::left_join(letras_cld, by="Cuadrante")
g_sup_letras <- grafico_cuadrantes + 
  geom_label(data = centros_quad, aes(x = CX, y = CY, label = paste0(Cuadrante, "\n[", Letra, "]")), fill = "white", color = "black", fontface = "bold", alpha = 0.8, inherit.aes = FALSE) +
  labs(title = "7. Superficie Validada (Letras de Significancia Mixta)")
print(g_sup_letras)

# GRÁFICO 8: Fusión de Zonas
grid_fusion <- grid_continuo %>%
  dplyr::mutate(Grp_H = cut(.data[[var_x2]], breaks = lims_h, labels = etiq_h_letras, include.lowest = TRUE), Grp_D = cut(.data[[var_x1]], breaks = lims_d, labels = etiq_d_letras, include.lowest = TRUE), Cuadrante = as.factor(paste(Grp_H, Grp_D, sep = "_"))) %>%
  dplyr::left_join(letras_cld, by = "Cuadrante") %>% tidyr::replace_na(list(Letra = "Sin Datos"))

g_fusion <- ggplot(grid_fusion, aes_string(x = var_x1, y = var_x2)) +
  geom_raster(aes(fill = Letra), interpolate = FALSE, alpha = 0.75) +
  geom_contour(aes(z = Prediccion_Z), color = "black", alpha = 0.3, linewidth = 0.3) +
  geom_point(data = datos_limpios, aes_string(x = var_x1, y = var_x2), color = "black", alpha = 0.4, size = 1.5, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "Set1") + scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) +
  labs(title = "8. Macro-Zonas por Similitud Estadística (LMM)", x = var_x1, y = var_x2) + theme_minimal()
print(g_fusion)

# ==============================================================================
# 7. MATRIZ PIXELADA Y EXTRACCIÓN AVANZADA DE RANGOS DE DOSIS
# ==============================================================================
cat("\n====================================================================\n")
cat("6. CREANDO MATRIZ DE PÍXELES Y EXTRACCIÓN DE DOSIS POR MESETA\n")
cat("====================================================================\n")

grid_pixeles <- expand.grid(X1 = seq(min_x1, max_x1, by = paso_pixel_dosis), X2 = seq(min_x2, max_x2, by = paso_pixel_humedad))
names(grid_pixeles) <- c(var_x1, var_x2)
grid_pixeles$TAH_Esperado <- predict(modelo_main, newdata = grid_pixeles)

# GRÁFICO 9: Mosaico Pixelado
g_pixeles <- ggplot(grid_pixeles, aes_string(x = var_x1, y = var_x2, fill = "TAH_Esperado")) +
  geom_tile(color = "white", linewidth = 0.5) + 
  scale_fill_gradientn(colors = paleta_agronomica, name = "Retorno") +
  geom_text(aes(label = sprintf(paste0("%.", round_output, "f"), TAH_Esperado)), size = 2.5, color = "black", fontface = "bold") +
  scale_x_continuous(breaks = unique(grid_pixeles[[var_x1]]), expand = c(0, 0)) + scale_y_continuous(breaks = unique(grid_pixeles[[var_x2]]), expand = c(0, 0)) +
  labs(title = "9. Mosaico Pixelado Operativo (Consulta Rápida)", subtitle = "Cada celda representa una intersección exacta de la predicción", x = var_x1, y = var_x2) + theme_minimal() + theme(panel.grid = element_blank())
print(g_pixeles)

# --- USO DE LAS ZONAS UNIFICADAS PARA EXTRACCIÓN ---
grid_zonificado <- grid_pixeles %>%
  dplyr::mutate(Zona_Humedad = cut(.data[[var_x2]], breaks = limites_finales, labels = nombres_base, include.lowest = TRUE)) %>%
  dplyr::group_by(Zona_Humedad) %>%
  dplyr::mutate(
    Pico_Maximo_Zona = max(TAH_Esperado),
    Pertenece_Al_Centro = TAH_Esperado >= (Pico_Maximo_Zona - tolerancia_centro),
    Pertenece_A_Orilla  = TAH_Esperado >= (Pico_Maximo_Zona - tolerancia_orillas)
  )

# --- TABLA A: RESUMEN SIMPLIFICADO ---
tabla_simple <- grid_zonificado %>%
  dplyr::filter(Pertenece_Al_Centro == TRUE) %>%
  dplyr::summarise(
    Humedad_Detectada = paste(min(.data[[var_x2]]), "a", max(.data[[var_x2]]), "%"),
    Dosis_Requerida_cc_ton = paste(min(.data[[var_x1]]), "a", max(.data[[var_x1]])),
    Retorno_Estimado = paste(sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado)), "a", sprintf(paste0("%.", round_output, "f"), max(TAH_Esperado))),
    .groups = "drop"
  )
tabla_simple_final <- as.data.frame(tabla_simple)

cat("\n--- TABLA SOLICITADA: RESUMEN SIMPLIFICADO DE DOSIS REQUERIDAS ---\n")
print(tabla_simple_final, row.names = FALSE)
print(sjPlot::tab_df(tabla_simple_final))

# --- TABLA B: PRECISIÓN (CENTRO VS ORILLAS) ---
tabla_precision <- grid_zonificado %>%
  dplyr::summarise(
    Rango_Humedad = paste(min(.data[[var_x2]]), "a", max(.data[[var_x2]]), "%"),
    Recomendacion_CENTRO = paste(min(.data[[var_x1]][Pertenece_Al_Centro]), "a", max(.data[[var_x1]][Pertenece_Al_Centro])),
    Val_Centro = paste(sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado[Pertenece_Al_Centro])), "a", sprintf(paste0("%.", round_output, "f"), max(Pico_Maximo_Zona))),
    Limites_ORILLAS = paste(min(.data[[var_x1]][Pertenece_A_Orilla]), "a", max(.data[[var_x1]][Pertenece_A_Orilla])),
    Val_Orillas = paste(sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado[Pertenece_A_Orilla])), "a", sprintf(paste0("%.", round_output, "f"), min(TAH_Esperado[Pertenece_Al_Centro]) - 10^(-round_output))),
    .groups = "drop"
  )
tabla_precision_final <- as.data.frame(tabla_precision)

# ==============================================================================
# 9. EXPORTACIÓN MASIVA A EXCEL
# ==============================================================================
matriz_excel_visual <- grid_pixeles %>% dplyr::mutate(TAH_Esperado = round(TAH_Esperado, round_output)) %>%
  tidyr::pivot_wider(names_from = all_of(var_x1), values_from = TAH_Esperado, names_prefix = "D_") %>% dplyr::arrange(desc(.data[[var_x2]]))

matriz_teorica <- grid_fusion %>% dplyr::group_by(Grp_H, Grp_D) %>%
  dplyr::summarise(Val = round(mean(Prediccion_Z), round_output), .groups="drop") %>%
  tidyr::pivot_wider(names_from = Grp_D, values_from = Val, names_prefix = "D_") %>% dplyr::arrange(Grp_H)

# Clave de tiempo única
tiempo_actual <- format(Sys.time(), "%Y%m%d_%H%M%S")
archivo_final <- paste0("Analisis_Maestro_Integrado_", tiempo_actual, ".xlsx")

writexl::write_xlsx(
  list(
    "1_Resumen_Simple_Optimos" = tabla_simple_final,
    "2_Matriz_Visual_Pixelada" = matriz_excel_visual,
    "3_Rangos_Centro_y_Orillas" = tabla_precision_final,
    "4_Validacion_Estadistica_LMM" = letras_cld,
    "5_Matriz_KMeans_Teorica" = matriz_teorica
  ), path = archivo_final
)

cat(paste("\n¡Ejecución Perfecta! Las tablas dinámicas y gráficos se exportaron a:", archivo_final, "\n"))