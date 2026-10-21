# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom) # Para extraer estadísticos del modelo fácilmente

datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)
attach(datos)
View(datos)

# ==============================================================================
# 1. PARÁMETROS DE ENTRADA (Automatización)
# ==============================================================================
# Define las variables de respuesta y los factores (ejes X e Y de la superficie)
variables_y <- c("TCH")
factores_x  <- c("Vinaza", "Nitrogeno")

# ==============================================================================
# 2. FUNCIÓN DE SUPERFICIE DE RESPUESTA Y OPTIMIZACIÓN EN DATOS REALES
# ==============================================================================
analizar_superficie <- function(df, var_y, f1, f2) {
  
  # 1. Ajustar el modelo de superficie de respuesta (cuadrático + interacción)
  formula_rsm <- as.formula(paste(var_y, "~", f1, "*", f2, "+ I(", f1, "^2) + I(", f2, "^2)"))
  modelo <- lm(formula_rsm, data = df)
  
  # 2. Extraer información útil del modelo (R2 y p-valor)
  resumen <- summary(modelo)
  r_cuadrado <- resumen$r.squared
  p_valor_modelo <- pf(resumen$fstatistic[1], resumen$fstatistic[2], resumen$fstatistic[3], lower.tail = FALSE)
  
  # 3. Crear una cuadrícula (grid) muy fina basada en los RANGOS REALES
  # Esto garantiza que el óptimo nunca sea un valor negativo o inalcanzable
  rango_f1 <- range(df[[f1]], na.rm = TRUE)
  rango_f2 <- range(df[[f2]], na.rm = TRUE)
  
  grid_f1 <- seq(rango_f1[1], rango_f1[2], length.out = 100)
  grid_f2 <- seq(rango_f2[1], rango_f2[2], length.out = 100)
  
  grid_pred <- expand.grid(F1 = grid_f1, F2 = grid_f2)
  names(grid_pred) <- c(f1, f2)
  
  # 4. Predecir los valores de Z (la variable Y) en toda la cuadrícula
  grid_pred$Prediccion_Z <- predict(modelo, newdata = grid_pred)
  
  # 5. Encontrar el punto de máxima respuesta (Óptimo)
  punto_optimo <- grid_pred[which.max(grid_pred$Prediccion_Z), ]
  
  # Retornar una lista con los datos procesados para usar luego
  return(list(
    Variable = var_y,
    Optimo_F1 = punto_optimo[[f1]],
    Optimo_F2 = punto_optimo[[f2]],
    Maximo_Z = punto_optimo$Prediccion_Z,
    R_Cuadrado = r_cuadrado,
    P_Valor = p_valor_modelo,
    Datos_Grid = grid_pred
  ))
}

# ==============================================================================
# 3. EJECUCIÓN Y GENERACIÓN DE RESULTADOS
# ==============================================================================

# Listas para almacenar resultados
tabla_resultados <- list()
graficos <- list()

cat("\n====================================================================\n")
cat("ANÁLISIS DE SUPERFICIE DE RESPUESTA\n")
cat("Factores:", factores_x[1], "y", factores_x[2], "\n")
cat("====================================================================\n")

for (var in variables_y) {
  
  # Ejecutar el análisis matemático
  resultado <- analizar_superficie(datos, var, factores_x[1], factores_x[2])
  
  # Guardar los datos numéricos para la tabla final
  tabla_resultados[[var]] <- data.frame(
    Variable = var,
    Optimo_Vinaza = resultado$Optimo_F1,
    Optimo_Nitrogeno = resultado$Optimo_F2,
    Valor_Maximo = resultado$Maximo_Z,
    R2_Modelo = round(resultado$R_Cuadrado, 4),
    Significancia_P = formatC(resultado$P_Valor, format = "e", digits = 2)
  )
  
  # Crear el Gráfico de Superficie (Contornos y Mapa de Calor)
  grid_data <- resultado$Datos_Grid
  
  grafico <- ggplot(grid_data, aes(x = .data[[factores_x[1]]], y = .data[[factores_x[2]]], z = Prediccion_Z)) +
    # Mapa de calor base
    geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
    # Líneas de contorno topográficas
    geom_contour(color = "white", alpha = 0.5, bins = 15) +
    # Paleta de colores (Verde/Amarillo/Rojo estilo agronómico)
    scale_fill_distiller(palette = "Spectral", name = paste("Max", var)) +
    # Marcar el punto óptimo
    geom_point(aes(x = resultado$Optimo_F1, y = resultado$Optimo_F2), 
               color = "black", shape = 8, size = 4, stroke = 1.5) +
    geom_text(aes(x = resultado$Optimo_F1, y = resultado$Optimo_F2, 
                  label = paste0("Óptimo:\n(", round(resultado$Optimo_F1, 2), ", ", round(resultado$Optimo_F2, 2), ")")),
              vjust = -1, color = "black", size = 3.5, fontface = "bold") +
    labs(title = paste("Superficie de Respuesta para", var),
         subtitle = paste("Interacción:", factores_x[1], "x", factores_x[2]),
         x = factores_x[1],
         y = factores_x[2]) +
    theme_minimal() +
    theme(legend.position = "right")
  
  # Imprimir el gráfico en la iteración
  print(grafico)
}

# ==============================================================================
# 4. RESUMEN DE EQUILIBRIO (Tabla Final)
# ==============================================================================
tabla_final <- bind_rows(tabla_resultados)

cat("\n--- TABLA DE PUNTOS ÓPTIMOS Y ESTADÍSTICOS DEL MODELO ---\n")
print(tabla_final)

cat("\n--- ANÁLISIS DE EQUILIBRIO ---\n")
rango_opt_cc <- range(tabla_final$Optimo_CCTon)
rango_opt_hi <- range(tabla_final$Optimo_Hidro_100)

cat(sprintf("=> El 'Grupo de Compromiso' para CCTon se encuentra entre: %.2f y %.2f\n", rango_opt_cc[1], rango_opt_cc[2]))
cat(sprintf("=> El 'Grupo de Compromiso' para Hidro_100 se encuentra entre: %.2f y %.2f\n", rango_opt_hi[1], rango_opt_hi[2]))
cat("NOTA: El punto óptimo de TAH suele ser la mejor decisión matemática integral, ya que capitaliza tanto el campo como la fábrica.\n")









#################################################################################

graphics.off()

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom)

# ==============================================================================
# 1. PARÁMETROS DE ENTRADA (Automatización)
# ==============================================================================
variables_y <- c("TCH")
factores_x  <- c("Vinaza", "Nitrogeno")

# --- INTERRUPTOR DE CLUSTERING ---
usar_clustering <- F
num_clusters <- 5 

# ==============================================================================
# 2. FUNCIÓN DE SUPERFICIE DE RESPUESTA Y OPTIMIZACIÓN
# ==============================================================================
analizar_superficie <- function(df, var_y, f1, f2, clustering, k_groups) {
  
  df_clean <- df %>% drop_na(all_of(c(var_y, f1, f2)))
  
  # Ajustar el modelo de superficie de respuesta
  formula_rsm <- as.formula(paste(var_y, "~", f1, "*", f2, "+ I(", f1, "^2) + I(", f2, "^2)"))
  modelo <- lm(formula_rsm, data = df_clean)
  
  resumen <- summary(modelo)
  r_cuadrado <- resumen$r.squared
  
  # Crear cuadrícula (Grid)
  rango_f1 <- range(df_clean[[f1]])
  rango_f2 <- range(df_clean[[f2]])
  
  grid_f1 <- seq(rango_f1[1], rango_f1[2], length.out = 100)
  grid_f2 <- seq(rango_f2[1], rango_f2[2], length.out = 100)
  
  grid_pred <- expand.grid(F1 = grid_f1, F2 = grid_f2)
  names(grid_pred) <- c(f1, f2)
  grid_pred$Prediccion_Z <- predict(modelo, newdata = grid_pred)
  
  # Óptimo General
  optimo_general <- grid_pred[which.max(grid_pred$Prediccion_Z), ]
  
  # Clustering (Opcional)
  optimos_clusters <- NULL
  if (clustering) {
    set.seed(123)
    factores_escala <- scale(df_clean[, c(f1, f2)])
    km_res <- kmeans(factores_escala, centers = k_groups)
    df_clean$Cluster <- as.factor(km_res$cluster)
    
    lista_optimos_locales <- list()
    for (i in 1:k_groups) {
      limites_cl <- df_clean %>% filter(Cluster == i) %>%
        summarise(
          min_f1 = min(!!sym(f1)), max_f1 = max(!!sym(f1)),
          min_f2 = min(!!sym(f2)), max_f2 = max(!!sym(f2))
        )
      
      grid_cl <- grid_pred %>% filter(
        !!sym(f1) >= limites_cl$min_f1 & !!sym(f1) <= limites_cl$max_f1 &
          !!sym(f2) >= limites_cl$min_f2 & !!sym(f2) <= limites_cl$max_f2
      )
      
      if(nrow(grid_cl) > 0) {
        opt_local <- grid_cl[which.max(grid_cl$Prediccion_Z), ]
        opt_local$Cluster <- paste("Grupo", i)
        lista_optimos_locales[[i]] <- opt_local
      }
    }
    optimos_clusters <- bind_rows(lista_optimos_locales)
  }
  
  return(list(
    Variable = var_y,
    Modelo = modelo, 
    Opt_Gen_F1 = optimo_general[[f1]],
    Opt_Gen_F2 = optimo_general[[f2]],
    Max_Gen_Z = optimo_general$Prediccion_Z,
    R_Cuadrado = r_cuadrado,
    Grid = grid_pred,
    Optimos_Locales = optimos_clusters
  ))
}

# ==============================================================================
# 3. EJECUCIÓN Y GRÁFICOS 2D (MAPAS DE CALOR)
# ==============================================================================
tabla_general <- list()
tabla_grupos <- list()
modelos_guardados <- list()

cat("\n====================================================================\n")
cat("ANÁLISIS DE SUPERFICIE DE RESPUESTA Y AGRUPAMIENTO\n")
cat("====================================================================\n")

for (var in variables_y) {
  
  res <- analizar_superficie(datos, var, factores_x[1], factores_x[2], usar_clustering, num_clusters)
  modelos_guardados[[var]] <- res$Modelo
  
  tabla_general[[var]] <- data.frame(
    Variable = var, Optimo_CCTon = res$Opt_Gen_F1, Optimo_Hidro_100 = res$Opt_Gen_F2,
    Valor_Maximo = res$Max_Gen_Z, R2 = round(res$R_Cuadrado, 4)
  )
  
  # Crear un data frame seguro para graficar el punto óptimo sin colapsar ggplot
  df_optimo_general <- data.frame(
    X_Opt = res$Opt_Gen_F1, 
    Y_Opt = res$Opt_Gen_F2,
    Label = paste0("ÓPTIMO\n(", round(res$Opt_Gen_F1, 2), ", ", round(res$Opt_Gen_F2, 2), ")")
  )
  
  # MAPA DE CALOR 2D REFORZADO (aes_string)
  g_2d <- ggplot(res$Grid, aes_string(x = factores_x[1], y = factores_x[2], z = "Prediccion_Z")) +
    geom_raster(aes_string(fill = "Prediccion_Z"), interpolate = TRUE) +
    geom_contour(color = "white", alpha = 0.5, bins = 15) +
    scale_fill_distiller(palette = "Spectral", name = paste("Max", var)) +
    
    # Capa del óptimo general usando el data frame seguro
    geom_point(data = df_optimo_general, aes(x = X_Opt, y = Y_Opt), fill = "white", color = "black", shape = 21, size = 5, stroke = 1.5, inherit.aes = FALSE) +
    geom_label(data = df_optimo_general, aes(x = X_Opt, y = Y_Opt, label = Label), vjust = -0.5, fontface = "bold", fill = alpha("white", 0.8), inherit.aes = FALSE)
  
  if (usar_clustering && !is.null(res$Optimos_Locales)) {
    g_2d <- g_2d + 
      geom_point(data = res$Optimos_Locales, aes_string(x = factores_x[1], y = factores_x[2]),
                 color = "black", shape = 4, size = 3, stroke = 1.2, inherit.aes = FALSE) +
      geom_text(data = res$Optimos_Locales, aes_string(x = factores_x[1], y = factores_x[2], label = "Cluster"),
                vjust = 2, size = 3, color = "black", fontface = "bold", inherit.aes = FALSE)
    
    res$Optimos_Locales$Variable <- var
    tabla_grupos[[var]] <- res$Optimos_Locales
  }
  
  g_2d <- g_2d + labs(title = paste("Superficie de Respuesta para", var), x = factores_x[1], y = factores_x[2]) + theme_minimal()
  print(g_2d)
}

df_general <- bind_rows(tabla_general)

# ==============================================================================
# 4. CURVAS DE PERFIL ESCALADAS (Normalización Min-Max)
# ==============================================================================
hidro_fijo <- mean(df_general$Optimo_Hidro_100)

seq_ccton <- seq(min(datos$CCTon, na.rm = TRUE), max(datos$CCTon, na.rm = TRUE), length.out = 200)
df_perfil <- data.frame(CCTon = seq_ccton, Hidro_100 = hidro_fijo)

# Escalar con Normalización Min-Max (0 a 1 estricto)
for(var in variables_y) {
  pred <- predict(modelos_guardados[[var]], newdata = df_perfil)
  df_perfil[[var]] <- (pred - min(pred)) / (max(pred) - min(pred))
}

df_perfil_long <- df_perfil %>%
  pivot_longer(cols = all_of(variables_y), names_to = "Variable", values_to = "Valor_Escalado")

optimos_escalados <- df_perfil_long %>%
  group_by(Variable) %>%
  filter(Valor_Escalado == max(Valor_Escalado)) %>%
  slice(1)

# GRÁFICO DE CURVAS
grafico_curvas <- ggplot(df_perfil_long, aes(x = CCTon, y = Valor_Escalado, color = Variable)) +
  geom_line(linewidth = 1.5) +
  geom_vline(data = optimos_escalados, aes(xintercept = CCTon, color = Variable), linetype = "dashed", linewidth = 0.8) +
  geom_point(data = optimos_escalados, aes(x = CCTon, y = Valor_Escalado), size = 4, shape = 8, stroke = 2) +
  geom_text(data = optimos_escalados, aes(x = CCTon, y = Valor_Escalado, label = round(CCTon, 2)),
            vjust = -1.5, show.legend = FALSE, size = 4, fontface = "bold") +
  labs(title = "Perfil de Equilibrio: Curvas Escaladas vs CCTon",
       subtitle = paste("Fijando Hidro_100 en su óptimo promedio (", round(hidro_fijo, 2), ") - El valor 1.0 representa el pico"),
       x = "CCTon", y = "Proporción del Rendimiento (0 a 1)") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_minimal() +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())

print(grafico_curvas)

# ==============================================================================
# 5. RESUMEN DE RESULTADOS EN CONSOLA
# ==============================================================================
cat("\n--- TABLA GENERAL DE PUNTOS ÓPTIMOS GLOBALES ---\n")
print(df_general)

if (usar_clustering) {
  cat("\n--- TABLA ESPECÍFICA: MEJORES PUNTOS POR GRUPO (CLÚSTER) ---\n")
  print(bind_rows(tabla_grupos) %>%
          dplyr::select(Variable, Cluster, CCTon, Hidro_100, Prediccion_Z) %>% # <-- Se agregó dplyr::
          dplyr::rename(Optimo_CCTon = CCTon, Optimo_Hidro = Hidro_100, Maximo_Esperado = Prediccion_Z) %>%
          dplyr::arrange(Variable, Cluster))
}



##############################################################################################
#  otros

# ==============================================================================
# 6. OPTIMIZACIÓN CONDICIONADA (Dosis específicas de CCTon: 2.5, 5.0, 7.5)
# ==============================================================================
cat("\n====================================================================\n")
cat("ANÁLISIS DE DOSIS ESPECÍFICAS DE CCTon PARA TAH_Rel\n")
cat("====================================================================\n")

# 1. Definir las dosis objetivo
dosis_objetivo <- c(3,4,5,6,7,8)  #2.5, 5.0, 7.5
var_interes <- "TAH_Rel"

# Extraer el modelo ya ajustado de tu lista de modelos guardados
modelo_tah <- modelos_guardados[[var_interes]]

# 2. Crear una secuencia muy fina para Hidro_100 basada en los datos reales
rango_hidro <- range(datos$Hidro_100, na.rm = TRUE)
secuencia_hidro <- seq(rango_hidro[1], rango_hidro[2], length.out = 1000)

# 3. Calcular el óptimo de Hidro_100 para cada dosis de CCTon
resultados_dosis <- data.frame()

for (dosis in dosis_objetivo) {
  # Crear un grid fijando la dosis actual de CCTon
  grid_temp <- data.frame(CCTon = dosis, Hidro_100 = secuencia_hidro)
  
  # Predecir TAH_Rel usando el modelo
  grid_temp$Prediccion_Z <- predict(modelo_tah, newdata = grid_temp)
  
  # Extraer la fila con la predicción máxima (Óptimo local para esta dosis)
  punto_maximo <- grid_temp[which.max(grid_temp$Prediccion_Z), ]
  punto_maximo$Etiqueta <- paste0("Dosis ", dosis)
  
  # Acumular resultados
  resultados_dosis <- bind_rows(resultados_dosis, punto_maximo)
}

# 4. Imprimir la tabla de resultados en consola
cat("\n--- ÓPTIMOS CONDICIONADOS PARA DOSIS ESPECÍFICAS ---\n")
tabla_imprimir <- resultados_dosis %>%
  rename(Dosis_CCTon = CCTon, 
         Optimo_Hidro_100 = Hidro_100, 
         Max_TAH_Rel_Esperado = Prediccion_Z) %>%
  mutate(across(where(is.numeric), round, 3)) # Redondear a 3 decimales para limpieza

print(tabla_imprimir)

# ==============================================================================
# 7. GRÁFICO INTEGRADO: SUPERFICIE + ÓPTIMO GLOBAL + ÓPTIMOS POR DOSIS
# ==============================================================================
# Reconstruir el grid general para repintar el mapa base
rango_cc <- range(datos$CCTon, na.rm = TRUE)
grid_general <- expand.grid(
  CCTon = seq(rango_cc[1], rango_cc[2], length.out = 150),
  Hidro_100 = seq(rango_hidro[1], rango_hidro[2], length.out = 150)
)
grid_general$Prediccion_Z <- predict(modelo_tah, newdata = grid_general)

# Obtener el óptimo global para graficarlo
optimo_global <- grid_general[which.max(grid_general$Prediccion_Z), ]

# Crear el gráfico
grafico_integrado <- ggplot(grid_general, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_distiller(palette = "Spectral", name = "TAH_Rel") +
  
  # 1. Capa del Óptimo Global (Círculo Blanco como en tu imagen original)
  geom_point(data = optimo_global, aes(x = CCTon, y = Hidro_100), 
             fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  
  # 2. Capa de las Dosis Específicas (Triángulos Negros)
  geom_vline(xintercept = dosis_objetivo, linetype = "dashed", color = "black", alpha = 0.4) +
  geom_point(data = resultados_dosis, aes(x = CCTon, y = Hidro_100), 
             fill = "black", color = "white", shape = 24, size = 4, stroke = 1, inherit.aes = FALSE) +
  
  # Etiquetas de los puntos específicos
  geom_label(data = resultados_dosis, 
             aes(x = CCTon, y = Hidro_100, 
                 label = paste0(Etiqueta, "\nHidro: ", round(Hidro_100, 1), "\nTAH: ", round(Prediccion_Z, 2))),
             vjust = 1.2, hjust = -0.1, size = 3, fontface = "bold", fill = alpha("white", 0.7), inherit.aes = FALSE) +
  
  labs(title = "Superficie de Respuesta Integrada: TAH_Rel",
       subtitle = "Comparativa de Óptimo Global (círculo) vs. Dosis Fijas (triángulos)",
       x = "CCTon",
       y = "Hidro_100") +
  theme_minimal() +
  coord_cartesian(expand = FALSE) # Para que los bordes ajusten perfectamente

# Imprimir gráfico
print(grafico_integrado)

##############################################################################################


# Asegúrate de instalar ggrepel ejecutando en la consola: install.packages("ggrepel")
# si es que no lo tienes instalado previamente.
library(ggrepel)

# ==============================================================================
# 6. OPTIMIZACIÓN CONDICIONADA (Nuevas Dosis de CCTon)
# ==============================================================================
cat("\n====================================================================\n")
cat("ANÁLISIS DE DOSIS ESPECÍFICAS DE CCTon PARA TAH_Rel\n")
cat("====================================================================\n")

# 1. Definir las nuevas dosis objetivo
dosis_objetivo <- c(3, 4, 5, 6, 7, 8)
var_interes <- "TAH_Rel"

# Extraer el modelo ya ajustado
modelo_tah <- modelos_guardados[[var_interes]]

# 2. Crear una secuencia muy fina para Hidro_100 basada en los datos reales
rango_hidro <- range(datos$Hidro_100, na.rm = TRUE)
secuencia_hidro <- seq(rango_hidro[1], rango_hidro[2], length.out = 1000)

# 3. Calcular el óptimo de Hidro_100 para cada dosis de CCTon
resultados_dosis <- data.frame()

for (dosis in dosis_objetivo) {
  # Crear un grid fijando la dosis actual de CCTon
  grid_temp <- data.frame(CCTon = dosis, Hidro_100 = secuencia_hidro)
  
  # Predecir TAH_Rel usando el modelo
  grid_temp$Prediccion_Z <- predict(modelo_tah, newdata = grid_temp)
  
  # Extraer la fila con la predicción máxima (Óptimo local para esta dosis)
  punto_maximo <- grid_temp[which.max(grid_temp$Prediccion_Z), ]
  punto_maximo$Etiqueta <- paste0("Dosis ", dosis)
  
  # Acumular resultados
  resultados_dosis <- bind_rows(resultados_dosis, punto_maximo)
}

# Imprimir la tabla de resultados en consola
cat("\n--- ÓPTIMOS CONDICIONADOS PARA DOSIS ESPECÍFICAS ---\n")
print(resultados_dosis %>%
        rename(Dosis_CCTon = CCTon, Optimo_Hidro = Hidro_100, Max_TAH = Prediccion_Z) %>%
        select(-Etiqueta) %>%
        mutate(across(where(is.numeric), round, 3)))

# ==============================================================================
# 7. GRÁFICO INTEGRADO (Eje Y Estratificado y Etiquetas Corregidas)
# ==============================================================================
# Reconstruir el grid general para repintar el mapa base
rango_cc <- range(datos$CCTon, na.rm = TRUE)
grid_general <- expand.grid(
  CCTon = seq(rango_cc[1], rango_cc[2], length.out = 150),
  Hidro_100 = seq(rango_hidro[1], rango_hidro[2], length.out = 150)
)
grid_general$Prediccion_Z <- predict(modelo_tah, newdata = grid_general)

# Obtener el óptimo global para graficarlo
optimo_global <- grid_general[which.max(grid_general$Prediccion_Z), ]

# Crear el gráfico
grafico_integrado <- ggplot(grid_general, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_distiller(palette = "Spectral", name = "TAH_Rel") +
  
  # 1. Capa del Óptimo Global (Círculo Blanco)
  geom_point(data = optimo_global, aes(x = CCTon, y = Hidro_100), 
             fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  
  # 2. Capa de las Dosis Específicas (Líneas y Triángulos)
  geom_vline(xintercept = dosis_objetivo, linetype = "dashed", color = "black", alpha = 0.4) +
  geom_point(data = resultados_dosis, aes(x = CCTon, y = Hidro_100), 
             fill = "black", color = "white", shape = 24, size = 4, stroke = 1, inherit.aes = FALSE) +
  
  # 3. Etiquetas Inteligentes (ggrepel evita que se amontonen o se corten)
  geom_label_repel(data = resultados_dosis, 
                   aes(x = CCTon, y = Hidro_100, 
                       label = paste0("Dosis ", CCTon, "\nH: ", round(Hidro_100, 1), "\nTAH: ", round(Prediccion_Z, 2))),
                   box.padding = 0.5, 
                   point.padding = 0.3,
                   nudge_y = -3, # Desplaza las etiquetas hacia abajo para alejarlas del borde superior
                   direction = "y", 
                   size = 3, fontface = "bold", fill = alpha("white", 0.8), inherit.aes = FALSE) +
  
  # 4. ESTRATIFICACIÓN DE EJES Y EXPANSIÓN DE MÁRGENES
  # Forza la escala Y de 10 en 10. El rango 0-200 asegura que cubra cualquier dataset automáticamente.
  # expand = expansion(...) le da un 15% de espacio extra "en blanco" al tope para las etiquetas.
  scale_y_continuous(breaks = seq(0, 200, by = 10),
                     expand = expansion(mult = c(0, 0.15))) +
  scale_x_continuous(breaks = dosis_objetivo) +
  
  labs(title = "Superficie de Respuesta Integrada: TAH_Rel",
       subtitle = "Óptimos Condicionados vs Óptimo Global (Escala Hidro_100 por decenas)",
       x = "CCTon",
       y = "Hidro_100") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank()) # Limpia las líneas guía menores para no saturar

# Imprimir gráfico
print(grafico_integrado)


###############333
######################################

# ==============================================================================
# 8. OPTIMIZACIÓN CONDICIONADA INVERSA (Humedades fijas: 60, 70, 80, 90)
# ==============================================================================
cat("\n====================================================================\n")
cat("ANÁLISIS DE DOSIS ÓPTIMA (CCTon) PARA HUMEDADES ESPECÍFICAS\n")
cat("====================================================================\n")

# 1. Definir las humedades objetivo (Eje Y)
humedad_objetivo <- c(65, 70,75, 80,85, 90)
var_interes <- "TAH_Rel"

# Extraer el modelo ya ajustado (asegurar que exista en la memoria)
modelo_tah <- modelos_guardados[[var_interes]]

# 2. Crear una secuencia muy fina para CCTon (Eje X) basada en los datos reales
rango_cc <- range(datos$CCTon, na.rm = TRUE)
secuencia_cc <- seq(rango_cc[1], rango_cc[2], length.out = 1000)

# 3. Calcular el óptimo de CCTon para cada nivel de Hidro_100
resultados_humedad <- data.frame()

for (humedad in humedad_objetivo) {
  # Crear un grid fijando la humedad actual
  grid_temp <- data.frame(CCTon = secuencia_cc, Hidro_100 = humedad)
  
  # Predecir TAH_Rel usando el modelo
  grid_temp$Prediccion_Z <- predict(modelo_tah, newdata = grid_temp)
  
  # Extraer la fila con la predicción máxima (Dosis óptima local para esta humedad)
  punto_maximo <- grid_temp[which.max(grid_temp$Prediccion_Z), ]
  punto_maximo$Etiqueta <- paste0("Humedad ", humedad)
  
  # Acumular resultados
  resultados_humedad <- bind_rows(resultados_humedad, punto_maximo)
}

# 4. Imprimir la tabla de resultados en consola
cat("\n--- DOSIS ÓPTIMAS (CCTon) PARA HUMEDADES ESPECÍFICAS ---\n")
tabla_humedad_imprimir <- resultados_humedad %>%
  rename(Humedad_Fija = Hidro_100, 
         Optimo_CCTon = CCTon, 
         Max_TAH_Rel_Esperado = Prediccion_Z) %>%
  select(Etiqueta, Humedad_Fija, Optimo_CCTon, Max_TAH_Rel_Esperado) %>%
  mutate(across(where(is.numeric), round, 3))

print(tabla_humedad_imprimir)

# ==============================================================================
# 9. GRÁFICO INTEGRADO: SUPERFICIE + ÓPTIMOS POR HUMEDAD
# ==============================================================================
# Reconstruir el grid general (por si acaso no está en memoria)
rango_hidro <- range(datos$Hidro_100, na.rm = TRUE)
grid_general <- expand.grid(
  CCTon = seq(rango_cc[1], rango_cc[2], length.out = 150),
  Hidro_100 = seq(rango_hidro[1], rango_hidro[2], length.out = 150)
)
grid_general$Prediccion_Z <- predict(modelo_tah, newdata = grid_general)

# Obtener el óptimo global para referencia
optimo_global <- grid_general[which.max(grid_general$Prediccion_Z), ]

# Crear el gráfico con cortes horizontales
grafico_humedad <- ggplot(grid_general, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_distiller(palette = "Spectral", name = "TAH_Rel") +
  
  # 1. Capa del Óptimo Global (Círculo Blanco)
  geom_point(data = optimo_global, aes(x = CCTon, y = Hidro_100), 
             fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  
  # 2. Capa de las Humedades Específicas (Líneas horizontales y Cuadrados Negros)
  geom_hline(yintercept = humedad_objetivo, linetype = "dashed", color = "black", alpha = 0.5) +
  geom_point(data = resultados_humedad, aes(x = CCTon, y = Hidro_100), 
             fill = "black", color = "white", shape = 22, size = 4, stroke = 1, inherit.aes = FALSE) +
  
  # 3. Etiquetas Inteligentes (Evitando traslapes en el eje X)
  geom_label_repel(data = resultados_humedad, 
                   aes(x = CCTon, y = Hidro_100, 
                       label = paste0("Humedad: ", Hidro_100, "\nDosis Ópt: ", round(CCTon, 2), "\nTAH: ", round(Prediccion_Z, 2))),
                   box.padding = 0.8, 
                   point.padding = 0.5,
                   nudge_x = 0.5, # Desplaza las etiquetas hacia la derecha
                   direction = "x", # Fuerza a que se muevan horizontalmente
                   size = 3.5, fontface = "bold", fill = alpha("white", 0.8), inherit.aes = FALSE) +
  
  # 4. Estratificación del Eje Y estricta de 10 en 10
  scale_y_continuous(breaks = seq(0, 200, by = 10)) +
  
  labs(title = "Optimización de Dosis (CCTon) condicionada por Humedad",
       subtitle = "Dosis ideal para alcanzar el máximo TAH_Rel en humedades de 60, 70, 80 y 90",
       x = "CCTon",
       y = "Hidro_100") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())

# Imprimir gráfico
print(grafico_humedad)


######################################################################################################################

#######################################################################################################################

# ==============================================================================
# 10. GRÁFICO MAESTRO INTEGRADO (Dosis Fijas + Humedades Fijas)
# ==============================================================================
cat("\n====================================================================\n")
cat("ANÁLISIS MAESTRO: DOSIS FIJAS Y HUMEDADES FIJAS INTEGRADOS\n")
cat("====================================================================\n")

# Asegurar que ggrepel esté cargado
library(ggrepel)

var_interes <- "TAH_Rel"
modelo_tah <- modelos_guardados[[var_interes]]

# --- A. CÁLCULOS PARA DOSIS FIJAS (Líneas Verticales) ---
dosis_objetivo <- c(3, 4, 5, 6, 7, 8)
rango_hidro <- range(datos$Hidro_100, na.rm = TRUE)
secuencia_hidro <- seq(rango_hidro[1], rango_hidro[2], length.out = 1000)

resultados_dosis <- data.frame()
for (dosis in dosis_objetivo) {
  grid_temp <- data.frame(CCTon = dosis, Hidro_100 = secuencia_hidro)
  grid_temp$Prediccion_Z <- predict(modelo_tah, newdata = grid_temp)
  punto_maximo <- grid_temp[which.max(grid_temp$Prediccion_Z), ]
  resultados_dosis <- bind_rows(resultados_dosis, punto_maximo)
}

# --- B. CÁLCULOS PARA HUMEDADES FIJAS (Líneas Horizontales) ---
#humedad_objetivo <- c(60,65, 70,75, 80,85, 90)
humedad_objetivo <- seq(60, 90, by = 2) 

rango_cc <- range(datos$CCTon, na.rm = TRUE)
secuencia_cc <- seq(rango_cc[1], rango_cc[2], length.out = 1000)

resultados_humedad <- data.frame()
for (humedad in humedad_objetivo) {
  grid_temp <- data.frame(CCTon = secuencia_cc, Hidro_100 = humedad)
  grid_temp$Prediccion_Z <- predict(modelo_tah, newdata = grid_temp)
  punto_maximo <- grid_temp[which.max(grid_temp$Prediccion_Z), ]
  resultados_humedad <- bind_rows(resultados_humedad, punto_maximo)
}

# --- C. PREPARAR GRID GENERAL Y ÓPTIMO GLOBAL ---
# Aumentamos la resolución del grid (200x200) para un mapa de calor más suave
grid_general <- expand.grid(
  CCTon = seq(rango_cc[1], rango_cc[2], length.out = 200),
  Hidro_100 = seq(rango_hidro[1], rango_hidro[2], length.out = 200)
)
grid_general$Prediccion_Z <- predict(modelo_tah, newdata = grid_general)
optimo_global <- grid_general[which.max(grid_general$Prediccion_Z), ]

# --- D. CONSTRUCCIÓN DEL GRÁFICO MAESTRO ---
grafico_maestro <- ggplot(grid_general, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  
  # 1. Capas Base (Mapa de Calor y Contornos)
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  geom_contour(color = "white", alpha = 0.5, bins = 15) +
  scale_fill_distiller(palette = "Spectral", name = "TAH_Rel") +
  
  # 2. Líneas Guía de la Cuadrícula Estratégica
  geom_vline(xintercept = dosis_objetivo, linetype = "dotted", color = "black", alpha = 0.4) +
  geom_hline(yintercept = humedad_objetivo, linetype = "dotted", color = "black", alpha = 0.4) +
  
  # 3. Puntos de los Óptimos Locales y Global
  # Triángulos para Dosis
  geom_point(data = resultados_dosis, aes(x = CCTon, y = Hidro_100), 
             fill = "black", color = "white", shape = 24, size = 4, stroke = 1, inherit.aes = FALSE) +
  # Cuadrados para Humedad
  geom_point(data = resultados_humedad, aes(x = CCTon, y = Hidro_100), 
             fill = "black", color = "white", shape = 22, size = 4, stroke = 1, inherit.aes = FALSE) +
  # Círculo para Global
  geom_point(data = optimo_global, aes(x = CCTon, y = Hidro_100), 
             fill = "white", color = "black", shape = 21, size = 6, stroke = 1.5, inherit.aes = FALSE) +
  
  # 4. Etiquetas Inteligentes (Separadas por color y dirección para evitar el caos)
  # Etiquetas de Dosis (Azules, forzadas hacia el eje Y/arriba)
  geom_label_repel(data = resultados_dosis, 
                   aes(x = CCTon, y = Hidro_100, 
                       label = paste0("D: ", CCTon, "\nH: ", round(Hidro_100, 1), "\nTAH: ", round(Prediccion_Z, 2))),
                   box.padding = 0.4, point.padding = 0.3, nudge_y = 2, direction = "y", 
                   size = 2.8, fontface = "bold", fill = alpha("white", 0.9), color = "#003366", inherit.aes = FALSE) +
  
  # Etiquetas de Humedad (Rojas, forzadas hacia el eje X/lados)
  geom_label_repel(data = resultados_humedad, 
                   aes(x = CCTon, y = Hidro_100, 
                       label = paste0("H: ", Hidro_100, "\nD: ", round(CCTon, 2), "\nTAH: ", round(Prediccion_Z, 2))),
                   box.padding = 0.4, point.padding = 0.3, nudge_x = 0.8, direction = "x", 
                   size = 2.8, fontface = "bold", fill = alpha("white", 0.9), color = "#800000", inherit.aes = FALSE) +
  
  # 5. Configuración estricta de Ejes (10 en 10 para Y, 1 en 1 para X) y expansión de márgenes
  scale_y_continuous(breaks = seq(0, 200, by = 10), expand = expansion(mult = c(0.05, 0.15))) +
  scale_x_continuous(breaks = seq(0, 20, by = 1), expand = expansion(mult = c(0.05, 0.15))) +
  
  labs(title = "Matriz de Decisiones Agronómicas: Optimización de TAH_Rel",
       subtitle = "Óptimos por Dosis (Triángulos/Azul), por Humedad (Cuadrados/Rojo) y Global (Círculo)",
       x = "CCTon",
       y = "Hidro_100") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

# Imprimir el súper gráfico
print(grafico_maestro)



########################################################################################################

# ==============================================================================
# 11. AGRUPAMIENTO POR CENTROIDES Y EXTRACCIÓN DE RANGOS ÓPTIMOS (EXCEL)
# ==============================================================================
cat("\n====================================================================\n")
cat("EXTRACCIÓN DE RANGOS OPERACIONALES POR CLÚSTER Y EXPORTACIÓN A EXCEL\n")
cat("====================================================================\n")

# Cargar librería para exportar a Excel
library(writexl)

# --- 1. Consolidar todos los puntos óptimos como Centroides ---
centroides_df <- bind_rows(
  resultados_dosis %>% mutate(Origen = paste("Estrategia: Dosis Fija en", CCTon)),
  resultados_humedad %>% mutate(Origen = paste("Estrategia: Humedad Fija en", Hidro_100))
) %>%
  select(Origen, CCTon, Hidro_100, Prediccion_Z) %>%
  # Eliminar duplicados exactos por si coinciden cálculos
  distinct(CCTon, Hidro_100, .keep_all = TRUE) 

# Convertir a matriz para el algoritmo
matriz_centros <- as.matrix(centroides_df[, c("CCTon", "Hidro_100")])

# --- 2. Clustering (K-Means) basado en los centroides ---
# Escalar los datos es vital para que el agrupamiento no se sesgue 
# porque la Humedad (60-90) tiene números más grandes que la Dosis (0-10)
grid_scaled <- scale(grid_general[, c("CCTon", "Hidro_100")])
centros_scaled <- scale(matriz_centros, 
                        center = attr(grid_scaled, "scaled:center"), 
                        scale = attr(grid_scaled, "scaled:scale"))

# Agrupar el grid completo asignando cada punto a su centroide más cercano
set.seed(123)
km_res <- kmeans(grid_scaled, centers = centros_scaled)
grid_general$Cluster <- km_res$cluster

# --- 3. Extraer las Ventanas de Aplicación (Rangos) ---
tabla_rangos <- grid_general %>%
  group_by(Cluster) %>%
  # Filtrar el "Top 5%" de TAH dentro de cada grupo para definir un rango tolerante pero óptimo
  filter(Prediccion_Z >= quantile(Prediccion_Z, 0.95)) %>%
  summarise(
    Rango_Recomendado_Dosis = paste(round(min(CCTon), 2), "a", round(max(CCTon), 2)),
    Rango_Recomendado_Humedad = paste(round(min(Hidro_100), 2), "a", round(max(Hidro_100), 2)),
    TAH_Promedio_del_Rango = round(mean(Prediccion_Z), 3)
  ) %>%
  # Cruzar la información con el centroide original
  mutate(
    Estrategia_Base = centroides_df$Origen[Cluster],
    Centroide_Dosis = round(centroides_df$CCTon[Cluster], 2),
    Centroide_Humedad = round(centroides_df$Hidro_100[Cluster], 2),
    TAH_Pico_Centroide = round(centroides_df$Prediccion_Z[Cluster], 3)
  ) %>%
  # Ordenar columnas para el Excel y ordenar filas de mayor a menor TAH
  select(
    Cluster, Estrategia_Base, 
    Centroide_Dosis, Centroide_Humedad, TAH_Pico_Centroide, 
    Rango_Recomendado_Dosis, Rango_Recomendado_Humedad, TAH_Promedio_del_Rango
  ) %>%
  arrange(desc(TAH_Pico_Centroide))

# Mostrar resumen en la consola
cat("\n--- RANGOS DE APLICACIÓN IDENTIFICADOS ---\n")
View(tabla_rangos %>% select(Estrategia_Base, Rango_Recomendado_Dosis, Rango_Recomendado_Humedad, TAH_Promedio_del_Rango))

# --- 4. Exportar a Excel ---
nombre_archivo <- "Matriz_Rangos_Optimos_TAH.xlsx"
write_xlsx(tabla_rangos, nombre_archivo)

cat(paste("\n¡Éxito! La tabla completa ha sido exportada como:", nombre_archivo, "en tu directorio de trabajo actual.\n"))


######################################################################################################3
########################################################################################################

# ==============================================================================
# 12. MAPA DE CALOR OPERATIVO CON LÍMITES BASADOS EN K-MEANS
# ==============================================================================
cat("\n====================================================================\n")
cat("GENERACIÓN DE ZONAS OPERATIVAS (K-MEANS) Y EXPORTACIÓN\n")
cat("====================================================================\n")

# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(writexl)
#install.packages("metR")
library(metR) # Fundamental para geom_text_contour (etiquetas en las líneas)

var_interes <- "TAH_Rel"
modelo_tah <- modelos_guardados[[var_interes]]

# --- 1. Calcular el camino óptimo continuo (Dosis ideal para cada Humedad) ---
rango_hidro_real <- range(datos$Hidro_100, na.rm = TRUE)
rango_cc_real <- range(datos$CCTon, na.rm = TRUE)

# Evaluamos la humedad de medio en medio punto para tener gran resolución
secuencia_h <- seq(floor(rango_hidro_real[1]), ceiling(rango_hidro_real[2]), by = 0.5)
camino_optimo <- data.frame()

for(h in secuencia_h) {
  grid_temp <- data.frame(CCTon = seq(0, 15, length.out = 300), Hidro_100 = h) # Dosis hasta 15 para asegurar que capte el pico
  grid_temp$Pred <- predict(modelo_tah, newdata = grid_temp)
  punto_max <- grid_temp[which.max(grid_temp$Pred), ]
  camino_optimo <- bind_rows(camino_optimo, punto_max)
}

# --- 2. Algoritmo K-Means para definir 3 Zonas Operativas (R1, R2, R3) ---
set.seed(42) # Semilla para reproducibilidad
# Escalamos las variables para que Humedad, Dosis y TAH pesen lo mismo en el clustering
datos_escalados <- scale(camino_optimo[, c("CCTon", "Hidro_100", "Pred")])
km_zonas <- kmeans(datos_escalados, centers = 3)
camino_optimo$Cluster <- as.factor(km_zonas$cluster)

# Resumir los clusters y ordenarlos de menor a mayor Humedad para nombrar R1, R2, R3
resumen_zonas <- camino_optimo %>%
  group_by(Cluster) %>%
  summarise(
    Min_H = min(Hidro_100),
    Max_H = max(Hidro_100),
    Media_H = mean(Hidro_100),
    Media_Dosis = mean(CCTon),
    Max_TAH_Promedio = mean(Pred)
  ) %>%
  arrange(Media_H) %>%
  mutate(Zona = c("R1", "R2", "R3"))

# Unir el nombre de la zona de vuelta al camino óptimo
camino_optimo <- camino_optimo %>% left_join(resumen_zonas %>% select(Cluster, Zona), by = "Cluster")

# --- 3. Calcular los Umbrales (Líneas de corte entre zonas) ---
# El corte se define como el punto medio entre el máximo de una zona y el mínimo de la siguiente
corte_R1_R2 <- mean(c(resumen_zonas$Max_H[1], resumen_zonas$Min_H[2]))
corte_R2_R3 <- mean(c(resumen_zonas$Max_H[2], resumen_zonas$Min_H[3]))

# Crear dataframe para las etiquetas de texto en el gráfico
etiquetas_zonas <- data.frame(
  X = 3, # Posición X (Dosis) donde se escribirá el texto
  Y = c(resumen_zonas$Media_H[1] - 1, resumen_zonas$Media_H[2], resumen_zonas$Media_H[3] + 1),
  Label = c(
    sprintf("R1: HUMEDAD < %.1f%% • APLICAR %.1f cc/ton", corte_R1_R2, resumen_zonas$Media_Dosis[1]),
    sprintf("R2: HUMEDAD %.1f%% A %.1f%% • APLICAR %.1f cc/ton (MÁX RETORNO)", corte_R1_R2, corte_R2_R3, resumen_zonas$Media_Dosis[2]),
    sprintf("R3: HUMEDAD >= %.1f%% • APLICAR %.1f cc/ton (ENFOQUE CALIDAD)", corte_R2_R3, resumen_zonas$Media_Dosis[3])
  ),
  Color = c("#990000", "#004d00", "#003366")
)

# --- 4. Preparar el Grid completo para el Heatmap (Fondo) ---
grid_heatmap <- expand.grid(
  CCTon = seq(0, 10, length.out = 200),
  Hidro_100 = seq(60, 90, length.out = 200)
)
grid_heatmap$Prediccion_Z <- predict(modelo_tah, newdata = grid_heatmap)

# --- 5. Construcción del Gráfico Estilo "Semáforo Agronómico" ---
grafico_operativo <- ggplot(grid_heatmap, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  
  # Mapa de calor base con paleta Rojo-Amarillo-Verde
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "Respuesta de Azúcar Neto\n(TAH Relativo)"
  ) +
  
  # Líneas de contorno finas con texto integrado (requiere metR)
  geom_contour(color = "black", alpha = 0.5, bins = 20, linewidth = 0.3) +
  geom_text_contour(stroke = 0.15, size = 3, skip = 1, check_overlap = TRUE) +
  
  # Puntos de datos reales (semi-transparentes para no saturar)
  geom_point(data = datos, aes(x = CCTon, y = Hidro_100), 
             color = "black", alpha = 0.2, size = 1.5, inherit.aes = FALSE) +
  
  # Líneas de límite K-Means (Isolíneas operativas horizontales)
  geom_hline(yintercept = corte_R1_R2, linetype = "dashed", color = "#cc0000", linewidth = 0.8) +
  geom_hline(yintercept = corte_R2_R3, linetype = "dashed", color = "#0066cc", linewidth = 0.8) +
  
  # Textos descriptivos de las zonas
  geom_text(data = etiquetas_zonas, aes(x = X, y = Y, label = Label), 
            color = etiquetas_zonas$Color, fontface = "bold", size = 3.5, hjust = 0, inherit.aes = FALSE) +
  
  # Ajustes de ejes y títulos (Ejes 0-10 para X, 60-90 para Y, según tu imagen)
  scale_x_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(60, 90, by = 5), limits = c(59, 90), expand = c(0, 0)) +
  
  labs(
    title = "3. Mapa de Calor de Superficie de Respuesta con Zonas K-Means",
    subtitle = "Zonas operativas (R1, R2, R3) definidas automáticamente por clústeres de rendimiento",
    x = "Dosis de Madurante en Campo (CCTon - cc/ton)",
    y = "Índice de Humedad Interna de la Caña (Hidro_100)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#003366", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(angle = 90, hjust = 0.5, face = "bold"),
    legend.key.height = unit(3, "cm"),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 1)
  )

print(grafico_operativo)

# --- 6. Formatear y Exportar la Tabla de Mejores Combinaciones por Zona ---
tabla_exportacion <- camino_optimo %>%
  # Limpiar la tabla para entregar las mejores recetas por cada medio punto de humedad
  select(Zona, Humedad_Cana = Hidro_100, Dosis_Recomendada_cc_ton = CCTon, TAH_Esperado = Pred) %>%
  mutate(
    Dosis_Recomendada_cc_ton = round(Dosis_Recomendada_cc_ton, 2),
    TAH_Esperado = round(TAH_Esperado, 3)
  ) %>%
  arrange(Humedad_Cana)

# Mostrar un resumen en consola
cat("\n--- RESUMEN DE ZONAS Y LÍMITES K-MEANS ---\n")
print(resumen_zonas %>% 
        select(Zona, Min_H, Max_H, Media_Dosis) %>%
        mutate(across(where(is.numeric), round, 2)))

cat(sprintf("\n-> LÍMITE INFERIOR (Corte R1 a R2): %.2f%% de Humedad\n", corte_R1_R2))
cat(sprintf("-> LÍMITE SUPERIOR (Corte R2 a R3): %.2f%% de Humedad\n", corte_R2_R3))

# Exportar el detalle exacto a Excel
archivo_k_means <- "Receta_Operativa_por_Zonas_KMeans.xlsx"
write_xlsx(list("Recetas_Optimas" = tabla_exportacion, "Resumen_Zonas" = resumen_zonas), archivo_k_means)

cat(paste("\n¡Éxito! El detalle de las combinaciones agrupadas se ha exportado como:", archivo_k_means, "\n"))



#######################################################################################3
###########################################################################################

# ==============================================================================
# 13. MAPA DE CALOR CON VECTORES DE INFLUENCIA Y ETIQUETAS DE ISOLÍNEAS
# ==============================================================================
cat("\n====================================================================\n")
cat("GENERACIÓN DE GRÁFICO CON ZONAS DE INFLUENCIA Y ETIQUETAS LIMPIAS\n")
cat("====================================================================\n")

library(ggplot2)
library(dplyr)
library(metR) # Requerido para geom_text_contour

# 1. Definir los puntos donde irán las flechas (Zonas de influencia)
# Usamos las dosis medias óptimas de cada zona K-Means, y agregamos un punto extra 
# en el cuartil superior para representar la zona de máxima dosis.
dosis_flechas <- c(resumen_zonas$Media_Dosis, quantile(camino_optimo$CCTon, 0.90))

# DataFrame para las flechas
df_flechas <- data.frame(
  X = dosis_flechas,
  Y_inicio = 60, 
  Y_fin = 90
)

# 2. Construcción del Gráfico Actualizado
grafico_influencia <- ggplot(grid_heatmap, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  
  # Mapa de calor base
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "Respuesta de Azúcar Neto\n(TAH Relativo)"
  ) +
  
  # Isolíneas con etiquetas limpias (Redondeadas a 2 decimales usando after_stat)
  geom_contour(color = "black", alpha = 0.4, bins = 20, linewidth = 0.3) +
  geom_text_contour(aes(label = sprintf("%.2f", after_stat(level))),
                    stroke = 0.2, size = 3.5, skip = 1, check_overlap = TRUE, fontface = "bold") +
  
  # Puntos de datos reales
  geom_point(data = datos, aes(x = CCTon, y = Hidro_100), 
             color = "black", alpha = 0.2, size = 1.5, inherit.aes = FALSE) +
  
  # Flechas verticales naranjas (Zonas de influencia)
  geom_segment(data = df_flechas, aes(x = X, xend = X, y = Y_inicio, yend = Y_fin),
               arrow = arrow(length = unit(0.4, "cm"), type = "closed"),
               color = "#E56B24", linewidth = 1.2, alpha = 0.9, inherit.aes = FALSE) +
  
  # Líneas de límite K-Means (Horizontales)
  geom_hline(yintercept = corte_R1_R2, linetype = "dashed", color = "#cc0000", linewidth = 0.8) +
  geom_hline(yintercept = corte_R2_R3, linetype = "dashed", color = "#0066cc", linewidth = 0.8) +
  
  # Textos descriptivos de las zonas
  geom_text(data = etiquetas_zonas, aes(x = X, y = Y, label = Label), 
            color = etiquetas_zonas$Color, fontface = "bold", size = 3.5, hjust = 0, inherit.aes = FALSE) +
  
  # Ajustes de ejes
  scale_x_continuous(breaks = seq(0, 10, by = 2), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(60, 90, by = 5), limits = c(59, 90), expand = c(0, 0)) +
  
  labs(
    title = "3. Mapa de Calor de Superficie de Respuesta con Zonas K-Means",
    subtitle = "Zonas operativas y Vectores de Influencia que maximizan el TAH_Rel",
    x = "Dosis de Madurante en Campo (CCTon - cc/ton)",
    y = "Índice de Humedad Interna de la Caña (Hidro_100)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, color = "#003366", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 15)),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.title = element_text(angle = 90, hjust = 0.5, face = "bold"),
    legend.key.height = unit(3, "cm"),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 1)
  )

# Imprimir gráfico
print(grafico_influencia)


###########################################################################################
#############################################################################################

# ==============================================================================
# 14. MAPA DE CUADRANTES K-MEANS BIVARIADO E ISOLÍNEAS 0.05
# ==============================================================================
cat("\n====================================================================\n")
cat("GENERACIÓN DE CUADRANTES K-MEANS (MATRIZ) E INTERVALOS DE 0.05\n")
cat("====================================================================\n")

# Cargar librerías
library(ggplot2)
library(dplyr)
library(tidyr)
library(metR)
library(writexl)

var_interes <- "TAH_Rel"
modelo_tah <- modelos_guardados[[var_interes]]

# --- 1. K-MEANS INDEPENDIENTE PARA HUMEDAD Y DOSIS ---
# Usamos los datos reales para entender cómo se agrupan naturalmente las 
# aplicaciones y condiciones del campo.

set.seed(123) # Semilla para reproducibilidad

# Función auxiliar para encontrar cortes de K-Means (1D)
calcular_cortes_kmeans <- function(variable, k = 4) {
  km <- kmeans(na.omit(variable), centers = k)
  df <- data.frame(val = na.omit(variable), cluster = km$cluster)
  resumen <- df %>% group_by(cluster) %>%
    summarise(min = min(val), max = max(val), centro = mean(val)) %>%
    arrange(centro)
  
  # El corte operativo es el punto medio entre el máximo de un grupo y el mínimo del siguiente
  cortes <- c()
  for(i in 1:(k-1)) {
    cortes <- c(cortes, mean(c(resumen$max[i], resumen$min[i+1])))
  }
  return(list(resumen = resumen, cortes = cortes))
}

# Ejecutar K-Means (3 grupos de Humedad, 3 grupos de Dosis)
km_humedad <- calcular_cortes_kmeans(datos$Hidro_100, k = 10)
km_dosis   <- calcular_cortes_kmeans(datos$CCTon, k = 10)

cortes_h <- km_humedad$cortes
cortes_d <- km_dosis$cortes

cat(sprintf("=> Límites de Humedad (K-Means): %.2f y %.2f\n", cortes_h[1], cortes_h[2]))
cat(sprintf("=> Límites de Dosis (K-Means): %.2f y %.2f\n", cortes_d[1], cortes_d[2]))

# --- 2. CONFIGURAR EL GRID Y LAS ISOLÍNEAS (0.05) ---
grid_heatmap <- expand.grid(
  CCTon = seq(0, 10, length.out = 250),
  Hidro_100 = seq(60, 90, length.out = 250)
)
grid_heatmap$Prediccion_Z <- predict(modelo_tah, newdata = grid_heatmap)

# Vector estricto de secuencias de 0.05 para TAH (cubriendo cualquier rango posible)
intervalos_isolineas <- seq(1, 1.10, by = 0.005)

# --- 3. CONSTRUCCIÓN DEL GRÁFICO DE CUADRANTES ---
grafico_cuadrantes <- ggplot(grid_heatmap, aes(x = CCTon, y = Hidro_100, z = Prediccion_Z)) +
  
  # Mapa de Calor base
  geom_raster(aes(fill = Prediccion_Z), interpolate = TRUE) +
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "TAH Relativo"
  ) +
  
  # Isolíneas forzadas estrictamente a saltos de 0.05
  geom_contour(color = "black", alpha = 0.5, breaks = intervalos_isolineas, linewidth = 0.4) +
  geom_text_contour(breaks = intervalos_isolineas, 
                    aes(label = sprintf("%.2f", after_stat(level))),
                    stroke = 0.15, size = 3.5, skip = 0, fontface = "bold") +
  
  # Puntos reales
  geom_point(data = datos, aes(x = CCTon, y = Hidro_100), 
             color = "black", alpha = 0.3, size = 1, inherit.aes = FALSE) +
  
  # LÍNEAS DE CUADRÍCULA K-MEANS
  # Cortes de Humedad (Horizontales - Rojas)
  geom_hline(yintercept = cortes_h, linetype = "twodash", color = "#cc0000", linewidth = 1) +
  # Cortes de Dosis (Verticales - Azules)
  geom_vline(xintercept = cortes_d, linetype = "twodash", color = "#003366", linewidth = 1) +
  
  # Ajustes de escalas
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(60, 90, by = 5), limits = c(59, 90), expand = c(0, 0)) +
  
  labs(
    title = "Matriz Operativa de Superficie de Respuesta (K-Means Bivariado)",
    subtitle = paste("Cuadrantes formados por clústeres de Humedad y Dosis. Isolíneas cada 0.05 TAH"),
    x = "Dosis de Madurante en Campo (CCTon)",
    y = "Índice de Humedad Interna (Hidro_100)"
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, margin = margin(b = 10)),
    axis.title = element_text(face = "bold"),
    legend.position = "right",
    legend.key.height = unit(2.5, "cm"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(grafico_cuadrantes)


# --- 4. EXTRACCIÓN DE ÓPTIMOS POR CUADRANTE Y EXCEL ---
# Añadimos límites ficticios en los extremos (0 a infinito) para clasificar el grid
limites_h <- c(0, cortes_h, 100)
limites_d <- c(0, cortes_d, 100)

nombres_h <- c("Baja", "Media", "Alta")
nombres_d <- c("Baja", "Media", "Alta")

# Clasificar cada punto del grid en su cuadrante
grid_heatmap_clasificado <- grid_heatmap %>%
  mutate(
    Grupo_Humedad = cut(Hidro_100, breaks = limites_h, labels = nombres_h, include.lowest = TRUE),
    Grupo_Dosis   = cut(CCTon, breaks = limites_d, labels = nombres_d, include.lowest = TRUE)
  )

# Encontrar el TAH máximo dentro de CADA UNO de los 9 cuadrantes
tabla_cuadrantes <- grid_heatmap_clasificado %>%
  group_by(Grupo_Humedad, Grupo_Dosis) %>%
  filter(Prediccion_Z == max(Prediccion_Z)) %>%
  slice(1) %>% # Por si hay empates exactos
  ungroup() %>%
  mutate(
    Cuadrante = paste("Humedad", Grupo_Humedad, "- Dosis", Grupo_Dosis),
    Rango_Humedad_Cuadrante = case_when(
      Grupo_Humedad == "Baja" ~ paste("<", round(cortes_h[1], 1)),
      Grupo_Humedad == "Media" ~ paste(round(cortes_h[1], 1), "a", round(cortes_h[2], 1)),
      Grupo_Humedad == "Alta" ~ paste(">", round(cortes_h[2], 1))
    ),
    Rango_Dosis_Cuadrante = case_when(
      Grupo_Dosis == "Baja" ~ paste("<", round(cortes_d[1], 1)),
      Grupo_Dosis == "Media" ~ paste(round(cortes_d[1], 1), "a", round(cortes_d[2], 1)),
      Grupo_Dosis == "Alta" ~ paste(">", round(cortes_d[2], 1))
    )
  ) %>%
  select(Cuadrante, Rango_Humedad_Cuadrante, Rango_Dosis_Cuadrante, 
         Humedad_Optima_Local = Hidro_100, Dosis_Optima_Local = CCTon, Max_TAH_Esperado = Prediccion_Z) %>%
  arrange(desc(Max_TAH_Esperado)) %>%
  mutate(across(where(is.numeric), round, 3))

# Imprimir resumen en consola
cat("\n--- MEJORES COMBINACIONES POR CUADRANTE K-MEANS ---\n")
print(tabla_cuadrantes)

# Exportar a Excel
archivo_cuadrantes <- "Optimos_por_Cuadrantes_KMeans.xlsx"
write_xlsx(tabla_cuadrantes, archivo_cuadrantes)

cat(paste("\n¡Éxito! El detalle de las combinaciones por cuadrante se ha exportado como:", archivo_cuadrantes, "\n"))


###########################################################################################3
###########################################################################################

# ==============================================================================
# 15. VALIDACIÓN CON MODELOS MIXTOS (lme4), SUPERFICIES Y AGRUPAMIENTO
# ==============================================================================
cat("\n====================================================================\n")
cat("VALIDACIÓN CON MODELOS MIXTOS Y FUSIÓN DE ZONAS OPERATIVAS\n")
cat("====================================================================\n")

# Cargar librerías necesarias
library(lme4)
library(lmerTest) 
library(emmeans)
library(multcomp)
library(multcompView)
library(ggplot2)
library(dplyr)
library(writexl)

# --- 1. CLASIFICACIÓN DE DATOS (Preparación para el Modelo Mixto) ---
limites_reales_h <- c(-Inf, cortes_h, Inf)
limites_reales_d <- c(-Inf, cortes_d, Inf)

etiquetas_h <- paste0("H", 1:(length(cortes_h) + 1)) 
etiquetas_d <- paste0("D", 1:(length(cortes_d) + 1)) 

datos_anova <- datos %>%
  tidyr::drop_na(CCTon, Hidro_100, TAH_Rel) %>%
  dplyr::mutate(
    Grupo_H = cut(Hidro_100, breaks = limites_reales_h, labels = etiquetas_h, include.lowest = TRUE),
    Grupo_D = cut(CCTon, breaks = limites_reales_d, labels = etiquetas_d, include.lowest = TRUE)
  ) %>%
  dplyr::mutate(
    Cuadrante = as.factor(paste(Grupo_H, Grupo_D, sep = "_")),
    # CREAMOS FACTORES ALEATORIOS VÁLIDOS:
    # Redondeamos a números enteros para agrupar la variabilidad continua en "bloques" reales,
    # evitando así el error KhatriRao por exceso de niveles únicos.
    Humedad_Bloque = as.factor(round(Hidro_100, 0)),
    Dosis_Bloque = as.factor(round(CCTon, 0))
  ) %>%
  dplyr::group_by(Cuadrante) %>%
  dplyr::filter(n() >= 3) %>%
  dplyr::ungroup()

# --- 2. MODELO LINEAL MIXTO ---
# Efecto Fijo: Cuadrante
# Efectos Aleatorios: Bloques enteros de Humedad y Dosis
modelo_mixto <- lme4::lmer(TAH_Rel ~ Cuadrante + (1 | Humedad_Bloque) , #+ (1 | Dosis_Bloque)
                           data = datos_anova)

cat("\n--- RESULTADOS DEL ANOVA DEL MODELO MIXTO ---\n")
print(anova(modelo_mixto))

# --- 3. EXTRACCIÓN DE MEDIAS MARGINALES Y LETRAS DE SIGNIFICANCIA ---
# Validamos que la variable Cuadrante esté explícitamente en el grid de referencia
medias_mixtas <- emmeans::emmeans(modelo_mixto, specs = ~ Cuadrante)

# Extraer el agrupamiento (letras). reversed = TRUE asigna la 'a' al valor más alto.
letras_cld <- multcomp::cld(medias_mixtas, Letters = letters, reversed = TRUE, adjust = "tukey") %>%
  as.data.frame() %>%
  dplyr::mutate(Letra = trimws(.group)) %>%
  dplyr::select(Cuadrante, Media_Estimada = emmean, Letra)

cat("\n--- MEDIAS ESTIMADAS Y GRUPOS DE SIGNIFICANCIA ---\n")
print(letras_cld)

# --- 4. PREPARACIÓN DE CENTROIDES PARA GRÁFICOS ---
centroides_cuadrantes <- datos_anova %>%
  dplyr::group_by(Cuadrante) %>%
  dplyr::summarise(
    Centro_Dosis = mean(CCTon),
    Centro_Humedad = mean(Hidro_100)
  ) %>%
  dplyr::left_join(letras_cld, by = "Cuadrante")

# --- 5. GRÁFICO A: SUPERFICIE CON LETRAS ASIGNADAS POR CUADRANTE ---
grafico_superficie_letras <- grafico_cuadrantes + 
  geom_label(data = centroides_cuadrantes, 
             aes(x = Centro_Dosis, y = Centro_Humedad, 
                 label = paste0(Cuadrante, "\n[", Letra, "]")), 
             fill = "white", color = "black", fontface = "bold", size = 3.5, 
             alpha = 0.8, inherit.aes = FALSE) +
  labs(title = "Superficie de Respuesta con Grupos Estadísticos (Modelo Mixto)",
       subtitle = "Letras iguales indican que no hay diferencia significativa en el TAH_Rel estimado")

print(grafico_superficie_letras)


# --- 6. GRÁFICO B: FUSIÓN DE ZONAS OPERATIVAS (SIMILITUD DE LETRAS) ---
grid_clasificado <- grid_heatmap %>%
  dplyr::mutate(
    Grupo_H = cut(Hidro_100, breaks = limites_reales_h, labels = etiquetas_h, include.lowest = TRUE),
    Grupo_D = cut(CCTon, breaks = limites_reales_d, labels = etiquetas_d, include.lowest = TRUE),
    Cuadrante = as.factor(paste(Grupo_H, Grupo_D, sep = "_"))
  ) %>%
  dplyr::left_join(letras_cld, by = "Cuadrante") %>%
  tidyr::replace_na(list(Letra = "Sin Datos"))

grafico_zonas_fusionadas <- ggplot(grid_clasificado, aes(x = CCTon, y = Hidro_100)) +
  geom_raster(aes(fill = Letra), interpolate = FALSE, alpha = 0.75) +
  geom_contour(aes(z = Prediccion_Z), color = "black", alpha = 0.3, linewidth = 0.3) +
  geom_point(data = datos, aes(x = CCTon, y = Hidro_100), 
             color = "black", alpha = 0.4, size = 1.5, inherit.aes = FALSE) +
  scale_fill_brewer(palette = "Set1", name = "Grupo Estadístico") +
  scale_x_continuous(breaks = seq(0, 10, by = 1), limits = c(0, 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(60, 90, by = 5), limits = c(59, 90), expand = c(0, 0)) +
  labs(
    title = "Macro-Zonas Operativas por Similitud Estadística",
    subtitle = "Cuadrantes con la misma letra se fusionan, indicando un potencial de rendimiento similar",
    x = "Dosis de Madurante en Campo (CCTon)",
    y = "Índice de Humedad Interna (Hidro_100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    legend.position = "right",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1)
  )

print(grafico_zonas_fusionadas)

# --- 7. EXTRACCIÓN DE RANGOS OPERATIVOS REFINADOS PARA EXCEL ---
rangos_operativos <- datos_anova %>%
  dplyr::group_by(Cuadrante) %>%
  dplyr::summarise(
    N_Datos = n(),
    Humedad_Min = round(min(Hidro_100), 1),
    Humedad_Max = round(max(Hidro_100), 1),
    Dosis_Min = round(min(CCTon), 2),
    Dosis_Max = round(max(CCTon), 2),
    TAH_Observado_Promedio = round(mean(TAH_Rel), 3),
    Desviacion_TAH = round(sd(TAH_Rel), 3)
  ) %>%
  dplyr::left_join(letras_cld, by = "Cuadrante") %>%
  dplyr::arrange(Letra, desc(Media_Estimada)) %>%
  dplyr::mutate(
    Recomendacion_Humedad = paste(Humedad_Min, "a", Humedad_Max, "%"),
    Recomendacion_Dosis_cc_ton = paste(Dosis_Min, "a", Dosis_Max)
  ) %>%
  dplyr::select(
    Grupo_Estadistico = Letra, Cuadrante, 
    TAH_Estimado_Modelo = Media_Estimada, TAH_Observado_Promedio, 
    Recomendacion_Humedad, Recomendacion_Dosis_cc_ton, 
    N_Datos, Desviacion_TAH
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 3))

cat("\n--- RANGOS OPERATIVOS VALIDADOS (MODELO MIXTO) ---\n")
print(rangos_operativos)

# Exportar a Excel
archivo_mixto <- "Rangos_Validados_Modelo_Mixto.xlsx"
writexl::write_xlsx(rangos_operativos, archivo_mixto)

cat(paste("\n¡Éxito! El modelo mixto se ha ejecutado y las macro-zonas se exportaron como:", archivo_mixto, "\n"))


####################3


# ==============================================================================
# 16. MATRIZ DE DECISIÓN (TABLA PIVOT) Y DOSIS ÓPTIMAS POR BANDA DE HUMEDAD
# ==============================================================================
cat("\n====================================================================\n")
cat("GENERACIÓN DE TABLA PIVOT DE TAH Y ÓPTIMOS POR RANGO DE HUMEDAD\n")
cat("====================================================================\n")

# --- 1. GENERACIÓN DE ETIQUETAS DINÁMICAS ESTILO EXCEL ---
crear_etiquetas_rangos <- function(cortes, prefijo, sufijo = "") {
  c_round <- round(cortes, 1)
  n <- length(c_round)
  
  etiquetas <- c(paste0(prefijo, "1: < ", c_round[1], sufijo))
  for (i in 1:(n - 1)) {
    etiquetas <- c(etiquetas, paste0(prefijo, i + 1, ": ", c_round[i], " - ", c_round[i+1], sufijo))
  }
  etiquetas <- c(etiquetas, paste0(prefijo, n + 1, ": >= ", c_round[n], sufijo))
  
  return(etiquetas)
}

etiquetas_h_matrix <- crear_etiquetas_rangos(cortes_h, "H", "%")
etiquetas_d_matrix <- crear_etiquetas_rangos(cortes_d, "D", " cc/ton")

# --- 2. EXTRACCIÓN DE LA DOSIS ÓPTIMA EXACTA POR BANDA DE HUMEDAD ---
optimos_por_banda <- grid_heatmap %>%
  dplyr::mutate(
    Rango_Humedad = cut(Hidro_100, breaks = c(-Inf, cortes_h, Inf), 
                        labels = etiquetas_h_matrix, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Rango_Humedad) %>%
  dplyr::filter(Prediccion_Z == max(Prediccion_Z)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    Rango_Humedad,
    Humedad_Exacta_del_Pico = Hidro_100,
    Dosis_Optima_Sugerida = CCTon,
    TAH_Maximo_Esperado = Prediccion_Z
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

cat("\n--- DOSIS ÓPTIMA REQUERIDA PARA MAXIMIZAR TAH POR BANDA DE HUMEDAD ---\n")
print(optimos_por_banda)

# --- 3. CONSTRUCCIÓN DE LA TABLA MATRICIAL (TIPO PIVOT) CON DATOS REALES ---
datos_matriz <- datos %>%
  tidyr::drop_na(CCTon, Hidro_100, TAH_Rel) %>%
  dplyr::mutate(
    Rango_Humedad = cut(Hidro_100, breaks = c(-Inf, cortes_h, Inf), 
                        labels = etiquetas_h_matrix, include.lowest = TRUE),
    Rango_Dosis = cut(CCTon, breaks = c(-Inf, cortes_d, Inf), 
                      labels = etiquetas_d_matrix, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Rango_Humedad, Rango_Dosis) %>%
  dplyr::summarise(
    Media_TAH = round(mean(TAH_Rel), 2),
    .groups = "drop"
  )

# Pivotar los datos para crear la matriz cruzada
tabla_pivot <- datos_matriz %>%
  tidyr::pivot_wider(
    names_from = Rango_Dosis, 
    values_from = Media_TAH
  ) %>%
  dplyr::arrange(Rango_Humedad)

cat("\n--- MATRIZ OPERATIVA DE TAH (HUMEDAD VS DOSIS) ---\n")
print(tabla_pivot)

# --- 4. EXPORTACIÓN A EXCEL ---
archivo_matriz <- "Matriz_Operativa_TAH.xlsx"
writexl::write_xlsx(
  list(
    "Matriz_TAH" = tabla_pivot,
    "Dosis_Optimas_por_Banda" = optimos_por_banda
  ), 
  path = archivo_matriz
)

cat(paste("\n¡Listo! La matriz operativa y los óptimos se han exportado exitosamente a:", archivo_matriz, "\n"))

###############################################################################################################33




# ==============================================================================
# 16. MATRICES DE DECISIÓN (TEÓRICA Y REAL) Y DOSIS ÓPTIMAS POR BANDA
# ==============================================================================
cat("\n====================================================================\n")
cat("GENERACIÓN DE MATRICES DE TAH (TEÓRICA VS REAL) Y ÓPTIMOS POR BANDA\n")
cat("====================================================================\n")

# --- 1. GENERACIÓN DE ETIQUETAS DINÁMICAS ESTILO EXCEL ---
crear_etiquetas_rangos <- function(cortes, prefijo, sufijo = "") {
  c_round <- round(cortes, 1)
  n <- length(c_round)
  
  etiquetas <- c(paste0(prefijo, "1: < ", c_round[1], sufijo))
  for (i in 1:(n - 1)) {
    etiquetas <- c(etiquetas, paste0(prefijo, i + 1, ": ", c_round[i], " - ", c_round[i+1], sufijo))
  }
  etiquetas <- c(etiquetas, paste0(prefijo, n + 1, ": >= ", c_round[n], sufijo))
  
  return(etiquetas)
}

etiquetas_h_matrix <- crear_etiquetas_rangos(cortes_h, "H", "%")
etiquetas_d_matrix <- crear_etiquetas_rangos(cortes_d, "D", " cc/ton")

# --- 2. EXTRACCIÓN DE LA DOSIS ÓPTIMA EXACTA POR BANDA DE HUMEDAD ---
optimos_por_banda <- grid_heatmap %>%
  dplyr::mutate(
    Rango_Humedad = cut(Hidro_100, breaks = c(-Inf, cortes_h, Inf), 
                        labels = etiquetas_h_matrix, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Rango_Humedad) %>%
  dplyr::filter(Prediccion_Z == max(Prediccion_Z)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    Rango_Humedad,
    Humedad_Exacta_del_Pico = Hidro_100,
    Dosis_Optima_Sugerida = CCTon,
    TAH_Maximo_Esperado = Prediccion_Z
  ) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), round, 2))

cat("\n--- DOSIS ÓPTIMA REQUERIDA PARA MAXIMIZAR TAH POR BANDA DE HUMEDAD ---\n")
print(optimos_por_banda)

# --- 3. CONSTRUCCIÓN DE MATRICES (TEÓRICA Y REAL) ---

# A. MATRIZ TEÓRICA (El "Deber Ser"): Promedio de la superficie modelada por cuadrante.
# Muestra claramente dónde se gana y dónde se pierde TAH cubriendo el 100% de los escenarios.
matriz_teorica <- grid_heatmap %>%
  dplyr::mutate(
    Rango_Humedad = cut(Hidro_100, breaks = c(-Inf, cortes_h, Inf), 
                        labels = etiquetas_h_matrix, include.lowest = TRUE),
    Rango_Dosis = cut(CCTon, breaks = c(-Inf, cortes_d, Inf), 
                      labels = etiquetas_d_matrix, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Rango_Humedad, Rango_Dosis) %>%
  dplyr::summarise(
    TAH_Esperado = round(mean(Prediccion_Z), 3),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = Rango_Dosis, values_from = TAH_Esperado) %>%
  dplyr::arrange(Rango_Humedad)

cat("\n--- MATRIZ TEÓRICA DEL MODELO (POTENCIAL DE TAH) ---\n")
print(matriz_teorica)

# B. MATRIZ REAL (Lo ejecutado en campo): Muestra el histórico, con posibles huecos (NA).
matriz_real <- datos %>%
  tidyr::drop_na(CCTon, Hidro_100, TAH_Rel) %>%
  dplyr::mutate(
    Rango_Humedad = cut(Hidro_100, breaks = c(-Inf, cortes_h, Inf), 
                        labels = etiquetas_h_matrix, include.lowest = TRUE),
    Rango_Dosis = cut(CCTon, breaks = c(-Inf, cortes_d, Inf), 
                      labels = etiquetas_d_matrix, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Rango_Humedad, Rango_Dosis) %>%
  dplyr::summarise(
    Media_TAH = round(mean(TAH_Rel), 3),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(names_from = Rango_Dosis, values_from = Media_TAH) %>%
  dplyr::arrange(Rango_Humedad)

# --- 4. EXPORTACIÓN A EXCEL ---
archivo_matriz <- "Matriz_Operativa_TAH.xlsx"
writexl::write_xlsx(
  list(
    "Matriz_Teorica_Completa" = matriz_teorica,
    "Matriz_Historica_Real" = matriz_real,
    "Dosis_Optimas_por_Humedad" = optimos_por_banda
  ), 
  path = archivo_matriz
)

cat(paste("\n¡Listo! Las matrices y los óptimos se han exportado a:", archivo_matriz, "\n"))

############################################################333



# ==============================================================================
# 16. MATRIZ DE "PÍXELES" (ALTA RESOLUCIÓN) Y RANGOS POR CUADRANTE K-MEANS
# ==============================================================================
cat("\n====================================================================\n")
cat("GENERACIÓN DE MATRIZ DE PÍXELES (PANORAMA COMPLETO) Y RANGOS\n")
cat("====================================================================\n")

# --- 1. ETIQUETAS K-MEANS (Para clasificar los píxeles) ---
crear_etiquetas_rangos <- function(cortes, prefijo, sufijo = "") {
  c_round <- round(cortes, 1)
  n <- length(c_round)
  
  etiquetas <- c(paste0(prefijo, "1: < ", c_round[1], sufijo))
  for (i in 1:(n - 1)) {
    etiquetas <- c(etiquetas, paste0(prefijo, i + 1, ": ", c_round[i], " - ", c_round[i+1], sufijo))
  }
  etiquetas <- c(etiquetas, paste0(prefijo, n + 1, ": >= ", c_round[n], sufijo))
  
  return(etiquetas)
}

etiquetas_h_matrix <- crear_etiquetas_rangos(cortes_h, "H", "%")
etiquetas_d_matrix <- crear_etiquetas_rangos(cortes_d, "D", " cc/ton")

# --- 2. CREACIÓN DE LA MATRIZ DE "PÍXELES" (PANORAMA COMPLETO) ---
# Creamos un grid forzado con pasos operativos lógicos para el campo:
# Humedad cada 2% y Dosis cada 1 cc/ton. Esto "pixela" la imagen en una tabla de Excel.
secuencia_humedad_pixeles <- seq(60, 90, by = 2)
secuencia_dosis_pixeles   <- seq(0, 10, by = 1)

grid_pixeles <- expand.grid(
  CCTon = secuencia_dosis_pixeles,
  Hidro_100 = secuencia_humedad_pixeles
)

# Predecir el TAH para absolutamente todos los píxeles
grid_pixeles$TAH_Esperado <- predict(modelo_tah, newdata = grid_pixeles)

# Convertir el grid largo en una matriz ancha (Pivot)
matriz_pixeles_completa <- grid_pixeles %>%
  dplyr::mutate(TAH_Esperado = round(TAH_Esperado, 3)) %>%
  tidyr::pivot_wider(
    names_from = CCTon, 
    values_from = TAH_Esperado,
    names_prefix = "Dosis_" # Para que las columnas en Excel se llamen Dosis_0, Dosis_1, etc.
  ) %>%
  dplyr::rename(Humedad_Cana = Hidro_100) %>%
  dplyr::arrange(Humedad_Cana)

cat("\n--- MATRIZ DE PÍXELES (MUESTRA DE LAS PRIMERAS FILAS) ---\n")
print(head(matriz_pixeles_completa))


# --- 3. EXTRACCIÓN DE RANGOS (MIN, MAX, PROMEDIO) POR CUADRANTE ---
# Usamos el grid de calor original (el de altísima resolución) para que los mínimos 
# y máximos sean perfectos matemáticamente dentro de cada frontera K-Means.
resumen_cuadrantes_teoricos <- grid_heatmap %>%
  dplyr::mutate(
    Cuadrante_Humedad = cut(Hidro_100, breaks = c(-Inf, cortes_h, Inf), 
                            labels = etiquetas_h_matrix, include.lowest = TRUE),
    Cuadrante_Dosis = cut(CCTon, breaks = c(-Inf, cortes_d, Inf), 
                          labels = etiquetas_d_matrix, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Cuadrante_Humedad, Cuadrante_Dosis) %>%
  dplyr::summarise(
    TAH_Minimo = round(min(Prediccion_Z), 3),
    TAH_Promedio = round(mean(Prediccion_Z), 3),
    TAH_Maximo = round(max(Prediccion_Z), 3),
    .groups = "drop"
  ) %>%
  dplyr::arrange(Cuadrante_Humedad, Cuadrante_Dosis)

cat("\n--- RANGOS DE TAH EXTRAÍDOS DE CADA CUADRANTE K-MEANS ---\n")
View(resumen_cuadrantes_teoricos)


# --- 4. EXPORTACIÓN A EXCEL ---
archivo_pixeles <- "Matriz_Pixeles_y_Rangos_TAH.xlsx"
writexl::write_xlsx(
  list(
    "Matriz_Panorama_Completo" = matriz_pixeles_completa,
    "Rangos_por_Cuadrante" = resumen_cuadrantes_teoricos
  ), 
  path = archivo_pixeles
)

cat(paste("\n¡Éxito! El panorama completo pixelado y los rangos se exportaron a:", archivo_pixeles, "\n"))

########################33
#####################################################

# ==============================================================================
# 17. MAPA DE PÍXELES (MOSAICO) Y RANGOS OPERATIVOS MEDIANTE K-MEANS
# ==============================================================================
cat("\n====================================================================\n")
cat("RECONSTRUCCIÓN DE MAPA PIXELADO Y AGRUPAMIENTO DE BANDAS DE HUMEDAD\n")
cat("====================================================================\n")

library(ggplot2)
library(dplyr)
library(writexl)

# --- 1. RECONSTRUCCIÓN VISUAL DEL MAPA CON PÍXELES ---
# Usamos geom_tile para crear celdas rígidas (píxeles) en lugar de un gradiente difuminado.
grafico_pixeles <- ggplot(grid_pixeles, aes(x = CCTon, y = Hidro_100, fill = TAH_Esperado)) +
  geom_tile(color = "white", linewidth = 0.5) + # Delinea cada "píxel"
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "TAH Relativo"
  ) +
  # Agregamos el valor del TAH en cada celda para consulta directa
  geom_text(aes(label = sprintf("%.2f", TAH_Esperado)), size = 2.5, color = "black", fontface = "bold") +
  scale_x_continuous(breaks = unique(grid_pixeles$CCTon), expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(grid_pixeles$Hidro_100), expand = c(0, 0)) +
  labs(
    title = "Matriz de Superficie Pixelada (Resolución Operativa)",
    subtitle = "Cada celda representa una intersección exacta de Humedad y Dosis",
    x = "Dosis de Madurante en Campo (CCTon)",
    y = "Índice de Humedad Interna (Hidro_100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid = element_blank(), # Quitamos la cuadrícula de fondo porque los tiles ya forman una
    legend.position = "right",
    legend.key.height = unit(2, "cm")
  )

print(grafico_pixeles)

# --- 2. EXTRACCIÓN DEL "CAMINO ÓPTIMO" DESDE LOS PÍXELES ---
# Buscamos, para cada fila de humedad (60, 62, 64...), cuál es el píxel con el TAH más alto
optimos_pixeles <- grid_pixeles %>%
  dplyr::group_by(Hidro_100) %>%
  dplyr::filter(TAH_Esperado == max(TAH_Esperado)) %>%
  dplyr::slice(1) %>% # Por si hay empates
  dplyr::ungroup()


# --- 3. K-MEANS SOBRE EL CAMINO ÓPTIMO (EJE DE HUMEDAD) ---
# Aplicamos K-Means (k=4) sobre las humedades de estos píxeles óptimos para crear las zonas
set.seed(42)
km_bandas <- kmeans(optimos_pixeles$Hidro_100, centers = 4)
optimos_pixeles$Grupo_KMeans <- km_bandas$cluster

# Ordenar los clusters para que el "1" sea la humedad más baja y el "4" la más alta
orden_clusters <- optimos_pixeles %>%
  dplyr::group_by(Grupo_KMeans) %>%
  dplyr::summarise(Centro_H = mean(Hidro_100)) %>%
  dplyr::arrange(Centro_H) %>%
  dplyr::mutate(Nuevo_Grupo = 1:4)

optimos_pixeles <- optimos_pixeles %>%
  dplyr::left_join(orden_clusters, by = "Grupo_KMeans") %>%
  dplyr::mutate(Zona = paste("Zona", Nuevo_Grupo))

# --- 4. CREACIÓN DE LA TABLA DE RANGOS (HUMEDAD -> DOSIS -> TAH) ---
# Resumimos los resultados agrupados por las nuevas Zonas K-Means
tabla_rangos_pixeles <- optimos_pixeles %>%
  dplyr::group_by(Zona) %>%
  dplyr::summarise(
    # Extraer los extremos de cada zona
    Humedad_Min = min(Hidro_100),
    Humedad_Max = max(Hidro_100),
    Dosis_Min = min(CCTon),
    Dosis_Max = max(CCTon),
    TAH_Min = min(TAH_Esperado),
    TAH_Max = max(TAH_Esperado),
    .groups = "drop"
  ) %>%
  # Formatear como rangos de texto limpios para el equipo de operaciones
  dplyr::mutate(
    Rango_Humedad_Detectada = paste(Humedad_Min, "a", Humedad_Max, "%"),
    Dosis_Requerida_cc_ton = paste(Dosis_Min, "a", Dosis_Max),
    Retorno_TAH_Esperado = paste(round(TAH_Min, 2), "a", round(TAH_Max, 2))
  ) %>%
  dplyr::select(Zona, Rango_Humedad_Detectada, Dosis_Requerida_cc_ton, Retorno_TAH_Esperado)
  
  cat("\n--- BANDAS OPERATIVAS K-MEANS BASADAS EN PÍXELES ÓPTIMOS ---\n")
  print(tabla_rangos_pixeles)
  
  sjPlot::tab_df(tabla_rangos_pixeles)

# --- 5. EXPORTACIÓN A EXCEL ---
archivo_zonas_pixeles <- "Zonas_Operativas_desde_Pixeles.xlsx"
writexl::write_xlsx(tabla_rangos_pixeles, archivo_zonas_pixeles)

cat(paste("\n¡Éxito! Las bandas operativas se extrajeron de la matriz y se guardaron en:", archivo_zonas_pixeles, "\n"))



#####################################################################################


# ==============================================================================
# 17. MAPA DE PÍXELES (MOSAICO) Y RANGOS DE PRECISIÓN (CENTRO VS ORILLAS)
# ==============================================================================
cat("\n====================================================================\n")
cat("RECONSTRUCCIÓN DE MAPA PIXELADO Y EXTRACCIÓN DE DIAGONAL ÓPTIMA\n")
cat("====================================================================\n")

library(ggplot2)
library(dplyr)
library(writexl)

# --- 1. RECONSTRUCCIÓN VISUAL DEL MAPA CON PÍXELES ---
grafico_pixeles <- ggplot(grid_pixeles, aes(x = CCTon, y = Hidro_100, fill = TAH_Esperado)) +
  geom_tile(color = "white", linewidth = 0.5) + 
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "TAH Relativo"
  ) +
  geom_text(aes(label = sprintf("%.2f", TAH_Esperado)), size = 2.5, color = "black", fontface = "bold") +
  scale_x_continuous(breaks = unique(grid_pixeles$CCTon), expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(grid_pixeles$Hidro_100), expand = c(0, 0)) +
  labs(
    title = "Matriz de Superficie Pixelada (Resolución Operativa)",
    subtitle = "Diagonal de aplicación: Centro (verde oscuro) y Orillas de tolerancia",
    x = "Dosis de Madurante en Campo (CCTon)",
    y = "Índice de Humedad Interna (Hidro_100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid = element_blank(), 
    legend.position = "right",
    legend.key.height = unit(2, "cm")
  )

print(grafico_pixeles)

# --- 2. K-MEANS SOBRE EL EJE DE HUMEDAD (CREACIÓN DE ZONAS) ---
# Extraemos el camino óptimo absoluto para calcular los 4 cortes naturales de humedad
optimos_absolutos <- grid_pixeles %>%
  dplyr::group_by(Hidro_100) %>%
  dplyr::filter(TAH_Esperado == max(TAH_Esperado)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

set.seed(42)
km_bandas <- kmeans(optimos_absolutos$Hidro_100, centers = 4)

# Ordenar los clusters de menor a mayor humedad y obtener los cortes
cortes_zonas <- data.frame(Humedad = optimos_absolutos$Hidro_100, Cluster = km_bandas$cluster) %>%
  dplyr::group_by(Cluster) %>%
  dplyr::summarise(Max_H = max(Humedad), Min_H = min(Humedad)) %>%
  dplyr::arrange(Max_H)

limites_k <- c(-Inf, 
               mean(c(cortes_zonas$Max_H[1], cortes_zonas$Min_H[2])),
               mean(c(cortes_zonas$Max_H[2], cortes_zonas$Min_H[3])),
               mean(c(cortes_zonas$Max_H[3], cortes_zonas$Min_H[4])), 
               Inf)

# --- 3. EXTRACCIÓN DEL CENTRO (MÁS VERDE) Y LAS ORILLAS (LÍMITES) ---
# Asignamos la Zona a TODO el grid de píxeles
grid_clasificado <- grid_pixeles %>%
  dplyr::mutate(Zona = cut(Hidro_100, breaks = limites_k, labels = paste("Zona", 1:4)))

# Escaneamos cada Zona para encontrar la "Diagonal"
tabla_rangos_precision <- grid_clasificado %>%
  dplyr::group_by(Zona) %>%
  dplyr::mutate(
    Max_TAH_Zona = max(TAH_Esperado),
    # Tolerancia estricta para el "Centro" (solo pierde 0.01 de TAH frente al pico)
    Es_Centro = TAH_Esperado >= (Max_TAH_Zona - 0.01),
    # Tolerancia amplia para las "Orillas" (hasta 0.03 de pérdida, cubre los verdes claros)
    Es_Orilla = TAH_Esperado >= (Max_TAH_Zona - 0.03)
  ) %>%
  dplyr::summarise(
    Rango_Humedad = paste(min(Hidro_100), "a", max(Hidro_100), "%"),
    
    # Cálculos del Centro Ideal
    Dosis_Centro_Min = min(CCTon[Es_Centro]),
    Dosis_Centro_Max = max(CCTon[Es_Centro]),
    TAH_Centro = paste(sprintf("%.2f", min(TAH_Esperado[Es_Centro])), "a", sprintf("%.2f", max(Max_TAH_Zona))),
    
    # Cálculos de las Orillas (Máximos y Mínimos permitidos)
    Dosis_Orilla_Min = min(CCTon[Es_Orilla]),
    Dosis_Orilla_Max = max(CCTon[Es_Orilla]),
    TAH_Orillas = paste(sprintf("%.2f", min(TAH_Esperado[Es_Orilla])), "a", sprintf("%.2f", min(TAH_Esperado[Es_Centro]) - 0.01)),
    
    .groups = "drop"
  ) %>%
  # Ensamblaje de la tabla final
  dplyr::mutate(
    `Recomendacion_CENTRO (cc/ton)` = paste(Dosis_Centro_Min, "a", Dosis_Centro_Max),
    `TAH_del_Centro` = TAH_Centro,
    `Limites_ORILLAS_Min_Max (cc/ton)` = paste(Dosis_Orilla_Min, "a", Dosis_Orilla_Max),
    `TAH_en_Orillas` = TAH_Orillas
  ) %>%
  dplyr::select(Zona, Rango_Humedad, `Recomendacion_CENTRO (cc/ton)`, `TAH_del_Centro`, `Limites_ORILLAS_Min_Max (cc/ton)`, `TAH_en_Orillas`)

cat("\n--- RANGOS DE PRECISIÓN: CENTRO IDEAL VS LÍMITES DE OPERACIÓN ---\n")
print(tabla_rangos_precision)

sjPlot::tab_df(tabla_rangos_precision)

# --- 4. EXPORTACIÓN A EXCEL ---
archivo_precision <- "Rangos_Operativos_Centro_y_Orillas.xlsx"
writexl::write_xlsx(tabla_rangos_precision, archivo_precision)

cat(paste("\n¡Éxito! Los rangos estructurados en Centro y Orillas se exportaron a:", archivo_precision, "\n"))


#############################################################################################################


# ==============================================================================
# 18. EXPORTACIÓN DE LA MATRIZ VISUAL PIXELADA A EXCEL
# ==============================================================================
cat("\n====================================================================\n")
cat("EXPORTANDO LA MATRIZ VISUAL DE PÍXELES A EXCEL\n")
cat("====================================================================\n")

library(dplyr)
library(tidyr)
library(writexl)

# Transformar el grid de píxeles al formato exacto de la imagen (Tabla de doble entrada)
matriz_excel <- grid_pixeles %>%
  # Redondear a 2 decimales exactamente como se ve en las celdas de tu imagen
  dplyr::mutate(TAH_Esperado = round(TAH_Esperado, 2)) %>%
  # Pivotar: Las columnas serán las dosis y las filas la humedad
  tidyr::pivot_wider(
    names_from = CCTon,
    values_from = TAH_Esperado,
    names_prefix = "Dosis_" # Agrega el prefijo para que las columnas se llamen "Dosis_0", "Dosis_1", etc.
  ) %>%
  dplyr::rename(Humedad_Hidro100 = Hidro_100) %>%
  # Ordenar de mayor a menor humedad (90 a 60) para que se lea igual que el eje Y del gráfico
  dplyr::arrange(desc(Humedad_Hidro100))

# Visualizar las primeras filas en consola para confirmar
cat("\n--- VISTA PREVIA DE LA MATRIZ PARA EXCEL ---\n")
print(head(matriz_excel))

# Exportar a Excel
archivo_matriz_visual <- "Matriz_Superficie_Pixelada.xlsx"
writexl::write_xlsx(matriz_excel, archivo_matriz_visual)

cat(paste("\n¡Listo! La matriz exacta que ves en el gráfico se ha exportado como:", archivo_matriz_visual, "\n"))



##################################################################################
############ Nicaragua ##########################


# ==============================================================================
# 18. EXPORTACIÓN DE MATRIZ VISUAL Y AGRUPAMIENTO POR RANGOS AJUSTADOS
# ==============================================================================
cat("\n====================================================================\n")
cat("EXPORTANDO LA MATRIZ VISUAL Y APLICANDO NUEVOS RANGOS (NICARAGUA)\n")
cat("====================================================================\n")

library(dplyr)
library(tidyr)
library(writexl)

# --- A. AJUSTE DE NUEVOS RANGOS OPERATIVOS (MODIFICABLES) ---
# Aquí puedes escribir los límites exactos que necesitas para tu operación.
# Ejemplo usando los límites visuales anteriores: 67.2 y 78.2
nuevos_limites_humedad <- c(0, 67.2, 78.2, 100)
nuevas_etiquetas_humedad <- c("R1: < 67.2%", "R2: 67.2% a 78.2%", "R3: >= 78.2%")

# Aplicar los nuevos rangos a la base de datos de píxeles
grid_rangos_ajustados <- grid_pixeles %>%
  dplyr::mutate(
    Rango_Humedad = cut(Hidro_100, breaks = nuevos_limites_humedad, labels = nuevas_etiquetas_humedad, include.lowest = TRUE)
  ) %>%
  dplyr::group_by(Rango_Humedad, CCTon) %>%
  dplyr::summarise(
    TAH_Promedio = round(mean(TAH_Esperado), 2),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = CCTon,
    values_from = TAH_Promedio,
    names_prefix = "Dosis_"
  )

# --- B. PREPARACIÓN DE LA MATRIZ VISUAL (PÍXELES) ---
matriz_excel_visual <- grid_pixeles %>%
  dplyr::mutate(TAH_Esperado = round(TAH_Esperado, 2)) %>%
  tidyr::pivot_wider(
    names_from = CCTon,
    values_from = TAH_Esperado,
    names_prefix = "Dosis_"
  ) %>%
  dplyr::rename(Humedad_Hidro100 = Hidro_100) %>%
  dplyr::arrange(desc(Humedad_Hidro100))

sjPlot::tab_df(matriz_excel_visual)

# --- C. EXPORTACIÓN A EXCEL (MÚLTIPLES PESTAÑAS) ---
archivo_matriz_nicaragua <- "Matriz_Superficie_Pixelada_NICARAGUA.xlsx"

writexl::write_xlsx(
  list(
    "Matriz_Visual_Pixelada" = matriz_excel_visual,
    "Resumen_Nuevos_Rangos" = grid_rangos_ajustados
  ),
  path = archivo_matriz_nicaragua
)

cat("\n--- VISTA PREVIA DEL RESUMEN POR RANGOS ---\n")
print(grid_rangos_ajustados)

cat(paste("\n¡Listo! El archivo", archivo_matriz_nicaragua, "contiene ahora la matriz visual y los rangos ajustados.\n"))









#########################



# ==============================================================================
# 17. MAPA DE PÍXELES (MOSAICO) Y RANGOS OPERATIVOS (LIMITES NICARAGUA)
# ==============================================================================
cat("\n====================================================================\n")
cat("RECONSTRUCCIÓN DE MAPA PIXELADO Y RANGOS AJUSTADOS (NICARAGUA)\n")
cat("====================================================================\n")

library(ggplot2)
library(dplyr)
library(writexl)

# --- 1. RECONSTRUCCIÓN VISUAL DEL MAPA CON PÍXELES ---
grafico_pixeles <- ggplot(grid_pixeles, aes(x = CCTon, y = Hidro_100, fill = TAH_Esperado)) +
  geom_tile(color = "white", linewidth = 0.5) + 
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "TAH Relativo"
  ) +
  geom_text(aes(label = sprintf("%.2f", TAH_Esperado)), size = 2.5, color = "black", fontface = "bold") +
  scale_x_continuous(breaks = unique(grid_pixeles$CCTon), expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(grid_pixeles$Hidro_100), expand = c(0, 0)) +
  labs(
    title = "Matriz de Superficie Pixelada (Resolución Operativa)",
    subtitle = "Intersecciones exactas de Humedad y Dosis",
    x = "Dosis de Madurante en Campo (CCTon)",
    y = "Índice de Humedad Interna (Hidro_100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid = element_blank(), 
    legend.position = "right",
    legend.key.height = unit(2, "cm")
  )

print(grafico_pixeles)

# --- 2. DEFINICIÓN DE LÍMITES AJUSTADOS (NICARAGUA) ---
limites_nica <- c(-Inf, 67.2, 78.2, Inf)
etiquetas_nica <- c("R1: < 67.2%", "R2: 67.2% a 78.2%", "R3: >= 78.2%")

# --- 3. EXTRACCIÓN DEL "CAMINO ÓPTIMO" Y CLASIFICACIÓN ---
optimos_pixeles <- grid_pixeles %>%
  dplyr::group_by(Hidro_100) %>%
  dplyr::filter(TAH_Esperado == max(TAH_Esperado)) %>%
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>%
  # Clasificamos directamente usando los nuevos rangos
  dplyr::mutate(Zona = cut(Hidro_100, breaks = limites_nica, labels = etiquetas_nica, include.lowest = TRUE))

# --- 4. CREACIÓN DE LA TABLA DE RANGOS ---
tabla_rangos_pixeles <- optimos_pixeles %>%
  dplyr::group_by(Zona) %>%
  dplyr::summarise(
    Humedad_Min = min(Hidro_100),
    Humedad_Max = max(Hidro_100),
    Dosis_Min = min(CCTon),
    Dosis_Max = max(CCTon),
    TAH_Min = min(TAH_Esperado),
    TAH_Max = max(TAH_Esperado),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Rango_Humedad_Detectada = paste(Humedad_Min, "a", Humedad_Max, "%"),
    Dosis_Requerida_cc_ton = paste(Dosis_Min, "a", Dosis_Max),
    Retorno_TAH_Esperado = paste(round(TAH_Min, 2), "a", round(TAH_Max, 2))
  ) %>%
  dplyr::select(Zona, Rango_Humedad_Detectada, Dosis_Requerida_cc_ton, Retorno_TAH_Esperado)

cat("\n--- BANDAS OPERATIVAS BASADAS EN PÍXELES ÓPTIMOS (NICARAGUA) ---\n")
print(tabla_rangos_pixeles)

sjPlot::tab_df(tabla_rangos_pixeles)

# --- 5. EXPORTACIÓN A EXCEL ---
archivo_zonas_pixeles <- "Zonas_Operativas_desde_Pixeles_Nicaragua.xlsx"
writexl::write_xlsx(tabla_rangos_pixeles, archivo_zonas_pixeles)

cat(paste("\n¡Éxito! Las bandas operativas se guardaron en:", archivo_zonas_pixeles, "\n"))




















# ==============================================================================
# 17. MAPA DE PÍXELES (MOSAICO) Y RANGOS DE PRECISIÓN (CENTRO VS ORILLAS)
# ==============================================================================
cat("\n====================================================================\n")
cat("RECONSTRUCCIÓN DE MAPA PIXELADO Y EXTRACCIÓN DE DIAGONAL ÓPTIMA (NICARAGUA)\n")
cat("====================================================================\n")

library(ggplot2)
library(dplyr)
library(writexl)

# --- 1. RECONSTRUCCIÓN VISUAL DEL MAPA CON PÍXELES ---
grafico_pixeles <- ggplot(grid_pixeles, aes(x = CCTon, y = Hidro_100, fill = TAH_Esperado)) +
  geom_tile(color = "white", linewidth = 0.5) + 
  scale_fill_gradientn(
    colors = c("#d73027", "#f46d43", "#fdae61", "#fee08b", "#d9ef8b", "#a6d96a", "#66bd63", "#1a9850"),
    name = "TAH Relativo"
  ) +
  geom_text(aes(label = sprintf("%.2f", TAH_Esperado)), size = 2.5, color = "black", fontface = "bold") +
  scale_x_continuous(breaks = unique(grid_pixeles$CCTon), expand = c(0, 0)) +
  scale_y_continuous(breaks = unique(grid_pixeles$Hidro_100), expand = c(0, 0)) +
  labs(
    title = "Matriz de Superficie Pixelada (Resolución Operativa)",
    subtitle = "Diagonal de aplicación: Centro (verde oscuro) y Orillas de tolerancia",
    x = "Dosis de Madurante en Campo (CCTon)",
    y = "Índice de Humedad Interna (Hidro_100)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    panel.grid = element_blank(), 
    legend.position = "right",
    legend.key.height = unit(2, "cm")
  )

print(grafico_pixeles)

# --- 2. DEFINICIÓN DE LÍMITES AJUSTADOS (NICARAGUA) ---
# Reemplazamos el K-Means por los límites operativos de negocio
limites_nica <- c(-Inf, 67.2, 78.2, Inf)
etiquetas_nica <- c("R1: < 67.2%", "R2: 67.2% a 78.2%", "R3: >= 78.2%")

# --- 3. EXTRACCIÓN DEL CENTRO (MÁS VERDE) Y LAS ORILLAS (LÍMITES) ---
# Asignamos la Zona a TODO el grid de píxeles usando los nuevos límites
grid_clasificado <- grid_pixeles %>%
  dplyr::mutate(Zona = cut(Hidro_100, breaks = limites_nica, labels = etiquetas_nica, include.lowest = TRUE))

# Escaneamos cada Zona para encontrar la "Diagonal"
tabla_rangos_precision <- grid_clasificado %>%
  dplyr::group_by(Zona) %>%
  dplyr::mutate(
    Max_TAH_Zona = max(TAH_Esperado),
    # Tolerancia estricta para el "Centro" (solo pierde 0.01 de TAH frente al pico)
    Es_Centro = TAH_Esperado >= (Max_TAH_Zona - 0.01),
    # Tolerancia amplia para las "Orillas" (hasta 0.03 de pérdida, cubre los verdes claros)
    Es_Orilla = TAH_Esperado >= (Max_TAH_Zona - 0.03)
  ) %>%
  dplyr::summarise(
    Rango_Humedad = paste(min(Hidro_100), "a", max(Hidro_100), "%"),
    
    # Cálculos del Centro Ideal
    Dosis_Centro_Min = min(CCTon[Es_Centro]),
    Dosis_Centro_Max = max(CCTon[Es_Centro]),
    TAH_Centro = paste(sprintf("%.2f", min(TAH_Esperado[Es_Centro])), "a", sprintf("%.2f", max(Max_TAH_Zona))),
    
    # Cálculos de las Orillas (Máximos y Mínimos permitidos)
    Dosis_Orilla_Min = min(CCTon[Es_Orilla]),
    Dosis_Orilla_Max = max(CCTon[Es_Orilla]),
    TAH_Orillas = paste(sprintf("%.2f", min(TAH_Esperado[Es_Orilla])), "a", sprintf("%.2f", min(TAH_Esperado[Es_Centro]) - 0.01)),
    
    .groups = "drop"
  ) %>%
  # Ensamblaje de la tabla final
  dplyr::mutate(
    `Recomendacion_CENTRO (cc/ton)` = paste(Dosis_Centro_Min, "a", Dosis_Centro_Max),
    `TAH_del_Centro` = TAH_Centro,
    `Limites_ORILLAS_Min_Max (cc/ton)` = paste(Dosis_Orilla_Min, "a", Dosis_Orilla_Max),
    `TAH_en_Orillas` = TAH_Orillas
  ) %>%
  dplyr::select(Zona, Rango_Humedad, `Recomendacion_CENTRO (cc/ton)`, `TAH_del_Centro`, `Limites_ORILLAS_Min_Max (cc/ton)`, `TAH_en_Orillas`)

cat("\n--- RANGOS DE PRECISIÓN: CENTRO IDEAL VS LÍMITES (NICARAGUA) ---\n")
print(tabla_rangos_precision)

sjPlot::tab_df(tabla_rangos_precision)

# --- 4. EXPORTACIÓN A EXCEL ---
archivo_precision <- "Rangos_Operativos_Centro_y_Orillas_Nicaragua.xlsx"
writexl::write_xlsx(tabla_rangos_precision, archivo_precision)

cat(paste("\n¡Éxito! Los rangos estructurados en Centro y Orillas se exportaron a:", archivo_precision, "\n"))
