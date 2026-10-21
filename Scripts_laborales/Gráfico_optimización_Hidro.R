
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)

attach(datos)







######################################################################################################


# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Función mejorada: Buscar óptimo SOLO en el rango real de CCTon
obtener_optimo_real <- function(df, x_col, y_col) {
  modelo <- lm(as.formula(paste(y_col, "~ poly(", x_col, ", 2, raw=TRUE)")), data = df)
  coefs <- coef(modelo)
  c <- coefs[1]
  b <- coefs[2]
  a <- coefs[3]
  
  # Extraer el rango real de los datos
  rango_x <- range(df[[x_col]], na.rm = TRUE)
  min_x <- rango_x[1]
  max_x <- rango_x[2]
  
  # Evaluar la ecuación en los extremos del rango
  y_min_x <- a*(min_x^2) + b*min_x + c
  y_max_x <- a*(max_x^2) + b*max_x + c
  
  # Por defecto, elegir el mejor entre los dos extremos
  x_opt <- min_x
  y_opt <- y_min_x
  if (y_max_x > y_opt) {
    x_opt <- max_x
    y_opt <- y_max_x
  }
  
  # Si la curva es cóncava hacia abajo (a < 0), evaluar el vértice
  if (!is.na(a) && a < 0) {
    x_vertice <- -b / (2 * a)
    # Solo considerar el vértice si cae DENTRO de los datos reales
    if (x_vertice >= min_x && x_vertice <= max_x) {
      y_vertice <- a*(x_vertice^2) + b*x_vertice + c
      if (y_vertice > y_opt) {
        x_opt <- x_vertice
        y_opt <- y_vertice
      }
    }
  }
  
  return(data.frame(Variable = y_col, CCTon_Optimo = x_opt, Valor_Maximo = y_opt))
}

# Limites del eje X basados en los datos reales
limites_x <- range(datos$CCTon, na.rm = TRUE)

# ---------------------------------------------------------
# GRÁFICO 1: VALORES ABSOLUTOS
# ---------------------------------------------------------

optimos_abs <- bind_rows(
  obtener_optimo_real(datos, "CCTon", "TCH"),
  obtener_optimo_real(datos, "CCTon", "Rendimiento"),
  obtener_optimo_real(datos, "CCTon", "TAH")
)

datos_abs_long <- datos %>%
  dplyr::select(CCTon, TCH, Rendimiento, TAH) %>%
  pivot_longer(cols = c(TCH, Rendimiento, TAH), names_to = "Variable", values_to = "Valor")

grafico_abs <- ggplot(datos_abs_long, aes(x = CCTon, y = Valor, color = Variable)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linewidth = 1) +
  geom_point(data = optimos_abs, aes(x = CCTon_Optimo, y = Valor_Maximo, color = Variable),
             size = 4, shape = 8, stroke = 2) +
  geom_text(data = optimos_abs, aes(x = CCTon_Optimo, y = Valor_Maximo,
                                    label = paste0("Máx en: ", round(CCTon_Optimo, 2))),
            vjust = -1.5, show.legend = FALSE, size = 3.5) +
  # Forzar a que la gráfica no se salga del rango real
  coord_cartesian(xlim = limites_x) +
  labs(title = "Curvas de TCH, Rendimiento y TAH vs CCTon",
       subtitle = "Valores absolutos (Rango acotado a datos reales)",
       x = "CCTon", y = "Valor Absoluto") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_abs)

# ---------------------------------------------------------
# GRÁFICO 2: VALORES RELATIVOS (Intersecciones)
# ---------------------------------------------------------

optimos_rel <- bind_rows(
  obtener_optimo_real(datos, "CCTon", "TCH_Rel"),
  obtener_optimo_real(datos, "CCTon", "Rend_Rel"),
  obtener_optimo_real(datos, "CCTon", "TAH_Rel")
)

datos_rel_long <- datos %>%
  dplyr::select(CCTon, TCH_Rel, Rend_Rel, TAH_Rel) %>%
  pivot_longer(cols = c(TCH_Rel, Rend_Rel, TAH_Rel), names_to = "Variable", values_to = "Valor")

grafico_rel <- ggplot(datos_rel_long, aes(x = CCTon, y = Valor, color = Variable)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linewidth = 1) +
  geom_point(data = optimos_rel, aes(x = CCTon_Optimo, y = Valor_Maximo, color = Variable),
             size = 4, shape = 8, stroke = 2) +
  geom_text(data = optimos_rel, aes(x = CCTon_Optimo, y = Valor_Maximo,
                                    label = paste0("Máx en: ", round(CCTon_Optimo, 2))),
            vjust = -1.5, show.legend = FALSE, size = 3.5) +
  # Forzar a que la gráfica no se salga del rango real
  coord_cartesian(xlim = limites_x) +
  labs(title = "Curvas Relativas de TCH, Rendimiento y TAH vs CCTon",
       subtitle = "Puntos de equilibrio en el rango real de medición",
       x = "CCTon", y = "Valor Relativo") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(grafico_rel)




###################################################################3


# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Función para buscar el óptimo en el rango real de CCTon
obtener_optimo_real <- function(df, x_col, y_col) {
  modelo <- lm(as.formula(paste(y_col, "~ poly(", x_col, ", 2, raw=TRUE)")), data = df)
  coefs <- coef(modelo)
  c <- coefs[1]
  b <- coefs[2]
  a <- coefs[3]
  
  rango_x <- range(df[[x_col]], na.rm = TRUE)
  min_x <- rango_x[1]
  max_x <- rango_x[2]
  
  y_min_x <- a*(min_x^2) + b*min_x + c
  y_max_x <- a*(max_x^2) + b*max_x + c
  
  x_opt <- min_x
  y_opt <- y_min_x
  if (y_max_x > y_opt) {
    x_opt <- max_x
    y_opt <- y_max_x
  }
  
  if (!is.na(a) && a < 0) {
    x_vertice <- -b / (2 * a)
    if (x_vertice >= min_x && x_vertice <= max_x) {
      y_vertice <- a*(x_vertice^2) + b*x_vertice + c
      if (y_vertice > y_opt) {
        x_opt <- x_vertice
        y_opt <- y_vertice
      }
    }
  }
  
  return(data.frame(Variable = y_col, CCTon_Optimo = x_opt, Valor_Maximo = y_opt))
}

# Límites del eje X basados en los datos reales
limites_x <- range(datos$CCTon, na.rm = TRUE)

# ---------------------------------------------------------
# GRÁFICO 1: VALORES ABSOLUTOS (Líneas de Grupo y Equilibrio)
# ---------------------------------------------------------

optimos_abs <- bind_rows(
  obtener_optimo_real(datos, "CCTon", "TCH"),
  obtener_optimo_real(datos, "CCTon", "Rendimiento"),
  obtener_optimo_real(datos, "CCTon", "TAH")
)

datos_abs_long <- datos %>%
  dplyr::select(CCTon, TCH, Rendimiento, TAH) %>%
  pivot_longer(cols = c(TCH, Rendimiento, TAH), names_to = "Variable", values_to = "Valor")

grafico_abs <- ggplot(datos_abs_long, aes(x = CCTon, y = Valor, color = Variable)) +
  # Se eliminó geom_point para limpiar el fondo
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linewidth = 1.2) +
  
  # Líneas verticales de corte para formar el grupo de optimización
  geom_vline(data = optimos_abs, aes(xintercept = CCTon_Optimo, color = Variable),
             linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
  
  # Marcadores de los puntos óptimos individuales
  geom_point(data = optimos_abs, aes(x = CCTon_Optimo, y = Valor_Maximo, color = Variable),
             size = 4, shape = 8, stroke = 2) +
  geom_text(data = optimos_abs, aes(x = CCTon_Optimo, y = Valor_Maximo,
                                    label = paste0("Ópt: ", round(CCTon_Optimo, 2))),
            vjust = -1.5, show.legend = FALSE, size = 3.5) +
  
  coord_cartesian(xlim = limites_x) +
  labs(title = "Curvas de TCH, Rendimiento y TAH vs CCTon",
       subtitle = "Líneas verticales delimitan el grupo de equilibrio óptimo",
       x = "CCTon", y = "Valor Absoluto") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(grafico_abs)

# ---------------------------------------------------------
# GRÁFICO 2: VALORES RELATIVOS (Líneas de Grupo y Equilibrio)
# ---------------------------------------------------------

optimos_rel <- bind_rows(
  obtener_optimo_real(datos, "CCTon", "TCH_Rel"),
  obtener_optimo_real(datos, "CCTon", "Rend_Rel"),
  obtener_optimo_real(datos, "CCTon", "TAH_Rel")
)

# Cambiar nombres para que coincidan con la leyenda de forma estética
optimos_rel <- optimos_rel %>%
  mutate(Variable_Etiqueta = case_when(
    Variable == "TCH_Rel" ~ "TCH_Rel",
    Variable == "Rend_Rel" ~ "Rend_Rel",
    Variable == "TAH_Rel" ~ "TAH_Rel"
  ))

datos_rel_long <- datos %>%
  dplyr::select(CCTon, TCH_Rel, Rend_Rel, TAH_Rel) %>%
  pivot_longer(cols = c(TCH_Rel, Rend_Rel, TAH_Rel), names_to = "Variable", values_to = "Valor")

grafico_rel <- ggplot(datos_rel_long, aes(x = CCTon, y = Valor, color = Variable)) +
  # Se eliminó geom_point para limpiar el fondo
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linewidth = 1.2) +
  
  # Líneas verticales de corte para formar el grupo de optimización relativo
  geom_vline(data = optimos_rel, aes(xintercept = CCTon_Optimo, color = Variable),
             linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
  
  # Marcadores de los puntos óptimos relativos
  geom_point(data = optimos_rel, aes(x = CCTon_Optimo, y = Valor_Maximo, color = Variable),
             size = 4, shape = 8, stroke = 2) +
  geom_text(data = optimos_rel, aes(x = CCTon_Optimo, y = Valor_Maximo,
                                    label = paste0("Ópt: ", round(CCTon_Optimo, 2))),
            vjust = -1.5, show.legend = FALSE, size = 3.5) +
  
  coord_cartesian(xlim = limites_x) +
  labs(title = "Curvas Relativas de TCH, Rendimiento y TAH vs CCTon",
       subtitle = "Rango de equilibrio e intersecciones clave",
       x = "CCTon", y = "Valor Relativo") +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

print(grafico_rel)





##################################################################################




# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)


datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)


# ==============================================================================
# 0. LIMPIEZA DE DATOS: Manejo de Continuas y Rangos
# ==============================================================================
# 1. Limpiar columnas que sí son puramente continuas
variables_continuas <- c("TCH", "Rendimiento", "TAH", "TCH_Rel", "Rend_Rel", "TAH_Rel", "CCTon")
datos[variables_continuas] <- lapply(datos[variables_continuas], function(x) {
  as.numeric(gsub(",", ".", as.character(x)))
})

# 2. Transformar la columna de rangos a su PUNTO MEDIO numérico
# Separa el texto por guiones o espacios y promedia los números encontrados
datos$Columna_R_Num <- sapply(strsplit(as.character(datos$Columna_R), "[-_a-zA-Z ]+"), function(x) {
  nums <- as.numeric(x[x != ""])
  if(length(nums) > 0) mean(nums, na.rm = TRUE) else NA
})

attach(datos)





# ==============================================================================
# 1. PARÁMETROS DE ENTRADA (Automatización)
# ==============================================================================
# Define aquí las variables y los factores que deseas iterar
variables_abs <- c("TCH", "Rendimiento", "TAH")
variables_rel <- c("TCH_Rel", "Rend_Rel", "TAH_Rel")
factores_x    <- c("CCTon")

# ==============================================================================
# 2. FUNCIÓN DE OPTIMIZACIÓN MATEMÁTICA
# ==============================================================================
obtener_optimo_real <- function(df, x_col, y_col) {
  # Ajustar el modelo polinómico de grado 2
  modelo <- lm(as.formula(paste(y_col, "~ poly(", x_col, ", 2, raw=TRUE)")), data = df)
  coefs <- coef(modelo)
  c <- coefs[1]
  b <- coefs[2]
  a <- coefs[3]
  
  # Extraer el rango real de los datos
  rango_x <- range(df[[x_col]], na.rm = TRUE)
  min_x <- rango_x[1]
  max_x <- rango_x[2]
  
  # Evaluar la ecuación en los extremos
  y_min_x <- a*(min_x^2) + b*min_x + c
  y_max_x <- a*(max_x^2) + b*max_x + c
  
  x_opt <- min_x
  y_opt <- y_min_x
  if (y_max_x > y_opt) {
    x_opt <- max_x
    y_opt <- y_max_x
  }
  
  # Evaluar el vértice si la curva es cóncava hacia abajo
  if (!is.na(a) && a < 0) {
    x_vertice <- -b / (2 * a)
    if (x_vertice >= min_x && x_vertice <= max_x) {
      y_vertice <- a*(x_vertice^2) + b*x_vertice + c
      if (y_vertice > y_opt) {
        x_opt <- x_vertice
        y_opt <- y_vertice
      }
    }
  }
  
  return(data.frame(Variable = y_col, Factor = x_col, Optimo_X = x_opt, Valor_Maximo = y_opt))
}

# ==============================================================================
# 3. BUCLE DE GENERACIÓN DE GRÁFICOS Y ANÁLISIS DE GRUPOS
# ==============================================================================

# Iterar sobre cada factor definido en factores_x
for (eje_x in factores_x) {
  
  cat("\n====================================================================\n")
  cat("ANALIZANDO FACTOR:", eje_x, "\n")
  cat("====================================================================\n")
  
  limites_x <- range(datos[[eje_x]], na.rm = TRUE)
  
  # ------------------------------------------------------------------
  # A. PROCESAMIENTO: VALORES ABSOLUTOS
  # ------------------------------------------------------------------
  optimos_abs_list <- lapply(variables_abs, function(var_y) obtener_optimo_real(datos, eje_x, var_y))
  optimos_abs <- bind_rows(optimos_abs_list)
  
  # Definir el Rango de Equilibrio (Grupo CC)
  grupo_min_abs <- min(optimos_abs$Optimo_X)
  grupo_max_abs <- max(optimos_abs$Optimo_X)
  
  cat("\n--- Óptimos Absolutos ---\n")
  print(optimos_abs)
  cat(sprintf("\n=> GRUPO DE EQUILIBRIO ABSOLUTO en %s: de %.2f a %.2f\n", eje_x, grupo_min_abs, grupo_max_abs))
  
  datos_abs_long <- datos %>%
    dplyr::select(all_of(c(eje_x, variables_abs))) %>%
    pivot_longer(cols = all_of(variables_abs), names_to = "Variable", values_to = "Valor")
  
  # Gráfico Absoluto
  grafico_abs <- ggplot(datos_abs_long, aes_string(x = eje_x, y = "Valor", color = "Variable")) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linewidth = 1.2) +
    geom_vline(data = optimos_abs, aes(xintercept = Optimo_X, color = Variable),
               linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
    geom_point(data = optimos_abs, aes(x = Optimo_X, y = Valor_Maximo, color = Variable),
               size = 4, shape = 8, stroke = 2) +
    geom_text(data = optimos_abs, aes(x = Optimo_X, y = Valor_Maximo,
                                      label = paste0("Ópt: ", round(Optimo_X, 2))),
              vjust = -1.5, show.legend = FALSE, size = 3.5) +
    # Sombrear la zona de equilibrio
    annotate("rect", xmin = grupo_min_abs, xmax = grupo_max_abs, ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "gray50") +
    coord_cartesian(xlim = limites_x) +
    labs(title = paste("Curvas de TCH, Rendimiento y TAH vs", eje_x),
         subtitle = paste("Rango de equilibrio (zona sombreada):", round(grupo_min_abs, 2), "-", round(grupo_max_abs, 2)),
         x = eje_x, y = "Valor Absoluto") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  
  print(grafico_abs)
  
  # ------------------------------------------------------------------
  # B. PROCESAMIENTO: VALORES RELATIVOS
  # ------------------------------------------------------------------
  optimos_rel_list <- lapply(variables_rel, function(var_y) obtener_optimo_real(datos, eje_x, var_y))
  optimos_rel <- bind_rows(optimos_rel_list)
  
  # Definir el Rango de Equilibrio Relativo (Grupo CC)
  grupo_min_rel <- min(optimos_rel$Optimo_X)
  grupo_max_rel <- max(optimos_rel$Optimo_X)
  
  cat("\n--- Óptimos Relativos ---\n")
  print(optimos_rel)
  cat(sprintf("\n=> GRUPO DE EQUILIBRIO RELATIVO en %s: de %.2f a %.2f\n\n", eje_x, grupo_min_rel, grupo_max_rel))
  
  datos_rel_long <- datos %>%
    dplyr::select(all_of(c(eje_x, variables_rel))) %>%
    pivot_longer(cols = all_of(variables_rel), names_to = "Variable", values_to = "Valor")
  
  # Gráfico Relativo
  grafico_rel <- ggplot(datos_rel_long, aes_string(x = eje_x, y = "Valor", color = "Variable")) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), se = FALSE, linewidth = 1.2) +
    geom_vline(data = optimos_rel, aes(xintercept = Optimo_X, color = Variable),
               linetype = "dashed", linewidth = 0.8, show.legend = FALSE) +
    geom_point(data = optimos_rel, aes(x = Optimo_X, y = Valor_Maximo, color = Variable),
               size = 4, shape = 8, stroke = 2) +
    geom_text(data = optimos_rel, aes(x = Optimo_X, y = Valor_Maximo,
                                      label = paste0("Ópt: ", round(Optimo_X, 2))),
              vjust = -1.5, show.legend = FALSE, size = 3.5) +
    # Sombrear la zona de equilibrio
    annotate("rect", xmin = grupo_min_rel, xmax = grupo_max_rel, ymin = -Inf, ymax = Inf, 
             alpha = 0.1, fill = "gray50") +
    coord_cartesian(xlim = limites_x) +
    labs(title = paste("Curvas Relativas de TCH, Rend. y TAH vs", eje_x),
         subtitle = paste("Rango de equilibrio e intersecciones (zona sombreada):", round(grupo_min_rel, 2), "-", round(grupo_max_rel, 2)),
         x = eje_x, y = "Valor Relativo") +
    theme_minimal() +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  
  print(grafico_rel)
}

