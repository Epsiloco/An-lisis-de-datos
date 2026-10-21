df <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas ---\n")
print(names(df))
View(df)



############### Lineas con segmento
# ==============================================================================
# 1. SECCIÓN DE CONFIGURACIÓN (Modifica aquí tus variables e interruptores)
# ==============================================================================

# Nombre exacto de la columna/variable que quieres analizar
var_interes  <- "Espacio"  
eje_x_label  <- "Espacio vacío (m)" # Etiqueta para el eje X en el gráfico
titulo_graf  <- "Función de distribución acumulada empírica"

# Punto de origen en X donde nacen las líneas horizontales punteadas (segmentos)
x_inicio_segmento <- 1 

# CONFIGURACIÓN DE LÍMITES E INTERRUPTORES
# Puedes añadir o quitar bloques 'list()' según necesites. 
# Interruptores: 
#   - rotulo   = TRUE/FALSE (Muestra u oculta la etiqueta de probabilidad)
#   - segmento = TRUE/FALSE (Muestra u oculta la línea horizontal punteada)
config_limites <- list(
  list(
    valor       = 1.0,         # Valor de corte en X
    color_linea = "#27ae60",   # Color de la línea vertical y punto
    color_texto = "#1e8449",   # Color del texto del rótulo
    rotulo      = FALSE,        # ¡INTERRUPTOR DE RÓTULO!
    segmento    = FALSE,       # ¡INTERRUPTOR DE SEGMENTO HORIZONTAL!
    vjust       = -0.5,        # Ajuste vertical del texto
    hjust       = -0.1         # Ajuste horizontal del texto
  ),
  list(
    valor       = 1.5, 
    color_linea = "#e67e22", 
    color_texto = "#d35400", 
    rotulo      = TRUE,        # Activado
    segmento    = TRUE,        # Activado
    vjust       = 1.8, 
    hjust       = 0.5
  ),
  list(
    valor       = 1.75, 
    color_linea = "#e74c3c", 
    color_texto = "#c0392b", 
    rotulo      = TRUE,       # DESACTIVADO: No mostrará texto en el gráfico
    segmento    = TRUE,        # Activado
    vjust       = -1.2, 
    hjust       = 0.5
  )
)

# ==============================================================================
# 2. SECCIÓN DE EJECUCIÓN AUTOMÁTICA (No requiere modificaciones)
# ==============================================================================

library(ggplot2)
library(scales)

# 1. Lectura de datos
df <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
cat("\n--- Variables detectadas en el portapapeles ---\n")
print(names(df))

# Validación de seguridad
if (!var_interes %in% names(df)) {
  stop(paste0("Error: La variable '", var_interes, "' no se encuentra en el dataframe."))
}
View(df)

# 2. Cálculo automático de la ECDF
fn_ecdf <- ecdf(df[[var_interes]])

# 3. Construcción dinámica de la tabla de límites y probabilidades
df_limites <- do.call(rbind, lapply(config_limites, function(item) {
  prob <- fn_ecdf(item$valor)
  data.frame(
    valor       = item$valor,
    color_linea = item$color_linea,
    color_texto = item$color_texto,
    rotulo      = item$rotulo,
    segmento    = item$segmento,
    vjust       = item$vjust,
    hjust       = item$hjust,
    prob        = prob,
    etiqueta    = paste0("P(≤", item$valor, ") = ", percent(prob, accuracy = 0.1)),
    stringsAsFactors = FALSE
  )
}))

# 4. Consola: Mostrar resultados calculados (reemplaza tus prints manuales del final)
cat("\n--- Resultados de Probabilidad Calculados ---\n")
for (i in 1:nrow(df_limites)) {
  cat(paste0("Valor de corte [", df_limites$valor[i], "] -> Probabilidad: ", 
             df_limites$prob[i], " (", df_limites$etiqueta[i], ")\n"))
}

# 5. Filtrado por interruptores
df_segmentos <- df_limites[df_limites$segmento == TRUE, ]
df_rotulos   <- df_limites[df_limites$rotulo == TRUE, ]

# 6. Construcción del gráfico dinámico con ggplot2
valores_subtitulo <- paste(df_limites$valor, collapse = ", ")
subtitulo_dinamico <- paste("n =", nrow(df), "| Límites en", valores_subtitulo)

g <- ggplot(df, aes(x = .data[[var_interes]])) +
  # ECDF base
  stat_ecdf(geom = "step", pad = TRUE, color = "#2980b9", linewidth = 1.2) +
  
  # Líneas verticales
  geom_vline(data = df_limites, aes(xintercept = valor, color = color_linea), 
             linetype = "dashed", linewidth = 0.9) +
  
  # Puntos de intersección
  geom_point(data = df_limites, aes(x = valor, y = prob, color = color_linea), size = 3)

# Aplicar segmentos horizontales dinámicamente si el interruptor está activo
if (nrow(df_segmentos) > 0) {
  g <- g + geom_segment(data = df_segmentos, 
                        aes(x = x_inicio_segmento, xend = valor, y = prob, yend = prob, color = color_linea),
                        linetype = "dotted", linewidth = 0.8)
}

# Aplicar rótulos/etiquetas dinámicamente si el interruptor está activo
if (nrow(df_rotulos) > 0) {
  g <- g + geom_label(data = df_rotulos, 
                      aes(x = valor, y = prob, label = etiqueta, color = color_texto, vjust = vjust, hjust = hjust), 
                      fill = "white", fontface = "bold", size = 3.2)
}

# Configuración estética final del gráfico
g <- g +
  scale_color_identity() + # Permite usar los colores HEX directo desde el dataframe
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) +
  labs(title = titulo_graf, subtitle = subtitulo_dinamico, x = eje_x_label, y = "Probabilidad acumulada") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title    = element_text(face = "bold", color = "#2c3e50"),
    plot.subtitle = element_text(color = "#7f8c8d", margin = margin(b = 15)),
    axis.title    = element_text(face = "bold", color = "#34495e"),
    axis.text     = element_text(color = "#7f8c8d"),
    panel.grid.major = element_line(color = "#ecf0f1", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    plot.margin   = margin(20, 20, 20, 20)
  )

# Desplegar el gráfico
print(g)
