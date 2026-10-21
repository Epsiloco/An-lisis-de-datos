# Cargar las librerías necesarias
if(!require(plotly)) install.packages("plotly")
if(!require(dplyr)) install.packages("dplyr")

library(plotly)
library(dplyr)

# ---------------------------------------------------------
# 1. CARGA DE DATOS Y CONFIGURACIÓN SIMPLE
# ---------------------------------------------------------

# Carga de datos desde el portapapeles
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
names(datos)

# Coloca dentro de c() los nombres exactos de tus columnas en este orden:
# 1° Variable de respuesta (Eje Y)
# 2° Factor principal (Eje X)
# 3° Factor de interacción (El que hará las líneas de colores)

mis_variables <- c("Individuo.Tallo", "Metodo", "Riego")

# ---------------------------------------------------------
# 2. PROCESAMIENTO (No necesitas tocar nada aquí)
# ---------------------------------------------------------

# Extraemos los nombres
y_resp <- mis_variables[1]
x_fact <- mis_variables[2]
g_fact <- mis_variables[3]

# Aseguramos que los factores sean leídos como categorías
datos[[x_fact]] <- as.factor(datos[[x_fact]])
datos[[g_fact]] <- as.factor(datos[[g_fact]])

# Calculamos las medias automáticamente usando dplyr
datos_medias <- datos %>%
  group_by(.data[[x_fact]], .data[[g_fact]]) %>%
  summarise(Media = mean(.data[[y_resp]], na.rm = TRUE), .groups = 'drop')

# ---------------------------------------------------------
# 3. GRÁFICO CON PLOTLY
# ---------------------------------------------------------

# Creamos el gráfico interactivo
grafico_plotly <- plot_ly(
  data = datos_medias,
  x = ~get(x_fact),
  y = ~Media,
  color = ~get(g_fact),
  type = 'scatter',
  mode = 'lines+markers', # Muestra líneas conectando los puntos
  line = list(width = 3),
  marker = list(size = 10)
) %>%
  layout(
    title = list(text = paste("<b>Gráfico de Interacción</b><br><sup>", y_resp, "según", x_fact, "y", g_fact, "</sup>")),
    xaxis = list(title = x_fact),
    yaxis = list(title = paste("Media de", y_resp)),
    legend = list(title = list(text = paste("<b>", g_fact, "</b>"))),
    hovermode = "x unified" # Muestra la información de todos los grupos al pasar el cursor
  )

# Imprimir el gráfico
grafico_plotly
