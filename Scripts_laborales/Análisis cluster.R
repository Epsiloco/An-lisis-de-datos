

# Análisis cluster



# ==========================================
# Cargar datos y definir número de clústeres
# ==========================================
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
attach(datos)
names(datos)

# ---> AQUÍ INDICAS CUÁNTOS GRUPOS QUIERES CREAR <---
k_grupos <- 5 

# ==========================================
# 1. ANÁLISIS CLÚSTER (K-MEANS)
# ==========================================

# Extraemos la columna como un vector numérico
datos_para_cluster <- datos$Hidro_100

# Aplicamos el algoritmo K-means 
set.seed(123) 
modelo_kmeans <- kmeans(datos_para_cluster, centers = k_grupos, nstart = 25)

# ---------------------------------------------------------
# NUEVO: ORDENAR GRUPOS PARA EVITAR TRASLAPES Y SER CONSISTENTES
# ---------------------------------------------------------
# Rankeamos los centros para que el Grupo 1 sea siempre el de menor media
mapeo_orden <- rank(modelo_kmeans$centers)

# Añadimos la asignación del clúster ya ordenado a nuestro dataframe
datos$Grupo <- as.factor(mapeo_orden[modelo_kmeans$cluster])


# ==========================================
# 2. VISUALIZACIÓN BÁSICA
# ==========================================

plot(datos$Hidro_100, 
     col = datos$Grupo, 
     pch = 19, 
     main = paste("Análisis Clúster (k = ", k_grupos, ") para Hidro_100", sep=""), 
     ylab = "Valor de Hidro_100", 
     xlab = "Índice de observación")

# Agregamos las líneas de los centros dinámicamente, usando los centros ordenados
centros_ordenados <- sort(modelo_kmeans$centers)
abline(h = centros_ordenados, col = 1:k_grupos, lty = 2, lwd = 2)


# ==========================================
# 3. CÁLCULO DE LÍMITES SUPERIOR E INFERIOR (SIN TRASLAPE)
# ==========================================

# Creamos un cuadro resumen con los límites empíricos de cada grupo formado
limites_grupos <- data.frame(
  Grupo = 1:k_grupos,
  Limite_Inferior = as.numeric(tapply(datos$Hidro_100, datos$Grupo, min)),
  Centroide       = round(centros_ordenados, 4),
  Limite_Superior = as.numeric(tapply(datos$Hidro_100, datos$Grupo, max)),
  Observaciones   = as.numeric(table(datos$Grupo)),
  Minimo = as.numeric(tapply(datos$Hidro_100, datos$Grupo, min)),
  Máximo = as.numeric(tapply(datos$Hidro_100, datos$Grupo, max)),
  P25 = as.numeric(tapply(datos$Hidro_100, datos$Grupo, function(x) quantile(x, 0.25))),
  P50 = as.numeric(tapply(datos$Hidro_100, datos$Grupo, function(x) quantile(x, 0.50))),
  p75 = as.numeric(tapply(datos$Hidro_100, datos$Grupo, function(x) quantile(x, 0.75)))
)

cat("\n--- Límites y Centroides de los Clústeres ---\n")
print(limites_grupos)


# ==========================================
# 4. GRÁFICO ADICIONAL (Boxplot / Cajas y Bigotes)
# ==========================================

boxplot(Hidro_100 ~ Grupo, data = datos,
        col = rainbow(k_grupos, alpha = 0.6), 
        main = paste("Distribución de Hidro_100 en", k_grupos, "Clústeres"),
        xlab = "Clúster (Ordenado de menor a mayor)",
        ylab = "Valor de Hidro_100",
        border = "darkblue")


# ==========================================
# 5. DENDROGRAMA (Clúster Jerárquico)
# ==========================================

# Paso 1: Calcular la matriz de distancias
matriz_distancias <- dist(datos$Hidro_100, method = "euclidean")

# Paso 2: Crear el modelo jerárquico (método Ward)
modelo_jerarquico <- hclust(matriz_distancias, method = "ward.D2")

# Paso 3: Graficar el dendrograma
plot(modelo_jerarquico, 
     main = "Dendrograma de la variable Hidro_100", 
     xlab = "Índice de la observación", 
     ylab = "Distancia (Altura)",
     sub = "Método de agrupamiento: Ward",
     cex = 0.8) 

# Paso 4: Dibujar rectángulos rojos basados en tu variable 'k_grupos'
rect.hclust(modelo_jerarquico, k = k_grupos, border = "red")


# ==========================================
# 6. TABLA DE VALORES INCLUIDOS EN CADA GRUPO
# ==========================================

# Separamos los valores por grupo en una lista
lista_valores <- split(datos$Hidro_100, datos$Grupo)

# Calculamos la longitud máxima para rellenar con NA los grupos más pequeños
max_longitud <- max(sapply(lista_valores, length))

# Construimos la tabla (data.frame) igualando las filas
tabla_valores <- data.frame(lapply(lista_valores, function(x) {
  length(x) <- max_longitud
  return(x)
}))

# Asignamos nombres a las columnas para que se identifiquen por clúster
colnames(tabla_valores) <- paste("Grupo", 1:k_grupos)

cat("\n--- Tabla con los valores exactos asignados a cada grupo ---\n")
print(tabla_valores)














