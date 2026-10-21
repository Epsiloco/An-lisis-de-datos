
# =====================================================================
# 1. CARGA DE LIBRERÍAS
# =====================================================================
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(ggplot2)

# --- LECTURA DE DATOS ---
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

# =====================================================================
# 2. CONFIGURACIÓN DEL USUARIO
# =====================================================================
VARIABLE_RESPUESTA  <- "KG.T_CORE_REAL"      
TRATAMIENTO         <- "Tipo.Manejo"         
FACTOR_AGRUPACION   <- "Variedad..Agrícola." 
NIVEL_SIGNIFICANCIA <- 0.15                  

# Limpieza y aseguramiento de formatos
datos[[TRATAMIENTO]] <- as.factor(datos[[TRATAMIENTO]])
datos[[FACTOR_AGRUPACION]] <- as.factor(datos[[FACTOR_AGRUPACION]])
datos[[VARIABLE_RESPUESTA]] <- as.numeric(datos[[VARIABLE_RESPUESTA]])

cat("\n=====================================================================")
cat("\n ANALIZANDO:", VARIABLE_RESPUESTA, "POR", TRATAMIENTO, "Y", FACTOR_AGRUPACION)
cat("\n=====================================================================\n")

# =====================================================================
# 3. MODELO MIXTO CORREGIDO
# =====================================================================
formula_mixta <- as.formula(
  paste(VARIABLE_RESPUESTA, "~", TRATAMIENTO, "*", FACTOR_AGRUPACION, 
        "+ (1 | cuadrante / Tipo.de.corte / Madurante)") # Madurante agregado
)

modelo_mixto <- lmer(formula_mixta, data = datos)

cat("\n> ANOVA DEL MODELO MIXTO:\n")
print(anova(modelo_mixto))

# =====================================================================
# 4. COMPARACIÓN DE MEDIAS GENERAL (TRATAMIENTO)
# =====================================================================
cat("\n> CUADRO: COMPARACIÓN GENERAL (PRUEBA DE FISHER - LSD, ALPHA = 15%)\n")

# A. Extraemos medias y letras (reversed = TRUE asigna 'a' al mayor)
medias_general <- emmeans(modelo_mixto, as.formula(paste("~", TRATAMIENTO)), lmer.df = "satterthwaite")
cuadro_general <- multcomp::cld(medias_general, alpha = NIVEL_SIGNIFICANCIA, adjust = "none", Letters = letters, reversed = TRUE)
df_general <- as.data.frame(cuadro_general)
df_general$.group <- trimws(df_general$.group)

# B. Calculamos la Media Real Aritmética
medias_reales_gen <- tapply(datos[[VARIABLE_RESPUESTA]], datos[[TRATAMIENTO]], mean, na.rm = TRUE)
df_general$Media_Real <- medias_reales_gen[as.character(df_general[[TRATAMIENTO]])]

# C. Reordenamos las columnas para mayor claridad
df_general <- df_general[, c(TRATAMIENTO, "Media_Real", "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")]
names(df_general)[3] <- "Media_Ajustada_Modelo" # Renombramos emmean

print(df_general)

# =====================================================================
# 5. GRÁFICO DE LÍNEAS HORIZONTALES (NIVEL GENERAL)
# =====================================================================
# Etiqueta usando la media real
df_general$etiqueta <- paste0(round(df_general$Media_Real, 1), " ", df_general$.group)

grafico <- ggplot(df_general, aes_string(x = "Media_Real", y = TRATAMIENTO)) +
  # Barras de error del modelo
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL), height = 0.2, color = "#2C3E50", linewidth = 0.8) +
  # Punto representando la Media Real
  geom_point(color = "#E74C3C", size = 3) +
  # Texto con la Media Real y la letra
  geom_text(aes(label = etiqueta), vjust = -1.5, color = "black", fontface = "bold") +
  labs(title = paste("Medias Reales:", TRATAMIENTO),
       subtitle = paste("Prueba Fisher (LSD). Alpha =", NIVEL_SIGNIFICANCIA * 100, "%. Barras = IC Modelo"),
       x = paste("Media Real de", VARIABLE_RESPUESTA),
       y = TRATAMIENTO) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(grafico)

# =====================================================================
# 6. COMPARACIÓN CLASIFICADA POR EL FACTOR (VARIEDAD)
# =====================================================================
cat("\n> CUADRO: COMPARACIÓN POR", toupper(FACTOR_AGRUPACION), "(PRUEBA DE FISHER - LSD, ALPHA = 15%)\n")

# A. Extraemos medias y letras por variedad
formula_interaccion <- as.formula(paste("~", TRATAMIENTO, "|", FACTOR_AGRUPACION))
medias_factor <- emmeans(modelo_mixto, formula_interaccion, lmer.df = "satterthwaite")

cuadro_factor <- multcomp::cld(medias_factor, alpha = NIVEL_SIGNIFICANCIA, adjust = "none", Letters = letters, reversed = TRUE)
df_factor <- as.data.frame(cuadro_factor)
df_factor$.group <- trimws(df_factor$.group)

# B. Calculamos la Media Real Aritmética para cada interacción (Tratamiento x Variedad)
raw_factor <- aggregate(datos[[VARIABLE_RESPUESTA]], 
                        by = list(datos[[TRATAMIENTO]], datos[[FACTOR_AGRUPACION]]), 
                        FUN = mean, na.rm = TRUE)
colnames(raw_factor) <- c(TRATAMIENTO, FACTOR_AGRUPACION, "Media_Real")

# C. Unimos y reordenamos las tablas
df_factor <- merge(df_factor, raw_factor, by = c(TRATAMIENTO, FACTOR_AGRUPACION))
df_factor <- df_factor[, c(TRATAMIENTO, FACTOR_AGRUPACION, "Media_Real", "emmean", "SE", "df", "lower.CL", "upper.CL", ".group")]
names(df_factor)[4] <- "Media_Ajustada_Modelo"

print(df_factor)

# =====================================================================
# D. COMPARACIONES ESPECÍFICAS (CONTRASTES POR PARES)
# =====================================================================
cat("\n> COMPARACIONES ESPECÍFICAS: PARES DE", toupper(TRATAMIENTO), "DENTRO DE CADA", toupper(FACTOR_AGRUPACION), "\n")

# Usamos pairs() para obtener los p-values exactos de cada comparación 
# adjust = "none" mantiene la lógica de Fisher LSD (sin castigo)
comparaciones_especificas <- pairs(medias_factor, adjust = "none")

# Imprimimos el resultado
print(comparaciones_especificas)

tab_df


# =====================================================================
# D. COMPARACIONES ESPECÍFICAS (CONTRASTES Y LETRAS)
# =====================================================================
cat("\n> COMPARACIONES ESPECÍFICAS: PARES DE", toupper(TRATAMIENTO), "DENTRO DE CADA", toupper(FACTOR_AGRUPACION), "\n")

# 1. Obtenemos los contrastes (p-values exactos de las diferencias)
comparaciones_especificas <- pairs(medias_factor, adjust = "none")
cat("\n--- Valores p de las diferencias (Contrastes) ---\n")
print(comparaciones_especificas)

# 2. Mostramos las letras correspondientes a estas comparaciones específicas
cat("\n--- Letras asignadas a cada tratamiento por variedad ---\n")

# Filtramos solo las columnas clave para una lectura rápida desde el df_factor que ya calculamos
resumen_letras <- df_factor[, c(FACTOR_AGRUPACION, TRATAMIENTO, "Media_Real", ".group")]

# Renombramos la columna del grupo para mayor claridad
names(resumen_letras)[4] <- "Letra_Significancia"

# Ordenamos la tabla: primero por Variedad (alfabético) y luego por Media Real (de mayor a menor)
resumen_letras <- resumen_letras[order(resumen_letras[[FACTOR_AGRUPACION]], -resumen_letras$Media_Real), ]

# Limpiamos el nombre de las filas para que la tabla se vea limpia en la consola
rownames(resumen_letras) <- NULL

print(resumen_letras)




# Darle formato
tab_df(resumen_letras)
