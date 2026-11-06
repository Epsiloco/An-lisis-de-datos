
                            # ANOVA Y PRUEBA DGC (InfoStat) EN R


# Carga e instalación de paquetes necesario 
# install.packages(c("ClustMC", "dplyr", "car", "clipr"))

library(ClustMC)  # Para prueba DGC
library(dplyr)    # Para manipular datos
library(car)      # QQ-plot, Levene
library(clipr)    # Copiar y pegar datos


# Carga de datos

datos <- read_clip_tbl() 
names(datos)

# Conversión a numérico y factor para ANOVA

datos$Tratamiento <- as.factor(datos$Tratamiento)
datos$Replica     <- as.factor(datos$Replica)
datos$TCH        <- as.numeric(datos$TCH)

# Supuestos de ANOVA

# Gráficos base
par(mfrow = c(1, 1))
boxplot(TCH ~ Tratamiento, data = datos, col = "red", ylab = "Brix")

# Modelo lineal de efectos fijos ANOVA: Diseño de bloques completos al azar 
andeva <- lm(TCH ~ Tratamiento + Replica, data = datos)
anova(andeva)

# Verificación de supuestos (Normalidad y Homocedasticidad)
qqPlot(andeva$residuals)
shapiro.test(andeva$residuals)
leveneTest(TCH ~ Tratamiento, data = datos, center = "mean")
bartlett.test(TCH ~ Tratamiento, data = datos)

# Prueba DGC

# console = FALSE: se coloca para evitar que la salida de los resultados en la consola sea por defecto
  # así poder usar una salida similar a INFOSTAT

dgc <- dgc_test(
  y         = andeva,
  trt       = "Tratamiento",
  alpha     = 0.05,
  show_plot = TRUE,
  console   = FALSE 
)

# Tabla de medias con letras de la prueba

    # Unir estadísticos y grupos de DGC
tabla_final <- merge(
  dgc$stats, 
  data.frame(treatment = rownames(dgc$groups), group = dgc$groups$group),
  by = "treatment"
)

# Obtener Error Estándar y ordenar de mayor a menor
cme <- anova(andeva)["Residuals", "Mean Sq"]

tabla_final <- tabla_final %>%
  mutate(
    EE = sqrt(cme / r),
    letra = LETTERS[group] # Asigna letras A, B, C...
  ) %>%
  arrange(desc(mean))      # Ordenar de mayor a menor media

# Dar formato e imprimir
tabla_mostrar <- tabla_final %>%
  select(
    Tratamiento = treatment, 
    Medias = mean, 
    n = r, 
    `E.E.` = EE, 
    Letra = letra
  ) %>%
  mutate(across(c(Medias, `E.E.`), ~round(., 2)))

print(tabla_mostrar, row.names = FALSE) # Resultados de la prueba DGC
