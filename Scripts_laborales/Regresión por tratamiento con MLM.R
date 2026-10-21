paquetes <- c(
  "openxlsx", "fBasics", "psych", "modeest", "ggrepel", "GGally", 
  "mice", "corrplot", "readxl", "doebioresearch", "performance", "dplyr", 
  "ScottKnott", "agricolae", "car", "broom", "data.table", "emmeans", 
  "ggplot2", "tidyverse", "lattice", "nlme", "lme4", "lmerTest", "multcomp", 
  "rstatix", "ggpubr", "see", "MASS", "lsmeans", "scales", "lmtest", "multcompView", 
  "googlesheets4", "googledrive", "clipr", "FactoMineR", "factoextra", 
  "glmmTMB", "DHARMa", "MuMIn", "hnp", "effects", "sjstats", "ExpDes", "sf", "tmap", "terra",
  "RVAideMemoire", "RColorBrewer", "DiagrammeR", "esquisse", "dlookr")

no_instalados <- paquetes[!(paquetes %in% installed.packages()[,"Package"])];no_instalados

# Instalar los que faltan
if(length(no_instalados)) {
  install.packages(no_instalados)
} else {
  message("Todos los paquetes ya están instalados.")
}

lapply(paquetes, library, character.only = TRUE)




# Carga de datos

excel_sheets("Nanotecnología Agua Blanca 104 TCH.xlsx")
datos<- read_excel("Nanotecnología Agua Blanca 104 TCH.xlsx", sheet = "Labo")

# Opcional
datos <- read_clip_tbl()
attach(datos)
view(datos)
colnames(datos)

# Renombrar columnas
names(datos)[names(datos) == "Trat"] <- "Tratamiento"
names(datos)[names(datos) == "Rendimiento"] <- "Rendimiento.Kg.Ton"
names(datos)



# Filtrar 

colnames(datos)

datos_original <- datos 
datos <- datos_original # Reiniciar


# Filtrar por filas
filtrar_por_tratamiento <- function(factor = "Tratamiento",
                                    incluir = NULL,
                                    excluir = "Roundup 35.6 SL + Azumix (P, K, Mo + B), 1 L/Ha") {
  if (!factor %in% names(datos)) {
    stop(paste("La columna", factor, "no existe en el objeto 'datos'."))
  }
  
  df <- datos
  if (!is.null(incluir)) {
    df <- df %>% dplyr::filter(.data[[factor]] %in% incluir)
  }
  if (!is.null(excluir)) {
    df <- df %>% dplyr::filter(!(.data[[factor]] %in% excluir))
  }
  assign("datos", df, envir = .GlobalEnv)
}

filtrar_por_tratamiento() 

view(datos)






############################## Modelación #####################
modelo <- lmer(Rendimiento.Kg.Ton ~ Tratamiento * DDA + (1 | Replica/No.de.muestreo), data = datos)

summary(modelo)
Anova(modelo)

# Coeficientes y p-valores
pendientes_tratamientos <- emtrends(modelo, ~ Tratamiento, var = "DDA")

# Mostrar coeficientes, errores estándar, grados de libertad, estadístico t y valor p
summary(pendientes_tratamientos, infer = c(TRUE, TRUE))


# Generar los datos predichos por el modelo para el gráfico
predicciones <- ggpredict(modelo, terms = c("DDA [2, 15, 29, 36, 51]", "Tratamiento"))

# Gráfico
ggplot(predicciones, aes(x = x, y = predicted, color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  geom_line(linetype = "solid", linewidth = 1) + 

  geom_point(size = 3) +

  labs(
    title = "Curvas de Regresión de Rendimiento por Tratamiento",
    x = "DDA (Días Después de Aplicación)",
    y = "Rendimiento (Kg/Ton)",
    color = "Tratamiento",
    fill = "Tratamiento"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )


########## Modelo con p_valores ###################

library(sjPlot)
library(knitr)


# Salida más limpia
tab_model(modelo, 
          show.df = TRUE, 
          show.stat = TRUE, 
          p.style = "numeric_stars", # Muestra el valor p y las estrellitas de significancia
          title = "Resultados del Modelo Lineal Mixto: Rendimiento por Tratamiento",
          dv.labels = "Rendimiento (Kg/Ton)",
          string.pred = "Predictores",
          string.est = "Estimación")


pendientes_df <- as.data.frame(summary(pendientes_tratamientos, infer = c(TRUE, TRUE)))

kable(pendientes_df, 
      digits = 3, 
      caption = "Análisis de Pendientes por Tratamiento",
      col.names = c("Tratamiento", "Pendiente (DDA)", "Error Est.", "GL", "Lím. Inf. CI", "Lím. Sup. CI", "Valor t", "Valor p"))

pendientes_df

# Modelo y graficos
modelo <- lmer(Rendimiento.Kg.Ton ~ Tratamiento * DDA + (1 | Replica/No.de.muestreo), data = datos)


# Interceptos y pendientes
interceptos <- as.data.frame(emmeans(modelo, ~ Tratamiento, at = list(DDA = 0)))
pendientes <- as.data.frame(emtrends(modelo, ~ Tratamiento, var = "DDA"))

# Unificación para crear los modelos de regresión
ecuaciones_df <- merge(interceptos[, c("Tratamiento", "emmean")], 
                       pendientes[, c("Tratamiento", "DDA.trend")], 
                       by = "Tratamiento")
colnames(ecuaciones_df) <- c("Tratamiento", "Intercepto", "Pendiente")

ecuaciones_df$Ecuacion <- paste0("Y = ", round(ecuaciones_df$Intercepto, 2), 
                                 " + (", round(ecuaciones_df$Pendiente, 4), " * DDA)")


cat("\n--- Modelos de regresión lineal por tratamiento ---\n")
kable(ecuaciones_df[, c("Tratamiento", "Ecuacion")], align = "l")

# Prueba de Tukey
pendientes_tratamientos <- emtrends(modelo, ~ Tratamiento, var = "DDA")

letras_tukey <- multcomp::cld(pendientes_tratamientos, alpha = 0.15, Letters = letters, adjust = "tukey", decreasing = TRUE)
letras_tukey <- as.data.frame(letras_tukey)
# Limpiamos los espacios en blanco de las letras
letras_tukey$Letra <- trimws(letras_tukey$.group)

cat("\n--- RESULTADO PRUEBA DE TUKEY (Pendientes) ---\n")
kable(letras_tukey[, c("Tratamiento", "DDA.trend", "Letra")], 
      col.names = c("Tratamiento", "Pendiente", "Grupo Tukey"))

# Gráfico de los modelos de regresión

predicciones <- ggpredict(modelo, terms = c("DDA [2, 15, 29, 36, 51]", "Tratamiento"))

grafico_1 <- ggplot(predicciones, aes(x = x, y = predicted, color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  geom_line(linetype = "solid", linewidth = 1) + 
  geom_point(size = 3) +
  labs(title = "Curvas de Regresión de Rendimiento por Tratamiento",
       x = "DDA (Días Después de Aplicación)", y = "Rendimiento (Kg/Ton)",
       color = "Tratamiento", fill = "Tratamiento") +
  theme_minimal() +
  theme(legend.position = "right", panel.grid.minor = element_blank())

print(grafico_1)

# Gráfico con las letras de Tukey

max_dda <- max(predicciones$x)
datos_letras <- predicciones[predicciones$x == max_dda, ]
datos_letras <- merge(datos_letras, letras_tukey, by.x = "group", by.y = "Tratamiento")

grafico_2 <- ggplot(predicciones, aes(x = x, y = predicted, color = group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.15, color = NA) +
  geom_line(linetype = "solid", linewidth = 1) + 
  geom_point(size = 3) +
  # Añadimos las letras de Tukey al final de las líneas
  geom_text(data = datos_letras, aes(x = x, y = predicted, label = Letra), 
            hjust = -0.5, vjust = 0.5, size = 5, fontface = "bold", show.legend = FALSE) +
  labs(title = "Curvas de Regresión por Tratamiento (con Prueba de Tukey)",
       subtitle = "Letras distintas indican diferencias significativas en la pendiente (tasa de crecimiento)",
       x = "DDA (Días Después de Aplicación)", y = "Rendimiento (Kg/Ton)",
       color = "Tratamiento", fill = "Tratamiento") +
  # Expandimos un poco el eje X para que las letras no se corten
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) + 
  theme_minimal() +
  theme(legend.position = "right", panel.grid.minor = element_blank())

print(grafico_2)




############################## Modelo Consolidado (Pendiente Fija) #####################
library(lme4)
library(lmerTest)
library(emmeans)
library(multcomp)
library(ggplot2)
library(knitr)
library(performance)
library(ggrepel)

# 1. Ajuste del Modelo (Pendiente general para todos los tratamientos)
modelo_cons <- lmer(Rendimiento.Kg.Ton ~ Tratamiento + DDA + (1 | Replica/No.de.muestreo), data = datos)

# 2. Métricas de Ajuste Global
aic_val <- round(AIC(modelo_cons), 2)
vif_dda <- round(check_collinearity(modelo_cons)$VIF[1], 2)

# 3. Modelos por Tratamiento (Intercepto propio + Pendiente fija)
# Extraemos la estadística de la pendiente general
pend_stats <- as.data.frame(summary(emtrends(modelo_cons, ~ 1, var = "DDA"), infer = TRUE))
pendiente_gen <- pend_stats$DDA.trend
p_valor_gen <- pend_stats$p.value

interceptos <- as.data.frame(emmeans(modelo_cons, ~ Tratamiento, at = list(DDA = 0)))

# Crear tabla de ecuaciones consolidada
ecuaciones_df <- data.frame(
  Tratamiento = interceptos$Tratamiento,
  Intercepto = round(interceptos$emmean, 2),
  Pendiente = round(pendiente_gen, 4),
  P_valor_DDA = round(p_valor_gen, 5),
  Ecuacion = paste0("Y = ", round(interceptos$emmean, 2), " + (", round(pendiente_gen, 4), " * DDA)"),
  AIC = aic_val,
  VIF_DDA = vif_dda
)

cat("\n--- Ecuaciones con Pendiente General y Métricas ---\n")
kable(ecuaciones_df[, c("Tratamiento", "Ecuacion", "P_valor_DDA", "AIC", "VIF_DDA")], align = "l")

# 4. Prueba de Tukey (Comparación de interceptos/altura de modelos)
medias_ajustadas <- emmeans(modelo_cons, ~ Tratamiento)
letras_tukey <- multcomp::cld(medias_ajustadas, alpha = 0.05, Letters = letters, adjust = "tukey", decreasing = TRUE)
letras_tukey <- as.data.frame(letras_tukey)
letras_tukey$Letra <- trimws(letras_tukey$.group)

# 5. Preparación de datos para el gráfico
predicciones <- ggpredict(modelo_cons, terms = c("DDA [all]", "Tratamiento"))

# Ubicar letras al final con repulsión
max_x <- max(predicciones$x)
datos_letras <- predicciones[predicciones$x == max_x, ]
datos_letras <- merge(datos_letras, letras_tukey, by.x = "group", by.y = "Tratamiento")

# 6. Gráfico de Alta Claridad
ggplot(predicciones, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_point(data = predicciones[predicciones$x %in% unique(datos$DDA), ], size = 2.5) +
  
  # Uso de ggrepel para que las letras no choquen entre sí
  geom_text_repel(data = datos_letras, 
                  aes(x = x, y = predicted, label = Letra),
                  nudge_x = 7, 
                  direction = "y", 
                  size = 6, 
                  fontface = "bold", 
                  segment.color = NA,
                  show.legend = FALSE) +
  
  labs(title = "Modelos de Regresión Consolidados (Pendiente Fija)",
       subtitle = "Mismo coeficiente angular para todos los tratamientos (Letras Tukey: a = mejor)",
       x = "Días Después de Aplicación (DDA)", 
       y = "Rendimiento (Kg/Ton)",
       color = "Tratamiento") +
  
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.25))) + 
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )












