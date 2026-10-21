
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "") # Pestaña: Precosecha
print(names(datos))


# ==============================================================================
# ANÁLISIS ESTADÍSTICO: EFECTO del NIVEL DE DAÑO (ND) EN VARIABLES DE CALIDAD
# (Script corregido para evitar conflictos de paquetes)
# ==============================================================================

# 1. PREPARACIÓN DEL ENTORNO Y DATOS
# ------------------------------------------------------------------------------
paquetes <- c("tidyverse", "corrplot", "ggpubr", "rstatix", "MASS", "ordinal", 
              "vegan", "FactoMineR", "factoextra", "car", "performance", "clinfun")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# Asegurarnos de que ND sea un factor ordenado
datos$ND <- as.factor(datos$ND)

# Definir el vector de variables de calidad
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Reductor", "Fibra", "Pol_caña", 
                  "Jugo", "Dextrana")

# Limpieza básica: eliminar filas con NA en las variables de interés
# Usamos R base para evitar el conflicto con MASS::select
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])

cat("Dimensiones del dataframe original:", dim(datos), "\n")
cat("Dimensiones después de eliminar NAs:", dim(datos_limpios), "\n")

# ==============================================================================
# 2. ANÁLISIS DE CORRELACIÓN NO PARAMÉTRICA (Spearman y Kendall)
# ==============================================================================

matriz_spearman <- cor(datos_limpios[, vars_calidad], method = "spearman")

corrplot(matriz_spearman, 
         method = "circle", 
         type = "lower", 
         order = "hclust", 
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.6,
         title = "Correlograma Spearman: Calidad vs ND",
         mar = c(0,0,2,0))

matriz_kendall <- cor(datos_limpios[, vars_calidad], method = "kendall")

# ==============================================================================
# 3. ANÁLISIS DE COMPARACIÓN DE GRUPOS (Kruskal-Wallis y Post-hoc Dunn)
# ==============================================================================
# Usamos dplyr:: explicitamente para evitar conflictos

resultados_kw <- datos_limpios %>%
  tidyr::pivot_longer(cols = all_of(vars_calidad), names_to = "Variable", values_to = "Valor") %>%
  dplyr::group_by(Variable) %>%
  rstatix::kruskal_test(Valor ~ ND) %>%
  rstatix::adjust_pvalue(method = "BH") %>%
  rstatix::add_significance()

print("Resultados de Kruskal-Wallis:")
print(resultados_kw)

vars_significativas <- resultados_kw %>% 
  dplyr::filter(p.adj < 0.05) %>% 
  dplyr::pull(Variable)

if(length(vars_significativas) > 0) {
  cat("\nRealizando Post-hoc de Dunn para variables significativas...\n")
  
  resultados_dunn <- datos_limpios %>%
    tidyr::pivot_longer(cols = all_of(vars_significativas), names_to = "Variable", values_to = "Valor") %>%
    dplyr::group_by(Variable) %>%
    rstatix::dunn_test(Valor ~ ND, p.adjust.method = "BH")
  
  print(resultados_dunn)
} else {
  cat("\nNinguna variable mostró diferencias significativas en Kruskal-Wallis.\n")
}

# ==============================================================================
# 4. ANÁLISIS DE TENDENCIA ESPECÍFICA (Jonckheere-Terpstra)
# ==============================================================================
cat("\n--- Prueba de Tendencia de Jonckheere-Terpstra ---\n")
for (v in vars_calidad) {
  nd_num <- as.numeric(as.character(datos_limpios$ND))
  test_jt <- clinfun::jonckheere.test(datos_limpios[[v]], nd_num, alternative = "two.sided")
  cat(sprintf("Variable: %s | p-value: %.4f\n", v, test_jt$p.value))
}

# ==============================================================================
# 5. MODELOS LINEALES: EFECTO DEL ND SOBRE LA CALIDAD (ND como predictor)
# ==============================================================================
cat("\n--- Efecto del ND sobre cada variable de calidad (Regresión Lineal) ---\n")

# Crear una función para correr el modelo y extraer resultados
analizar_efecto <- function(data, var_respuesta) {
  form <- as.formula(paste(var_respuesta, "~ ND"))
  modelo <- lm(form, data = data)
  res <- summary(modelo)
  
  # Extraer coeficientes
  coefs <- as.data.frame(res$coefficients)
  coefs$Variable <- var_respuesta
  coefs$Termino <- rownames(coefs)
  
  # Calcular intervalos de confianza
  ic <- confint(modelo)
  coefs$IC_2.5 <- ic[,1]
  coefs$IC_97.5 <- ic[,2]
  
  return(coefs)
}

# Aplicar a todas las variables de calidad
resultados_lm <- map_dfr(vars_calidad, ~analizar_efecto(datos_limpios, .x))

# Mostrar solo los efectos significativos del ND (ignorando el intercepto)
efectos_nd <- resultados_lm %>% 
  dplyr::filter(grepl("ND", Termino)) %>%
  dplyr::filter(`Pr(>|t|)` < 0.05)

print("Efectos significativos del Nivel de Daño sobre la Calidad:")
print(efectos_nd)

# ==============================================================================
# 6. ANÁLISIS MULTIVARIADO (PERMANOVA y PCA)
# ==============================================================================
cat("\n--- PERMANOVA (Adonis) ---\n")
permanova_res <- vegan::adonis2(datos_limpios[, vars_calidad] ~ ND, 
                                data = datos_limpios, 
                                method = "euclidean", 
                                permutations = 999)
print(permanova_res)

cat("\n--- PCA ---\n")
pca_res <- FactoMineR::PCA(datos_limpios[, vars_calidad], graph = FALSE)

factoextra::fviz_pca_ind(pca_res,
                         geom.ind = "point",
                         col.ind = datos_limpios$ND, 
                         palette = c("#00AFBB", "#E7B800", "#FC4E07", "#8B0000"), 
                         addEllipses = TRUE, 
                         legend.title = "Nivel de Daño",
                         title = "PCA: Perfil de Calidad según Nivel de Daño"
)

factoextra::fviz_pca_var(pca_res,
                         col.var = "contrib", 
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE, 
                         title = "Contribución de Variables al PCA"
)

# ==============================================================================
# 7. GRÁFICAS DE TENDENCIA MEJORADAS
# ==============================================================================
graficar_variable <- function(data, var_name) {
  p <- ggplot(data, aes(x = ND, y = .data[[var_name]], group = 1)) +
    stat_summary(fun = mean, geom = "line", color = "steelblue", size = 1) +
    stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, color = "steelblue") +
    stat_summary(fun = mean, geom = "point", size = 3, color = "steelblue") +
    labs(title = var_name, x = "Nivel de Daño (ND)", y = "Media ± EE") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"))
  
  kw_p <- resultados_kw %>% dplyr::filter(Variable == var_name) %>% dplyr::pull(p.adj)
  if(length(kw_p) > 0 && kw_p < 0.05) {
    p <- p + annotate("text", x = 1, y = max(data[[var_name]], na.rm=TRUE), 
                      label = paste("KW p =", round(kw_p, 4)), 
                      hjust = 0, vjust = 1, color = "red", fontface = "italic")
  }
  return(p)
}

lista_graficos <- lapply(vars_calidad, function(v) graficar_variable(datos_limpios, v))
names(lista_graficos) <- vars_calidad

print(lista_graficos$Dextrana)
print(lista_graficos$Brix)
print(lista_graficos$Pureza)



# ==============================================================================
# GRÁFICO DE IMPACTO RELATIVO (CAMBIO PORCENTUAL RESPECTO AL ND1)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Reconstruir la tabla de resultados (la que me mostraste)
# Nota: Excluimos Dextrana por el problema de escala que ya identificamos.
resultados <- data.frame(
  Variable = c(rep("Peso_tallo", 2), rep("Altura", 2), "Brix", "Pol", "Humedad", 
               "Pol_Jugo", "Pureza", "Pol_caña"),
  Termino = c("ND3", "ND4", "ND3", "ND4", "ND4", "ND4", "ND4", "ND4", "ND4", "ND4"),
  Estimate = c(-0.3066, -0.2991, -0.3047, -0.2833, -3.3091, -14.0900, 3.5500, 
               -3.3041, -4.2825, -2.7700),
  IC_2.5 = c(-0.4519, -0.4676, -0.5198, -0.5329, -5.2325, -22.5212, 1.0818, 
             -5.2766, -7.7795, -4.3896),
  IC_97.5 = c(-0.1614, -0.1306, -0.0896, -0.0337, -1.3858, -5.6587, 6.0181, 
              -1.3316, -0.7854, -1.1503)
)

# 2. Calcular la media del ND1 (Línea base) para cada variable en tus datos originales
# Esto es crucial para calcular el porcentaje real
medias_nd1 <- datos_limpios %>%
  filter(ND == "1") %>%
  summarise(across(all_of(unique(resultados$Variable)), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Media_ND1")

# 3. Unir los resultados con las medias y calcular el porcentaje
resultados_pct <- resultados %>%
  left_join(medias_nd1, by = "Variable") %>%
  mutate(
    # Calcular el cambio porcentual: (Estimado / Media_ND1) * 100
    Pct_Cambio = (Estimate / Media_ND1) * 100,
    Pct_IC_2.5 = (IC_2.5 / Media_ND1) * 100,
    Pct_IC_97.5 = (IC_97.5 / Media_ND1) * 100
  )

# 4. Crear el gráfico de barras/lollipop porcentual
grafico_pct <- ggplot(resultados_pct, aes(x = Pct_Cambio, y = Variable, color = Termino)) +
  
  # Línea base (0% de cambio = ND1)
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
  
  # Barras de error (Intervalos de confianza en porcentaje)
  geom_errorbarh(aes(xmin = Pct_IC_2.5, xmax = Pct_IC_97.5), height = 0.2, size = 0.8) +
  
  # Puntos (Cambio porcentual estimado)
  geom_point(size = 4) +
  
  # Separar por nivel de daño (ND3 y ND4)
  facet_wrap(~ Termino, ncol = 2, scales = "free_x") +
  
  # Colores y estética
  scale_color_manual(values = c("ND3" = "#E69F00", "ND4" = "#D55E00")) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.major.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  ) +
  
  # Etiquetas
  labs(
    title = "Impacto Relativo del Nivel de Daño en la Calidad del Jugo",
    subtitle = "Cambio porcentual (%) respecto al Nivel de Daño 1 (Línea base = 0%)",
    x = "Cambio Porcentual (%)",
    y = NULL
  )

# 5. Mostrar y guardar
print(grafico_pct)
ggsave("Impacto_Porcentual_ND.png", grafico_pct, width = 10, height = 6, dpi = 300)




# ==============================================================================
# OPCIÓN 1: MAPA DE CALOR GERENCIAL (SEMÁFORO)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# 1. Preparar los datos con nombres gerenciales
# (Usando los datos de tu tabla anterior, excluyendo Dextrana)
datos_gerenciales <- data.frame(
  Variable = c(rep("Peso del Tallo", 2), rep("Altura", 2), "Brix", "Pol", "Humedad", 
               "Pol_Jugo", "Pureza", "Pol_caña"),
  Nivel = c("Daño Moderado (ND3)", "Daño Severo (ND4)", "Daño Moderado (ND3)", "Daño Severo (ND4)", 
            "Daño Severo (ND4)", "Daño Severo (ND4)", "Daño Severo (ND4)", "Daño Severo (ND4)", 
            "Daño Severo (ND4)", "Daño Severo (ND4)"),
  Pct_Cambio = c(-17.0, -16.6, -16.6, -15.5, -20.0, -20.1, 4.8, -28.5, -5.2, -23.9) # Valores aproximados de tu gráfico
)

# Asegurar el orden de los niveles
datos_gerenciales$Nivel <- factor(datos_gerenciales$Nivel, levels = c("Daño Moderado (ND3)", "Daño Severo (ND4)"))

# 2. Crear el Mapa de Calor
plot_gerencial <- ggplot(datos_gerenciales, aes(x = Nivel, y = reorder(Variable, Pct_Cambio), fill = Pct_Cambio)) +
  # Cuadrículas del mapa
  geom_tile(color = "white", size = 1) +
  # Texto dentro de las celdas (con formato de porcentaje)
  geom_text(aes(label = sprintf("%+.1f%%", Pct_Cambio)), 
            color = "black", size = 5, fontface = "bold") +
  # Escala de colores: Rojo para pérdidas, Verde para ganancias
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850", 
                       midpoint = 0, limits = c(-35, 10),
                       name = "Impacto en Calidad") +
  # Estética limpia y ejecutiva
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30")
  ) +
  labs(
    title = "Impacto del Daño de Cochinilla en la Calidad del Jugo",
    subtitle = "Variación porcentual respecto a plantas sanas (Testigo)",
    x = "", 
    y = ""
  )

print(plot_gerencial)



# ==============================================================================
# OPCIÓN 2: GRÁFICO DE BARRAS COMPARATIVO (SIN JERGA ESTADÍSTICA)
# ==============================================================================

# 1. Usamos los mismos datos gerenciales del paso anterior

# 2. Crear gráfico de barras agrupadas
plot_barras <- ggplot(datos_gerenciales, aes(x = reorder(Variable, Pct_Cambio), y = Pct_Cambio, fill = Nivel)) +
  # Barras
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Línea base en 0
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  # Texto con el valor exacto
  geom_text(aes(label = sprintf("%.0f%%", Pct_Cambio)), 
            position = position_dodge(width = 0.8), 
            vjust = ifelse(datos_gerenciales$Pct_Cambio < 0, 1.5, -0.5), 
            size = 4, fontface = "bold") +
  # Invertir ejes para leer mejor
  coord_flip() +
  # Colores corporativos
  scale_fill_manual(values = c("Daño Moderado (ND3)" = "#F4A582", "Daño Severo (ND4)" = "#B2182B")) +
  # Estética limpia
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.y = element_blank(),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30")
  ) +
  labs(
    title = "Pérdida de Calidad por Nivel de Daño",
    subtitle = "Porcentaje de reducción en comparación con plantas sanas (Testigo)",
    x = "",
    y = "Pérdida de Calidad (%)"
  )

print(plot_barras)

# ==============================================================================
# MAPA DE CALOR GERENCIAL: PÉRDIDAS Y PUNTO DE QUIEBRE ESTADÍSTICO
# ==============================================================================

library(tidyverse)
library(ggplot2)

# 1. Definir variables (Excluimos Dextrana por el problema de escala que ya vimos)
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

# Asegurarnos de que ND sea factor
datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])

# 2. Calcular la media del ND1 (Línea base o Testigo)
medias_nd1 <- datos_limpios %>%
  filter(ND == "1") %>%
  summarise(across(all_of(vars_calidad), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Media_ND1")

# 3. Función para extraer resultados de la regresión lineal por variable
obtener_resultados <- function(data, var_name) {
  form <- as.formula(paste(var_name, "~ ND"))
  modelo <- lm(form, data = data)
  res <- summary(modelo)$coefficients
  
  # Extraer solo los coeficientes de ND2, ND3, ND4
  coefs <- as.data.frame(res)
  coefs$Termino <- rownames(coefs)
  coefs <- coefs %>% filter(grepl("ND", Termino))
  
  # Si no hay suficientes niveles, crear filas vacías
  if(nrow(coefs) == 0) return(NULL)
  
  coefs$Variable <- var_name
  coefs$Estimate <- coefs[, "Estimate"]
  coefs$P_value <- coefs[, "Pr(>|t|)"]
  
  return(coefs[, c("Variable", "Termino", "Estimate", "P_value")])
}

# Aplicar a todas las variables
resultados_lm <- map_dfr(vars_calidad, ~obtener_resultados(datos_limpios, .x))

# 4. Calcular el cambio porcentual y la significancia
datos_grafico <- resultados_lm %>%
  left_join(medias_nd1, by = "Variable") %>%
  mutate(
    # Calcular % de cambio
    Pct_Cambio = (Estimate / Media_ND1) * 100,
    
    # Crear etiqueta de significancia (El "Punto de Quiebre")
    Significancia = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      TRUE            ~ "ns" # No significativo
    ),
    
    # Limpiar nombre del término (ND2, ND3, ND4)
    Nivel_Daño = case_when(
      Termino == "ND2" ~ "Daño Leve (ND2)",
      Termino == "ND3" ~ "Daño Moderado (ND3)",
      Termino == "ND4" ~ "Daño Severo (ND4)"
    )
  )

# Asegurar el orden correcto de los niveles
datos_grafico$Nivel_Daño <- factor(datos_grafico$Nivel_Daño, 
                                   levels = c("Daño Leve (ND2)", "Daño Moderado (ND3)", "Daño Severo (ND4)"))

# 5. Crear el Mapa de Calor Gerencial
mapa_calor <- ggplot(datos_grafico, aes(x = Nivel_Daño, y = reorder(Variable, Pct_Cambio), fill = Pct_Cambio)) +
  # Cuadrículas del mapa
  geom_tile(color = "white", size = 1.5) +
  
  # Texto dentro de las celdas (Porcentaje + Asteriscos de significancia)
  geom_text(aes(label = sprintf("%+.1f%%\n%s", Pct_Cambio, Significancia)), 
            color = "black", size = 4.5, fontface = "bold", lineheight = 0.8) +
  
  # Escala de colores: Rojo para pérdidas, Verde para ganancias
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850", 
                       midpoint = 0, limits = c(-35, 15),
                       name = "Impacto en\nCalidad (%)") +
  
  # Estética limpia y ejecutiva
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
  ) +
  labs(
    title = "Impacto del Daño de Cochinilla en la Calidad del Jugo",
    subtitle = "Variación porcentual respecto a plantas sanas (Testigo ND1)",
    x = "", 
    y = "",
    caption = "Significancia estadística vs Testigo: *** p<0.001, ** p<0.01, * p<0.05, ns = No significativo\nLos recuadros con asteriscos indican el punto de quiebre donde el daño es real y no producto del azar."
  )

# 6. Mostrar y guardar
print(mapa_calor)



# ==============================================================================
# MAPA DE CALOR GERENCIAL CON LETRAS DE FISHER LSD (ALFA = 15%)
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(agricolae) # Paquete para la prueba de Fisher LSD

# 1. Definir variables (Excluimos Dextrana por el problema de escala)
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])

# 2. Calcular la media del ND1 (Línea base o Testigo)
medias_nd1 <- datos_limpios %>%
  filter(ND == "1") %>%
  summarise(across(all_of(vars_calidad), mean, na.rm = TRUE)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Media_ND1")

# 3. Función para obtener las letras de Fisher LSD al 15%
obtener_letras_lsd <- function(data, var_name) {
  form <- as.formula(paste(var_name, "~ ND"))
  modelo <- aov(form, data = data)
  
  # Prueba de Fisher LSD con alpha = 0.15
  lsd <- LSD.test(modelo, "ND", alpha = 0.15, console = FALSE)
  
  grupos <- lsd$groups
  grupos$ND <- rownames(grupos)
  
  # Renombrar columnas para el merge
  colnames(grupos)[which(colnames(grupos) == "groups")] <- "Letra"
  
  return(grupos[, c("ND", "Letra")])
}

# Aplicar a todas las variables y crear un dataframe de letras
letras_lsd <- map_dfr(vars_calidad, ~obtener_letras_lsd(datos_limpios, .x) %>% mutate(Variable = .x))

# 4. Recalcular los resultados del modelo lineal (para los asteriscos y %)
obtener_resultados <- function(data, var_name) {
  form <- as.formula(paste(var_name, "~ ND"))
  modelo <- lm(form, data = data)
  res <- summary(modelo)$coefficients
  
  coefs <- as.data.frame(res)
  coefs$Termino <- rownames(coefs)
  coefs <- coefs %>% filter(grepl("ND", Termino))
  
  if(nrow(coefs) == 0) return(NULL)
  
  coefs$Variable <- var_name
  coefs$Estimate <- coefs[, "Estimate"]
  coefs$P_value <- coefs[, "Pr(>|t|)"]
  
  return(coefs[, c("Variable", "Termino", "Estimate", "P_value")])
}

resultados_lm <- map_dfr(vars_calidad, ~obtener_resultados(datos_limpios, .x))

# 5. Unir todo: Porcentajes, Asteriscos y Letras de Fisher
datos_grafico <- resultados_lm %>%
  left_join(medias_nd1, by = "Variable") %>%
  mutate(
    Pct_Cambio = (Estimate / Media_ND1) * 100,
    Significancia = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      TRUE            ~ "ns"
    ),
    # Mapear Termino a Nivel_Daño para el gráfico
    ND = case_when(
      Termino == "ND2" ~ "2",
      Termino == "ND3" ~ "3",
      Termino == "ND4" ~ "4"
    )
  ) %>%
  # Unir con las letras de Fisher
  left_join(letras_lsd, by = c("Variable", "ND")) %>%
  mutate(
    Nivel_Daño = case_when(
      ND == "2" ~ "Daño Leve (ND2)",
      ND == "3" ~ "Daño Moderado (ND3)",
      ND == "4" ~ "Daño Severo (ND4)"
    )
  )

# Asegurar el orden correcto
datos_grafico$Nivel_Daño <- factor(datos_grafico$Nivel_Daño, 
                                   levels = c("Daño Leve (ND2)", "Daño Moderado (ND3)", "Daño Severo (ND4)"))

# 6. Crear el Mapa de Calor Gerencial con Letras y Asteriscos
mapa_calor <- ggplot(datos_grafico, aes(x = Nivel_Daño, y = reorder(Variable, Pct_Cambio), fill = Pct_Cambio)) +
  geom_tile(color = "white", size = 1.5) +
  
  # Texto dentro de las celdas: Porcentaje, Letra de Fisher y Asterisco
  # Se usa \n para saltos de línea y que se vea ordenado
  geom_text(aes(label = sprintf("%+.1f%%\nLetra: %s\n%s", Pct_Cambio, Letra, Significancia)), 
            color = "black", size = 4, fontface = "bold", lineheight = 0.9) +
  
  scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF", high = "#1A9850", 
                       midpoint = 0, limits = c(-35, 15),
                       name = "Impacto en\nCalidad (%)") +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(face = "bold", size = 12),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
  ) +
  labs(
    title = "Impacto del Daño de Cochinilla en la Calidad del Jugo",
    subtitle = "Variación porcentual respecto a plantas sanas (Testigo ND1)",
    x = "", 
    y = "",
    caption = "Letras (Fisher LSD, α=15%): Letras distintas indican diferencias significativas entre niveles de daño.\nSignificancia vs Testigo ND1: *** p<0.001, ** p<0.01, * p<0.05, ns = No significativo."
  )

print(mapa_calor)


# ==============================================================================
# CURVAS CON ZONA DE PÉRDIDA SOMBREADA (SIN ETIQUETAS)
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(agricolae)
library(ggrepel)

# 1. Definir variables (Excluyendo Dextrana)
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])

# 2. Calcular Medias y Errores Estándar por ND
stats_summary <- datos_limpios %>%
  tidyr::pivot_longer(cols = all_of(vars_calidad), names_to = "Variable", values_to = "Valor") %>%
  dplyr::group_by(Variable, ND) %>%
  dplyr::summarise(Media = mean(Valor, na.rm = TRUE), 
                   SE = sd(Valor, na.rm = TRUE)/sqrt(dplyr::n()), .groups = "drop")

# 3. Calcular el % de Cambio respecto al ND1
nd1_stats <- stats_summary %>% 
  dplyr::filter(ND == "1") %>% 
  dplyr::select(Variable, Media_ND1 = Media)

plot_data <- stats_summary %>%
  dplyr::left_join(nd1_stats, by = "Variable") %>%
  dplyr::mutate(
    Pct_Cambio = ((Media - Media_ND1) / Media_ND1) * 100,
    Pct_SE_min = (((Media - SE) - Media_ND1) / Media_ND1) * 100,
    Pct_SE_max = (((Media + SE) - Media_ND1) / Media_ND1) * 100
  )

# 4. Obtener letras de Fisher LSD (alpha = 15%)
obtener_letras_lsd <- function(data, var_name) {
  form <- as.formula(paste(var_name, "~ ND"))
  modelo <- aov(form, data = data)
  lsd <- agricolae::LSD.test(modelo, "ND", alpha = 0.15, console = FALSE)
  grupos <- lsd$groups
  grupos$ND <- rownames(grupos)
  colnames(grupos)[which(colnames(grupos) == "groups")] <- "Letra"
  return(grupos[, c("ND", "Letra")])
}

letras_lsd <- purrr::map_dfr(vars_calidad, ~obtener_letras_lsd(datos_limpios, .x) %>% dplyr::mutate(Variable = .x))

# Unir datos para graficar
plot_data <- plot_data %>% 
  dplyr::left_join(letras_lsd, by = c("Variable", "ND"))

# 5. Calcular el Punto de Quiebre (sin imprimir texto, solo para el sombreado)
puntos_quiebre <- letras_lsd %>%
  dplyr::left_join(letras_lsd %>% dplyr::filter(ND == "1") %>% dplyr::select(Variable, Ref_Letra = Letra), by = "Variable") %>%
  dplyr::mutate(Num_ND = as.numeric(as.character(ND))) %>%
  dplyr::filter(Num_ND > 1) %>%
  dplyr::group_by(Variable) %>%
  dplyr::group_modify(~ {
    ref <- .x$Ref_Letra[1]
    comparten <- sapply(.x$Letra, function(l) {
      l1 <- strsplit(as.character(l), "")[[1]]
      l2 <- strsplit(as.character(ref), "")[[1]]
      length(intersect(l1, l2)) > 0
    })
    idx <- which(!comparten)
    if(length(idx) > 0) {
      data.frame(Punto_Quiebre = min(.x$Num_ND[idx]))
    } else {
      data.frame(Punto_Quiebre = NA)
    }
  }) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(Punto_Quiebre))

# ==============================================================================
# GRÁFICO FINAL CON SOMBREADO
# ==============================================================================

grafico_curvas <- ggplot(plot_data, aes(x = as.numeric(as.character(ND)), y = Pct_Cambio, group = 1)) +
  
  # NUEVO: Sombreado rojo tenue desde el punto de quiebre hacia la derecha
  geom_rect(data = puntos_quiebre, 
            aes(xmin = Punto_Quiebre - 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.08, inherit.aes = FALSE) +
  
  # Línea base en 0% (Testigo)
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  
  # Línea vertical de Punto de Quiebre
  geom_vline(data = puntos_quiebre, aes(xintercept = Punto_Quiebre), 
             color = "red", linetype = "dashed", size = 1) +
  
  # Línea de tendencia
  geom_line(color = "steelblue", size = 1.2) +
  
  # Barras de error
  geom_errorbar(aes(ymin = Pct_SE_min, ymax = Pct_SE_max), width = 0.1, color = "steelblue", size = 0.8) +
  
  # Puntos
  geom_point(aes(color = Letra), size = 4) +
  
  # Letras con ggrepel
  geom_text_repel(aes(label = Letra), 
                  size = 4.5, fontface = "bold", 
                  bg.color = "white", bg.r = 0.15, 
                  nudge_y = 2.5, direction = "y", box.padding = 0.5) +
  
  # Separar por variable
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  
  # Escalas y ejes
  scale_x_continuous(breaks = c(1, 2, 3, 4)) +
  scale_color_brewer(palette = "Set1") +
  
  # Estética ejecutiva
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
    plot.caption = element_text(size = 10, color = "gray50", hjust = 0)
  ) +
  labs(
    title = "Curvas de Impacto del Daño de Cochinilla en la Calidad",
    subtitle = "Variación porcentual respecto a plantas sanas (Testigo ND1 = 0%)",
    x = "Nivel de Daño (ND)", 
    y = "Cambio Porcentual (%)",
    caption = "La zona sombreada en rojo indica los niveles de daño donde las pérdidas ya son estadísticamente significativas (Fisher LSD, α=15%)."
  )

print(grafico_curvas)


# ==============================================================================
# CURVAS DE REGRESIÓN CON P-VALUE DEL MODELO LINEAL EN CADA PANEL
# ==============================================================================

library(tidyverse)
library(ggplot2)

# 1. Definir variables (Excluyendo Dextrana)
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])
datos_limpios$ND_num <- as.numeric(as.character(datos_limpios$ND))

# ==============================================================================
# 2. AJUSTAR MODELOS DE REGRESIÓN POR VARIABLE
# ==============================================================================
resultados_regresion <- purrr::map(vars_calidad, function(v) {
  
  # --- Modelo lineal global (tendencia) ---
  mod_lineal <- lm(as.formula(paste(v, "~ ND_num")), data = datos_limpios)
  resumen_mod <- summary(mod_lineal)
  
  # Extraer p-valor de la pendiente y R²
  p_pendiente <- resumen_mod$coefficients["ND_num", "Pr(>|t|)"]
  r2 <- resumen_mod$r.squared
  
  # Formatear p-valor
  p_label <- dplyr::case_when(
    p_pendiente < 0.001 ~ "p < 0.001 ***",
    p_pendiente < 0.01  ~ paste0("p = ", sprintf("%.3f", p_pendiente), " **"),
    p_pendiente < 0.05  ~ paste0("p = ", sprintf("%.3f", p_pendiente), " *"),
    p_pendiente < 0.15  ~ paste0("p = ", sprintf("%.3f", p_pendiente), " ."),
    TRUE                ~ paste0("p = ", sprintf("%.3f", p_pendiente), " ns")
  )
  
  # Predecir para la línea suave
  pred_data <- data.frame(ND_num = seq(1, 4, length.out = 100))
  pred <- predict(mod_lineal, newdata = pred_data, interval = "confidence")
  
  media_nd1 <- mean(datos_limpios[[v]][datos_limpios$ND == "1"], na.rm = TRUE)
  
  pred_df <- data.frame(
    Variable = v,
    ND_num = pred_data$ND_num,
    Fit = pred[, "fit"],
    Lower = pred[, "lwr"],
    Upper = pred[, "upr"]
  ) %>%
    dplyr::mutate(
      Pct_Cambio = ((Fit - media_nd1) / media_nd1) * 100,
      Pct_Lower = ((Lower - media_nd1) / media_nd1) * 100,
      Pct_Upper = ((Upper - media_nd1) / media_nd1) * 100
    )
  
  # --- Modelo con dummies (solo para identificar punto de quiebre) ---
  mod_dummy <- lm(as.formula(paste(v, "~ ND")), data = datos_limpios)
  coefs <- summary(mod_dummy)$coefficients
  
  pvals <- data.frame(
    ND = c("2", "3", "4"),
    P_value = c(
      coefs["ND2", "Pr(>|t|)"],
      coefs["ND3", "Pr(>|t|)"],
      coefs["ND4", "Pr(>|t|)"]
    ),
    Significativo = c(
      coefs["ND2", "Pr(>|t|)"] < 0.15,
      coefs["ND3", "Pr(>|t|)"] < 0.15,
      coefs["ND4", "Pr(>|t|)"] < 0.15
    )
  )
  
  promedios <- datos_limpios %>%
    dplyr::group_by(ND) %>%
    dplyr::summarise(Media = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      Variable = v,
      ND_num = as.numeric(as.character(ND)),
      Pct_Cambio = ((Media - media_nd1) / media_nd1) * 100
    ) %>%
    dplyr::left_join(pvals, by = "ND") %>%
    dplyr::mutate(Significativo = ifelse(is.na(Significativo), FALSE, Significativo))
  
  # Dataframe con la etiqueta del modelo para este panel
  label_df <- data.frame(
    Variable = v,
    Label = p_label,
    R2 = sprintf("R² = %.3f", r2)
  )
  
  list(pred = pred_df, puntos = promedios, label = label_df)
})

predicciones <- purrr::map_dfr(resultados_regresion, "pred")
puntos_df <- purrr::map_dfr(resultados_regresion, "puntos")
labels_df <- purrr::map_dfr(resultados_regresion, "label")

# ==============================================================================
# 3. IDENTIFICAR EL PUNTO DE QUIEBRE
# ==============================================================================
puntos_quiebre <- puntos_df %>%
  dplyr::filter(ND != "1") %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(
    Punto_Quiebre = {
      sig <- which(Significativo)
      if(length(sig) > 0) min(ND_num[sig]) else NA
    },
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(Punto_Quiebre))

# ==============================================================================
# 4. GRÁFICO FINAL
# ==============================================================================

grafico_regresion <- ggplot() +
  
  # Sombreado rojo tenue desde el punto de quiebre
  geom_rect(data = puntos_quiebre, 
            aes(xmin = Punto_Quiebre - 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.06, inherit.aes = FALSE) +
  
  # Línea base en 0%
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.7) +
  
  # Línea vertical de Punto de Quiebre
  geom_vline(data = puntos_quiebre, aes(xintercept = Punto_Quiebre), 
             color = "red", linetype = "dashed", size = 0.9) +
  
  # Banda de Confianza
  geom_ribbon(data = predicciones, aes(x = ND_num, ymin = Pct_Lower, ymax = Pct_Upper), 
              fill = "steelblue", alpha = 0.18) +
  
  # Línea de Regresión
  geom_line(data = predicciones, aes(x = ND_num, y = Pct_Cambio), 
            color = "steelblue", size = 1.1) +
  
  # Puntos
  geom_point(data = puntos_df, aes(x = ND_num, y = Pct_Cambio, 
                                   color = Significativo), 
             size = 3.5) +
  
  # Etiqueta del modelo lineal en la esquina inferior izquierda de cada panel
  geom_text(data = labels_df, 
            aes(x = 0.9, y = -Inf, label = Label),
            hjust = 0, vjust = -1.5, size = 3.2, 
            fontface = "italic", color = "steelblue4",
            inherit.aes = FALSE) +
  
  # Etiqueta del R² en la esquina inferior derecha
  geom_text(data = labels_df, 
            aes(x = 4.4, y = -Inf, label = R2),
            hjust = 1, vjust = -1.5, size = 3.2, 
            fontface = "italic", color = "gray40",
            inherit.aes = FALSE) +
  
  # Separar por variable
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  
  # Escalas y colores
  scale_x_continuous(breaks = c(1, 2, 3, 4), expand = expansion(mult = c(0.05, 0.1))) +
  scale_color_manual(values = c("TRUE" = "#D73027", "FALSE" = "gray60"),
                     labels = c("TRUE" = "Significativo vs ND1", "FALSE" = "No significativo"),
                     name = "Regresión:") +
  
  # Estética ejecutiva
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Curvas de Regresión: Impacto del Daño de Cochinilla en la Calidad",
    subtitle = "Línea azul: Modelo de regresión lineal con banda de confianza al 95%",
    x = "Nivel de Daño (ND)", 
    y = "Cambio Porcentual respecto a Testigo (%)",
    caption = "El p-valor indica si la tendencia lineal (pendiente) es estadísticamente significativa. *** p<0.001, ** p<0.01, * p<0.05, . p<0.15, ns = no significativo.\nPuntos rojos: Significativamente diferentes del Testigo (ND1). La zona sombreada marca el inicio de las pérdidas significativas."
  )

print(grafico_regresion)




# ==============================================================================
# CURVAS DE REGRESIÓN CON DATOS REALES Y TABLA DE MODELOS EN CONSOLA
# ==============================================================================

library(tidyverse)
library(ggplot2)

# 1. Definir variables (Excluyendo Dextrana)
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])
datos_limpios$ND_num <- as.numeric(as.character(datos_limpios$ND))

# ==============================================================================
# 2. AJUSTAR MODELOS DE REGRESIÓN Y EXTRAER ESTADÍSTICAS
# ==============================================================================
resultados_regresion <- purrr::map(vars_calidad, function(v) {
  
  # --- Modelo lineal global (tendencia) ---
  mod_lineal <- lm(as.formula(paste(v, "~ ND_num")), data = datos_limpios)
  resumen_mod <- summary(mod_lineal)
  
  # Extraer estadísticas para la tabla de consola
  coefs_lineal <- as.data.frame(resumen_mod$coefficients)
  coefs_lineal$Variable <- v
  coefs_lineal$Termino <- rownames(coefs_lineal)
  coefs_lineal$R2 <- resumen_mod$r.squared
  
  # Extraer p-valor de la pendiente y formatear etiqueta
  p_pendiente <- resumen_mod$coefficients["ND_num", "Pr(>|t|)"]
  r2 <- resumen_mod$r.squared
  
  p_label <- dplyr::case_when(
    p_pendiente < 0.001 ~ "p < 0.001 ***",
    p_pendiente < 0.01  ~ paste0("p = ", sprintf("%.3f", p_pendiente), " **"),
    p_pendiente < 0.05  ~ paste0("p = ", sprintf("%.3f", p_pendiente), " *"),
    p_pendiente < 0.15  ~ paste0("p = ", sprintf("%.3f", p_pendiente), " ."),
    TRUE                ~ paste0("p = ", sprintf("%.3f", p_pendiente), " ns")
  )
  
  # Predecir para la línea suave (DATOS REALES)
  pred_data <- data.frame(ND_num = seq(1, 4, length.out = 100))
  pred <- predict(mod_lineal, newdata = pred_data, interval = "confidence")
  
  media_nd1 <- mean(datos_limpios[[v]][datos_limpios$ND == "1"], na.rm = TRUE)
  
  pred_df <- data.frame(
    Variable = v,
    ND_num = pred_data$ND_num,
    Fit = pred[, "fit"],
    Lower = pred[, "lwr"],
    Upper = pred[, "upr"]
  )
  
  # --- Modelo con dummies (para identificar punto de quiebre) ---
  mod_dummy <- lm(as.formula(paste(v, "~ ND")), data = datos_limpios)
  coefs_dummy <- summary(mod_dummy)$coefficients
  
  pvals <- data.frame(
    ND = c("2", "3", "4"),
    P_value = c(
      coefs_dummy["ND2", "Pr(>|t|)"],
      coefs_dummy["ND3", "Pr(>|t|)"],
      coefs_dummy["ND4", "Pr(>|t|)"]
    ),
    Significativo = c(
      coefs_dummy["ND2", "Pr(>|t|)"] < 0.15,
      coefs_dummy["ND3", "Pr(>|t|)"] < 0.15,
      coefs_dummy["ND4", "Pr(>|t|)"] < 0.15
    )
  )
  
  # Promedios observados (DATOS REALES)
  promedios <- datos_limpios %>%
    dplyr::group_by(ND) %>%
    dplyr::summarise(Media = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(
      Variable = v,
      ND_num = as.numeric(as.character(ND))
    ) %>%
    dplyr::left_join(pvals, by = "ND") %>%
    dplyr::mutate(Significativo = ifelse(is.na(Significativo), FALSE, Significativo))
  
  # Dataframe con la etiqueta del modelo y la línea base
  label_df <- data.frame(
    Variable = v,
    Label = p_label,
    R2 = sprintf("R² = %.3f", r2),
    Media_ND1 = media_nd1
  )
  
  list(pred = pred_df, puntos = promedios, label = label_df, stats = coefs_lineal)
})

# Extraer los dataframes
predicciones <- purrr::map_dfr(resultados_regresion, "pred")
puntos_df <- purrr::map_dfr(resultados_regresion, "puntos")
labels_df <- purrr::map_dfr(resultados_regresion, "label")

# ==============================================================================
# 3. IMPRIMIR TABLA DE MODELOS EN LA CONSOLA
# ==============================================================================
tabla_consola <- purrr::map_dfr(resultados_regresion, "stats") %>%
  dplyr::select(Variable, Termino, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, R2) %>%
  dplyr::rename(P_Value = `Pr(>|t|)`, Std_Error = `Std. Error`, t_value = `t value`) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4)))

cat("\n================================================================================\n")
cat("           TABLA RESUMEN DE MODELOS DE REGRESIÓN (Variable ~ ND_num)\n")
cat("================================================================================\n")
print(tabla_consola, row.names = FALSE)
cat("================================================================================\n\n")

# ==============================================================================
# 4. IDENTIFICAR EL PUNTO DE QUIEBRE
# ==============================================================================
puntos_quiebre <- puntos_df %>%
  dplyr::filter(ND != "1") %>%
  dplyr::group_by(Variable) %>%
  dplyr::summarise(
    Punto_Quiebre = {
      sig <- which(Significativo)
      if(length(sig) > 0) min(ND_num[sig]) else NA
    },
    .groups = "drop"
  ) %>%
  dplyr::filter(!is.na(Punto_Quiebre))

# ==============================================================================
# 5. GRÁFICO FINAL (DATOS REALES)
# ==============================================================================

grafico_regresion <- ggplot() +
  
  # Sombreado rojo tenue desde el punto de quiebre
  geom_rect(data = puntos_quiebre, 
            aes(xmin = Punto_Quiebre - 0.5, xmax = 4.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.06, inherit.aes = FALSE) +
  
  # Línea base (Media del ND1 para cada variable)
  geom_hline(data = labels_df, aes(yintercept = Media_ND1), 
             linetype = "dashed", color = "gray50", size = 0.7) +
  
  # Línea vertical de Punto de Quiebre
  geom_vline(data = puntos_quiebre, aes(xintercept = Punto_Quiebre), 
             color = "red", linetype = "dashed", size = 0.9) +
  
  # Banda de Confianza
  geom_ribbon(data = predicciones, aes(x = ND_num, ymin = Lower, ymax = Upper), 
              fill = "steelblue", alpha = 0.18) +
  
  # Línea de Regresión
  geom_line(data = predicciones, aes(x = ND_num, y = Fit), 
            color = "steelblue", size = 1.1) +
  
  # Puntos (Promedios reales)
  geom_point(data = puntos_df, aes(x = ND_num, y = Media, 
                                   color = Significativo), 
             size = 3.5) +
  
  # Etiqueta del p-valor del modelo
  geom_text(data = labels_df, 
            aes(x = 0.9, y = -Inf, label = Label),
            hjust = 0, vjust = -1.5, size = 3.2, 
            fontface = "italic", color = "steelblue4",
            inherit.aes = FALSE) +
  
  # Etiqueta del R²
  geom_text(data = labels_df, 
            aes(x = 4.4, y = -Inf, label = R2),
            hjust = 1, vjust = -1.5, size = 3.2, 
            fontface = "italic", color = "gray40",
            inherit.aes = FALSE) +
  
  # Separar por variable (escalas libres en Y)
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  
  # Escalas y colores
  scale_x_continuous(breaks = c(1, 2, 3, 4), expand = expansion(mult = c(0.05, 0.1))) +
  scale_color_manual(values = c("TRUE" = "#D73027", "FALSE" = "gray60"),
                     labels = c("TRUE" = "Significativo vs ND1", "FALSE" = "No significativo"),
                     name = "Regresión:") +
  
  # Estética ejecutiva
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Curvas de Regresión: Impacto del Daño de Cochinilla en la Calidad",
    subtitle = "Línea azul: Modelo de regresión lineal | Puntos: Promedios observados (datos reales)",
    x = "Nivel de Daño (ND)", 
    y = "Valor Medio (Unidades originales)",
    caption = "El p-valor indica si la tendencia lineal (pendiente) es estadísticamente significativa. *** p<0.001, ** p<0.01, * p<0.05, . p<0.15, ns = no significativo.\nPuntos rojos: Significativamente diferentes del Testigo (ND1). La zona sombreada marca el inicio de las pérdidas significativas."
  )

print(grafico_regresion)



# ==============================================================================
# MODELO CUADRÁTICO: PUNTO ÓPTIMO Y PUNTO DE QUIEBRE
# ==============================================================================

library(tidyverse)
library(ggplot2)

# 1. Definir variables (Excluyendo Dextrana)
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])
datos_limpios$ND_num <- as.numeric(as.character(datos_limpios$ND))

# ==============================================================================
# 2. AJUSTAR MODELOS CUADRÁTICOS Y EXTRAER ESTADÍSTICAS
# ==============================================================================
resultados_cuad <- purrr::map(vars_calidad, function(v) {
  
  # --- Modelo Cuadrático: y ~ ND + ND² ---
  mod_cuad <- lm(as.formula(paste(v, "~ ND_num + I(ND_num^2)")), data = datos_limpios)
  resumen_mod <- summary(mod_cuad)
  coefs <- resumen_mod$coefficients
  
  # Extraer coeficientes
  a <- coefs["(Intercept)", "Estimate"]
  b <- coefs["ND_num", "Estimate"]
  c <- coefs["I(ND_num^2)", "Estimate"]
  
  # P-valor del término cuadrático (indica si la curvatura es real)
  p_cuad <- coefs["I(ND_num^2)", "Pr(>|t|)"]
  r2 <- resumen_mod$r.squared
  
  # --- Calcular Vértice (Punto Óptimo) ---
  # Vértice: x = -b / (2c)
  ND_vertice <- if (c != 0) -b / (2 * c) else NA
  
  # Determinar si es máximo o mínimo según el signo de 'c'
  Tipo_Vertice <- if (c < 0) "Máximo" else if (c > 0) "Mínimo" else "Lineal"
  
  # Punto óptimo solo es válido si está dentro del rango [1, 4]
  Vertice_Valido <- !is.na(ND_vertice) & ND_vertice >= 1 & ND_vertice <= 4
  
  # --- Media del Testigo (ND1) para comparar ---
  media_nd1 <- mean(datos_limpios[[v]][datos_limpios$ND == "1"], na.rm = TRUE)
  
  # --- Predecir con banda de confianza al 95% ---
  pred_data <- data.frame(ND_num = seq(1, 4, length.out = 200))
  pred <- predict(mod_cuad, newdata = pred_data, interval = "confidence")
  
  pred_df <- data.frame(
    Variable = v,
    ND_num = pred_data$ND_num,
    Fit = pred[, "fit"],
    Lower = pred[, "lwr"],
    Upper = pred[, "upr"]
  )
  
  # --- IDENTIFICAR PUNTO DE QUIEBRE ---
  # El punto de quiebre es el primer ND donde la banda superior de confianza
  # cae por debajo de la media del Testigo (ND1). Es decir, ya no toca la línea base.
  # Solo se considera pérdida (Fit < media_nd1)
  pred_df_quiebre <- pred_df %>%
    dplyr::filter(Fit < media_nd1) %>% # Solo en zona de pérdida
    dplyr::filter(Upper < media_nd1)   # Donde la banda ya no toca la línea base
  
  punto_quiebre <- if (nrow(pred_df_quiebre) > 0) {
    min(pred_df_quiebre$ND_num)
  } else {
    NA
  }
  
  # --- Promedios observados para puntos ---
  # Modelo con dummies para marcar significancia de cada nivel
  mod_dummy <- lm(as.formula(paste(v, "~ ND")), data = datos_limpios)
  coefs_dummy <- summary(mod_dummy)$coefficients
  
  pvals <- data.frame(
    ND = c("2", "3", "4"),
    Significativo = c(
      coefs_dummy["ND2", "Pr(>|t|)"] < 0.15,
      coefs_dummy["ND3", "Pr(>|t|)"] < 0.15,
      coefs_dummy["ND4", "Pr(>|t|)"] < 0.15
    )
  )
  
  promedios <- datos_limpios %>%
    dplyr::group_by(ND) %>%
    dplyr::summarise(Media = mean(.data[[v]], na.rm = TRUE), .groups = "drop") %>%
    dplyr::mutate(Variable = v, ND_num = as.numeric(as.character(ND))) %>%
    dplyr::left_join(pvals, by = "ND") %>%
    dplyr::mutate(Significativo = ifelse(is.na(Significativo), FALSE, Significativo))
  
  # --- Etiquetas para el gráfico ---
  p_label <- dplyr::case_when(
    p_cuad < 0.001 ~ "Curvatura: p < 0.001 ***",
    p_cuad < 0.01  ~ paste0("Curvatura: p = ", sprintf("%.3f", p_cuad), " **"),
    p_cuad < 0.05  ~ paste0("Curvatura: p = ", sprintf("%.3f", p_cuad), " *"),
    p_cuad < 0.15  ~ paste0("Curvatura: p = ", sprintf("%.3f", p_cuad), " ."),
    TRUE           ~ paste0("Curvatura: p = ", sprintf("%.3f", p_cuad), " ns")
  )
  
  label_df <- data.frame(
    Variable = v,
    Label_P = p_label,
    R2 = sprintf("R² = %.3f", r2),
    Media_ND1 = media_nd1,
    Vertice = ND_vertice,
    Tipo_Vertice = Tipo_Vertice,
    Vertice_Valido = Vertice_Valido,
    Punto_Quiebre = punto_quiebre,
    a = a, b = b, c = c,
    P_Cuadratico = p_cuad
  )
  
  # Guardar coefs para la tabla de consola
  coefs_lineal <- as.data.frame(coefs)
  coefs_lineal$Variable <- v
  coefs_lineal$Termino <- rownames(coefs_lineal)
  coefs_lineal$R2 <- r2
  
  list(pred = pred_df, puntos = promedios, label = label_df, stats = coefs_lineal)
})

predicciones <- purrr::map_dfr(resultados_cuad, "pred")
puntos_df <- purrr::map_dfr(resultados_cuad, "puntos")
labels_df <- purrr::map_dfr(resultados_cuad, "label")

# ==============================================================================
# 3. IMPRIMIR TABLA DE MODELOS CUADRÁTICOS EN LA CONSOLA
# ==============================================================================
tabla_consola <- purrr::map_dfr(resultados_cuad, "stats") %>%
  dplyr::select(Variable, Termino, Estimate, `Std. Error`, `t value`, `Pr(>|t|)`, R2) %>%
  dplyr::rename(P_Value = `Pr(>|t|)`, Std_Error = `Std. Error`, t_value = `t value`) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4)))

cat("\n================================================================================\n")
cat("    TABLA RESUMEN DE MODELOS CUADRÁTICOS (Variable ~ ND + ND²)\n")
cat("================================================================================\n")
print(tabla_consola, row.names = FALSE)

cat("\n================================================================================\n")
cat("    RESUMEN DE VÉRTICES Y PUNTOS DE QUIEBRE\n")
cat("================================================================================\n")
resumen_final <- labels_df %>%
  dplyr::select(Variable, a, b, c, P_Cuadratico, Vertice, Tipo_Vertice, 
                Vertice_Valido, Punto_Quiebre) %>%
  dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4)))
print(resumen_final, row.names = FALSE)
cat("================================================================================\n\n")

# ==============================================================================
# 4. GRÁFICO FINAL
# ==============================================================================

grafico_cuad <- ggplot() +
  
  # Sombreado rojo desde el punto de quiebre
  geom_rect(data = labels_df %>% dplyr::filter(!is.na(Punto_Quiebre)), 
            aes(xmin = Punto_Quiebre - 0.02, xmax = 4.5, ymin = -Inf, ymax = Inf), 
            fill = "red", alpha = 0.06, inherit.aes = FALSE) +
  
  # Línea base (Media del Testigo ND1)
  geom_hline(data = labels_df, aes(yintercept = Media_ND1), 
             linetype = "dashed", color = "gray50", size = 0.7) +
  
  # Línea vertical del Punto de Quiebre
  geom_vline(data = labels_df %>% dplyr::filter(!is.na(Punto_Quiebre)), 
             aes(xintercept = Punto_Quiebre), 
             color = "red", linetype = "dashed", size = 0.9) +
  
  # Banda de Confianza
  geom_ribbon(data = predicciones, aes(x = ND_num, ymin = Lower, ymax = Upper), 
              fill = "steelblue", alpha = 0.18) +
  
  # Curva Cuadrática
  geom_line(data = predicciones, aes(x = ND_num, y = Fit), 
            color = "steelblue", size = 1.2) +
  
  # Puntos (Promedios reales)
  geom_point(data = puntos_df, aes(x = ND_num, y = Media, 
                                   color = Significativo), 
             size = 3.5) +
  
  # Marcar el Vértice (Punto Óptimo) si es válido
  geom_point(data = labels_df %>% dplyr::filter(Vertice_Valido), 
             aes(x = Vertice, y = predict(lm(1 ~ 1)) ), 
             shape = 8, size = 3, color = "darkgreen", 
             inherit.aes = FALSE) +
  
  # Etiqueta del p-valor de curvatura
  geom_text(data = labels_df, 
            aes(x = 0.9, y = -Inf, label = Label_P),
            hjust = 0, vjust = -1.5, size = 3.1, 
            fontface = "italic", color = "steelblue4",
            inherit.aes = FALSE) +
  
  # Etiqueta del R²
  geom_text(data = labels_df, 
            aes(x = 4.4, y = -Inf, label = R2),
            hjust = 1, vjust = -1.5, size = 3.2, 
            fontface = "italic", color = "gray40",
            inherit.aes = FALSE) +
  
  # Separar por variable
  facet_wrap(~ Variable, scales = "free_y", ncol = 3) +
  
  # Escalas y colores
  scale_x_continuous(breaks = c(1, 2, 3, 4), expand = expansion(mult = c(0.05, 0.1))) +
  scale_color_manual(values = c("TRUE" = "#D73027", "FALSE" = "gray60"),
                     labels = c("TRUE" = "Significativo vs ND1", "FALSE" = "No significativo"),
                     name = "Comparación ND1:") +
  
  # Estética ejecutiva
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray92"),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Curvas Cuadráticas: Impacto del Daño de Cochinilla en la Calidad",
    subtitle = "Modelo de segundo grado (y = a + b·ND + c·ND²) | Puntos: Promedios observados",
    x = "Nivel de Daño (ND)", 
    y = "Valor Medio (Unidades originales)",
    caption = "El p-valor de curvatura indica si el modelo cuadrático se ajusta mejor que uno lineal. *** p<0.001, ** p<0.01, * p<0.05, . p<0.15.\nLa línea roja vertical marca el punto donde la banda de confianza del modelo cruza la línea base del Testigo (ND1). La zona sombreada es la región de pérdida confirmada."
  )

print(grafico_cuad)




# ==============================================================================
# PUNTO DE QUIEBRE MULTIVARIADO: ESCALA CENTRADA EN ND1 = 0
# ==============================================================================

library(tidyverse)
library(ggplot2)
library(vegan)
library(MASS)
library(patchwork)

# 1. Preparar datos
vars_calidad <- c("Peso_tallo", "Altura", "Brix", "Pol", "Humedad", 
                  "Pol_Jugo", "Pureza", "Pol_caña")

datos$ND <- as.factor(datos$ND)
datos_limpios <- na.omit(datos[, c("ND", vars_calidad)])
datos_scaled <- as.data.frame(scale(datos_limpios[, vars_calidad]))
datos_scaled$ND <- datos_limpios$ND

# 2. Estadísticos
punto_quiebre <- 3
permanova_global <- list(R2 = 0.501, F_stat = 6.025, p_value = 0.001)

pairwise_stats <- data.frame(
  ND = c("1", "2", "3", "4"),
  R2_vs_ND1 = c(NA, 0.0791, 0.3160, 0.5436),
  P_adj_BH = c(NA, 0.476, 0.0195, 0.0195),
  Significativo = c(FALSE, FALSE, TRUE, TRUE)
)

# ==============================================================================
# 3. ICG ESCALADO: ND1 = 0, caídas en negativo
# ==============================================================================
# Estrategia: para cada muestra, calcular el cambio relativo de cada variable
# respecto al Testigo: (valor - media_ND1) / media_ND1
# Luego promediar los 8 cambios → ICG escalado

nd1_medias <- datos_limpios %>%
  dplyr::filter(ND == "1") %>%
  dplyr::summarise(dplyr::across(all_of(vars_calidad), mean, na.rm = TRUE))

cambio_por_muestra <- datos_limpios %>%
  dplyr::mutate(dplyr::across(all_of(vars_calidad),
                              ~ (.x - nd1_medias[[dplyr::cur_column()]]) / nd1_medias[[dplyr::cur_column()]])) %>%
  dplyr::mutate(
    ICG_scaled = rowMeans(dplyr::across(all_of(vars_calidad)), na.rm = TRUE),
    ND_num = as.numeric(as.character(ND))
  )

icg_stats <- cambio_por_muestra %>%
  dplyr::group_by(ND, ND_num) %>%
  dplyr::summarise(
    ICG_mean = mean(ICG_scaled),
    ICG_se = sd(ICG_scaled)/sqrt(dplyr::n()),
    ICG_lower = ICG_mean - 1.96*ICG_se,
    ICG_upper = ICG_mean + 1.96*ICG_se,
    .groups = "drop"
  ) %>%
  dplyr::left_join(pairwise_stats, by = "ND")

# ==============================================================================
# 4. LDA: SCORE LD1 INVERTIDO Y ESCALADO A ND1 = 0
# ==============================================================================
lda_res <- lda(ND ~ ., data = datos_scaled)
lda_scores <- predict(lda_res)$x
prop_var_lda <- lda_res$sv^2 / sum(lda_res$sv^2) * 100

# Invertir signo y centrar en la media del ND1 para que ND1 = 0
lda_df <- data.frame(
  ND = datos_scaled$ND,
  LD1_raw = -1 * lda_scores[,1],
  ND_num = as.numeric(as.character(datos_scaled$ND))
)

# Centrar en la media del ND1
media_nd1_lda <- mean(lda_df$LD1_raw[lda_df$ND == "1"])
lda_df$LD1 <- lda_df$LD1_raw - media_nd1_lda

lda_centroides <- lda_df %>%
  dplyr::group_by(ND, ND_num) %>%
  dplyr::summarise(LD1_mean = mean(LD1), LD1_se = sd(LD1)/sqrt(n()), .groups = "drop") %>%
  dplyr::mutate(LD1_lower = LD1_mean - 1.96*LD1_se,
                LD1_upper = LD1_mean + 1.96*LD1_se)

# ==============================================================================
# 5. GRÁFICO A: ICG ESCALADO (ND1 = 0)
# ==============================================================================
grafico_icg <- ggplot(icg_stats, aes(x = ND_num, y = ICG_mean)) +
  annotate("rect", xmin = punto_quiebre - 0.5, xmax = 4.5, 
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.06) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  geom_vline(xintercept = punto_quiebre, color = "red", 
             linetype = "dashed", size = 1.2) +
  
  geom_ribbon(aes(ymin = ICG_lower, ymax = ICG_upper), 
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", size = 1.6) +
  geom_point(size = 5, color = "steelblue") +
  
  # Etiqueta del punto de quiebre
  annotate("text", x = punto_quiebre, y = max(icg_stats$ICG_upper)*1.2, 
           label = "Punto de Quiebre", 
           color = "red", fontface = "bold.italic", size = 4.5, 
           vjust = 0, hjust = -0.05) +
  
  # P-valores y R²
  geom_text(data = icg_stats %>% dplyr::filter(!is.na(P_adj_BH)),
            aes(label = paste0("p = ", sprintf("%.3f", P_adj_BH),
                               ifelse(Significativo, " *", " ns"))),
            vjust = -2, size = 3.2, color = "gray35", fontface = "italic") +
  geom_text(data = icg_stats %>% dplyr::filter(!is.na(R2_vs_ND1)),
            aes(label = paste0("R² = ", sprintf("%.3f", R2_vs_ND1))),
            vjust = 3, size = 3.2, color = "steelblue4", fontface = "italic") +
  
  # Etiqueta del valor final
  annotate("text", x = 4.15, y = icg_stats$ICG_mean[icg_stats$ND == "4"] - 0.01,
           label = sprintf("Δ final\n= %+.3f", icg_stats$ICG_mean[icg_stats$ND == "4"]),
           hjust = 0, size = 3.3, color = "steelblue4", fontface = "bold") +
  
  # PERMANOVA global
  annotate("text", x = 0.9, y = max(icg_stats$ICG_upper)*1.35,
           label = paste0("PERMANOVA: R² = ", permanova_global$R2,
                          " | F = ", permanova_global$F_stat,
                          " | p = ", permanova_global$p_value, " ***"),
           hjust = 0, vjust = 0, size = 3.2, 
           color = "gray30", fontface = "italic") +
  
  scale_x_continuous(breaks = c(1, 2, 3, 4), expand = expansion(mult = c(0.08, 0.15))) +
  scale_y_continuous(labels = function(x) sprintf("%+.2f", x),
                     expand = expansion(mult = c(0.15, 0.2))) +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.margin = margin(10, 20, 10, 10)
  ) +
  labs(
    title = "A. Cambio del Perfil Multivariado (Índice de Calidad Global Escalado)",
    subtitle = "Cambio relativo respecto al Testigo ND1 (= 0) | Valores negativos = pérdida de calidad",
    x = "Nivel de Daño (ND)",
    y = "Cambio respecto al Testigo (Δ escalado)"
  )

# ==============================================================================
# 6. GRÁFICO B: SCORE LD1 ESCALADO (ND1 = 0)
# ==============================================================================
grafico_lda <- ggplot() +
  annotate("rect", xmin = punto_quiebre - 0.5, xmax = 4.5, 
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.06) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.8) +
  geom_vline(xintercept = punto_quiebre, color = "red", 
             linetype = "dashed", size = 1.2) +
  
  geom_jitter(data = lda_df, aes(x = ND_num, y = LD1, color = ND), 
              width = 0.15, size = 2.5, alpha = 0.5) +
  
  geom_ribbon(data = lda_centroides, aes(x = ND_num, ymin = LD1_lower, ymax = LD1_upper),
              fill = "darkgreen", alpha = 0.15) +
  geom_line(data = lda_centroides, aes(x = ND_num, y = LD1_mean), 
            color = "darkgreen", size = 1.4) +
  geom_point(data = lda_centroides, aes(x = ND_num, y = LD1_mean), 
             color = "darkgreen", size = 5) +
  
  # Etiqueta del punto de quiebre
  annotate("text", x = punto_quiebre, y = max(lda_centroides$LD1_upper)*1.2, 
           label = "Punto de Quiebre", 
           color = "red", fontface = "bold.italic", size = 4.5, 
           vjust = 0, hjust = -0.05) +
  
  # LDA info
  annotate("text", x = 1.1, y = min(lda_centroides$LD1_lower) - 0.3,
           label = paste0("LDA: LD1 = ", sprintf("%.1f", prop_var_lda[1]), "% var. | ",
                          "LD2 = ", sprintf("%.1f", prop_var_lda[2]), "% var. | ",
                          "LD3 = ", sprintf("%.1f", prop_var_lda[3]), "% var."),
           hjust = 0, vjust = 0, size = 3.2, 
           color = "gray30", fontface = "italic") +
  
  scale_x_continuous(breaks = c(1, 2, 3, 4), expand = expansion(mult = c(0.08, 0.12))) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  scale_color_brewer(palette = "Set1", name = "ND") +
  
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray40"),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  labs(
    title = "B. Score Discriminante LD1 (Centrado en Testigo = 0)",
    subtitle = paste0("LD1 explica el ", sprintf("%.1f", prop_var_lda[1]), "% de la separación | Centroides con IC 95%"),
    x = "Nivel de Daño (ND)",
    y = "Score LD1 (Δ respecto al Testigo)"
  )

# ==============================================================================
# 7. COMBINAR Y EXPORTAR
# ==============================================================================
grafico_final <- grafico_icg / grafico_lda +
  plot_annotation(
    title = "Análisis Multivariado: Punto de Quiebre del Perfil de Calidad",
    subtitle = "El perfil completo (8 variables escaladas) cae significativamente a partir del Daño Moderado (ND3)",
    caption = paste0("Ambas curvas están centradas en el Testigo ND1 = 0. Valores negativos indican pérdida de calidad.\n",
                     "Fuente: PERMANOVA (R² = ", permanova_global$R2, 
                     ", F = ", permanova_global$F_stat, ", p = ", permanova_global$p_value, ") y LDA. ",
                     "La zona sombreada en rojo indica el rango de ND con pérdida multivariada confirmada ",
                     "(ND1 vs ND3: p_adj = 0.020 *; R² = 0.316)."),
    theme = theme(
      plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
      plot.caption = element_text(size = 9, color = "gray50", hjust = 0)
    )
  )

print(grafico_final)
ggsave("Punto_Quiebre_Escalado_ND1_cero.png", grafico_final, 
       width = 13, height = 12, dpi = 600, bg = "white")

cat("\n>>> Gráfico guardado: Punto_Quiebre_Escalado_ND1_cero.png\n")
cat("\n>>> Cambio escalado por nivel (ND1 = 0):\n")
print(icg_stats %>% 
        dplyr::select(ND, ICG_mean, ICG_lower, ICG_upper, P_adj_BH, R2_vs_ND1) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 4))),
      row.names = FALSE)