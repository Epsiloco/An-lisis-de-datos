# Leer datos desde el portapapeles
datos <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")
View(datos)
attach(datos)
names(datos)

library(tidyverse)
library(emmeans)
library(multcomp)
library(multcompView)

# ====================================================================
# 1. PREPARACIÓN DE DATOS
# ====================================================================
datos$Tratamiento <- as.factor(datos$Tratamiento)
dda_unicos <- sort(unique(datos$DDA))

variables_a_analizar <- c(
  "Cobertura_Hoja_Ancha", "Cobertura_Graminea", "Cobertura_Cipereacea",
  "Control_Hoja_Ancha", "Control_Graminea", "Control_Cipereacea"
)

# ====================================================================
# 2. FUNCIÓN MLG (QUASI-BINOMIAL + LSD PROTEGIDA)
# ====================================================================
analizar_combinacion <- function(var_name, dda_val, data_full) {
  
  d_sub <- data_full %>% 
    dplyr::filter(DDA == dda_val) %>%
    dplyr::filter(!is.na(!!sym(var_name)))
  
  # Calculamos las medias crudas para que no sufran distorsiones
  medias_reales_df <- d_sub %>%
    dplyr::group_by(Tratamiento) %>%
    dplyr::summarise(
      Media_Real = mean(!!sym(var_name), na.rm = TRUE),
      SE = sd(!!sym(var_name), na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  d_sub$y_prop <- d_sub[[var_name]] / 100
  
  if(all(d_sub$y_prop == 0)) {
    return(medias_reales_df %>% dplyr::mutate(DDA = dda_val, Variable = var_name, Letra = "---", P_valor = NA))
  }
  
  n_obs <- nrow(d_sub)
  d_sub$y_prop_mod <- (d_sub$y_prop * (n_obs - 1) + 0.5) / n_obs
  
  formula_modelo <- as.formula("y_prop_mod ~ Tratamiento")
  modelo <- glm(formula_modelo, family = quasibinomial(link = "logit"), data = d_sub)
  
  anova_mod <- anova(modelo, test = "F")
  p_val <- anova_mod$`Pr(>F)`[2] 
  
  medias <- emmeans(modelo, ~ Tratamiento)
  
  # Lógica original: Si la variable es "Control", el más alto recibe la 'a'
  es_control <- grepl("Control", var_name, ignore.case = TRUE)
  letras <- multcomp::cld(medias, alpha = 0.15, adjust = "none", Letters = letters, reversed = es_control)
  
  resultados_limpios <- as.data.frame(letras) %>%
    dplyr::select(Tratamiento, .group) %>%
    dplyr::mutate(Letra = trimws(.group)) %>%
    dplyr::select(-.group)
  
  # ==================================================================
  # APLICACIÓN DE LSD PROTEGIDA (Tu regla para forzar la "a")
  # Si el P-valor es >= 0.15, forzamos que todos tengan la misma letra
  # ==================================================================
  if (!is.na(p_val) && p_val >= 0.15) {
    resultados_limpios$Letra <- "a"
  }
  
  res_final <- medias_reales_df %>%
    dplyr::left_join(resultados_limpios, by = "Tratamiento") %>%
    dplyr::mutate(
      DDA = dda_val,
      Variable = var_name,
      P_valor = p_val,
      SE = ifelse(is.na(SE), 0, SE)
    )
  
  return(res_final)
}

# ====================================================================
# 3. EJECUCIÓN MASIVA
# ====================================================================
grid_analisis <- expand.grid(Variable = variables_a_analizar, DDA = dda_unicos, stringsAsFactors = FALSE)
resultados_finales <- purrr::map2_dfr(grid_analisis$Variable, grid_analisis$DDA, ~analizar_combinacion(.x, .y, datos))

# ====================================================================
# 4. CONSTRUCCIÓN DE LA TABLA RESUMEN GLOBAL
# ====================================================================
tabla_resumen_global <- data.frame()

for (d in dda_unicos) {
  res_dda <- resultados_finales %>% dplyr::filter(DDA == d)
  
  tabla_medias <- res_dda %>%
    dplyr::mutate(
      Media_Letra = paste0(round(Media_Real, 1), " ", Letra),
      Tratamiento = as.character(Tratamiento)
    ) %>%
    dplyr::select(Tratamiento, Variable, Media_Letra) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = Media_Letra)
  
  tabla_pval <- res_dda %>%
    dplyr::select(Variable, P_valor) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      Tratamiento = "P-valor MLG (α=0.15)",
      P_valor_fmt = ifelse(is.na(P_valor), "-", ifelse(P_valor < 0.001, "<0.001", sprintf("%.4f", P_valor)))
    ) %>%
    dplyr::select(-P_valor) %>%
    tidyr::pivot_wider(names_from = Variable, values_from = P_valor_fmt)
  
  tabla_parcial <- dplyr::bind_rows(tabla_medias, tabla_pval) %>%
    dplyr::mutate(Evaluacion_DDA = paste("DDA", d)) %>%
    dplyr::select(Evaluacion_DDA, Tratamiento, everything())
  
  tabla_resumen_global <- dplyr::bind_rows(tabla_resumen_global, tabla_parcial)
}

print(tabla_resumen_global)
# Abre la tabla limpia para que puedas verla y copiarla fácil
View(tabla_resumen_global)

# ====================================================================
# 5. GRÁFICOS INDIVIDUALES POR VARIABLE (SOLO LAS SIGNIFICATIVAS)
#    CON ORIENTACIÓN HORIZONTAL, SIN LÍNEA DE CONEXIÓN Y EJE Y AJUSTADO
# ====================================================================
# Refrescamos la etiqueta para que agarre las "a" forzadas del paso 2
resultados_finales <- resultados_finales %>%
  dplyr::mutate(Etiqueta = paste0(round(Media_Real, 1), "% ", Letra))

# Generamos un gráfico individual para cada Variable × DDA que sea significativa (p_val < 0.15)
for (d in dda_unicos) {
  # Obtener las variables significativas para esta DDA
  vars_significativas <- resultados_finales %>%
    dplyr::filter(DDA == d, !is.na(P_valor), P_valor < 0.15) %>%
    dplyr::distinct(Variable) %>%
    dplyr::pull(Variable)
  
  # Si no hay variables significativas, saltamos esta DDA
  if (length(vars_significativas) == 0) {
    next
  }
  
  # Para cada variable significativa, generar un gráfico individual horizontal
  for (var_actual in vars_significativas) {
    
    datos_plot <- resultados_finales %>%
      dplyr::filter(DDA == d, Variable == var_actual)
    
    # Calcular límites del eje X (antes era Y)
    x_max <- max(datos_plot$Media_Real + datos_plot$SE, na.rm = TRUE)
    x_min <- min(datos_plot$Media_Real - datos_plot$SE, na.rm = TRUE)
    x_min <- ifelse(x_min < 0, 0, x_min)
    
    # Espacio adicional para las etiquetas (20% del rango)
    x_rango <- x_max - x_min
    x_margen <- x_rango * 0.25
    x_max_ajustado <- x_max + x_margen
    
    # Nombre limpio para el título
    nombre_limpio <- gsub("_", " ", var_actual)
    p_valor_actual <- unique(datos_plot$P_valor)
    p_text <- ifelse(p_valor_actual < 0.001, "<0.001", sprintf("%.4f", p_valor_actual))
    
    # Posición de las etiquetas: al final de la barra de error + un pequeño margen
    # También nos aseguramos de que la etiqueta no se salga del límite superior
    grafico <- ggplot(datos_plot, aes(x = Media_Real, y = Tratamiento)) +
      geom_errorbarh(aes(xmin = ifelse(Media_Real - SE < 0, 0, Media_Real - SE), 
                         xmax = Media_Real + SE),
                     height = 0.2, color = "gray40", size = 0.7) +
      geom_point(color = "black", size = 3) +
      geom_text(aes(x = Media_Real + SE, 
                    label = Etiqueta),
                hjust = -0.2,  # Ajuste negativo para poner la etiqueta a la derecha
                vjust = 0.5,
                size = 3.5, 
                fontface = "bold") +
      coord_cartesian(xlim = c(x_min, x_max_ajustado)) +  # Usamos coord_cartesian para no perder datos
      labs(
        title = paste(nombre_limpio, "- DDA:", d),
        subtitle = paste("Medias reales ± SE | Letras MLG Quasi-Binomial (LSD Protegida α=0.15) | p =", p_text),
        x = "Porcentaje (%)",
        y = "Tratamientos"
      ) +
      theme_bw() +
      theme(
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9),
        plot.title = element_text(face = "bold", color = "#c0392b", size = 13),
        plot.subtitle = element_text(size = 9, color = "gray40"),
        plot.margin = margin(10, 30, 10, 10)  # Margen derecho extra para etiquetas
      )
    
    # Imprime el gráfico directamente en la pestaña "Plots" de RStudio
    print(grafico)
  }
}

# ====================================================================
# 6. GRÁFICO DE BARRAS COMPUESTAS (APILADAS) PARA COBERTURA
# ====================================================================
df_cobertura_apilada <- resultados_finales %>%
  dplyr::filter(grepl("Cobertura", Variable, ignore.case = TRUE)) %>%
  dplyr::mutate(Tipo_Maleza = gsub("Cobertura_", "", Variable))

grafico_apilado <- ggplot(df_cobertura_apilada, aes(x = Tratamiento, y = Media_Real, fill = Tipo_Maleza)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.3) +
  facet_wrap(~ DDA, labeller = as_labeller(function(x) paste("Evaluación a los", x, "DDA"))) +
  scale_fill_manual(values = c("Hoja_Ancha" = "green", "Graminea" = "red", "Cipereacea" = "yellow")) +
  theme_bw(base_size = 14) +
  labs(
    title = "Composición de Cobertura de Malezas por Tratamiento",
    subtitle = "Porcentaje de cobertura apilado por tipo de maleza",
    x = "Tratamientos",
    y = "Porcentaje Total de Cobertura (%)",
    fill = "Tipo de Maleza"
  ) +
  theme(
    strip.background = element_rect(fill = "#ecf0f1"),
    strip.text = element_text(color = "black", face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    legend.position = "bottom"
  )

# Imprime el gráfico apilado en la pestaña "Plots"
print(grafico_apilado)

# ===================================================================
# 6.2. GRÁFICO DE BARRAS COMPUESTAS (APILADAS) PARA COBERTURA - MEJORADO
# ====================================================================

# Preparar datos para cobertura
df_cobertura_apilada <- resultados_finales %>%
  dplyr::filter(grepl("Cobertura", Variable, ignore.case = TRUE)) %>%
  dplyr::mutate(Tipo_Maleza = gsub("Cobertura_", "", Variable))

# Calcular el total por tratamiento y DDA para ordenamiento
df_totales <- df_cobertura_apilada %>%
  group_by(DDA, Tratamiento) %>%
  summarise(Total = sum(Media_Real), .groups = "drop")

# Unir totales y ordenar tratamientos de mayor a menor cobertura dentro de cada DDA
df_cobertura_apilada <- df_cobertura_apilada %>%
  left_join(df_totales, by = c("DDA", "Tratamiento")) %>%
  # Ordenar dentro de cada DDA
  group_by(DDA) %>%
  mutate(Tratamiento_ordenado = factor(Tratamiento, levels = unique(Tratamiento[order(Total, decreasing = TRUE)]))) %>%
  ungroup()

# Función para formatear números pequeños en las etiquetas
df_cobertura_apilada <- df_cobertura_apilada %>%
  group_by(DDA, Tratamiento) %>%
  mutate(
    pos_y = cumsum(Media_Real) - (Media_Real / 2),
    etiqueta = ifelse(Media_Real >= 5, paste0(round(Media_Real, 1), "%"), "")
  ) %>%
  ungroup()

# Definir paleta de colores más atractiva
colores_maleza <- c(
  "Hoja_Ancha" = "green",    # Verde brillante
  "Graminea" = "red",      # Rojo
  "Cipereacea" = "yellow"     # Naranja/ámbar
)

# Crear gráfico mejorado
grafico_apilado <- ggplot(df_cobertura_apilada, 
                          aes(x = Tratamiento_ordenado, y = Media_Real, fill = Tipo_Maleza)) +
  # Barras con borde más delgado y ancho controlado
  geom_bar(stat = "identity", position = "stack", 
           color = "white", linewidth = 0.2, width = 0.7) +
  # Etiquetas de porcentaje dentro de las barras (solo para valores >=5%)
  geom_text(aes(y = pos_y, label = etiqueta),
            size = 2.8, color = "white", fontface = "bold", na.rm = TRUE) +
  # Facetas por DDA
  facet_wrap(~ DDA, labeller = as_labeller(function(x) paste("Evaluación a los", x, "DDA")),
             scales = "free_x") +
  # Paleta de colores mejorada
  scale_fill_manual(values = colores_maleza) +
  # Escala Y con límite hasta 100
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, by = 20),
                     expand = expansion(mult = c(0, 0.05))) +
  # Tema mejorado
  theme_bw(base_size = 12) +
  labs(
    title = "Composición de Cobertura de Malezas por Tratamiento",
    subtitle = "Porcentaje de cobertura apilado por tipo de maleza | Valores <5% no etiquetados",
    x = "Tratamientos (ordenados por cobertura total descendente)",
    y = "Porcentaje Total de Cobertura (%)",
    fill = "Tipo de Maleza"
  ) +
  theme(
    # Estilo de las facetas
    strip.background = element_rect(fill = "#34495e", color = "#2c3e50"),
    strip.text = element_text(color = "white", face = "bold", size = 11),
    # Ejes
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 9),
    axis.text.y = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    # Leyenda
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.8, "cm"),
    # Panel
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1, "lines"),
    # Títulos
    plot.title = element_text(face = "bold", color = "#2c3e50", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5)
  )

# Imprime el gráfico apilado mejorado
print(grafico_apilado)

# ====================================================================
# 6b. VERSIÓN ALTERNATIVA: BARRAS SEPARADAS (NO APILADAS)
# Útil cuando hay valores muy pequeños o quieres comparar categorías
# ====================================================================

# Verificar si hay datos para la versión agrupada
if(nrow(df_cobertura_apilada) > 0) {
  
  # Preparar datos para gráfico de barras agrupadas (alternativa)
  df_cobertura_agrupada <- resultados_finales %>%
    dplyr::filter(grepl("Cobertura", Variable, ignore.case = TRUE)) %>%
    dplyr::mutate(Tipo_Maleza = gsub("Cobertura_", "", Variable))
  
  grafico_agrupado <- ggplot(df_cobertura_agrupada, 
                             aes(x = Tratamiento, y = Media_Real, fill = Tipo_Maleza)) +
    geom_bar(stat = "identity", position = position_dodge(0.8), 
             color = "white", linewidth = 0.2, width = 0.7) +
    # Añadir barras de error
    geom_errorbar(aes(ymin = ifelse(Media_Real - SE < 0, 0, Media_Real - SE),
                      ymax = ifelse(Media_Real + SE > 100, 100, Media_Real + SE)),
                  width = 0.2, position = position_dodge(0.8), color = "gray30", linewidth = 0.4) +
    # Etiquetas con letras de significancia
    geom_text(aes(y = Media_Real + SE + 2, label = Letra),
              position = position_dodge(0.8), size = 3.5, fontface = "bold", vjust = 0) +
    facet_wrap(~ DDA, labeller = as_labeller(function(x) paste("Evaluación a los", x, "DDA"))) +
    scale_fill_manual(values = colores_maleza) +
    scale_y_continuous(limits = function(x) c(0, max(x) * 1.1), 
                       breaks = seq(0, 100, by = 20)) +
    labs(
      title = "Cobertura de Malezas por Tipo - Barras Agrupadas",
      subtitle = "Medias ± SE | Letras indican diferencias significativas (α=0.15)",
      x = "Tratamientos",
      y = "Cobertura (%)",
      fill = "Tipo de Maleza"
    ) +
    theme_bw(base_size = 12) +
    theme(
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(color = "black", face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 8),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", color = "#2c3e50", size = 13, hjust = 0.5),
      plot.subtitle = element_text(size = 9, color = "gray40", hjust = 0.5),
      panel.grid.major.x = element_blank()
    )
  
  # Descomentar la línea siguiente si quieres ver la versión agrupada
  print(grafico_agrupado)
}

# Mostrar resumen de datos para verificar
cat("\n=== Resumen de Cobertura por DDA y Tratamiento ===\n")
print(df_cobertura_apilada %>%
        group_by(DDA, Tratamiento, Tipo_Maleza) %>%
        summarise(Cobertura = round(Media_Real, 1), .groups = "drop") %>%
        arrange(DDA, desc(Cobertura)))
