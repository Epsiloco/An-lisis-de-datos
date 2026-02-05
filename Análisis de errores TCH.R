
# -----------------------------------------------------------------------------
# 1. LIBRERÍAS
# -----------------------------------------------------------------------------
library(tidyverse)
library(janitor)
library(clipr)

datos <- read_clip_tbl()  
view(datos)

# -----------------------------------------------------------------------------
# 2. CARGA DE DATOS
# -----------------------------------------------------------------------------
datos_raw <- read_clip_tbl() 
view(datos_raw)
datos <- datos_raw %>%
  clean_names() %>%
  rename(
    metros_lineales = matches("metos_lineales|metros_lineales"),
    peso_neto = matches("peso_neto"),
    tch = matches("tch"),
    replica = matches("replica"),
    tratamiento = matches("tratamientos"),
    viaje = matches("viaje")
  ) %>%
  mutate(
    metros_lineales = as.numeric(metros_lineales),
    peso_neto = as.numeric(peso_neto),
    tch = as.numeric(tch),
    ue_id = paste(replica, tratamiento, viaje, sep = "_")
  )

# -----------------------------------------------------------------------------
# 3. CONTROL DE CALIDAD (QC) - DETECCIÓN DE DISCREPANCIAS
# -----------------------------------------------------------------------------
discrepancias <- datos %>%
  group_by(ue_id, replica, tratamiento, descripcion) %>%
  summarise(
    n_carretas = n(),
    carretas_ids = paste(no_carreta, collapse = ", "),
    
    # Análisis de Metros
    metros_min = min(metros_lineales),
    metros_max = max(metros_lineales),
    rango_metros = metros_max - metros_min, # <--- La diferencia que pediste
    pct_dif_metros = (rango_metros / metros_max) * 100,
    
    # Análisis de Pesos
    peso_min = min(peso_neto),
    peso_max = max(peso_neto),
    dif_peso_kg = peso_max - peso_min,
    cv_peso = (sd(peso_neto, na.rm = TRUE) / mean(peso_neto, na.rm = TRUE)) * 100,
    
    .groups = "drop"
  ) %>%
  # Criterios de Alerta
  mutate(
    alerta_metros = if_else(rango_metros > 2, "REVISAR METROS", "OK"),
    alerta_pesos = if_else(cv_peso > 20, "REVISAR PESO", "OK")
  ) %>%
  # Filtramos para mostrar solo donde hay algo extraño
  filter(alerta_metros != "OK" | alerta_pesos != "OK") %>%
  # Seleccionamos las columnas clave para que la tabla sea fácil de leer
  select(ue_id, carretas_ids, alerta_metros, rango_metros, pct_dif_metros, dif_peso_kg, cv_peso)

cat("==============================================================\n")
cat("   REPORTE DE DISCREPANCIAS (Diferencias detectadas en UE)    \n")
cat("==============================================================\n")
print(discrepancias, n = Inf)

# -----------------------------------------------------------------------------
# 4. TABLA RESUMEN Y ANÁLISIS DE BLOQUE
# -----------------------------------------------------------------------------
tabla_resumen <- datos %>%
  group_by(replica, tratamiento, descripcion) %>%
  summarise(
    carretas = n(),
    total_metros = sum(metros_lineales, na.rm = TRUE),
    total_peso = sum(peso_neto, na.rm = TRUE),
    tch_promedio = mean(tch, na.rm = TRUE),
    .groups = "drop"
  )

analisis_bloque <- tabla_resumen %>%
  group_by(tratamiento) %>%
  mutate(
    media_trat = mean(tch_promedio),
    desviacion_tch = tch_promedio - media_trat,
    pct_sobre_media = (desviacion_tch / media_trat) * 100
  ) %>%
  ungroup() %>%
  arrange(tratamiento, replica)

cat("\n==============================================================\n")
cat("           ANÁLISIS DE RENDIMIENTO Y EFECTO BLOQUE            \n")
cat("==============================================================\n")
print(analisis_bloque, n = Inf)


# -----------------------------------------------------------------------------
# 5. DIAGNÓSTICO DE PATRONES POR RÉPLICA (¿Dónde está el problema?)
# -----------------------------------------------------------------------------

diagnostico_replica <- datos %>%
  group_by(replica) %>%
  summarise(
    total_ue = n_distinct(ue_id),
    # Contamos cuántas UE de esta réplica aparecieron en la tabla de discrepancias
    ue_con_error = sum(ue_id %in% discrepancias$ue_id) / 2, # /2 porque cada UE tiene 2 carretas
    error_metros_avg = mean(abs(metros_lineales - mean(metros_lineales))), # Variabilidad interna
    tch_promedio_rep = mean(tch, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_error_replica = (ue_con_error / total_ue) * 100,
    status_replica = case_when(
      pct_error_replica > 50 ~ "CRÍTICO: Datos poco confiables",
      pct_error_replica > 20 ~ "OBSERVACIÓN: Mucho ruido",
      TRUE ~ "CONFIABLE"
    )
  )

cat("\n==============================================================\n")
cat("      DIAGNÓSTICO DE CALIDAD POR RÉPLICA (PATRONES)           \n")
cat("==============================================================\n")
print(diagnostico_replica)

# -----------------------------------------------------------------------------
# 6. VISUALIZACIÓN DE PATRONES (HEATMAP)
# -----------------------------------------------------------------------------
# Este gráfico te dirá visualmente si el error sigue un patrón por Réplica o Tratamiento

# Primero preparamos una matriz de error
heatmap_data <- datos %>%
  group_by(replica, tratamiento) %>%
  summarise(
    # Calculamos la discrepancia de metros promedio para ese cruce
    error_m = max(metros_lineales) - min(metros_lineales),
    .groups = "drop"
  )

ggplot(heatmap_data, aes(x = tratamiento, y = replica, fill = error_m)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "green", high = "red") +
  labs(title = "Mapa de Calor de Discrepancias (Metros)",
       subtitle = "Rojo indica donde los datos de campo están fallando más",
       fill = "Error (m)") +
  theme_minimal()


# -----------------------------------------------------------------------------
# 7. ANÁLISIS DE INFLUENCIA: DISCREPANCIA VS. DATOS RESUMIDOS
# -----------------------------------------------------------------------------

analisis_influencia <- datos %>%
  group_by(replica, tratamiento, descripcion, ue_id) %>%
  summarise(
    # Datos Resumidos (lo que ya tienes)
    tch_resumido = mean(tch, na.rm = TRUE),
    peso_total_kg = sum(peso_neto, na.rm = TRUE),
    metros_totales = sum(metros_lineales, na.rm = TRUE),
    
    # Cálculo de la Discrepancia (El error interno)
    dif_metros = max(metros_lineales) - min(metros_lineales),
    dif_peso = max(peso_neto) - min(peso_neto),
    
    # TCH de cada carreta para ver la brecha
    tch_c1 = first(tch),
    tch_c2 = last(tch),
    brecha_tch = abs(tch_c1 - tch_c2),
    
    .groups = "drop"
  ) %>%
  mutate(
    # ÍNDICE DE INFLUENCIA: Qué tanto del TCH resumido depende de una discrepancia
    # Si este % es alto, el dato resumido "miente" porque las carretas son muy distintas
    influencia_error_pct = (brecha_tch / tch_resumido) * 100,
    
    confiabilidad = case_when(
      influencia_error_pct < 5  ~ "ALTA (Carretas Similares)",
      influencia_error_pct < 15 ~ "MEDIA (Variación aceptable)",
      TRUE                      ~ "BAJA (Discrepancia domina el dato)"
    )
  ) %>%
  arrange(desc(influencia_error_pct))

cat("\n==============================================================\n")
cat("   INFLUENCIA DE LA DISCREPANCIA EN EL TCH RESUMIDO          \n")
cat("==============================================================\n")
print(analisis_influencia %>% select(ue_id, tch_resumido, brecha_tch, influencia_error_pct, confiabilidad), n = Inf)



# -----------------------------------------------------------------------------
# 8. RELACIÓN DISCREPANCIA VS VARIABILIDAD ENTRE RÉPLICAS
# -----------------------------------------------------------------------------

# Calculamos la desviación estándar del TCH entre réplicas por cada tratamiento
# Esto nos dice qué tanto varían los resultados "normales" del ensayo
variabilidad_campo <- tabla_resumen %>%
  group_by(tratamiento) %>%
  summarise(
    sd_tch_total = sd(tch_promedio), 
    media_tch = mean(tch_promedio),
    .groups = "drop"
  )

# Unimos con el análisis de influencia para ver la "Causalidad del Atípico"
diagnostico_causalidad <- analisis_influencia %>%
  left_join(variabilidad_campo, by = "tratamiento") %>%
  mutate(
    # ¿Es la brecha interna mayor que la diferencia entre réplicas?
    peso_del_error = brecha_tch / sd_tch_total
  ) %>%
  select(ue_id, tch_resumido, brecha_tch, sd_tch_total, peso_del_error, confiabilidad) %>%
  arrange(desc(peso_del_error))

cat("\n==============================================================\n")
cat("   ¿QUÉ TANTO INFLUYE EL ERROR EN LOS DATOS ATÍPICOS?        \n")
cat("==============================================================\n")
print(diagnostico_causalidad, n = Inf)




# -----------------------------------------------------------------------------
# 9. (MEJORADO) ANÁLISIS DE CAUSALIDAD: ¿METROS O PESO?
# -----------------------------------------------------------------------------
# Aquí determinamos matemáticamente qué variable "empuja" más el error del TCH

causalidad_error <- datos %>%
  group_by(ue_id) %>%
  filter(n() > 1) %>% # Solo UE con al menos 2 carretas
  summarise(
    # Diferencias absolutas entre carretas de la misma UE
    delta_tch = abs(max(tch) - min(tch)),
    delta_metros = abs(max(metros_lineales) - min(metros_lineales)),
    delta_peso = abs(max(peso_neto) - min(peso_neto)),
    
    # Promedios para normalizar
    mean_tch = mean(tch),
    mean_metros = mean(metros_lineales),
    
    # Ratios de Variación (Coeficiente de Variación Interno)
    cv_metros = (sd(metros_lineales)/mean(metros_lineales))*100,
    cv_peso = (sd(peso_neto)/mean(peso_neto))*100,
    
    .groups = "drop"
  ) %>%
  mutate(
    # DIAGNÓSTICO FINAL POR UNIDAD
    causa_principal = case_when(
      delta_tch < 5 ~ "VARIACIÓN NATURAL (No significativa)",
      cv_metros > cv_peso * 1.5 ~ "ERROR DE MUESTREO (Medición de Metros)",
      cv_peso > cv_metros * 1.5 ~ "ERROR DE CAMPO (Peso de caña desigual)",
      TRUE ~ "ERROR MIXTO (Ambos varían)"
    )
  )

cat("\n==============================================================\n")
cat("   DIAGNÓSTICO DE CAUSALIDAD POR UNIDAD EXPERIMENTAL          \n")
cat("==============================================================\n")
print(causalidad_error %>% 
        select(ue_id, delta_tch, cv_metros, cv_peso, causa_principal) %>% 
        arrange(desc(delta_tch)) %>% 
        head(15)) # Mostramos las 15 peores







# -----------------------------------------------------------------------------
# 10. DESCOMPOSICIÓN DE LA VARIANZA (CORREGIDO - MÉTODO ROBUSTO)
# -----------------------------------------------------------------------------

# 1. Calculamos la varianza interna de cada Unidad Experimental (UE)
# Usamos dplyr en lugar de tapply para evitar el error de "pureza"
varianzas_por_ue <- datos %>%
  group_by(ue_id) %>%
  summarise(
    n_datos = n(),
    # Usamos stats::var explícitamente para asegurar que es la varianza
    varianza_interna = stats::var(tch, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  # Solo podemos calcular varianza si hay más de 1 carreta (n > 1)
  filter(n_datos > 1, !is.na(varianza_interna))

# 2. Promedio de las varianzas internas (Esto es el Error de Muestreo)
var_muestreo <- mean(varianzas_por_ue$varianza_interna, na.rm = TRUE)

# 3. Varianza Total del conjunto de datos
var_total <- stats::var(datos$tch, na.rm = TRUE)

# 4. Cálculo de porcentajes (Evitando negativos si la estimación es ruidosa)
var_experimental <- max(0, var_total - var_muestreo) # Si da negativo, asumimos 0

pct_muestreo <- (var_muestreo / var_total) * 100
pct_experimental <- (var_experimental / var_total) * 100

# 5. REPORTE FINAL
cat("\n==============================================================\n")
cat("          VEREDICTO FINAL: FUENTES DE VARIACIÓN               \n")
cat("==============================================================\n")
cat(sprintf("VARIANZA TOTAL DEL SISTEMA:  %.2f\n", var_total))
cat("--------------------------------------------------------------\n")
cat(sprintf("1. ERROR DE MUESTREO (Carretas):   %.2f%%\n", pct_muestreo))
cat("   (Culpa de: Medición de metros, carga desigual en carretas)\n")
cat("--------------------------------------------------------------\n")
cat(sprintf("2. ERROR EXPERIMENTAL (Lugar):     %.2f%%\n", pct_experimental))
cat("   (Culpa de: Suelo, Tratamientos, Clima, Riego)\n")
cat("--------------------------------------------------------------\n")

if(is.nan(pct_muestreo)) {
  cat("\nERROR: No se pudo calcular. Revisa si tus datos de TCH son todos iguales.\n")
} else if(pct_muestreo > 30) {
  cat("\n!!! ALERTA CRÍTICA !!!\n")
  cat("El Error de Muestreo es muy alto (>30%).\n")
  cat("Tus diferencias de TCH se deben a mala medición de metros o carga desigual.\n")
} else {
  cat("\nCONCLUSIÓN: Tu muestreo es consistente. La variación es real del campo.\n")
}

# -----------------------------------------------------------------------------
# 10.1. FUNDAMENTO TÉCNICO: DESCOMPOSICIÓN DEL ERROR (MUESTREO VS SITIO)
# -----------------------------------------------------------------------------

detalle_fundamento <- datos %>%
  group_by(ue_id, replica, tratamiento) %>%
  summarise(
    n_carretas = n(),
    # Varianza interna (Error de muestreo/medición en esta parcela)
    varianza_muestreo_ue = stats::var(tch, na.rm = TRUE),
    
    # Análisis de los dos componentes del Error de Medición
    cv_metros = (stats::sd(metros_lineales)/mean(metros_lineales)) * 100,
    cv_peso   = (stats::sd(peso_neto)/mean(peso_neto)) * 100,
    
    dif_metros = max(metros_lineales) - min(metros_lineales),
    dif_peso   = max(peso_neto) - min(peso_neto),
    .groups = "drop"
  ) %>%
  filter(n_carretas > 1) %>%
  mutate(
    # ¿Qué tanto aporta esta unidad al error operativo total?
    contribucion_error_pct = (varianza_muestreo_ue / sum(varianza_muestreo_ue, na.rm = TRUE)) * 100,
    
    # Clasificación detallada del Error de Medición/Muestreo
    tipo_error_medicion = case_when(
      varianza_muestreo_ue < 5 ~ "Mínimo (Variación aceptable)",
      cv_metros > (cv_peso * 1.5) ~ "Medición: Metros Lineales (Error Humano)",
      cv_peso > (cv_metros * 1.5) ~ "Medición: Carga Desigual (Error Operativo)",
      TRUE ~ "Medición: Mixto (Metros y Peso)"
    )
  ) %>%
  arrange(desc(varianza_muestreo_ue))

# -----------------------------------------------------------------------------
# 10.2. COMPARATIVA FINAL: ERROR OPERATIVO VS ERROR NATURAL DEL SITIO
# -----------------------------------------------------------------------------

# Calculamos la varianza total de los promedios (Error del Sitio + Tratamiento)
var_total_experimento <- stats::var(tabla_resumen$tch_promedio, na.rm = TRUE)
error_muestreo_global <- mean(detalle_fundamento$varianza_muestreo_ue, na.rm = TRUE)

cat("\n========================================================================\n")
cat("   BALANCE GLOBAL: ¿DÓNDE ESTÁ EL ORIGEN DE LA VARIACIÓN?               \n")
cat("========================================================================\n")
cat(sprintf("ERROR DE MUESTREO (Medición/Operativo): %.2f%%\n", 
            (error_muestreo_global / (error_muestreo_global + var_total_experimento)) * 100))
cat(sprintf("ERROR EXPERIMENTAL (Variación Natural del Sitio): %.2f%%\n", 
            (var_total_experimento / (error_muestreo_global + var_total_experimento)) * 100))
cat("------------------------------------------------------------------------\n")

cat("\n========================================================================\n")
cat("   FUNDAMENTO DETALLADO: TOP 10 UEs CON MAYOR ERROR DE MEDICIÓN\n")
cat("========================================================================\n")
print(detalle_fundamento %>% 
        select(ue_id, varianza_muestreo_ue, contribucion_error_pct, tipo_error_medicion, dif_metros, dif_peso) %>%
        head(10))

# -----------------------------------------------------------------------------
# 10.3. RESUMEN EJECUTIVO PARA TOMA DE DECISIONES
# -----------------------------------------------------------------------------
resumen_medicion <- detalle_fundamento %>%
  filter(varianza_muestreo_ue >= 5) %>%
  count(tipo_error_medicion) %>%
  mutate(pct_casos = n / sum(n) * 100)

cat("\n--- FRECUENCIA DE ERRORES DE MEDICIÓN DETECTADOS ---\n")
print(resumen_medicion)



# -----------------------------------------------------------------------------
# 11. GRÁFICO DE EVIDENCIA: TCH VS ERROR DE MEDICIÓN
# -----------------------------------------------------------------------------
# Este gráfico es el fundamento visual: 
# Si los puntos suben en diagonal, la culpa es de la mala medición de metros.

grafico_evidencia <- ggplot(detalle_fundamento, aes(x = dif_metros, y = varianza_muestreo_ue)) +
  geom_point(aes(color = tipo_error_medicion, size = contribucion_error_pct), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dotted") +
  scale_color_manual(values = c(
    "Medición: Metros Lineales (Error Humano)" = "#E41A1C",
    "Medición: Carga Desigual (Error Operativo)" = "#377EB8",
    "Medición: Mixto (Metros y Peso)" = "#984EA3",
    "Mínimo (Variación aceptable)" = "#4DAF4A"
  )) +
  labs(
    title = "EVIDENCIA FÍSICA DEL ERROR DE MEDICIÓN",
    subtitle = "Correlación entre fallas de medición (metros) y el ruido del experimento",
    x = "Diferencia de Metros entre carretas (m)",
    y = "Varianza del TCH (Ruido Interno)",
    color = "Causa del Error",
    size = "% Contribución al Error Total"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", legend.direction = "vertical")

print(grafico_evidencia)



# Ejecutamos un modelo lineal rápido para extraer la varianza residual (experimental)
# Usamos los promedios por unidad (tabla_resumen)
modelo_sitio <- lm(tch_promedio ~ tratamiento + replica, data = tabla_resumen)
var_residual_sitio <- deviance(modelo_sitio) / df.residual(modelo_sitio)

# Varianza de Tratamientos (Señal que buscamos)
var_tratamientos <- stats::var(tabla_resumen$tch_promedio, na.rm = TRUE)

# Creamos el dataframe para el gráfico de barras de comparación
df_pesos <- data.frame(
  Fuente = c("Error de Medición (Carretas)", "Error Residual (Sitio/Campo)", "Variación Tratamientos"),
  Varianza = c(error_muestreo_global, var_residual_sitio, var_tratamientos)
) %>%
  mutate(Porcentaje = (Varianza / sum(Varianza)) * 100)

# Gráfico de Pesos de Variación
grafico_pesos <- ggplot(df_pesos, aes(x = reorder(Fuente, Porcentaje), y = Porcentaje, fill = Fuente)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", Porcentaje)), vjust = -0.5, fontface = "bold") +
  scale_fill_manual(values = c("#E41A1C", "#377EB8", "#4DAF4A")) +
  labs(
    title = "COMPOSICIÓN DEL ERROR TOTAL DEL EXPERIMENTO",
    subtitle = "¿Qué tiene más peso: la mala medición o la variabilidad del campo?",
    x = "Fuente de Variación",
    y = "Porcentaje del Peso Total (%)"
  ) +
  theme_minimal()

print(grafico_pesos)

# -----------------------------------------------------------------------------
# 12. TABLA PARA EXPORTAR (RESUMEN EJECUTIVO Y ACCIÓN)
# -----------------------------------------------------------------------------
# Esta tabla clasifica las UEs según la gravedad de su error de medición.

resumen_accion <- detalle_fundamento %>%
  mutate(
    prioridad = case_when(
      contribucion_error_pct > 10 ~ "1. CRÍTICA (Revisar/Eliminar)",
      contribucion_error_pct > 5  ~ "2. ALTA (Auditar datos)",
      varianza_muestreo_ue > 15   ~ "3. MEDIA (Ruido considerable)",
      TRUE ~ "4. BAJA (Aceptable)"
    )
  ) %>%
  select(prioridad, ue_id, varianza_muestreo_ue, contribucion_error_pct, tipo_error_medicion, dif_metros, dif_peso) %>%
  arrange(prioridad, desc(contribucion_error_pct))

cat("\n========================================================================\n")
cat("   LISTA NEGRA: CLASIFICACIÓN DE UEs POR ERROR DE MEDICIÓN (OPERATIVO)  \n")
cat("   (Fundamento técnico para limpieza de datos)                          \n")
cat("========================================================================\n")
print(resumen_accion, n = Inf)

# Opcional: Guardar el reporte en el portapapeles para pegar en Excel
# write_clip(resumen_accion)


# Esta tabla ahora te dice cuánto "ensucia" cada unidad el error del experimento

resumen_accion_final <- detalle_fundamento %>%
  mutate(
    # Si la varianza de medición es mayor que el residual del sitio, la unidad es inaceptable
    ratio_error = varianza_muestreo_ue / var_residual_sitio,
    impacto_en_precision = case_when(
      ratio_error > 2   ~ "1. CRÍTICO: Supera error de campo",
      ratio_error > 1   ~ "2. ALTO: Igual al error de campo",
      ratio_error > 0.5 ~ "3. MEDIO: Ruido tolerable",
      TRUE              ~ "4. BAJO: Medición limpia"
    )
  ) %>%
  select(impacto_en_precision, ue_id, varianza_muestreo_ue, ratio_error, tipo_error_medicion, dif_metros, dif_peso) %>%
  arrange(desc(ratio_error))

cat("\n========================================================================\n")
cat("   COMPARATIVA TÉCNICA: ERROR DE MEDICIÓN VS ERROR RESIDUAL (CAMPO)     \n")
cat(sprintf("   Varianza Residual del Campo: %.2f | Varianza de Medición Avg: %.2f\n", 
            var_residual_sitio, error_muestreo_global))
cat("========================================================================\n")
print(resumen_accion_final, n = Inf)

# -----------------------------------------------------------------------------
# 13. GRÁFICO DE EVIDENCIA MULTIVARIABLE (METROS + PESO + RUIDO)
# -----------------------------------------------------------------------------
# Incluimos la línea de corte del residual para ver visualmente quiénes sobran

grafico_diagnostico <- ggplot(detalle_fundamento, aes(x = cv_metros, y = cv_peso)) +
  geom_point(aes(size = varianza_muestreo_ue, color = varianza_muestreo_ue > var_residual_sitio), alpha = 0.7) +
  geom_vline(xintercept = 5, linetype = "dashed", color = "gray") +
  geom_hline(yintercept = 5, linetype = "dashed", color = "gray") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "darkgreen"), 
                     labels = c("TRUE" = "Error > Ruido de Campo", "FALSE" = "Error < Ruido de Campo")) +
  labs(
    title = "MATRIZ DE CALIDAD DE MEDICIÓN",
    subtitle = "Puntos rojos: La mala medición genera más ruido que el propio campo",
    x = "Inconsistencia en Metros (CV %)",
    y = "Inconsistencia en Carga/Peso (CV %)",
    size = "Varianza TCH",
    color = "Estado de la UE"
  ) +
  theme_minimal()

print(grafico_diagnostico)

