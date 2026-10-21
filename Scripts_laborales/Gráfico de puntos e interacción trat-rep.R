# ==============================================================================
# SCRIPT DE ANÁLISIS AGRONÓMICO: MULTI-MÉTRICA + ZONAS DINÁMICAS (ANCLADO Y FACETADO)
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. PAQUETES DE R
# ------------------------------------------------------------------------------
paquetes <- c("dplyr", "tidyr", "ggplot2", "plotly", "stringr")
no_instalados <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
if(length(no_instalados)) install.packages(no_instalados)
lapply(paquetes, library, character.only = TRUE)

# ==============================================================================
# 1. PANEL DE CONFIGURACIÓN
# ==============================================================================
VARIABLE_A_EVALUAR   <- "Rendimiento"               # Nombre de la variable a graficar
COLUMNA_TRATAMIENTOS <- "Dosis"       # Nombre exacto de tu columna de tratamientos
TESTIGO_A_BUSCAR     <- "0"   # Nombre exacto del tratamiento control

# --- OPCIONES DE VISUALIZACIÓN ---
MOSTRAR_DATOS        <- "TODOS"     # Opciones: "TODOS" (puntos crudos) o "MEDIAS" (promedios)
VARIABLE_COLOR       <- "Zafra"     # Columna de agrupación (ej: "Zafra", "Finca")

# ==============================================================================
# 2. LECTURA DE DATOS
# ==============================================================================
message(">>> POR FAVOR, COPIA TU MATRIZ DE DATOS AL PORTAPAPELES AHORA <<<")
invisible(readline(prompt="Presiona [Enter] cuando hayas copiado los datos de Excel..."))

df_raw <- read.delim("clipboard", sep = "\t", header = TRUE, fill = TRUE, quote = "")

names(df_raw)
View(df_raw)
# ==============================================================================
# 3. FUNCIÓN DE DETECCIÓN MEJORADA Y ASIGNACIÓN DE COLUMNAS
# ==============================================================================
buscar_columna_flexible <- function(nombres, patron) {
  coincidencia_exacta <- grep(paste0("^(", patron, ")$"), nombres, ignore.case = TRUE, value = TRUE)
  if (length(coincidencia_exacta) > 0) return(coincidencia_exacta[1])
  
  coincidencia_parcial <- grep(patron, nombres, ignore.case = TRUE, value = TRUE)
  if (length(coincidencia_parcial) > 0) return(coincidencia_parcial[1])
  
  return(NULL)
}

c_trat  <- buscar_columna_flexible(names(df_raw), COLUMNA_TRATAMIENTOS)
c_rep   <- buscar_columna_flexible(names(df_raw), "replica|rep") 
c_var   <- buscar_columna_flexible(names(df_raw), VARIABLE_A_EVALUAR)
c_color <- buscar_columna_flexible(names(df_raw), VARIABLE_COLOR)

if(is.null(c_trat)) stop(paste0("Error: No se encontró la columna de tratamientos llamada '", COLUMNA_TRATAMIENTOS, "'."))
if(is.null(c_rep))  stop("Error: No se encontró la columna 'Replica'.")
if(is.null(c_var))  stop(paste0("Error: No se encontró la variable '", VARIABLE_A_EVALUAR, "'."))

if(is.null(c_color)) {
  c_color <- c_trat
  message(paste0("-> Nota: No se halló la columna '", VARIABLE_COLOR, "', se coloreará por Tratamiento."))
}

# ==============================================================================
# 4. PROCESAMIENTO DE DATOS 
# ==============================================================================
df_trabajo <- df_raw
df_trabajo$Trat_Orig  <- trimws(as.character(df_raw[[c_trat]]))
df_trabajo$Rep_Orig   <- trimws(as.character(df_raw[[c_rep]]))
df_trabajo$Color_Orig <- trimws(as.character(df_raw[[c_color]]))
df_trabajo$Var_Orig   <- as.numeric(as.character(df_raw[[c_var]]))

df_trabajo <- df_trabajo %>% filter(!is.na(Var_Orig))

df_resumen <- df_trabajo %>%
  group_by(Trat_Orig, Rep_Orig, Color_Orig) %>%
  summarise(Valor_Original = mean(Var_Orig, na.rm = TRUE), .groups = 'drop')

# ==============================================================================
# 5. DETECCIÓN BLINDADA DEL TESTIGO
# ==============================================================================
testigo_limpio <- trimws(TESTIGO_A_BUSCAR)
todos_los_tratamientos <- unique(df_trabajo$Trat_Orig)
coincidencia_testigo <- grep(testigo_limpio, todos_los_tratamientos, ignore.case = TRUE, value = TRUE)

if (length(coincidencia_testigo) == 0) {
  stop(paste0("\nERROR CRÍTICO: No se encontró el testigo '", testigo_limpio, "'.\n",
              "---> R detectó estos tratamientos en tu columna '", COLUMNA_TRATAMIENTOS, "':\n",
              paste(paste0("[", todos_los_tratamientos, "]"), collapse = ", ")))
}

testigo_exacto <- coincidencia_testigo[1]

# ==============================================================================
# 6. CÁLCULOS DE LÍNEA BASE DINÁMICA (CON ANCLAJE DE TESTIGO)
# ==============================================================================
testigo_local <- df_trabajo %>%
  filter(Trat_Orig == testigo_exacto) %>%
  group_by(Color_Orig) %>%
  summarise(Testigo_Local_Media = mean(Var_Orig, na.rm = TRUE), .groups = 'drop')

# Unimos la media local y forzamos al testigo a anclarse en 1 (Relativo) y 0 (Diferencia)
df_trabajo <- df_trabajo %>% 
  left_join(testigo_local, by = "Color_Orig") %>%
  mutate(
    Valor_Relativo = ifelse(Trat_Orig == testigo_exacto, 1, Var_Orig / Testigo_Local_Media),
    Valor_Diferencia = ifelse(Trat_Orig == testigo_exacto, 0, Var_Orig - Testigo_Local_Media)
  )

df_resumen <- df_resumen %>% 
  left_join(testigo_local, by = "Color_Orig") %>%
  mutate(
    Valor_Relativo = ifelse(Trat_Orig == testigo_exacto, 1, Valor_Original / Testigo_Local_Media),
    Valor_Diferencia = ifelse(Trat_Orig == testigo_exacto, 0, Valor_Original - Testigo_Local_Media)
  )

if (toupper(MOSTRAR_DATOS) == "TODOS") {
  df_plot <- df_trabajo %>% rename(V_Rel = Valor_Relativo, V_Diff = Valor_Diferencia, V_Real = Var_Orig)
} else {
  df_plot <- df_resumen %>% rename(V_Rel = Valor_Relativo, V_Diff = Valor_Diferencia, V_Real = Valor_Original)
}

df_plot <- df_plot %>%
  mutate(Clasificacion = ifelse(Trat_Orig == testigo_exacto, 
                                "★ EL TESTIGO (Línea Base)", 
                                "TRATAMIENTO EVALUADO"))

# ==============================================================================
# 7. CONSTRUCCIÓN DE LOS GRÁFICOS (EJES NOMBRADOS Y TOOLTIPS)
# ==============================================================================
# GRÁFICO 1: RELATIVO GLOBAL
p1 <- ggplot(df_plot, aes(x = Trat_Orig, y = V_Rel, color = Color_Orig)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#E91E63", linewidth = 1) +
  geom_jitter(width = 0.15, size = 3.5, alpha = 0.8, height = 0, 
              aes(text = paste("Tratamiento:", Trat_Orig,
                               "<br>Grupo:", Color_Orig,
                               "<br>Réplica:", Rep_Orig,
                               "<br>Valor Relativo:", round(V_Rel, 3)))) +
  labs(title = paste("MÉTODO 1A: Relativo Global (Base 1 Dinámica -", c_var, ")"),
       x = "Tratamientos", y = paste("Valor Relativo (", c_var, ")"), color = "Grupos") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# GRÁFICO 1B: RELATIVO FACETADO (NUEVO)
p1b <- ggplot(df_plot, aes(x = Trat_Orig, y = V_Rel, color = Clasificacion)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "#E91E63", linewidth = 1) +
  geom_jitter(width = 0.15, size = 3.5, alpha = 0.8, height = 0, 
              aes(text = paste("Tratamiento:", Trat_Orig,
                               "<br>Grupo:", Color_Orig,
                               "<br>Réplica:", Rep_Orig,
                               "<br>Valor Relativo:", round(V_Rel, 3)))) +
  facet_wrap(~ Color_Orig, scales = "free_x") +
  scale_color_manual(values = c("★ EL TESTIGO (Línea Base)" = "#F39C12",  
                                "TRATAMIENTO EVALUADO" = "#34495E")) +
  labs(title = paste("MÉTODO 1B: Relativo Facetado (", c_var, ")"),
       x = "Tratamientos", y = paste("Valor Relativo (", c_var, ")"), color = "Clasificación") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
        strip.text = element_text(face = "bold", size = 11, color = "white"),
        strip.background = element_rect(fill = "#2C3E50"))

# GRÁFICO 2: DIFERENCIA
p2 <- ggplot(df_plot, aes(x = Trat_Orig, y = V_Diff, color = Color_Orig)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#17A589", linewidth = 1) +
  geom_jitter(width = 0.15, size = 3.5, alpha = 0.8, height = 0,
              aes(text = paste("Tratamiento:", Trat_Orig,
                               "<br>Grupo:", Color_Orig,
                               "<br>Réplica:", Rep_Orig,
                               "<br>Diferencia Real:", round(V_Diff, 2)))) +
  labs(title = paste("MÉTODO 2: Pérdidas y Ganancias (", c_var, ")"),
       x = "Tratamientos", y = paste("Diferencia vs Testigo Local (", c_var, ")"), color = "Grupos") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# GRÁFICO 3: INTERACCIÓN VALORES REALES
p3 <- ggplot(df_resumen, aes(x = Trat_Orig, y = Valor_Original, group = Rep_Orig, color = Rep_Orig)) +
  geom_line(linewidth = 1.2, alpha = 0.7) + 
  geom_point(size = 3.5,
             aes(text = paste("Tratamiento:", Trat_Orig,
                              "<br>Grupo:", Color_Orig,
                              "<br>Réplica:", Rep_Orig,
                              "<br>Valor Real:", round(Valor_Original, 2)))) +
  labs(title = paste("Interacción: Valores Originales (", c_var, ")"),
       x = "Tratamientos", y = paste("Valor Real (", c_var, ")"), color = "Réplicas") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# GRÁFICO 4: INTERACCIÓN DIFERENCIA
p4 <- ggplot(df_resumen, aes(x = Trat_Orig, y = Valor_Diferencia, group = Rep_Orig, color = Rep_Orig)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(linewidth = 1.2, alpha = 0.7) + 
  geom_point(size = 3.5,
             aes(text = paste("Tratamiento:", Trat_Orig,
                              "<br>Grupo:", Color_Orig,
                              "<br>Réplica:", Rep_Orig,
                              "<br>Pérdida/Ganancia:", round(Valor_Diferencia, 2)))) +
  labs(title = paste("Interacción: Pérdidas/Ganancias (", c_var, ")"),
       x = "Tratamientos", y = paste("Diferencia vs Testigo (", c_var, ")"), color = "Réplicas") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# GRÁFICO 5: PUNTOS REALES Y ZONAS GLOBALES
p5 <- ggplot(df_plot, aes(x = Trat_Orig, y = V_Real)) +
  geom_jitter(width = 0.15, size = 4, alpha = 0.85, height = 0, aes(color = Color_Orig,
                                                                    text = paste("Tratamiento:", Trat_Orig,
                                                                                 "<br>Grupo:", Color_Orig,
                                                                                 "<br>Réplica:", Rep_Orig,
                                                                                 "<br>Valor Real:", round(V_Real, 2)))) +
  labs(title = paste("MÉTODO 3: Visualización de Puntos y Rangos (", c_var, ")"),
       x = "Tratamientos", y = paste("Valor Real (", c_var, ")"), color = "Grupos") + 
  theme_minimal() + theme(axis.text.x = element_text(angle = 35, hjust = 1))

# ==============================================================================
# 8. GRÁFICO 6: ZONAS DINÁMICAS (TESTIGO LOCAL Y FONDOS COLOREADOS)
# ==============================================================================
limite_max <- max(df_plot$V_Real, na.rm = TRUE) * 1.10
limite_min <- min(df_plot$V_Real, na.rm = TRUE) * 0.90

p6 <- ggplot() +
  geom_rect(data = testigo_local, aes(xmin = 0, xmax = Inf, ymin = Testigo_Local_Media, ymax = limite_max), fill = "#2ECC71", alpha = 0.15, inherit.aes = FALSE) +
  geom_rect(data = testigo_local, aes(xmin = 0, xmax = Inf, ymin = limite_min, ymax = Testigo_Local_Media), fill = "#E74C3C", alpha = 0.15, inherit.aes = FALSE) +
  
  geom_hline(data = testigo_local, aes(yintercept = Testigo_Local_Media), linetype = "dashed", color = "#2C3E50", linewidth = 1) +
  
  geom_jitter(data = df_plot, width = 0.15, size = 3.5, alpha = 0.9, height = 0,
              aes(x = Trat_Orig, y = V_Real, color = Clasificacion,
                  text = paste("Tratamiento:", Trat_Orig,
                               "<br>Grupo:", Color_Orig,
                               "<br>Réplica:", Rep_Orig,
                               "<br>Valor Real:", round(V_Real, 2),
                               "<br>Dif. vs Testigo Local:", round(V_Diff, 2)))) +
  
  facet_wrap(~ Color_Orig, scales = "free_x") +
  
  scale_color_manual(values = c("★ EL TESTIGO (Línea Base)" = "#F39C12",  
                                "TRATAMIENTO EVALUADO" = "#34495E")) +     
  
  labs(title = paste("MÉTODO 4: Ganancias y Pérdidas Dinámicas en Valores Reales (", c_var, ")"),
       subtitle = "La línea base (el testigo) se recalcula automáticamente por cada bloque/grupo",
       x = "Tratamientos", y = paste("Valor Real (", c_var, ")"), color = "Clasificación") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13), 
        axis.text.x = element_text(angle = 35, hjust = 1, face = "bold"),
        strip.text = element_text(face = "bold", size = 11, color = "white"),
        strip.background = element_rect(fill = "#2C3E50"))

# ==============================================================================
# 9. DESPLIEGUE INTERACTIVO
# ==============================================================================
message(paste0("\n>>> Variable trazada exitosamente: ", c_var, " <<<"))
message("> Renderizando gráficos interactivos...")

print(ggplotly(p1, tooltip = "text"))
print(ggplotly(p1b, tooltip = "text"))  # <-- EL NUEVO GRÁFICO RELATIVO FACETADO
print(ggplotly(p2, tooltip = "text"))
print(ggplotly(p3, tooltip = "text"))
print(ggplotly(p4, tooltip = "text"))
print(ggplotly(p5, tooltip = "text"))
print(ggplotly(p6, tooltip = "text"))

message("*** ANÁLISIS GLOBAL MULTI-MÉTRICA FINALIZADO ***")

